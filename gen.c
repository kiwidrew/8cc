// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include "8cc.h"

bool dumpstack = false;
bool dumpsource = true;

static char *REGS[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
static char *SREGS[] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};
static char *MREGS[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static int TAB = 8;
static Vector *functions = &EMPTY_VECTOR;
static int stackpos;
static int numgp;
static int numfp;
static FILE *outputfp;
static Map *source_files = &EMPTY_MAP;
static Map *source_lines = &EMPTY_MAP;
static char *last_loc = "";

static void emit_addr(Node *node);
static void emit_expr(Node *node);
static void emit_decl_init(Vector *inits, int off, int totalsize);
static void do_emit_data(Vector *inits, int size, int off, int depth);
static void emit_data(Node *v, int off, int depth);

#define REGAREA_SIZE 176

#define emit(...)        emitf(__LINE__, "\t" __VA_ARGS__)
#define emit_noindent(...)  emitf(__LINE__, __VA_ARGS__)

#ifdef __GNUC__
#define SAVE                                                            \
    int save_hook __attribute__((unused, cleanup(pop_function)));       \
    if (dumpstack)                                                      \
        vec_push(functions, (void *)__func__);

static void pop_function(void *ignore) {
    if (dumpstack)
        vec_pop(functions);
}
#else
#define SAVE
#endif

static char *get_caller_list() {
    Buffer *b = make_buffer();
    for (int i = 0; i < vec_len(functions); i++) {
        if (i > 0)
            buf_printf(b, " -> ");
        buf_printf(b, "%s", vec_get(functions, i));
    }
    buf_write(b, '\0');
    return buf_body(b);
}

void set_output_file(FILE *fp) {
    outputfp = fp;
}

void close_output_file() {
    fclose(outputfp);
}

static void emitf(int line, char *fmt, ...) {
    // Replace "#" with "%%" so that vfprintf prints out "#" as "%".
    char buf[256];
    int i = 0;
    for (char *p = fmt; *p; p++) {
        assert(i < sizeof(buf) - 3);
        if (*p == '#') {
            buf[i++] = '%';
            buf[i++] = '%';
        } else {
            buf[i++] = *p;
        }
    }
    buf[i] = '\0';

    va_list args;
    va_start(args, fmt);
    int col = vfprintf(outputfp, buf, args);
    va_end(args);

    if (dumpstack) {
        for (char *p = fmt; *p; p++)
            if (*p == '\t')
                col += TAB - 1;
        int space = (28 - col) > 0 ? (30 - col) : 2;
        fprintf(outputfp, "%*c %s:%d", space, '#', get_caller_list(), line);
    }
    fprintf(outputfp, "\n");
}

static void emit_nostack(char *fmt, ...) {
    fprintf(outputfp, "\t");
    va_list args;
    va_start(args, fmt);
    vfprintf(outputfp, fmt, args);
    va_end(args);
    fprintf(outputfp, "\n");
}

static char *get_int_reg(Type *ty, char r) {
    assert(r == 'a' || r == 'c');
    switch (ty->size) {
    case 1: return (r == 'a') ? "al" : "cl";
    case 2: return (r == 'a') ? "ax" : "cx";
    case 4: return (r == 'a') ? "eax" : "ecx";
    case 8: return (r == 'a') ? "rax" : "rcx";
    default:
        error("Unknown data size: %s: %d", ty2s(ty), ty->size);
    }
}

static char *get_load_inst(Type *ty) {
    switch (ty->size) {
    case 1: return "movsbq";
    case 2: return "movswq";
    case 4: return "movslq";
    case 8: return "mov";
    default:
        error("Unknown data size: %s: %d", ty2s(ty), ty->size);
    }
}

static int align(int n, int m) {
    int rem = n % m;
    return (rem == 0) ? n : n - rem + m;
}

static void push_xmm(int reg) {
    SAVE;
    emit("sub $8, #rsp");
    emit("movsd #xmm%d, (#rsp)", reg);
    stackpos += 8;
}

static void pop_xmm(int reg) {
    SAVE;
    emit("movsd (#rsp), #xmm%d", reg);
    emit("add $8, #rsp");
    stackpos -= 8;
    assert(stackpos >= 0);
}

static void push(char *reg) {
    SAVE;
    emit("push #%s", reg);
    stackpos += 8;
}

static void pop(char *reg) {
    SAVE;
    emit("pop #%s", reg);
    stackpos -= 8;
    assert(stackpos >= 0);
}

static int push_struct(int size) {
    SAVE;
    int aligned = align(size, 8);
    emit("sub $%d, #rsp", aligned);
    emit("mov #rcx, -8(#rsp)");
    emit("mov #r11, -16(#rsp)");
    emit("mov #rax, #rcx");
    int i = 0;
    for (; i < size; i += 8) {
        emit("movq %d(#rcx), #r11", i);
        emit("mov #r11, %d(#rsp)", i);
    }
    for (; i < size; i += 4) {
        emit("movl %d(#rcx), #r11", i);
        emit("movl #r11d, %d(#rsp)", i);
    }
    for (; i < size; i++) {
        emit("movb %d(#rcx), #r11", i);
        emit("movb #r11b, %d(#rsp)", i);
    }
    emit("mov -8(#rsp), #rcx");
    emit("mov -16(#rsp), #r11");
    stackpos += aligned;
    return aligned;
}

static void maybe_emit_bitshift_load(Type *ty) {
    SAVE;
    if (ty->bitsize <= 0)
        return;
    emit("shr $%d, #rax", ty->bitoff);
    push("rcx");
    emit("mov $0x%lx, #rcx", (1 << (long)ty->bitsize) - 1);
    emit("and #rcx, #rax");
    pop("rcx");
}

static void maybe_emit_bitshift_save(Type *ty, char *addr) {
    SAVE;
    if (ty->bitsize <= 0)
        return;
    push("rcx");
    push("rdi");
    emit("mov $0x%lx, #rdi", (1 << (long)ty->bitsize) - 1);
    emit("and #rdi, #rax");
    emit("shl $%d, #rax", ty->bitoff);
    emit("mov %s, #%s", addr, get_int_reg(ty, 'c'));
    emit("mov $0x%lx, #rdi", ~(((1 << (long)ty->bitsize) - 1) << ty->bitoff));
    emit("and #rdi, #rcx");
    emit("or #rcx, #rax");
    pop("rdi");
    pop("rcx");
}

static void emit_gload(Type *ty, char *label, int off) {
    SAVE;
    if (ty->kind == KIND_ARRAY) {
        if (off)
            emit("lea %s+%d(#rip), #rax", label, off);
        else
            emit("lea %s(#rip), #rax", label);
        return;
    }
    char *inst = get_load_inst(ty);
    emit("%s %s+%d(#rip), #rax", inst, label, off);
    maybe_emit_bitshift_load(ty);
}

static void emit_intcast(Type *ty) {
    switch(ty->kind) {
    case KIND_BOOL:
    case KIND_CHAR:
        ty->usig ? emit("movzbq #al, #rax") : emit("movsbq #al, #rax");
        return;
    case KIND_SHORT:
        ty->usig ? emit("movzwq #ax, #rax") : emit("movswq #ax, #rax");
        return;
    case KIND_INT:
        ty->usig ? emit("mov #eax, #eax") : emit("cltq");
        return;
    case KIND_LONG:
    case KIND_LLONG:
        return;
    }
}

static void emit_toint(Type *ty) {
    SAVE;
    if (ty->kind == KIND_FLOAT)
        emit("cvttss2si #xmm0, #eax");
    else if (ty->kind == KIND_DOUBLE)
        emit("cvttsd2si #xmm0, #eax");
}

static void emit_lload(Type *ty, char *base, int off) {
    SAVE;
    if (ty->kind == KIND_ARRAY) {
        emit("lea %d(#%s), #rax", off, base);
    } else if (ty->kind == KIND_FLOAT) {
        emit("movss %d(#%s), #xmm0", off, base);
    } else if (ty->kind == KIND_DOUBLE || ty->kind == KIND_LDOUBLE) {
        emit("movsd %d(#%s), #xmm0", off, base);
    } else {
        char *inst = get_load_inst(ty);
        emit("%s %d(#%s), #rax", inst, off, base);
        maybe_emit_bitshift_load(ty);
    }
}

static void maybe_convert_bool(Type *ty) {
    if (ty->kind == KIND_BOOL) {
        emit("test #rax, #rax");
        emit("setne #al");
    }
}

static void emit_gsave(char *varname, Type *ty, int off) {
    SAVE;
    assert(ty->kind != KIND_ARRAY);
    maybe_convert_bool(ty);
    char *reg = get_int_reg(ty, 'a');
    char *addr = format("%s+%d(%%rip)", varname, off);
    maybe_emit_bitshift_save(ty, addr);
    emit("mov #%s, %s", reg, addr);
}

static void emit_lsave(Type *ty, int off) {
    SAVE;
    if (ty->kind == KIND_FLOAT) {
        emit("movss #xmm0, %d(#rbp)", off);
    } else if (ty->kind == KIND_DOUBLE) {
        emit("movsd #xmm0, %d(#rbp)", off);
    } else {
        maybe_convert_bool(ty);
        char *reg = get_int_reg(ty, 'a');
        char *addr = format("%d(%%rbp)", off);
        maybe_emit_bitshift_save(ty, addr);
        emit("mov #%s, %s", reg, addr);
    }
}

static void do_emit_assign_deref(Type *ty, int off) {
    SAVE;
    emit("mov (#rsp), #rcx");
    char *reg = get_int_reg(ty, 'c');
    if (off)
        emit("mov #%s, %d(#rax)", reg, off);
    else
        emit("mov #%s, (#rax)", reg);
    pop("rax");
}

static void emit_assign_deref(Node *var) {
    SAVE;
    push("rax");
    emit_expr(node_operand(var));
    do_emit_assign_deref(node_type(node_operand(var))->ptr, 0);
}

static void emit_pointer_arith(Node *n) {
    SAVE;
    assert(node_type(n)->kind == KIND_PTR);
    Node *left = node_left(n);
    Node *right = node_right(n);
    emit_expr(left);
    push("rcx");
    push("rax");
    emit_expr(right);
    int size = node_type(left)->ptr->size;
    if (size > 1)
        emit("imul $%d, #rax", size);
    emit("mov #rax, #rcx");
    pop("rax");
    switch (node_kind(n)) {
    case '+': emit("add #rcx, #rax"); break;
    case '-': emit("sub #rcx, #rax"); break;
    default: error("invalid operator '%d'", node_kind(n));
    }
    pop("rcx");
}

static void emit_zero_filler(int start, int end) {
    SAVE;
    for (; start <= end - 4; start += 4)
        emit("movl $0, %d(#rbp)", start);
    for (; start < end; start++)
        emit("movb $0, %d(#rbp)", start);
}

static void ensure_lvar_init(Node *node) {
    SAVE;
    assert(node_kind(node) == AST_LVAR);
    if (node_lvarinit(node))
        emit_decl_init(node_lvarinit(node), node_loff(node), node_type(node)->size);
    node_set_lvarinit(node, NULL);
}

static void emit_assign_struct_ref(Node *struc, Type *field, int off) {
    SAVE;
    switch (node_kind(struc)) {
    case AST_LVAR:
        ensure_lvar_init(struc);
        emit_lsave(field, node_loff(struc) + field->offset + off);
        break;
    case AST_GVAR:
        emit_gsave(node_glabel(struc), field, field->offset + off);
        break;
    case AST_STRUCT_REF:
        emit_assign_struct_ref(node_struct_ref_struc(struc), field, off + node_type(struc)->offset);
        break;
    case AST_DEREF:
        push("rax");
        emit_expr(node_operand(struc));
        do_emit_assign_deref(field, field->offset + off);
        break;
    default:
        error("internal error: %s", node2s(struc));
    }
}

static void emit_load_struct_ref(Node *struc, Type *field, int off) {
    SAVE;
    switch (node_kind(struc)) {
    case AST_LVAR:
        ensure_lvar_init(struc);
        emit_lload(field, "rbp", node_loff(struc) + field->offset + off);
        break;
    case AST_GVAR:
        emit_gload(field, node_glabel(struc), field->offset + off);
        break;
    case AST_STRUCT_REF:
        emit_load_struct_ref(node_struct_ref_struc(struc), field, node_type(struc)->offset + off);
        break;
    case AST_DEREF:
        emit_expr(node_operand(struc));
        emit_lload(field, "rax", field->offset + off);
        break;
    default:
        error("internal error: %s", node2s(struc));
    }
}

static void emit_store(Node *var) {
    SAVE;
    switch (node_kind(var)) {
    case AST_DEREF: emit_assign_deref(var); break;
    case AST_STRUCT_REF: emit_assign_struct_ref(node_struct_ref_struc(var), node_type(var), 0); break;
    case AST_LVAR:
        ensure_lvar_init(var);
        emit_lsave(node_type(var), node_loff(var));
        break;
    case AST_GVAR: emit_gsave(node_glabel(var), node_type(var), 0); break;
    default: error("internal error");
    }
}

static void emit_to_bool(Type *ty) {
    SAVE;
    if (is_flotype(ty)) {
        push_xmm(1);
        emit("xorpd #xmm1, #xmm1");
        emit("%s #xmm1, #xmm0", (ty->kind == KIND_FLOAT) ? "ucomiss" : "ucomisd");
        emit("setne #al");
        pop_xmm(1);
    } else {
        emit("cmp $0, #rax");
        emit("setne #al");
    }
    emit("movzb #al, #eax");
}

static void emit_comp(char *inst, char *usiginst, Node *node) {
    SAVE;
    if (is_flotype(node_type(node_left(node)))) {
        emit_expr(node_left(node));
        push_xmm(0);
        emit_expr(node_right(node));
        pop_xmm(1);
        if (node_type(node_left(node))->kind == KIND_FLOAT)
            emit("ucomiss #xmm0, #xmm1");
        else
            emit("ucomisd #xmm0, #xmm1");
    } else {
        emit_expr(node_left(node));
        push("rax");
        emit_expr(node_right(node));
        pop("rcx");
        int kind = node_type(node_left(node))->kind;
        if (kind == KIND_LONG || kind == KIND_LLONG)
          emit("cmp #rax, #rcx");
        else
          emit("cmp #eax, #ecx");
    }
    if (is_flotype(node_type(node_left(node))) || node_type(node_left(node))->usig)
        emit("%s #al", usiginst);
    else
        emit("%s #al", inst);
    emit("movzb #al, #eax");
}

static void emit_binop_int_arith(Node *node) {
    SAVE;
    char *op = NULL;
    switch (node_kind(node)) {
    case '+': op = "add"; break;
    case '-': op = "sub"; break;
    case '*': op = "imul"; break;
    case '^': op = "xor"; break;
    case OP_SAL: op = "sal"; break;
    case OP_SAR: op = "sar"; break;
    case OP_SHR: op = "shr"; break;
    case '/': case '%': break;
    default: error("invalid operator '%d'", node_kind(node));
    }
    emit_expr(node_left(node));
    push("rax");
    emit_expr(node_right(node));
    emit("mov #rax, #rcx");
    pop("rax");
    if (node_kind(node) == '/' || node_kind(node) == '%') {
        if (node_type(node)->usig) {
          emit("xor #edx, #edx");
          emit("div #rcx");
        } else {
          emit("cqto");
          emit("idiv #rcx");
        }
        if (node_kind(node) == '%')
            emit("mov #edx, #eax");
    } else if (node_kind(node) == OP_SAL || node_kind(node) == OP_SAR || node_kind(node) == OP_SHR) {
        emit("%s #cl, #%s", op, get_int_reg(node_type(node_left(node)), 'a'));
    } else {
        emit("%s #rcx, #rax", op);
    }
}

static void emit_binop_float_arith(Node *node) {
    SAVE;
    char *op;
    bool isdouble = (node_type(node)->kind == KIND_DOUBLE);
    switch (node_kind(node)) {
    case '+': op = (isdouble ? "addsd" : "addss"); break;
    case '-': op = (isdouble ? "subsd" : "subss"); break;
    case '*': op = (isdouble ? "mulsd" : "mulss"); break;
    case '/': op = (isdouble ? "divsd" : "divss"); break;
    default: error("invalid operator '%d'", node_kind(node));
    }
    emit_expr(node_left(node));
    push_xmm(0);
    emit_expr(node_right(node));
    emit("%s #xmm0, #xmm1", (isdouble ? "movsd" : "movss"));
    pop_xmm(0);
    emit("%s #xmm1, #xmm0", op);
}

static void emit_load_convert(Type *to, Type *from) {
    SAVE;
    if (is_inttype(from) && to->kind == KIND_FLOAT)
        emit("cvtsi2ss #eax, #xmm0");
    else if (is_inttype(from) && to->kind == KIND_DOUBLE)
        emit("cvtsi2sd #eax, #xmm0");
    else if (from->kind == KIND_FLOAT && to->kind == KIND_DOUBLE)
        emit("cvtps2pd #xmm0, #xmm0");
    else if ((from->kind == KIND_DOUBLE || from->kind == KIND_LDOUBLE) && to->kind == KIND_FLOAT)
        emit("cvtpd2ps #xmm0, #xmm0");
    else if (to->kind == KIND_BOOL)
        emit_to_bool(from);
    else if (is_inttype(from) && is_inttype(to))
        emit_intcast(from);
    else if (is_inttype(to))
        emit_toint(from);
}

static void emit_ret() {
    SAVE;
    emit("leave");
    emit("ret");
}

static void emit_binop(Node *node) {
    SAVE;
    if (node_type(node)->kind == KIND_PTR) {
        emit_pointer_arith(node);
        return;
    }
    switch (node_kind(node)) {
    case '<': emit_comp("setl", "setb", node); return;
    case OP_EQ: emit_comp("sete", "sete", node); return;
    case OP_LE: emit_comp("setle", "setna", node); return;
    case OP_NE: emit_comp("setne", "setne", node); return;
    }
    if (is_inttype(node_type(node)))
        emit_binop_int_arith(node);
    else if (is_flotype(node_type(node)))
        emit_binop_float_arith(node);
    else
        error("internal error: %s", node2s(node));
}

static void emit_save_literal(Node *node, Type *totype, int off) {
    switch (totype->kind) {
    case KIND_BOOL:  emit("movb $%d, %d(#rbp)", !!node_literal_ival(node), off); break;
    case KIND_CHAR:  emit("movb $%d, %d(#rbp)", node_literal_ival(node), off); break;
    case KIND_SHORT: emit("movw $%d, %d(#rbp)", node_literal_ival(node), off); break;
    case KIND_INT:   emit("movl $%d, %d(#rbp)", node_literal_ival(node), off); break;
    case KIND_LONG:
    case KIND_LLONG:
    case KIND_PTR: {
        emit("movl $%lu, %d(#rbp)", ((uint64_t)node_literal_ival(node)) & ((1L << 32) - 1), off);
        emit("movl $%lu, %d(#rbp)", ((uint64_t)node_literal_ival(node)) >> 32, off + 4);
        break;
    }
    case KIND_FLOAT: {
        float fval = node_literal_fval(node);
        emit("movl $%u, %d(#rbp)", *(uint32_t *)&fval, off);
        break;
    }
    case KIND_DOUBLE:
    case KIND_LDOUBLE: {
        double fval = node_literal_fval(node);
        emit("movl $%lu, %d(#rbp)", *(uint64_t *)&fval & ((1L << 32) - 1), off);
        emit("movl $%lu, %d(#rbp)", *(uint64_t *)&fval >> 32, off + 4);
        break;
    }
    default:
        error("internal error: <%s> <%s> <%d>", node2s(node), ty2s(totype), off);
    }
}

static void emit_addr(Node *node) {
    switch (node_kind(node)) {
    case AST_LVAR:
        ensure_lvar_init(node);
        emit("lea %d(#rbp), #rax", node_loff(node));
        break;
    case AST_GVAR:
        emit("lea %s(#rip), #rax", node_glabel(node));
        break;
    case AST_DEREF:
        emit_expr(node_operand(node));
        break;
    case AST_STRUCT_REF:
        emit_addr(node_struct_ref_struc(node));
        emit("add $%d, #rax", node_type(node)->offset);
        break;
    case AST_FUNCDESG:
        emit("lea %s(#rip), #rax", node_funcname(node));
        break;
    default:
        error("internal error: %s", node2s(node));
    }
}

static void emit_copy_struct(Node *left, Node *right) {
    push("rcx");
    push("r11");
    emit_addr(right);
    emit("mov #rax, #rcx");
    emit_addr(left);
    int i = 0;
    for (; i < node_type(left)->size; i += 8) {
        emit("movq %d(#rcx), #r11", i);
        emit("movq #r11, %d(#rax)", i);
    }
    for (; i < node_type(left)->size; i += 4) {
        emit("movl %d(#rcx), #r11", i);
        emit("movl #r11, %d(#rax)", i);
    }
    for (; i < node_type(left)->size; i++) {
        emit("movb %d(#rcx), #r11", i);
        emit("movb #r11, %d(#rax)", i);
    }
    pop("r11");
    pop("rcx");
}

static int cmpinit(const void *x, const void *y) {
    Node *a = *(Node **)x;
    Node *b = *(Node **)y;
    return node_initoff(a) - node_initoff(b);
}

static void emit_fill_holes(Vector *inits, int off, int totalsize) {
    // If at least one of the fields in a variable are initialized,
    // unspecified fields has to be initialized with 0.
    int len = vec_len(inits);
    Node **buf = calloc(len, sizeof(Node *));
    for (int i = 0; i < len; i++)
        buf[i] = vec_get(inits, i);
    qsort(buf, len, sizeof(Node *), cmpinit);

    int lastend = 0;
    for (int i = 0; i < len; i++) {
        Node *node = buf[i];
        if (lastend < node_initoff(node))
            emit_zero_filler(lastend + off, node_initoff(node) + off);
        lastend = node_initoff(node) + node_totype(node)->size;
    }
    emit_zero_filler(lastend + off, totalsize + off);
}

static void emit_decl_init(Vector *inits, int off, int totalsize) {
    emit_fill_holes(inits, off, totalsize);
    for (int i = 0; i < vec_len(inits); i++) {
        Node *node = vec_get(inits, i);
        assert(node_kind(node) == AST_INIT);
        bool isbitfield = (node_totype(node)->bitsize > 0);
        if (node_kind(node_initval(node)) == AST_LITERAL && !isbitfield) {
            emit_save_literal(node_initval(node), node_totype(node), node_initoff(node) + off);
        } else {
            emit_expr(node_initval(node));
            emit_lsave(node_totype(node), node_initoff(node) + off);
        }
    }
}

static void emit_pre_inc_dec(Node *node, char *op) {
    emit_expr(node_operand(node));
    emit("%s $1, #rax", op);
    emit_store(node_operand(node));
}

static void emit_post_inc_dec(Node *node, char *op) {
    SAVE;
    emit_expr(node_operand(node));
    push("rax");
    emit("%s $1, #rax", op);
    emit_store(node_operand(node));
    pop("rax");
}

static void set_reg_nums(Vector *args) {
    numgp = numfp = 0;
    for (int i = 0; i < vec_len(args); i++) {
        Node *arg = vec_get(args, i);
        if (is_flotype(node_type(arg)))
            numfp++;
        else
            numgp++;
    }
}

static void emit_je(char *label) {
    emit("test #rax, #rax");
    emit("je %s", label);
}

static void emit_label(char *label) {
    emit("%s:", label);
}

static void emit_jmp(char *label) {
    emit("jmp %s", label);
}

static void emit_literal(Node *node) {
    SAVE;
    switch (node_type(node)->kind) {
    case KIND_BOOL:
    case KIND_CHAR:
    case KIND_SHORT:
        emit("mov $%u, #rax", node_literal_ival(node));
        break;
    case KIND_INT:
        emit("mov $%u, #rax", node_literal_ival(node));
        break;
    case KIND_LONG:
    case KIND_LLONG: {
        emit("mov $%lu, #rax", node_literal_ival(node));
        break;
    }
    case KIND_FLOAT: {
        char *flabel = node_literal_flabel(node);
        if (!flabel) {
            flabel = make_label();
            node_set_flabel(node, flabel);
            float fval = node_literal_fval(node);
            emit_noindent(".data");
            emit_label(flabel);
            emit(".long %d", *(uint32_t *)&fval);
            emit_noindent(".text");
        }
        emit("movss %s(#rip), #xmm0", flabel);
        break;
    }
    case KIND_DOUBLE:
    case KIND_LDOUBLE: {
        char *flabel = node_literal_flabel(node);
        if (!flabel) {
            flabel = make_label();
            node_set_flabel(node, flabel);
            double fval = node_literal_fval(node);
            emit_noindent(".data");
            emit_label(flabel);
            emit(".quad %lu", *(uint64_t *)&fval);
            emit_noindent(".text");
        }
        emit("movsd %s(#rip), #xmm0", flabel);
        break;
    }
    case KIND_ARRAY: {
        char *slabel = node_literal_slabel(node);
        if (!slabel) {
            slabel = make_label();
            node_set_slabel(node, slabel);
            emit_noindent(".data");
            emit_label(slabel);
            emit(".string \"%s\"", quote_cstring_len(node_literal_sval(node), node_type(node)->size));
            emit_noindent(".text");
        }
        emit("lea %s(#rip), #rax", slabel);
        break;
    }
    default:
        error("internal error");
    }
}

static char **split(char *buf) {
    char *p = buf;
    int len = 1;
    while (*p) {
        if (p[0] == '\r' && p[1] == '\n') {
            len++;
            p += 2;
            continue;
        }
        if (p[0] == '\r' || p[0] == '\n')
            len++;
        p++;
    }
    p = buf;
    char **r = calloc(len + 1, sizeof(char *));
    int i = 0;
    while (*p) {
        if (p[0] == '\r' && p[1] == '\n') {
            p[0] = '\0';
            p += 2;
            r[i++] = p;
            continue;
        }
        if (p[0] == '\r' || p[0] == '\n') {
            p[0] = '\0';
            r[i++] = p + 1;
        }
        p++;
    }
    r[i] = NULL;
    return r;
}

static char **read_source_file(char *file) {
    FILE *fp = fopen(file, "r");
    if (!fp)
        return NULL;
    struct stat st;
    fstat(fileno(fp), &st);
    char *buf = calloc(1, st.st_size + 1);
    if (fread(buf, 1, st.st_size, fp) != st.st_size)
        return NULL;
    fclose(fp);
    buf[st.st_size] = '\0';
    return split(buf);
}

static void maybe_print_source_line(char *file, int line) {
    if (!dumpsource)
        return;
    char **lines = map_get(source_lines, file);
    if (!lines) {
        lines = read_source_file(file);
        if (!lines)
            return;
        map_put(source_lines, file, lines);
    }
    int len = 0;
    for (char **p = lines; *p; p++)
        len++;
    emit_nostack("# %s", lines[line - 1]);
}

static void maybe_print_source_loc(Node *node) {
    SourceLoc *sloc = node_loc(node);
    if (!sloc)
        return;
    char *file = sloc->file;
    long fileno = (long)map_get(source_files, file);
    if (!fileno) {
        fileno = map_len(source_files) + 1;
        map_put(source_files, file, (void *)fileno);
        emit(".file %ld \"%s\"", fileno, quote_cstring(file));
    }
    char *loc = format(".loc %ld %d 0", fileno, sloc->line);
    if (strcmp(loc, last_loc)) {
        emit("%s", loc);
        maybe_print_source_line(file, sloc->line);
    }
    last_loc = loc;
}

static void emit_lvar(Node *node) {
    SAVE;
    ensure_lvar_init(node);
    emit_lload(node_type(node), "rbp", node_loff(node));
}

static void emit_gvar(Node *node) {
    SAVE;
    emit_gload(node_type(node), node_glabel(node), 0);
}

static void emit_builtin_return_address(Node *node) {
    push("r11");
    Vector *args = node_funcall_args(node);
    assert(vec_len(args) == 1);
    emit_expr(vec_head(args));
    char *loop = make_label();
    char *end = make_label();
    emit("mov #rbp, #r11");
    emit_label(loop);
    emit("test #rax, #rax");
    emit("jz %s", end);
    emit("mov (#r11), #r11");
    emit("sub $1, #rax");
    emit_jmp(loop);
    emit_label(end);
    emit("mov 8(#r11), #rax");
    pop("r11");
}

// Set the register class for parameter passing to RAX.
// 0 is INTEGER, 1 is SSE, 2 is MEMORY.
static void emit_builtin_reg_class(Node *node) {
    Node *arg = vec_get(node_funcall_args(node), 0);
    assert(node_type(arg)->kind == KIND_PTR);
    Type *ty = node_type(arg)->ptr;
    if (ty->kind == KIND_STRUCT)
        emit("mov $2, #eax");
    else if (is_flotype(ty))
        emit("mov $1, #eax");
    else
        emit("mov $0, #eax");
}

static void emit_builtin_va_start(Node *node) {
    SAVE;
    assert(vec_len(node_funcall_args(node)) == 1);
    emit_expr(vec_head(node_funcall_args(node)));
    push("rcx");
    emit("movl $%d, (#rax)", numgp * 8);
    emit("movl $%d, 4(#rax)", 48 + numfp * 16);
    emit("lea %d(#rbp), #rcx", -REGAREA_SIZE);
    emit("mov #rcx, 16(#rax)");
    pop("rcx");
}

static bool maybe_emit_builtin(Node *node) {
    SAVE;
    if (!strcmp("__builtin_return_address", node_funcname(node))) {
        emit_builtin_return_address(node);
        return true;
    }
    if (!strcmp("__builtin_reg_class", node_funcname(node))) {
        emit_builtin_reg_class(node);
        return true;
    }
    if (!strcmp("__builtin_va_start", node_funcname(node))) {
        emit_builtin_va_start(node);
        return true;
    }
    return false;
}

static void classify_args(Vector *ints, Vector *floats, Vector *rest, Vector *args) {
    SAVE;
    int ireg = 0, xreg = 0;
    int imax = 6, xmax = 8;
    for (int i = 0; i < vec_len(args); i++) {
        Node *v = vec_get(args, i);
        if (node_type(v)->kind == KIND_STRUCT)
            vec_push(rest, v);
        else if (is_flotype(node_type(v)))
            vec_push((xreg++ < xmax) ? floats : rest, v);
        else
            vec_push((ireg++ < imax) ? ints : rest, v);
    }
}

static void save_arg_regs(int nints, int nfloats) {
    SAVE;
    assert(nints <= 6);
    assert(nfloats <= 8);
    for (int i = 0; i < nints; i++)
        push(REGS[i]);
    for (int i = 1; i < nfloats; i++)
        push_xmm(i);
}

static void restore_arg_regs(int nints, int nfloats) {
    SAVE;
    for (int i = nfloats - 1; i > 0; i--)
        pop_xmm(i);
    for (int i = nints - 1; i >= 0; i--)
        pop(REGS[i]);
}

static int emit_args(Vector *vals) {
    SAVE;
    int r = 0;
    for (int i = 0; i < vec_len(vals); i++) {
        Node *v = vec_get(vals, i);
        if (node_type(v)->kind == KIND_STRUCT) {
            emit_addr(v);
            r += push_struct(node_type(v)->size);
        } else if (is_flotype(node_type(v))) {
            emit_expr(v);
            push_xmm(0);
            r += 8;
        } else {
            emit_expr(v);
            push("rax");
            r += 8;
        }
    }
    return r;
}

static void pop_int_args(int nints) {
    SAVE;
    for (int i = nints - 1; i >= 0; i--)
        pop(REGS[i]);
}

static void pop_float_args(int nfloats) {
    SAVE;
    for (int i = nfloats - 1; i >= 0; i--)
        pop_xmm(i);
}

static void maybe_booleanize_retval(Type *ty) {
    if (ty->kind == KIND_BOOL) {
        emit("movzx #al, #rax");
    }
}

static void emit_func_call(Node *node) {
    SAVE;
    int opos = stackpos;
    bool isptr = (node_kind(node) == AST_FUNCPTR_CALL);
    Type *ftype = isptr ? node_type(node_funcptr(node))->ptr : node_functype(node);

    Vector *ints = make_vector();
    Vector *floats = make_vector();
    Vector *rest = make_vector();
    classify_args(ints, floats, rest, node_funcall_args(node));
    save_arg_regs(vec_len(ints), vec_len(floats));

    bool padding = stackpos % 16;
    if (padding) {
        emit("sub $8, #rsp");
        stackpos += 8;
    }

    int restsize = emit_args(vec_reverse(rest));
    if (isptr) {
        emit_expr(node_funcptr(node));
        push("rax");
    }
    emit_args(ints);
    emit_args(floats);
    pop_float_args(vec_len(floats));
    pop_int_args(vec_len(ints));

    if (isptr) pop("r11");
    if (ftype->hasva)
        emit("mov $%u, #eax", vec_len(floats));

    if (isptr)
        emit("call *#r11");
    else
        emit("call %s", node_funcname(node));
    maybe_booleanize_retval(node_type(node));
    if (restsize > 0) {
        emit("add $%d, #rsp", restsize);
        stackpos -= restsize;
    }
    if (padding) {
        emit("add $8, #rsp");
        stackpos -= 8;
    }
    restore_arg_regs(vec_len(ints), vec_len(floats));
    assert(opos == stackpos);
}

static void emit_decl(Node *node) {
    SAVE;
    Vector *declinit = node_declinit(node);
    if (!declinit)
        return;
    Node *declvar = node_declvar(node);
    emit_decl_init(declinit, node_loff(declvar), node_type(declvar)->size);
}

static void emit_conv(Node *node) {
    SAVE;
    emit_expr(node_operand(node));
    emit_load_convert(node_type(node), node_type(node_operand(node)));
}

static void emit_deref(Node *node) {
    SAVE;
    emit_expr(node_operand(node));
    emit_lload(node_type(node_operand(node))->ptr, "rax", 0);
    emit_load_convert(node_type(node), node_type(node_operand(node))->ptr);
}

static void emit_ternary(Node *node) {
    SAVE;
    emit_expr(node_cond(node));
    char *ne = make_label();
    emit_je(ne);
    if (node_then(node))
        emit_expr(node_then(node));
    if (node_els(node)) {
        char *end = make_label();
        emit_jmp(end);
        emit_label(ne);
        emit_expr(node_els(node));
        emit_label(end);
    } else {
        emit_label(ne);
    }
}

static void emit_goto(Node *node) {
    SAVE;
    assert(node_newlabel(node));
    emit_jmp(node_newlabel(node));
}

static void emit_return(Node *node) {
    SAVE;
    Node *retval = node_retval(node);
    if (retval) {
        emit_expr(retval);
        maybe_booleanize_retval(node_type(retval));
    }
    emit_ret();
}

static void emit_compound_stmt(Node *node) {
    SAVE;
    for (int i = 0; i < vec_len(node_stmts(node)); i++)
        emit_expr(vec_get(node_stmts(node), i));
}

static void emit_logand(Node *node) {
    SAVE;
    char *end = make_label();
    emit_expr(node_left(node));
    emit("test #rax, #rax");
    emit("mov $0, #rax");
    emit("je %s", end);
    emit_expr(node_right(node));
    emit("test #rax, #rax");
    emit("mov $0, #rax");
    emit("je %s", end);
    emit("mov $1, #rax");
    emit_label(end);
}

static void emit_logor(Node *node) {
    SAVE;
    char *end = make_label();
    emit_expr(node_left(node));
    emit("test #rax, #rax");
    emit("mov $1, #rax");
    emit("jne %s", end);
    emit_expr(node_right(node));
    emit("test #rax, #rax");
    emit("mov $1, #rax");
    emit("jne %s", end);
    emit("mov $0, #rax");
    emit_label(end);
}

static void emit_lognot(Node *node) {
    SAVE;
    emit_expr(node_operand(node));
    emit("cmp $0, #rax");
    emit("sete #al");
    emit("movzb #al, #eax");
}

static void emit_bitand(Node *node) {
    SAVE;
    emit_expr(node_left(node));
    push("rax");
    emit_expr(node_right(node));
    pop("rcx");
    emit("and #rcx, #rax");
}

static void emit_bitor(Node *node) {
    SAVE;
    emit_expr(node_left(node));
    push("rax");
    emit_expr(node_right(node));
    pop("rcx");
    emit("or #rcx, #rax");
}

static void emit_bitnot(Node *node) {
    SAVE;
    emit_expr(node_left(node));
    emit("not #rax");
}

static void emit_cast(Node *node) {
    SAVE;
    emit_expr(node_operand(node));
    emit_load_convert(node_type(node), node_type(node_operand(node)));
    return;
}

static void emit_comma(Node *node) {
    SAVE;
    emit_expr(node_left(node));
    emit_expr(node_right(node));
}

static void emit_assign(Node *node) {
    SAVE;
    Node *left = node_left(node);
    Node *right = node_right(node);
    if (node_type(left)->kind == KIND_STRUCT &&
        node_type(left)->size > 8) {
        emit_copy_struct(left, right);
    } else {
        emit_expr(right);
        emit_load_convert(node_type(node), node_type(right));
        emit_store(left);
    }
}

static void emit_label_addr(Node *node) {
    SAVE;
    emit("mov $%s, #rax", node_newlabel(node));
}

static void emit_computed_goto(Node *node) {
    SAVE;
    emit_expr(node_operand(node));
    emit("jmp *#rax");
}

static void emit_expr(Node *node) {
    SAVE;
    maybe_print_source_loc(node);
    switch (node_kind(node)) {
    case AST_LITERAL: emit_literal(node); return;
    case AST_LVAR:    emit_lvar(node); return;
    case AST_GVAR:    emit_gvar(node); return;
    case AST_FUNCDESG: return;
    case AST_FUNCALL:
        if (maybe_emit_builtin(node))
            return;
        // fall through
    case AST_FUNCPTR_CALL:
        emit_func_call(node);
        return;
    case AST_DECL:    emit_decl(node); return;
    case AST_CONV:    emit_conv(node); return;
    case AST_ADDR:    emit_addr(node_operand(node)); return;
    case AST_DEREF:   emit_deref(node); return;
    case AST_IF:
    case AST_TERNARY:
        emit_ternary(node);
        return;
    case AST_GOTO:    emit_goto(node); return;
    case AST_LABEL:
        if (node_newlabel(node))
            emit_label(node_newlabel(node));
        return;
    case AST_RETURN:  emit_return(node); return;
    case AST_COMPOUND_STMT: emit_compound_stmt(node); return;
    case AST_STRUCT_REF:
        emit_load_struct_ref(node_struct_ref_struc(node), node_type(node), 0);
        return;
    case OP_PRE_INC:   emit_pre_inc_dec(node, "add"); return;
    case OP_PRE_DEC:   emit_pre_inc_dec(node, "sub"); return;
    case OP_POST_INC:  emit_post_inc_dec(node, "add"); return;
    case OP_POST_DEC:  emit_post_inc_dec(node, "sub"); return;
    case '!': emit_lognot(node); return;
    case '&': emit_bitand(node); return;
    case '|': emit_bitor(node); return;
    case '~': emit_bitnot(node); return;
    case OP_LOGAND: emit_logand(node); return;
    case OP_LOGOR:  emit_logor(node); return;
    case OP_CAST:   emit_cast(node); return;
    case ',': emit_comma(node); return;
    case '=': emit_assign(node); return;
    case OP_LABEL_ADDR: emit_label_addr(node); return;
    case AST_COMPUTED_GOTO: emit_computed_goto(node); return;
    default:
        emit_binop(node);
    }
}

static void emit_zero(int size) {
    SAVE;
    for (; size >= 8; size -= 8) emit(".quad 0");
    for (; size >= 4; size -= 4) emit(".long 0");
    for (; size > 0; size--)     emit(".byte 0");
}

static void emit_padding(Node *node, int off) {
    SAVE;
    int diff = node_initoff(node) - off;
    assert(diff >= 0);
    emit_zero(diff);
}

static void emit_data_addr(Node *operand, int depth) {
    switch (node_kind(operand)) {
    case AST_LVAR: {
        char *label = make_label();
        emit(".data %d", depth + 1);
        emit_label(label);
        do_emit_data(node_lvarinit(operand), node_type(operand)->size, 0, depth + 1);
        emit(".data %d", depth);
        emit(".quad %s", label);
        return;
    }
    case AST_GVAR:
        emit(".quad %s", node_glabel(operand));
        return;
    default:
        error("internal error");
    }
}

static void emit_data_charptr(char *s, int depth) {
    char *label = make_label();
    emit(".data %d", depth + 1);
    emit_label(label);
    emit(".string \"%s\"", quote_cstring(s));
    emit(".data %d", depth);
    emit(".quad %s", label);
}

static void emit_data_primtype(Type *ty, Node *val, int depth) {
    switch (ty->kind) {
    case KIND_FLOAT: {
        float f = node_literal_fval(val);
        emit(".long %d", *(uint32_t *)&f);
        break;
    }
    case KIND_DOUBLE: {
        double fval = node_literal_fval(val);
        emit(".quad %ld", *(uint64_t *)&fval);
        break;
    }
    case KIND_BOOL:
        emit(".byte %d", !!eval_intexpr(val, NULL));
        break;
    case KIND_CHAR:
        emit(".byte %d", eval_intexpr(val, NULL));
        break;
    case KIND_SHORT:
        emit(".short %d", eval_intexpr(val, NULL));
        break;
    case KIND_INT:
        emit(".long %d", eval_intexpr(val, NULL));
        break;
    case KIND_LONG:
    case KIND_LLONG:
    case KIND_PTR:
        if (node_kind(val) == OP_LABEL_ADDR) {
            emit(".quad %s", node_newlabel(val));
            break;
        }
        bool is_char_ptr = (node_type(node_operand(val))->kind == KIND_ARRAY && node_type(node_operand(val))->ptr->kind == KIND_CHAR);
        if (is_char_ptr) {
            emit_data_charptr(node_literal_sval(node_operand(val)), depth);
        } else if (node_kind(val) == AST_GVAR) {
            emit(".quad %s", node_glabel(val));
        } else {
            Node *base = NULL;
            int v = eval_intexpr(val, &base);
            if (base == NULL) {
                emit(".quad %u", v);
                break;
            }
            Type *ty = node_type(base);
            if (node_kind(base) == AST_CONV || node_kind(base) == AST_ADDR)
                base = node_operand(base);
            if (node_kind(base) != AST_GVAR)
                error("global variable expected, but got %s", node2s(base));
            assert(ty->ptr);
            emit(".quad %s+%u", node_glabel(base), v * ty->ptr->size);
        }
        break;
    default:
        error("don't know how to handle\n  <%s>\n  <%s>", ty2s(ty), node2s(val));
    }
}

static void do_emit_data(Vector *inits, int size, int off, int depth) {
    SAVE;
    for (int i = 0; i < vec_len(inits) && 0 < size; i++) {
        Node *node = vec_get(inits, i);
        Node *v = node_initval(node);
        emit_padding(node, off);
        if (node_totype(node)->bitsize > 0) {
            assert(node_totype(node)->bitoff == 0);
            long data = eval_intexpr(v, NULL);
            Type *totype = node_totype(node);
            for (i++ ; i < vec_len(inits); i++) {
                node = vec_get(inits, i);
                if (node_totype(node)->bitsize <= 0) {
                    break;
                }
                v = node_initval(node);
                totype = node_totype(node);
                data |= ((((long)1 << totype->bitsize) - 1) & eval_intexpr(v, NULL)) << totype->bitoff;
            }
            Node *prim = ast_inttype(totype, data);
            emit_data_primtype(totype, prim, depth);
            off += totype->size;
            size -= totype->size;
            if (i == vec_len(inits))
                break;
        } else {
            off += node_totype(node)->size;
            size -= node_totype(node)->size;
        }
        if (node_kind(v) == AST_ADDR) {
            emit_data_addr(node_operand(v), depth);
            continue;
        }
        if (node_kind(v) == AST_LVAR && node_lvarinit(v)) {
            do_emit_data(node_lvarinit(v), node_type(v)->size, 0, depth);
            continue;
        }
        emit_data_primtype(node_totype(node), node_initval(node), depth);
    }
    emit_zero(size);
}

static void emit_data(Node *v, int off, int depth) {
    SAVE;
    emit(".data %d", depth);
    Node *declvar = node_declvar(v);
    if (!node_type(declvar)->isstatic)
        emit_noindent(".global %s", node_glabel(declvar));
    emit_noindent("%s:", node_glabel(declvar));
    do_emit_data(node_declinit(v), node_type(declvar)->size, off, depth);
}

static void emit_bss(Node *v) {
    SAVE;
    emit(".data");
    Node *declvar = node_declvar(v);
    if (!node_type(declvar)->isstatic)
        emit(".global %s", node_glabel(declvar));
    emit(".lcomm %s, %d", node_glabel(declvar), node_type(declvar)->size);
}

static void emit_global_var(Node *v) {
    SAVE;
    if (node_declinit(v))
        emit_data(v, 0, 0);
    else
        emit_bss(v);
}

static int emit_regsave_area() {
    emit("sub $%d, #rsp", REGAREA_SIZE);
    emit("mov #rdi, (#rsp)");
    emit("mov #rsi, 8(#rsp)");
    emit("mov #rdx, 16(#rsp)");
    emit("mov #rcx, 24(#rsp)");
    emit("mov #r8, 32(#rsp)");
    emit("mov #r9, 40(#rsp)");
    emit("movaps #xmm0, 48(#rsp)");
    emit("movaps #xmm1, 64(#rsp)");
    emit("movaps #xmm2, 80(#rsp)");
    emit("movaps #xmm3, 96(#rsp)");
    emit("movaps #xmm4, 112(#rsp)");
    emit("movaps #xmm5, 128(#rsp)");
    emit("movaps #xmm6, 144(#rsp)");
    emit("movaps #xmm7, 160(#rsp)");
    return REGAREA_SIZE;
}

static void push_func_params(Vector *params, int off) {
    int ireg = 0;
    int xreg = 0;
    int arg = 2;
    for (int i = 0; i < vec_len(params); i++) {
        Node *v = vec_get(params, i);
        if (node_type(v)->kind == KIND_STRUCT) {
            emit("lea %d(#rbp), #rax", arg * 8);
            int size = push_struct(node_type(v)->size);
            off -= size;
            arg += size / 8;
        } else if (is_flotype(node_type(v))) {
            if (xreg >= 8) {
                emit("mov %d(#rbp), #rax", arg++ * 8);
                push("rax");
            } else {
                push_xmm(xreg++);
            }
            off -= 8;
        } else {
            if (ireg >= 6) {
                if (node_type(v)->kind == KIND_BOOL) {
                    emit("mov %d(#rbp), #al", arg++ * 8);
                    emit("movzb #al, #eax");
                } else {
                    emit("mov %d(#rbp), #rax", arg++ * 8);
                }
                push("rax");
            } else {
                if (node_type(v)->kind == KIND_BOOL)
                    emit("movzb #%s, #%s", SREGS[ireg], MREGS[ireg]);
                push(REGS[ireg++]);
            }
            off -= 8;
        }
        node_set_loff(v, off);
    }
}

static void emit_func_prologue(Node *func) {
    SAVE;
    emit(".text");
    if (!node_type(func)->isstatic)
        emit_noindent(".global %s", node_funcname(func));
    emit_noindent("%s:", node_funcname(func));
    emit("nop");
    push("rbp");
    emit("mov #rsp, #rbp");
    int off = 0;
    if (node_type(func)->hasva) {
        set_reg_nums(node_funcparams(func));
        off -= emit_regsave_area();
    }
    push_func_params(node_funcparams(func), off);
    off -= vec_len(node_funcparams(func)) * 8;

    int localarea = 0;
    for (int i = 0; i < vec_len(node_funclocals(func)); i++) {
        Node *v = vec_get(node_funclocals(func), i);
        int size = align(node_type(v)->size, 8);
        assert(size % 8 == 0);
        off -= size;
        node_set_loff(v, off);
        localarea += size;
    }
    if (localarea) {
        emit("sub $%d, #rsp", localarea);
        stackpos += localarea;
    }
}

void emit_toplevel(Node *v) {
    stackpos = 8;
    if (node_kind(v) == AST_FUNC) {
        emit_func_prologue(v);
        emit_expr(node_funcbody(v));
        emit_ret();
    } else if (node_kind(v) == AST_DECL) {
        emit_global_var(v);
    } else {
        error("internal error");
    }
}
