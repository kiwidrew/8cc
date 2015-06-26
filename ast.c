#include <stdlib.h>


#include "8cc.h"
#include "ast.h"


static Node *ast_node(int kind, Type *ty) {
    Node *n = calloc(1, sizeof(Node));

    n->kind = kind;
    n->ty = ty;
    n->sourceLoc = NULL;
    return n;
}

void ast_add_source(Node *n, SourceLoc *loc) {
    n->sourceLoc = loc;
}

void node_set_type(Node *n, Type *ty) {
    n->ty = ty;
}


/* Node constructors. */

Node *ast_uop(int kind, Type *ty, Node *operand) {
    Node *n;

    n = ast_node(kind, ty);
    n->operand = operand;
    return n;
}

Node *ast_binop(Type *ty, int kind, Node *left, Node *right) {
    Node *n;

    n = ast_node(kind, ty);
    n->left = left;
    n->right = right;
    return n;
}

Node *ast_typedef(Type *ty) {
    Node *n;

    n = ast_node(AST_TYPEDEF, ty);
    return n;
}


// AST_FUNCALL

Node *ast_funcall(Type *ftype, char *fname, Vector *args) {
    Node *n;

    n = ast_node(AST_FUNCALL, ftype->rettype);
    n->ftype = ftype;
    n->fname = fname;
    n->args = args;
    return n;
}

Type *node_functype(Node *n) {
    assert(n->kind == AST_FUNCALL);
    return n->ftype;
}

Vector *node_funcall_args(Node *n) {
    //assert(n->kind == AST_FUNCALL);
    return n->args;
}


Node *ast_funcdesg(Type *ty, char *fname) {
    Node *n;

    n = ast_node(AST_FUNCDESG, ty);
    n->fname = fname;
    return n;
}

Node *ast_funcptr_call(Node *fptr, Vector *args) {
    Node *n;

    assert(fptr->ty->kind == KIND_PTR);
    assert(fptr->ty->ptr->kind == KIND_FUNC);
    n = ast_node(AST_FUNCPTR_CALL, fptr->ty->ptr->rettype);
    n->fptr = fptr;
    n->args = args;
    return n;
}

Node *node_funcptr(Node *n) {
    assert(n->kind == AST_FUNCPTR_CALL);
    return n->fptr;
}


// AST_FUNC

Node *ast_func(Type *ty, char *fname, Vector *params, Node *body, Vector *localvars) {
    Node *n;

    n = ast_node(AST_FUNC, ty);
    n->fname = fname;
    n->params = params;
    n->localvars = localvars;
    n->body = body;
    return n;
}

Node *node_funcbody(Node *n) {
    assert(n->kind == AST_FUNC);

    return n->body;
}

Vector *node_funclocals(Node *n) {
    assert(n->kind == AST_FUNC);

    return n->localvars;
}

Vector *node_funcparams(Node *n) {
    assert(n->kind == AST_FUNC);

    return n->params;
}

char *node_funcname(Node *n) {
    //assert(n->kind == AST_FUNC || n->kind == AST_FUNCALL);

    return n->fname;
}


// AST_DECL

Node *ast_decl(Node *var, Vector *init) {
    Node *n;

    n = ast_node(AST_DECL, NULL);
    n->declvar = var;
    n->declinit = init;
    return n;
}

Node *node_declvar(Node *n) {
    assert(n->kind == AST_DECL);
    return n->declvar;
}

Vector *node_declinit(Node *n) {
    assert(n->kind == AST_DECL);
    return n->declinit;
}


// AST_INIT

Node *ast_init(Node *val, Type *totype, int off) {
    Node *n;

    n = ast_node(AST_INIT, NULL);
    n->initval = val;
    n->initoff = off;
    n->totype = totype;
    return n;
}

int node_initoff(Node *n) {
    assert(n->kind == AST_INIT);
    return n->initoff;
}

Node *node_initval(Node *n) {
    assert(n->kind == AST_INIT);
    return n->initval;
}

Type *node_totype(Node *n) {
    assert(n->kind == AST_INIT);
    return n->totype;
}


// AST_CONV

Node *ast_conv(Type *totype, Node *val) {
    Node *n;

    n = ast_node(AST_CONV, totype);
    n->operand = val;
    return n;
}


// AST_RETURN

Node *ast_return(Node *retval) {
    Node *n;

    n = ast_node(AST_RETURN, NULL);
    n->retval = retval;
    return n;
}

Node *node_retval(Node *n) {
    assert(n->kind == AST_RETURN);
    return n->retval;
}


// AST_COMPOUND_STMT

Node *ast_compound_stmt(Vector *stmts) {
    Node *n;

    n = ast_node(AST_COMPOUND_STMT, NULL);
    n->stmts = stmts;
    return n;
}

Vector *node_stmts(Node *n) {
    assert(n->kind == AST_COMPOUND_STMT);
    return n->stmts;
}


// AST_GOTO

Node *ast_goto(char *label) {
    Node *n;

    n = ast_node(AST_GOTO, NULL);
    n->label = label;
    return n;
}

Node *ast_jump(char *label) {
    Node *n;

    n = ast_node(AST_GOTO, NULL);
    n->label = label;
    n->newlabel = label;
    return n;
}

Node *ast_computed_goto(Node *expr) {
    Node *n;

    n = ast_node(AST_COMPUTED_GOTO, NULL);
    n->operand = expr;
    return n;
}

Node *ast_label(char *label) {
    Node *n;

    n = ast_node(AST_LABEL, NULL);
    n->label = label;
    return n;
}

Node *ast_label_addr(Type *ty, char *label) {
    Node *n;

    n = ast_node(OP_LABEL_ADDR, ty);
    n->label = label;
    return n;
}

Node *ast_dest(char *label) {
    Node *n;

    n = ast_node(AST_LABEL, NULL);
    n->label = label;
    n->newlabel = label;
    return n;
}


/* Node accessor functions. */

int node_kind(Node *n) {
    assert(n != NULL);
    return n->kind;
}

Type *node_type(Node *n) {
    assert(n != NULL);
    return n->ty;
}

SourceLoc *node_loc(Node *n) {
    return n->sourceLoc;
}

Node *node_operand(Node *n) {
    // TODO:  ensure we are only called on unary nodes
    return n->operand;
}

Node *node_left(Node *n) {
    return n->left;
}

Node *node_right(Node *n) {
    return n->right;
}

char *node_fname(Node *n) {
    assert(n->kind == AST_FUNCALL || n->kind == AST_FUNCDESG || n->kind == AST_FUNC);
    return n->fname;
}

char *node_label(Node *n) {
    return n->label;
}

char *node_newlabel(Node *n) {
    return n->newlabel;
}

void node_set_newlabel(Node *n, char *newlabel) {
    n->newlabel = newlabel;
}


// AST_LITERAL

Node *ast_inttype(Type *ty, long val) {
    Node *n;

    n = ast_node(AST_LITERAL, ty);
    n->ival = val;
    return n;
}

Node *ast_floattype(Type *ty, double val) {
    Node *n;

    n = ast_node(AST_LITERAL, ty);
    n->fval = val;
    return n;
}

Node *ast_string(Type *ty, char *sval) {
    Node *n;

    n = ast_node(AST_LITERAL, ty);
    n->sval = sval;
    return n;
}

long node_literal_ival(Node *n) {
    assert(n->kind == AST_LITERAL);
    // TODO:  ensure n->ty is an inttype
    return n->ival;
}

double node_literal_fval(Node *n) {
    assert(n->kind == AST_LITERAL);
    // TODO:  ensure n->ty is a floattype
    return n->fval;
}

void node_set_flabel(Node *n, char *flabel) {
    assert(n->kind == AST_LITERAL);
    // TODO:  ensure n->ty is a floattype
    n->flabel = flabel;
}

char *node_literal_flabel(Node *n) {
    assert(n->kind == AST_LITERAL);
    // TODO:  ensure n->ty is a floattype
    return n->flabel;
}

char *node_literal_sval(Node *n) {
    assert(n->kind == AST_LITERAL);
    // TODO:  ensure n->ty is a string
    return n->sval;
}

void node_set_slabel(Node *n, char *slabel) {
    assert(n->kind == AST_LITERAL);
    // TODO:  ensure n->ty is a string
    n->slabel = slabel;
}

char *node_literal_slabel(Node *n) {
    assert(n->kind == AST_LITERAL);
    // TODO:  ensure n->ty is a string
    return n->slabel;
}

// AST_LVAR and AST_GVAR

Node *ast_lvar(Type *ty, char *name, Vector *init) {
    Node *n;

    n = ast_node(AST_LVAR, ty);
    n->varname = name;
    n->lvarinit = init;
    // TODO:  do we need to initialize n->loff?
    return n;
}

Node *ast_gvar(Type *ty, char *name, char *glabel) {
    Node *n;

    n = ast_node(AST_GVAR, ty);
    n->varname = name;
    n->glabel = glabel;
    return n;
}

char *node_varname(Node *n) {
    assert(n->kind == AST_LVAR || n->kind == AST_GVAR);
    return n->varname;
}

char *node_glabel(Node *n) {
    assert(n->kind == AST_GVAR);
    return n->glabel;
}

Vector *node_lvarinit(Node *n) {
    assert(n->kind == AST_LVAR);
    return n->lvarinit;
}

void node_set_lvarinit(Node *n, Vector *init) {
    assert(n->kind == AST_LVAR);
    n->lvarinit = init;
}

int node_loff(Node *n) {
    assert(n->kind == AST_LVAR);
    return n->loff;
}

void node_set_loff(Node *n, int off) {
    assert(n->kind == AST_LVAR);
    n->loff = off;
}


// AST_STRUCT_REF

Node *ast_struct_ref(Type *ty, Node *struc, char *name) {
    Node *n;

    n = ast_node(AST_STRUCT_REF, ty);
    n->struc = struc;
    n->field = name;
    return n;
}

Node *node_struct_ref_struc(Node *n) {
    assert(n->kind == AST_STRUCT_REF);

    return n->struc;
}

char *node_struct_ref_field(Node *n) {
    assert(n->kind == AST_STRUCT_REF);

    return n->field;
}


// AST_IF and AST_TERNARY

Node *ast_if(Node *cond, Node *then, Node *els) {
    Node *n;

    n = ast_node(AST_IF, NULL);
    n->cond = cond;
    n->then = then;
    n->els = els;
    return n;
}

Node *ast_ternary(Type *ty, Node *cond, Node *then, Node *els) {
    Node *n;

    n = ast_node(AST_TERNARY, ty);
    n->cond = cond;
    n->then = then;
    n->els = els;
    return n;
}

Node *node_cond(Node *n) {
    assert(n->kind == AST_IF || n->kind == AST_TERNARY);
    return n->cond;
}

Node *node_then(Node *n) {
    assert(n->kind == AST_IF || n->kind == AST_TERNARY);
    return n->then;
}

Node *node_els(Node *n) {
    assert(n->kind == AST_IF || n->kind == AST_TERNARY);
    return n->els;
}

    
/* Textual representation of AST nodes. */

static void uop_to_string(Buffer *b, char *op, Node *node) {
    buf_printf(b, "(%s %s)", op, node2s(node->operand));
}

static void binop_to_string(Buffer *b, char *op, Node *node) {
    buf_printf(b, "(%s %s %s)", op, node2s(node->left), node2s(node->right));
}

static void a2s_declinit(Buffer *b, Vector *initlist) {
    for (int i = 0; i < vec_len(initlist); i++) {
        if (i > 0)
            buf_printf(b, " ");
        Node *init = vec_get(initlist, i);
        buf_printf(b, "%s", node2s(init));
    }
}

static void do_node2s(Buffer *b, Node *node) {
    if (!node) {
        buf_printf(b, "(nil)");
        return;
    }
    switch (node->kind) {
    case AST_LITERAL:
        switch (node->ty->kind) {
        case KIND_CHAR:
            if (node->ival == '\n')      buf_printf(b, "'\n'");
            else if (node->ival == '\\') buf_printf(b, "'\\\\'");
            else if (node->ival == '\0') buf_printf(b, "'\\0'");
            else buf_printf(b, "'%c'", node->ival);
            break;
        case KIND_INT:
            buf_printf(b, "%d", node->ival);
            break;
        case KIND_LONG:
            buf_printf(b, "%ldL", node->ival);
            break;
        case KIND_LLONG:
            buf_printf(b, "%lldL", node->ival);
            break;
        case KIND_FLOAT:
        case KIND_DOUBLE:
        case KIND_LDOUBLE:
            buf_printf(b, "%f", node->fval);
            break;
        case KIND_ARRAY:
            buf_printf(b, "\"%s\"", quote_cstring(node->sval));
            break;
        default:
            error("internal error");
        }
        break;
    case AST_LABEL:
        buf_printf(b, "%s:", node->label);
        break;
    case AST_LVAR:
        buf_printf(b, "lv=%s", node->varname);
        if (node->lvarinit) {
            buf_printf(b, "(");
            a2s_declinit(b, node->lvarinit);
            buf_printf(b, ")");
        }
        break;
    case AST_GVAR:
        buf_printf(b, "gv=%s", node->varname);
        break;
    case AST_FUNCALL:
    case AST_FUNCPTR_CALL: {
        buf_printf(b, "(%s)%s(", ty2s(node->ty),
                   node->kind == AST_FUNCALL ? node->fname : node2s(node));
        for (int i = 0; i < vec_len(node->args); i++) {
            if (i > 0)
                buf_printf(b, ",");
            buf_printf(b, "%s", node2s(vec_get(node->args, i)));
        }
        buf_printf(b, ")");
        break;
    }
    case AST_FUNCDESG: {
        buf_printf(b, "(funcdesg %s)", node->fname);
        break;
    }
    case AST_FUNC: {
        buf_printf(b, "(%s)%s(", ty2s(node->ty), node->fname);
        for (int i = 0; i < vec_len(node->params); i++) {
            if (i > 0)
                buf_printf(b, ",");
            Node *param = vec_get(node->params, i);
            buf_printf(b, "%s %s", ty2s(param->ty), node2s(param));
        }
        buf_printf(b, ")");
        do_node2s(b, node->body);
        break;
    }
    case AST_GOTO:
        buf_printf(b, "goto(%s)", node->label);
        break;
    case AST_DECL:
        buf_printf(b, "(decl %s %s",
                   ty2s(node->declvar->ty),
                   node->declvar->varname);
        if (node->declinit) {
            buf_printf(b, " ");
            a2s_declinit(b, node->declinit);
        }
        buf_printf(b, ")");
        break;
    case AST_INIT:
        buf_printf(b, "%s@%d", node2s(node->initval), node->initoff, ty2s(node->totype));
        break;
    case AST_CONV:
        buf_printf(b, "(conv %s=>%s)", node2s(node->operand), ty2s(node->ty));
        break;
    case AST_IF:
        buf_printf(b, "(if %s %s",
                   node2s(node->cond),
                   node2s(node->then));
        if (node->els)
            buf_printf(b, " %s", node2s(node->els));
        buf_printf(b, ")");
        break;
    case AST_TERNARY:
        buf_printf(b, "(? %s %s %s)",
                   node2s(node->cond),
                   node2s(node->then),
                   node2s(node->els));
        break;
    case AST_RETURN:
        buf_printf(b, "(return %s)", node2s(node->retval));
        break;
    case AST_COMPOUND_STMT: {
        buf_printf(b, "{");
        for (int i = 0; i < vec_len(node->stmts); i++) {
            do_node2s(b, vec_get(node->stmts, i));
            buf_printf(b, ";");
        }
        buf_printf(b, "}");
        break;
    }
    case AST_STRUCT_REF:
        do_node2s(b, node->struc);
        buf_printf(b, ".");
        buf_printf(b, node->field);
        break;
    case AST_ADDR:  uop_to_string(b, "addr", node); break;
    case AST_DEREF: uop_to_string(b, "deref", node); break;
    case OP_SAL:  binop_to_string(b, "<<", node); break;
    case OP_SAR:
    case OP_SHR:  binop_to_string(b, ">>", node); break;
    case OP_GE:  binop_to_string(b, ">=", node); break;
    case OP_LE:  binop_to_string(b, "<=", node); break;
    case OP_NE:  binop_to_string(b, "!=", node); break;
    case OP_PRE_INC: uop_to_string(b, "pre++", node); break;
    case OP_PRE_DEC: uop_to_string(b, "pre--", node); break;
    case OP_POST_INC: uop_to_string(b, "post++", node); break;
    case OP_POST_DEC: uop_to_string(b, "post--", node); break;
    case OP_LOGAND: binop_to_string(b, "and", node); break;
    case OP_LOGOR:  binop_to_string(b, "or", node); break;
    case OP_A_ADD:  binop_to_string(b, "+=", node); break;
    case OP_A_SUB:  binop_to_string(b, "-=", node); break;
    case OP_A_MUL:  binop_to_string(b, "*=", node); break;
    case OP_A_DIV:  binop_to_string(b, "/=", node); break;
    case OP_A_MOD:  binop_to_string(b, "%=", node); break;
    case OP_A_AND:  binop_to_string(b, "&=", node); break;
    case OP_A_OR:   binop_to_string(b, "|=", node); break;
    case OP_A_XOR:  binop_to_string(b, "^=", node); break;
    case OP_A_SAL:  binop_to_string(b, "<<=", node); break;
    case OP_A_SAR:
    case OP_A_SHR:  binop_to_string(b, ">>=", node); break;
    case '!': uop_to_string(b, "!", node); break;
    case '&': binop_to_string(b, "&", node); break;
    case '|': binop_to_string(b, "|", node); break;
    case OP_CAST: {
        buf_printf(b, "((%s)=>(%s) %s)",
                   ty2s(node->operand->ty),
                   ty2s(node->ty),
                   node2s(node->operand));
        break;
    }
    case OP_LABEL_ADDR:
        buf_printf(b, "&&%s", node->label);
        break;
    default: {
        char *left = node2s(node->left);
        char *right = node2s(node->right);
        if (node->kind == OP_EQ)
            buf_printf(b, "(== ");
        else
            buf_printf(b, "(%c ", node->kind);
        buf_printf(b, "%s %s)", left, right);
    }
    }
}

char *node2s(Node *node) {
    Buffer *b = make_buffer();
    do_node2s(b, node);
    return buf_body(b);
}
