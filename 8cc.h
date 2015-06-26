// Copyright 2012 Rui Ueyama. Released under the MIT license.

#ifndef EIGHTCC_H
#define EIGHTCC_H

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdnoreturn.h>
#include <time.h>

enum {
    TIDENT,
    TKEYWORD,
    TNUMBER,
    TCHAR,
    TSTRING,
    TEOF,
    TINVALID,
    // Only in CPP
    MIN_CPP_TOKEN,
    TNEWLINE,
    TSPACE,
    TMACRO_PARAM,
};

enum {
    ENC_NONE,
    ENC_CHAR16,
    ENC_CHAR32,
    ENC_UTF8,
    ENC_WCHAR,
};

typedef struct Map {
    struct Map *parent;
    char **key;
    void **val;
    int size;
    int nelem;
    int nused;
} Map;

typedef struct {
    void **body;
    int len;
    int nalloc;
} Vector;

typedef struct {
    struct Map *map;
    Vector *key;
} Dict;

typedef struct Set {
    char *v;
    struct Set *next;
} Set;

typedef struct {
    char *body;
    int nalloc;
    int len;
} Buffer;

typedef struct {
    FILE *file;  // stream backed by FILE *
    char *p;     // stream backed by string
    char *name;
    int line;
    int column;
    int ntok;     // token counter
    int last;     // the last character read from file
    int buf[3];   // push-back buffer for unread operations
    int buflen;   // push-back buffer size
    time_t mtime; // last modified time. 0 if string-backed file
} File;

typedef struct {
    int kind;
    File *file;
    int line;
    int column;
    bool space;   // true if the token has a leading space
    bool bol;     // true if the token is at the beginning of a line
    int count;    // token number in a file, counting from 0.
    Set *hideset; // used by the preprocessor for macro expansion
    union {
        // TKEYWORD
        int id;
        // TSTRING or TCHAR
        struct {
            char *sval;
            int slen;
            int c;
            int enc;
        };
        // TMACRO_PARAM
        struct {
            bool is_vararg;
            int position;
        };
    };
} Token;

enum {
    AST_LITERAL = 256,
    AST_LVAR,
    AST_GVAR,
    AST_TYPEDEF,
    AST_FUNCALL,
    AST_FUNCPTR_CALL,
    AST_FUNCDESG,
    AST_FUNC,
    AST_DECL,
    AST_INIT,
    AST_CONV,
    AST_ADDR,
    AST_DEREF,
    AST_IF,
    AST_TERNARY,
    AST_DEFAULT,
    AST_RETURN,
    AST_COMPOUND_STMT,
    AST_STRUCT_REF,
    AST_GOTO,
    AST_COMPUTED_GOTO,
    AST_LABEL,
    OP_SIZEOF,
    OP_CAST,
    OP_SHR,
    OP_SHL,
    OP_A_SHR,
    OP_A_SHL,
    OP_PRE_INC,
    OP_PRE_DEC,
    OP_POST_INC,
    OP_POST_DEC,
    OP_LABEL_ADDR,
#define op(name, _) name,
#define keyword(name, x, y) name,
#include "keyword.inc"
#undef keyword
#undef op
};

enum {
    KIND_VOID,
    KIND_BOOL,
    KIND_CHAR,
    KIND_SHORT,
    KIND_INT,
    KIND_LONG,
    KIND_LLONG,
    KIND_FLOAT,
    KIND_DOUBLE,
    KIND_LDOUBLE,
    KIND_ARRAY,
    KIND_ENUM,
    KIND_PTR,
    KIND_STRUCT,
    KIND_FUNC,
    // used only in parser
    KIND_STUB,
};

typedef struct {
    char *file;
    int line;
} SourceLoc;


#define EMPTY_MAP ((Map){})
#define EMPTY_VECTOR ((Vector){})


// type.h
typedef struct Type Type;

// type.c
extern Type *type_void;
extern Type *type_bool;
extern Type *type_char;
extern Type *type_short;
extern Type *type_int;
extern Type *type_long;
extern Type *type_llong;
extern Type *type_uchar;
extern Type *type_ushort;
extern Type *type_uint;
extern Type *type_ulong;
extern Type *type_ullong;
extern Type *type_float;
extern Type *type_double;
extern Type *type_ldouble;
extern Type *type_enum;
Type *type_create_numeric(int, bool);
Type *type_create_pointer(Type *);
Type *type_create_array(Type *, int);
Type *type_create_stub();
Type *type_create_func(Type *rettype, Vector *paramtypes, bool has_varargs, bool oldstyle);
Type *type_create_struct(bool is_struct);
Type *type_copy(Type *ty);
bool type_is_same_struct(Type *, Type *);
bool type_is_same_arith_type(Type *, Type *);
bool type_iscompatible(Type *, Type *);
Type *type_promote(Type *, Type *);
int type_kind(Type *);
Type *type_ptr(Type *);
int type_align(Type *);
int type_size(Type *);
int type_len(Type *);
bool type_isint(Type *);
bool type_isfloat(Type *);
bool type_isstring(Type *);
Type *type_rettype(Type *);
int type_usig(Type *);
int type_bitsize(Type *);
bool type_isstatic(Type *);
Dict *type_fields(Type *);
int type_offset(Type *);
bool type_isoldstyle(Type *);
Vector *type_funcparams(Type *);
bool type_isstruct(Type *);
char *ty2s(Type *);

// ast.h
typedef struct Node Node;

// ast.c
void ast_add_source(Node *n, SourceLoc *loc);
void node_set_type(Node *, Type *);
void node_set_newlabel(Node *, char *);
char *node2s(Node *node);
Node *ast_uop(int kind, Type *ty, Node *operand);
Node *ast_binop(Type *ty, int kind, Node *left, Node *right);
Node *ast_typedef(Type *ty);
Node *ast_funcall(Type *ftype, char *fname, Vector *args);
Node *ast_funcdesg(Type *ty, char *fname);
Node *ast_funcptr_call(Node *fptr, Vector *args);
Node *ast_func(Type *ty, char *fname, Vector *params, Node *body, Vector *localvars);
Node *ast_decl(Node *var, Vector *init);
Node *node_declvar(Node *);
Vector *node_declinit(Node *);
Node *ast_init(Node *val, Type *totype, int off);
int node_initoff(Node *);
Node *ast_conv(Type *totype, Node *val);
Node *ast_return(Node *retval);
Node *ast_compound_stmt(Vector *stmts);
Vector *node_stmts(Node *);
Node *ast_goto(char *label);
Node *ast_jump(char *label);
Node *ast_computed_goto(Node *expr);
Node *ast_label(char *label);
Node *ast_label_addr(Type *ty, char *label);
Node *ast_dest(char *label);
int node_kind(Node *);
Type *node_type(Node *);
Node *node_operand(Node *);
Node *node_left(Node *);
Node *node_right(Node *);
char *node_fname(Node *);
char *node_label(Node *);
char *node_newlabel(Node *);
Node *ast_inttype(Type *ty, long val);
Node *ast_floattype(Type *ty, double val);
Node *ast_string(Type *ty, char *sval);
long node_literal_ival(Node *);
double node_literal_fval(Node *);
Node *ast_lvar(Type *ty, char *name, Vector *init);
Node *ast_gvar(Type *ty, char *name, char *glabel);
char *node_varname(Node *);
Node *ast_struct_ref(Type *ty, Node *struc, char *name);
Node *node_struct_ref_struc(Node *);
char *node_struct_ref_field(Node *);
Node *ast_if(Node *cond, Node *then, Node *els);
Node *ast_ternary(Type *ty, Node *cond, Node *then, Node *els);
Node *node_cond(Node *);
Node *node_then(Node *);
Node *node_els(Node *);

// encoding.c
Buffer *to_utf16(char *p, int len);
Buffer *to_utf32(char *p, int len);
void write_utf8(Buffer *b, uint32_t rune);

// buffer.c
Buffer *make_buffer(void);
char *buf_body(Buffer *b);
int buf_len(Buffer *b);
void buf_write(Buffer *b, char c);
void buf_append(Buffer *b, char *s, int len);
void buf_printf(Buffer *b, char *fmt, ...);
char *vformat(char *fmt, va_list ap);
char *format(char *fmt, ...);
char *quote_cstring(char *p);
char *quote_cstring_len(char *p, int len);
char *quote_char(char c);

// cpp.c
void read_from_string(char *buf);
bool is_ident(Token *tok, char *s);
void expect_newline(void);
void add_include_path(char *path);
void init_now(void);
void cpp_init(void);
Token *peek_token(void);
Token *read_token(void);

// debug.c
char *tok2s(Token *tok);

// dict.c
Dict *make_dict(void);
void *dict_get(Dict *dict, char *key);
void dict_put(Dict *dict, char *key, void *val);
Vector *dict_keys(Dict *dict);

// error.c
extern bool enable_warning;
extern bool dumpstack;
extern bool dumpsource;
extern bool warning_is_error;

#define STR2(x) #x
#define STR(x) STR2(x)
#define error(...)       errorf(__FILE__ ":" STR(__LINE__), NULL, __VA_ARGS__)
#define errort(tok, ...) errorf(__FILE__ ":" STR(__LINE__), token_pos(tok), __VA_ARGS__)
#define warn(...)        warnf(__FILE__ ":" STR(__LINE__), NULL, __VA_ARGS__)
#define warnt(tok, ...)  warnf(__FILE__ ":" STR(__LINE__), token_pos(tok), __VA_ARGS__)

noreturn void errorf(char *line, char *pos, char *fmt, ...);
void warnf(char *line, char *pos, char *fmt, ...);
char *token_pos(Token *tok);

// file.c
File *make_file(FILE *file, char *name);
File *make_file_string(char *s);
int readc(void);
void unreadc(int c);
File *current_file(void);
void stream_push(File *file);
int stream_depth(void);
char *input_position(void);
void stream_stash(File *f);
void stream_unstash(void);

// gen.c
void set_output_file(FILE *fp);
void close_output_file(void);
void emit_toplevel(Node *v);

// lex.c
void lex_init(char *filename);
char *get_base_file(void);
void skip_cond_incl(void);
char *read_header_file_name(bool *std);
bool is_keyword(Token *tok, int c);
void token_buffer_stash(Vector *buf);
void token_buffer_unstash();
void unget_token(Token *tok);
Token *lex_string(char *s);
Token *lex(void);

// map.c
Map *make_map(void);
Map *make_map_parent(Map *parent);
void *map_get(Map *m, char *key);
void map_put(Map *m, char *key, void *val);
void map_remove(Map *m, char *key);
size_t map_len(Map *m);

// parse.c
char *make_tempname(void);
char *make_label(void);
bool is_inttype(Type *ty);
bool is_flotype(Type *ty);
void *make_pair(void *first, void *second);
int eval_intexpr(Node *node, Node **addr);
Node *read_expr(void);
Vector *read_toplevels(void);
void parse_init(void);
char *fullpath(char *path);

// set.c
Set *set_add(Set *s, char *v);
bool set_has(Set *s, char *v);
Set *set_union(Set *a, Set *b);
Set *set_intersection(Set *a, Set *b);

// vector.c
Vector *make_vector(void);
Vector *make_vector1(void *e);
Vector *vec_copy(Vector *src);
void vec_push(Vector *vec, void *elem);
void vec_append(Vector *a, Vector *b);
void *vec_pop(Vector *vec);
void *vec_get(Vector *vec, int index);
void vec_set(Vector *vec, int index, void *val);
void *vec_head(Vector *vec);
void *vec_tail(Vector *vec);
Vector *vec_reverse(Vector *vec);
void *vec_body(Vector *vec);
int vec_len(Vector *vec);

#endif
