// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include <stdlib.h>
#include <string.h>


#include "8cc.h"
#include "type.h"

static char *do_ty2s(Dict *dict, Type *ty);


// Objects representing basic types. All variables will be of one of these types
// or a derived type from one of them. Note that (typename){initializer} is C99
// feature to write struct literals.
Type *type_void = &(Type){ KIND_VOID, 0, 0, false };
Type *type_bool = &(Type){ KIND_BOOL, 1, 1, true };
Type *type_char = &(Type){ KIND_CHAR, 1, 1, false };
Type *type_short = &(Type){ KIND_SHORT, 2, 2, false };
Type *type_int = &(Type){ KIND_INT, 4, 4, false };
Type *type_long = &(Type){ KIND_LONG, 8, 8, false };
Type *type_llong = &(Type){ KIND_LLONG, 8, 8, false };
Type *type_uchar = &(Type){ KIND_CHAR, 1, 1, true };
Type *type_ushort = &(Type){ KIND_SHORT, 2, 2, true };
Type *type_uint = &(Type){ KIND_INT, 4, 4, true };
Type *type_ulong = &(Type){ KIND_LONG, 8, 8, true };
Type *type_ullong = &(Type){ KIND_LLONG, 8, 8, true };
Type *type_float = &(Type){ KIND_FLOAT, 4, 4, false };
Type *type_double = &(Type){ KIND_DOUBLE, 8, 8, false };
Type *type_ldouble = &(Type){ KIND_LDOUBLE, 8, 8, false };
Type *type_enum = &(Type){ KIND_ENUM, 4, 4, false };


Type *type_create_numeric(int kind, bool usig) {
    Type *r = calloc(1, sizeof(Type));
    r->kind = kind;
    r->usig = usig;
    if (kind == KIND_VOID)         r->size = r->align = 0;
    else if (kind == KIND_BOOL)    r->size = r->align = 1;
    else if (kind == KIND_CHAR)    r->size = r->align = 1;
    else if (kind == KIND_SHORT)   r->size = r->align = 2;
    else if (kind == KIND_INT)     r->size = r->align = 4;
    else if (kind == KIND_LONG)    r->size = r->align = 8;
    else if (kind == KIND_LLONG)   r->size = r->align = 8;
    else if (kind == KIND_FLOAT)   r->size = r->align = 4;
    else if (kind == KIND_DOUBLE)  r->size = r->align = 8;
    else if (kind == KIND_LDOUBLE) r->size = r->align = 8;
    else error("internal error");

    return r;
}

Type *type_create_pointer(Type *ty) {
    Type *r = calloc(1, sizeof(Type));
    r->kind = KIND_PTR;
    r->ptr = ty;
    r->size = 8;
    r->align = 8;

    return r;
}

Type *type_create_array(Type *ty, int len) {
    Type *r = calloc(1, sizeof(Type));
    r->kind = KIND_ARRAY;
    r->ptr = ty;
    r->len = len;
    r->align = ty->align;
    r->size = (len < 0) ? -1 : ty->size * len;

    return r;
}

Type *type_create_stub() {
    Type *r = calloc(1, sizeof(Type));
    r->kind = KIND_STUB;

    return r;
}

Type *type_create_func(Type *rettype, Vector *paramtypes, bool has_varargs, bool oldstyle) {
    Type *r = calloc(1, sizeof(Type));

    r->kind = KIND_FUNC;
    r->rettype = rettype;
    r->params = paramtypes;
    r->hasva = has_varargs;
    r->oldstyle = oldstyle;
    return r;
}

Type *type_create_struct(bool is_struct) {
    Type *r = calloc(1, sizeof(Type));

    r->kind = KIND_STRUCT;
    r->is_struct = is_struct;
    return r;
}

Type *type_copy(Type *ty) {
    Type *r = malloc(sizeof(Type));

    memcpy(r, ty, sizeof(Type));
    return r;
}

bool type_is_same_struct(Type *a, Type *b) {
    if (a->kind != b->kind)
        return false;
    switch (a->kind) {
    case KIND_ARRAY:
        return a->len == b->len &&
            type_is_same_struct(a->ptr, b->ptr);
    case KIND_PTR:
        return type_is_same_struct(a->ptr, b->ptr);
    case KIND_STRUCT: {
        if (a->is_struct != b->is_struct)
            return false;
        Vector *ka = dict_keys(a->fields);
        Vector *kb = dict_keys(b->fields);
        if (vec_len(ka) != vec_len(kb))
            return false;
        for (int i = 0; i < vec_len(ka); i++)
            if (!type_is_same_struct(vec_get(ka, i), vec_get(kb, i)))
                return false;
        return true;
    }
    default:
        return true;
    }
}

bool type_is_same_arith_type(Type *t, Type *u) {
    if (!type_isint(t) && !type_isfloat(t))
        return false;
    if (!type_isint(u) && !type_isfloat(u))
        return false;
    return t->kind == u->kind && t->usig == u->usig;
}

bool type_iscompatible(Type *a, Type *b) {
    if (a->kind == KIND_STRUCT)
        return type_is_same_struct(a, b);
    if (a->kind != b->kind)
        return false;
    if (a->ptr && b->ptr)
        return type_iscompatible(a->ptr, b->ptr);
    return type_is_same_arith_type(a, b);
}

// C11 6.3.1.1: Integer promotion
// C11 6.3.1.8: Usual arithmetic conversions
Type *type_promote(Type *t, Type *u) {
    assert(type_isint(t) || type_isfloat(t));
    assert(type_isint(u) || type_isfloat(u));
    if (t->kind < u->kind) {
        // Make t the larger type
        Type *tmp = t;
        t = u;
        u = tmp;
    }
    if (type_isfloat(t))
        return t;
    assert(type_isint(t) && t->size >= type_int->size);
    assert(type_isint(u) && t->size >= type_int->size);
    if (t->size > u->size)
        return t;
    assert(t->size == u->size);
    if (t->usig == u->usig)
        return t;
    Type *r = type_copy(t);
    r->usig = true;
    return r;
}

int type_kind(Type *ty) {
    return ty->kind;
}

Type *type_ptr(Type *ty) {
    assert(ty->kind == KIND_PTR || ty->kind == KIND_ARRAY);
    return ty->ptr;
}

int type_align(Type *ty) {
    assert(ty->align > 0);
    return ty->align;
}

int type_size(Type *ty) {
    return ty->size;
}

int type_len(Type *ty) {
    //assert(ty->kind == KIND_ARRAY);
    return ty->len;
}

bool type_isint(Type *ty) {
    switch (ty->kind) {
      case KIND_BOOL:
      case KIND_CHAR:
      case KIND_SHORT:
      case KIND_INT:
      case KIND_LONG:
      case KIND_LLONG:
        return true;
    
      default:
        return false;
    }
}

bool type_isfloat(Type *ty) {
    switch (ty->kind) {
      case KIND_FLOAT:
      case KIND_DOUBLE:
      case KIND_LDOUBLE:
        return true;

      default:
        return false;
    }
}

bool type_isstring(Type *ty) {
    return (ty->kind == KIND_ARRAY && ty->ptr->kind == KIND_CHAR);
}

Type *type_rettype(Type *ty) {
    assert(ty->kind == KIND_FUNC);
    return ty->rettype;
}

int type_usig(Type *ty) {
    assert(ty->kind <= KIND_LLONG);
    return ty->usig;
}

int type_bitsize(Type *ty) {
    //assert(ty->kind <= KIND_LLONG || ty->bitsize == 0);
    return ty->bitsize;
}

bool type_isstatic(Type *ty) {
    return ty->isstatic;
}

Dict *type_fields(Type *ty) {
    return ty->fields;
}

int type_offset(Type *ty) {
    return ty->offset;
}

// KIND_FUNC

bool type_isoldstyle(Type *ty) {
    assert(ty->kind == KIND_FUNC);
    return ty->oldstyle;
}

Vector *type_funcparams(Type *ty) {
    assert(ty->kind == KIND_FUNC);
    return ty->params;
}

bool type_isstruct(Type *ty) {
    assert(ty->kind == KIND_STRUCT);
    return ty->is_struct;
}

char *ty2s(Type *ty) {
    return do_ty2s(make_dict(), ty);
}



/* Debugging utils. */

static char *decorate_int(char *name, Type *ty) {
    char *u = (ty->usig) ? "u" : "";
    if (ty->bitsize > 0)
        return format("%s%s:%d:%d", u, name, ty->bitoff, ty->bitoff + ty->bitsize);
    return format("%s%s", u, name);
}

static char *do_ty2s(Dict *dict, Type *ty) {
    if (!ty)
        return "(nil)";
    switch (ty->kind) {
    case KIND_VOID: return "void";
    case KIND_BOOL: return "_Bool";
    case KIND_CHAR: return decorate_int("char", ty);
    case KIND_SHORT: return decorate_int("short", ty);
    case KIND_INT:  return decorate_int("int", ty);
    case KIND_LONG: return decorate_int("long", ty);
    case KIND_LLONG: return decorate_int("llong", ty);
    case KIND_FLOAT: return "float";
    case KIND_DOUBLE: return "double";
    case KIND_LDOUBLE: return "long double";
    case KIND_PTR:
        return format("*%s", do_ty2s(dict, ty->ptr));
    case KIND_ARRAY:
        return format("[%d]%s", ty->len, do_ty2s(dict, ty->ptr));
    case KIND_STRUCT: {
        char *kind = ty->is_struct ? "struct" : "union";
        if (dict_get(dict, format("%p", ty)))
            return format("(%s)", kind);
        dict_put(dict, format("%p", ty), (void *)1);
        if (ty->fields) {
            Buffer *b = make_buffer();
            buf_printf(b, "(%s", kind);
            Vector *keys = dict_keys(ty->fields);
            for (int i = 0; i < vec_len(keys); i++) {
                char *key = vec_get(keys, i);
                Type *fieldtype = dict_get(ty->fields, key);
                buf_printf(b, " (%s)", do_ty2s(dict, fieldtype));
            }
            buf_printf(b, ")");
            return buf_body(b);
        }
    }
    case KIND_FUNC: {
        Buffer *b = make_buffer();
        buf_printf(b, "(");
        if (ty->params) {
            for (int i = 0; i < vec_len(ty->params); i++) {
                if (i > 0)
                    buf_printf(b, ",");
                Type *t = vec_get(ty->params, i);
                buf_printf(b, "%s", do_ty2s(dict, t));
            }
        }
        buf_printf(b, ")=>%s", do_ty2s(dict, ty->rettype));
        return buf_body(b);
    }
    default:
        return format("(Unknown ty: %d)", ty->kind);
    }
}

