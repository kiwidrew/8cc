// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "8cc.h"

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

char *ty2s(Type *ty) {
    return do_ty2s(make_dict(), ty);
}

static char *encoding_prefix(int enc) {
    switch (enc) {
    case ENC_CHAR16: return "u";
    case ENC_CHAR32: return "U";
    case ENC_UTF8:   return "u8";
    case ENC_WCHAR:  return "L";
    }
    return "";
}

char *tok2s(Token *tok) {
    if (!tok)
        return "(null)";
    switch (tok->kind) {
    case TIDENT:
        return tok->sval;
    case TKEYWORD:
        switch (tok->id) {
#define op(id, str)         case id: return str;
#define keyword(id, str, _) case id: return str;
#include "keyword.inc"
#undef keyword
#undef op
        default: return format("%c", tok->id);
        }
    case TCHAR:
        return format("%s'%s'",
                      encoding_prefix(tok->enc),
                      quote_char(tok->c));
    case TNUMBER:
        return tok->sval;
    case TSTRING:
        return format("%s\"%s\"",
                      encoding_prefix(tok->enc),
                      quote_cstring(tok->sval));
    case TEOF:
        return "(eof)";
    case TINVALID:
        return format("%c", tok->c);
    case TNEWLINE:
        return "(newline)";
    case TSPACE:
        return "(space)";
    case TMACRO_PARAM:
        return "(macro-param)";
    }
    error("internal error: unknown token kind: %d", tok->kind);
}
