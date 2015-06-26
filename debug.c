// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "8cc.h"


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
