#include "lexer.h"

bool is_numeric(TokenType t) {
    switch(t) {
        case INT_LITERAL: case FLOAT_LITERAL:
        case U8: case U16: case U32: case U64: case I8:
        case I16: case I32: case I64: case F64:
            return true;
        default: return false;
    }
}

bool is_integer(TokenType t) {
    return t != F64 && t != FLOAT_LITERAL && is_numeric(t);
}

bool is_float(TokenType t) {
    return t == F64 || t == FLOAT_LITERAL;
}

bool can_implicit_cast(TokenType from, TokenType to) {
    if (from == to) return true;
    if (from == STRING && to == STRING) return true;
    if (is_numeric(from) && is_numeric(to)) return true;

    if (from == BOOL && is_numeric(to)) return true;
    if (is_numeric(from) && to == BOOL) return true;

    if (from == CHAR_LITERAL && is_numeric(to)) return true;
    return false;
}
