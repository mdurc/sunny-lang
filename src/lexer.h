#ifndef LEXER_H
#define LEXER_H

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    // keywords:
    FUNC,           // func
    AND,            // and
    OR,             // or
    IF,             // if
    ELSE,           // else
    FOR,            // for
    WHILE,          // while
    PRINT,          // print
    BREAK,          // break
    CONTINUE,       // continue
    RETURN,         // return
    RETURNS,        // returns
    MUT,            // mut
    IDENTIFIER,

    // primitive types:
    U0_TYPE,        // u0 type
    U8_TYPE,        // u8 type
    U16_TYPE,       // u16 type
    U32_TYPE,       // u32 type
    U64_TYPE,       // u64 type
    I8_TYPE,        // i8 type
    I16_TYPE,       // i16 type
    I32_TYPE,       // i32 type
    I64_TYPE,       // i64 type
    F64_TYPE,       // f64 type

    BOOL_TYPE,      // bool will be mapped to i8 but will be forced to be 1/0
    STRING_TYPE,    // String

    // literals:
    CHAR_LITERAL,   // 'a'
    NULL_LITERAL,   // null (of type u0)
    TRUE,           // true
    FALSE,          // false
    INT_LITERAL,    // 42, 0xFF, 0b1010
    FLOAT_LITERAL,  // 3.14
    STRING_LITERAL, // "text"

    // syntax:
    LPAREN,         // (
    RPAREN,         // )
    LBRACE,         // {
    RBRACE,         // }
    LBRACKET,       // [
    RBRACKET,       // ]
    COLON,          // :
    SEMICOLON,      // ;
    COMMA,          // ,
    TILDE,          // ~

    // operators:
    BANG,           // !
    PLUS,           // +
    MINUS,          // -
    FSLASH,         // /
    STAR,           // *
    EQUAL,          // =
    MODULO,         // %

    // compound operators:
    WALRUS,         // :=
    BANG_EQUAL,     // !=
    LESS,           // <
    LESS_EQUAL,     // <=
    GREATER,        // >
    GREATER_EQUAL,  // >=

    NO_TYPE
} TokenType;

typedef enum {
    Keyword,
    Identifier,
    Primitive_type,
    Literal,
    Punct,
    Operator
} Category;

typedef struct {
    TokenType type;
    Category category;
    int line;
    union {
        // integer (and char/bool/null) literal:
        // for bool: 0 is false, non-zero is true
        uint64_t int_t;

        // float literal:
        double f64_value;

        // string literals, identifier names, keywords
        char* lexeme;
    } data;
} Token;

const char* tok_string(TokenType type);
bool match_char(FILE* fp, char c);
void free_token_data(Token* token);
Token* alloc_token_lexeme_data(TokenType type, const char* lexeme, int line);
Token* create_literal_token(const char* lexeme, int line);
Token* create_token(const char* lexeme, int line);
void lex_file(FILE* fp, Token*** tokens, int* token_count, int* token_capacity);

#endif
