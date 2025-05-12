#ifndef LEXER_H
#define LEXER_H

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    // keywords:
    FUNC,       // func
    MUT,        // mut (mutable)
    IF,         // if
    ELSE,       // else
    FOR,        // for
    WHILE,      // while
    PRINT,      // print
    RETURN,     // return
    RETURNS,    // returns
    //STRUCT,     // struct
    BREAK,      // break
    CONTINUE,   // continue
    TRUE,       // true
    FALSE,      // false
    NULL_,      // null
    AND,        // and
    OR,         // or
    IDENTIFIER,

    // primitive types:
    // Unsigned
    U8, U16, U32, U64,
    // Signed
    I8, I16, I32, I64,
    // Floating
    F64,
    // Special
    BOOL, STRING, U0,

    // literals
    INT_LITERAL,     // 42, 0x1F, 0b1010
    FLOAT_LITERAL,   // 3.14, 2e5
    CHAR_LITERAL,    // 'a'
    STRING_LITERAL, // "text"

    // syntax:
    LPAREN, RPAREN,    // ()
    LBRACE, RBRACE,    // {}
    LBRACK, RBRACK,    // []
    COMMA, COLON, SEMICOLON, TILDE,

    // operators:
    BANG,           // !
    PLUS,           // +
    MINUS,          // -
    SLASH,          // /
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

    EOF_
} TokenType;

typedef struct {
    TokenType type;
    char* start; // source text from program
    int length; // length of lexeme string
    int line;

    union {
        uint64_t int_val;
        double float_val;
        char* str_val;
    } data;
} Token;

const char* tok_string(TokenType type);
bool is_primitive(TokenType t);
bool is_literal(TokenType t);
bool match_char(FILE* fp, char c);
void free_token_data(Token* token);
Token* alloc_token_lexeme_data(TokenType type, const char* lexeme, int line);
Token* create_literal_token(const char* lexeme, int line);
Token* create_token(const char* lexeme, int line);
void lex_file(FILE* fp, Token*** tokens, int* token_count, int* token_capacity);

#endif
