#include "lexer.h"
#include "error.h"
#include <assert.h>
#include <limits.h>
#include <stdint.h>

const char* tok_string(TokenType type) {
    switch (type) {
        // keywords
        case FUNC:          return "func";
        case MUT:           return "mut";
        case IF:            return "if";
        case ELSE:          return "else";
        case FOR:           return "for";
        case WHILE:         return "while";
        case PRINT:         return "print";
        case RETURN:        return "return";
        case RETURNS:       return "returns";
        case BREAK:         return "break";
        case CONTINUE:      return "continue";
        case TRUE:          return "true";
        case FALSE:         return "false";
        case NULL_:         return "null";
        case AND:           return "and";
        case OR:            return "or";
        case IDENTIFIER:    return "_identifier_";

        // primitive types
        case U0:            return "u0";
        case U8:            return "u8";
        case U16:           return "u16";
        case U32:           return "u32";
        case U64:           return "u64";
        case I8:            return "i8";
        case I16:           return "i16";
        case I32:           return "i32";
        case I64:           return "i64";
        case F64:           return "f64";
        case BOOL:          return "bool";
        case STRING:        return "String";

        // literals
        case INT_LITERAL:   return "_int_literal_";
        case FLOAT_LITERAL: return "_float_literal_";
        case CHAR_LITERAL:  return "_char_literal_";
        case STRING_LITERAL:return "_string_literal_";

        // syntax
        case LPAREN:        return "(";
        case RPAREN:        return ")";
        case LBRACE:        return "{";
        case RBRACE:        return "}";
        case LBRACK:        return "[";
        case RBRACK:        return "]";
        case COMMA:         return ",";
        case COLON:         return ":";
        case SEMICOLON:     return ";";
        case TILDE:         return "~";

        // operators
        case BANG:          return "!";
        case PLUS:          return "+";
        case MINUS:         return "-";
        case SLASH:        return "/";
        case STAR:          return "*";
        case EQUAL:         return "=";
        case MODULO:        return "%";

        // compound operators
        case WALRUS:        return ":=";
        case BANG_EQUAL:    return "!=";
        case LESS:          return "<";
        case LESS_EQUAL:    return "<=";
        case GREATER:       return ">";
        case GREATER_EQUAL: return ">=";

        case EOF_:          return "_EOF_";
        default:            return "UNKNOWN";
    }
}

typedef struct {
    const char* keyword;
    TokenType type;
} KeywordEntry;

static KeywordEntry keyword_map[] = {
    {"func",    FUNC},
    {"mut",     MUT},
    {"if",      IF},
    {"else",    ELSE},
    {"for",     FOR},
    {"while",   WHILE},
    {"print",   PRINT},
    {"return",  RETURN},
    {"returns", RETURNS},
    {"break",   BREAK},
    {"continue",CONTINUE},
    {"true", TRUE}, {"false", FALSE}, {"null", NULL_}, {"and", AND}, {"or", OR},
    {"u0", U0}, {"u8", U8}, {"u16", U16}, {"u32", U32}, {"u64", U64},
    {"i8", I8}, {"i16", I16}, {"i32", I32}, {"i64", I64}, {"f64", F64},
    {"bool",    BOOL},
    {"String",  STRING},
};

#define KEYWORD_COUNT (sizeof(keyword_map) / sizeof(keyword_map[0]))

TokenType lookup_type(const char* str) {
    for (int i = 0; i < (int) KEYWORD_COUNT; ++i) {
        if (strcmp(str, keyword_map[i].keyword) == 0) {
            return keyword_map[i].type;
        }
    }
    return IDENTIFIER;
}

bool is_primitive(TokenType t) {
    switch(t) {
        case U0: case U8: case U16: case U32: case U64: case I8: case I16:
        case I32: case I64: case F64: case BOOL: case STRING:
            return true;
        default: return false;
    }
}

bool is_literal(TokenType t) {
    switch(t) {
        case INT_LITERAL:
        case FLOAT_LITERAL:
        case CHAR_LITERAL:
        case STRING_LITERAL:
            return true;
        default: return false;
    }
}

void free_token_data(Token* token) {
    if (token->type == STRING_LITERAL) {
        free(token->data.str_val);
    }
    free(token->start);
}

Token* create_lexeme_token(TokenType type, const char* lexeme, int line) {
    Token* t = malloc(sizeof(Token)); // memory passed to the caller
    t->type = type;
    t->line = line;

    t->start = strdup(lexeme);
    t->length = strlen(lexeme);

    if (type == STRING_LITERAL && lexeme[0] == '"' && lexeme[t->length - 1] == '"') {
        // trim the quotation marks for the literal
        t->data.str_val = malloc(t->length - 1);
        // move past " and copy up to and including end quote
        strncpy(t->data.str_val, lexeme + 1, t->length - 1);
        t->data.str_val[t->length - 2] = '\0'; // replace end quote with null byte
    }
    return t;
}

// Adding token to the list of Tokens
void add_token(Token*** tokens, int* token_count, int* token_capacity, Token* token){
    if (token == NULL) return;
    if (*token_count >= *token_capacity) {
        *token_capacity *= 2;
        *tokens = realloc(*tokens, sizeof(Token*) * (*token_capacity));
        for (int i = *token_count; i < *token_capacity; ++i) {
            (*tokens)[i] = NULL;
        }
    }
    (*tokens)[*token_count] = token;
    ++(*token_count);
}

// Put either keyword or identifier (whichever the following chars that are read are) into the list of tokens
// Only called when c is an alpha or _, so that we know it is not any string/number literal
void put_keyword_indentifier_token(Token*** tokens, int* token_count, int* token_capacity, FILE* fp, char c, int line) {
    int len = 0;
    int capacity = 16;
    char* lexeme = malloc(sizeof(char) * capacity);
    lexeme[len++] = c;

    int c_int;
    while ((c_int = fgetc(fp)) != EOF) {
        char next_c = (char) c_int;

        if (isalnum(next_c) || next_c == '_') {
            // resize if necessary
            if (len >= capacity - 1) {
                capacity *= 2;
                lexeme = realloc(lexeme, capacity);
            }
            lexeme[len++] = next_c;
        } else {
            ungetc(c_int, fp);
            break;
        }
    }
    lexeme[len] = '\0';

    TokenType type = lookup_type(lexeme);
    Token* t = create_lexeme_token(type, lexeme, line);
    free(lexeme);
    add_token(tokens, token_count, token_capacity, t);
}

// Parse subsequent number (int and float) literal
Token* get_number_token(FILE* fp, char c, int line) {
    int len = 0;
    int capacity = 16;
    char* lexeme = malloc(sizeof(char) * capacity);
    lexeme[len++] = c;

    int base = 10;
    bool is_hex = false;
    bool is_binary = false;
    bool has_decimal = false;

    int c_int;
    // if it starts with a zero, we have to deduce the base
    if (c == '0') {
        c_int = fgetc(fp);
        if (c_int != EOF) {
            c = (char) c_int;
            if (c == 'x' || c == 'X') {
                is_hex = true;
                base = 16;
                lexeme[len++] = c;
            } else if (c == 'b' || c == 'B') {
                is_binary = true;
                base = 2;
                lexeme[len++] = c;
            } else {
                // it is either just another integer, or an invalid character
                // either way we can simply un-get and continue, because these
                // will be addressed in the loop below
                ungetc(c_int, fp);
            }
        }
    }

    while (1) {
        c_int = fgetc(fp);
        if (c_int == EOF) break;
        c = (char) c_int;

        bool valid = false;

        if (is_hex) {
            valid = isxdigit(c);
        } else if (is_binary) {
            valid = (c == '0' || c == '1');
        } else if (has_decimal) {
            if (isdigit(c)) {
                valid = true;
            }
        } else {
            if (isdigit(c)) {
                valid = true;
            } else if (c == '.' && !has_decimal) {
                valid = true;
                has_decimal = true;
            }
        }

        if (valid) {
            if (len >= capacity - 1) {
                capacity *= 2;
                lexeme = realloc(lexeme, capacity);
            }
            lexeme[len++] = c;
        } else {
            ungetc(c_int, fp);
            break;
        }
    }

    if (len >= 1 && lexeme[len-1] == '.') {
        free(lexeme);
        fatal_error(line, "Lexer", "trailing '.' characters are not allowed as identifiers or floats\n");
    }
    lexeme[len] = '\0';

    if (!has_decimal) {
        char* iEnd;
        uint64_t u_val;
        char* start_num = base == 2 ? lexeme + 2 : lexeme; // we have to omit the 0b
        u_val = strtoull(start_num, &iEnd, base); // handles different bases for us
        bool is_num = (iEnd == lexeme + strlen(lexeme)) && (*iEnd == '\0');

        free(lexeme);
        if (!is_num) {
            fatal_error(line, "Lexer", "Parsed number lexeme was not a number\n");
        }

        Token* t = malloc(sizeof(Token));
        t->type = INT_LITERAL;
        t->data.int_val = u_val;
        t->line = line;
        return t;
    } else {
        char* fEnd;
        double f_val = strtod(lexeme, &fEnd);
        bool is_float = (fEnd == lexeme + strlen(lexeme)) && (*fEnd == '\0');

        free(lexeme);
        if (!is_float) {
            fatal_error(line, "Lexer", "Parsed number lexeme was not a number\n");
        }

        Token* t = malloc(sizeof(Token));
        t->type = FLOAT_LITERAL;
        t->data.float_val = f_val;
        t->line = line;
        return t;
    }
}

// Parse subsequent string literal
Token* get_string_token(FILE* fp, char c, int line) {
    bool escape_next = false;
    int c_int;
    int len = 0;
    int capacity = 16;
    char* lexeme = malloc(sizeof(char) * capacity);
    lexeme[len++] = c;
    while ((c_int = fgetc(fp)) != EOF) {
        char next_c = (char)c_int;

        // escape sequences (e.g., \", \n, \\)
        if (escape_next) {
            escape_next = false;
            switch (next_c) {
                case 'n': next_c = '\n'; break;
                case 't': next_c = '\t'; break;
                case 'r': next_c = '\r'; break;
                case '0': next_c = '\0'; break;
            }
        } else if (next_c == '\\') {
            escape_next = true;
            continue; // skip adding '\', process next character
        } else if (next_c == '"') {
            // end of string literal
            lexeme[len++] = next_c;
            lexeme[len] = '\0';
            Token* str_token = create_lexeme_token(STRING_LITERAL, lexeme, line);
            free(lexeme);
            return str_token;
        }

        if (len >= capacity - 1) {
            capacity *= 2;
            lexeme = realloc(lexeme, capacity);
        }

        lexeme[len++] = next_c;
    }

    // we did not find closing quote
    free(lexeme);
    fatal_error(line, "Lexer", "unterminated string literal in lexeme\n");
    return NULL;
}

// Parse subsequent char literal
Token* get_char_token(FILE* fp, int line) {
    int char_content = fgetc(fp);
    if (char_content == EOF) fatal_error(line, "Lexer", "no char or end single quote found before EOF\n");
    if (((char) char_content) == '\\') fatal_error(line, "Lexer", "char content cannot be backslash\n");

    // get the closing quote
    int c_int = fgetc(fp);
    if (c_int == EOF || ((char) c_int) != '\'') fatal_error(line, "Lexer", "no end single quote found\n");

    Token* t = malloc(sizeof(Token));
    t->line = line;
    t->type = CHAR_LITERAL;
    t->data.int_val = char_content;
    return t;
}

// See if the next character in the file stream matches c, consumes if yes, otherwise puts back
bool match_char(FILE* fp, char c) {
    int c_int = fgetc(fp);
    if (c_int != EOF && (char)c_int == c) {
        return true;
    }
    if (c_int != EOF) {
        ungetc(c_int, fp);
    }
    return false;
}

void lex_file(FILE* fp, Token*** tokens, int* token_count, int* token_capacity){
    if (*token_capacity == 0) {
        // initialize our lexer to default capacity and allocation
        *token_capacity = 100;
        *token_count = 0;
        *tokens = malloc(sizeof(Token*) * (*token_capacity));
        // (*tokens)[token*, token*, ..., token*]
        for (int i = *token_count; i < *token_capacity; ++i) {
            (*tokens)[i] = NULL;
        }
    }

    int line = 1;

    int c_int;
    while ((c_int = getc(fp)) != EOF) {
        char c = (char) c_int;
        // we don't really have a lot of care for whitespace, semicolons and commas are primary
        // for separating statements (besides parentheses and content blocks {} [])
        if (isspace(c_int)) {
            if (c == '\n') ++line;
            continue;
        }

        switch (c) {
            case '(': add_token(tokens, token_count, token_capacity, create_lexeme_token(LPAREN, "(", line)); break;
            case ')': add_token(tokens, token_count, token_capacity, create_lexeme_token(RPAREN, ")", line)); break;
            case '{': add_token(tokens, token_count, token_capacity, create_lexeme_token(LBRACE, "{", line)); break;
            case '}': add_token(tokens, token_count, token_capacity, create_lexeme_token(RBRACE, "}", line)); break;
            case '[': add_token(tokens, token_count, token_capacity, create_lexeme_token(LBRACK, "[", line)); break;
            case ']': add_token(tokens, token_count, token_capacity, create_lexeme_token(RBRACK, "]", line)); break;
            case ';': add_token(tokens, token_count, token_capacity, create_lexeme_token(SEMICOLON, ";", line)); break;
            case ',': add_token(tokens, token_count, token_capacity, create_lexeme_token(COMMA, ":", line)); break;
            case '~': add_token(tokens, token_count, token_capacity, create_lexeme_token(TILDE, "~", line)); break;
            case '+': add_token(tokens, token_count, token_capacity, create_lexeme_token(PLUS, "+", line)); break;
            case '-': add_token(tokens, token_count, token_capacity, create_lexeme_token(MINUS, "-", line)); break;
            case '*': add_token(tokens, token_count, token_capacity, create_lexeme_token(STAR, "*", line)); break;
            case '=': add_token(tokens, token_count, token_capacity, create_lexeme_token(EQUAL, "=", line)); break;
            case '%': add_token(tokens, token_count, token_capacity, create_lexeme_token(MODULO, "%", line)); break;

            // compounds operators
            case ':': {
                          bool match = match_char(fp, '=');
                          add_token(tokens, token_count,
                                  token_capacity,
                                  create_lexeme_token(match ?
                                      WALRUS : COLON,
                                      match ? ":=" : ":", line));
                          break;
                      }
            case '!': {
                          bool match = match_char(fp, '=');
                          add_token(tokens, token_count,
                                  token_capacity,
                                  create_lexeme_token(match ?
                                      BANG_EQUAL : BANG,
                                      match ? "!=" : "!", line));
                          break;
                      }
            case '<': {
                          bool match = match_char(fp, '=');
                          add_token(tokens, token_count,
                                  token_capacity,
                                  create_lexeme_token(match ?
                                      LESS_EQUAL : LESS,
                                      match ? "<=" : "<", line));
                          break;
                      }
            case '>': {
                          bool match = match_char(fp, '=');
                          add_token(tokens, token_count,
                                  token_capacity,
                                  create_lexeme_token(match ?
                                      GREATER_EQUAL : GREATER,
                                      match ? ">=" : ">", line));
                          break;
                      }

            case '\'': add_token(tokens, token_count,
                               token_capacity,
                               get_char_token(fp, line)); break;
            case '"': add_token(tokens, token_count,
                              token_capacity,
                              get_string_token(fp, c, line)); break;

            // comments
            case '/':
              {
                  int starting_line = line;
                  if (match_char(fp, '/')) { // single line comment
                      // comment out the rest of the line
                      while ((c_int = getc(fp)) != EOF && (char)c_int != '\n');
                      if (c_int != EOF) ++line;
                  } else if (match_char(fp, '*')) { // block comment
                      bool comment_closed = false;
                      while ((c_int = getc(fp)) != EOF) {
                          c = (char)c_int;
                          if (c == '*') {
                              if (match_char(fp, '/')) {
                                  comment_closed = true;
                                  break;
                              }
                          } else if (c == '\n') {
                              ++line;
                          }
                      }
                      if (!comment_closed) {
                          fatal_error(starting_line, "Lexer", "unterminated block comment\n");
                      }
                  } else {
                      add_token(tokens, token_count, token_capacity, create_lexeme_token(SLASH, "/", line));
                  }
                  break;
              }
            default:
              {
                  if (isdigit(c_int)) {
                      add_token(tokens, token_count,
                              token_capacity,
                              get_number_token(fp, c, line));
                  } else if (isalpha(c_int) || c == '_') {
                      put_keyword_indentifier_token(tokens, token_count,
                              token_capacity, fp, c, line);
                  } else {
                      fatal_error(line, "Lexer", "unexpected token (%c)\n", c);
                  }
                  break;
              }
        }
    }
}
