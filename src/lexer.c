#include "lexer.h"
#include "util.h"
#include <assert.h>
#include <limits.h>
#include <stdint.h>

Category type_category(TokenType type) {
    switch (type) {
        case FUNC: case AND: case MUT:
        case OR: case IF: case ELSE: case FOR:
        case WHILE: case PRINT: case RETURN: case RETURNS:
            return Keyword;

        case IDENTIFIER:
            return Identifier;

        case U0_TYPE: case U8_TYPE: case U16_TYPE: case U32_TYPE:
        case U64_TYPE: case I8_TYPE: case I16_TYPE: case I32_TYPE:
        case I64_TYPE: case F64_TYPE: case BOOL_TYPE: case STRING_TYPE:
            return Primitive_type;

        case TRUE: case FALSE: case CHAR_LITERAL: case NULL_LITERAL:
        case INT_LITERAL: case STRING_LITERAL: case FLOAT_LITERAL:
            return Literal;

        case LPAREN: case RPAREN: case LBRACE: case RBRACE:
        case LBRACKET: case RBRACKET: case COLON: case SEMICOLON:
        case COMMA: case TILDE:
            return Punct;

        case BANG: case PLUS: case MINUS: case FSLASH: case STAR:
        case EQUAL: case MODULO: case WALRUS: case BANG_EQUAL:
        case LESS: case LESS_EQUAL: case GREATER: case GREATER_EQUAL:
            return Operator;

        case NO_TYPE:
            return Keyword;
    }
    return Keyword;
}

const char* tok_string(TokenType type) {
    switch (type) {
        // keywords
        case FUNC:          return "func";
        case AND:           return "and";
        case OR:            return "or";
        case IF:            return "if";
        case ELSE:          return "else";
        case FOR:           return "for";
        case WHILE:         return "while";
        case PRINT:         return "print";
        case RETURN:        return "return";
        case RETURNS:       return "returns";
        case MUT:           return "mut";
        case NULL_LITERAL:  return "null";
        case TRUE:          return "true";
        case FALSE:         return "false";

                            // primitive types
        case U0_TYPE:       return "u0";
        case U8_TYPE:       return "u8";
        case U16_TYPE:      return "u16";
        case U32_TYPE:      return "u32";
        case U64_TYPE:      return "u64";
        case I8_TYPE:       return "i8";
        case I16_TYPE:      return "i16";
        case I32_TYPE:      return "i32";
        case I64_TYPE:      return "i64";
        case F64_TYPE:      return "f64";
        case BOOL_TYPE:     return "bool";
        case STRING_TYPE:   return "String";

                            // syntax
        case LPAREN:        return "(";
        case RPAREN:        return ")";
        case LBRACE:        return "{";
        case RBRACE:        return "}";
        case LBRACKET:      return "[";
        case RBRACKET:      return "]";
        case COLON:         return ":";
        case SEMICOLON:     return ";";
        case COMMA:         return ",";
        case TILDE:         return "~";

                            // operators
        case BANG:          return "!";
        case PLUS:          return "+";
        case MINUS:         return "-";
        case FSLASH:        return "/";
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

        // either an identifier or literal (besides true/false/null), as these are the two
        // categories that are ommitted due to not being constant
        default:            return "UNKNOWN";
    }
}

TokenType lookup_type(const char* str) {
    for (int i = 0; i < NO_TYPE; ++i) {
        TokenType type = (TokenType)i;
        if (strcmp(str, tok_string(type)) == 0) {
            return type;
        }
    }
    return NO_TYPE;
}

void free_token_data(Token* token) {
    if ((token->category == Literal && token->type != STRING_LITERAL)
        || token->category == Punct || token->category == Operator) return;
    free(token->data.lexeme);
}

// Allocate a new Token and store lexeme in its char* segment.
// Used for string literals, keywords, and identifiers (keywords and idents should be verified first)
Token* create_lexeme_token(TokenType type, const char* lexeme, int line) {
    Token* t = malloc(sizeof(Token)); // memory passed to the caller
    t->category = type_category(type);
    t->type = type;
    t->line = line;

    int sz = strlen(lexeme);
    if (type == STRING_LITERAL && lexeme[0] == '"' && lexeme[sz - 1] == '"') {
        // trim the quotation marks for the literal
        t->data.lexeme = malloc(sz - 1);
        // move past " and copy up to and including end quote
        strncpy(t->data.lexeme, lexeme + 1, sz - 1);
        t->data.lexeme[sz - 2] = '\0'; // replace end quote with null byte
    } else {
        t->data.lexeme = strdup(lexeme);
    }
    return t;
}

// For tokens that store no data in the Token.data
Token* create_no_data_token(TokenType type, int line) {
    Token* t = malloc(sizeof(Token)); // memory passed to the caller
    t->category = type_category(type);
    t->type = type;
    t->line = line;
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

    Token* t = NULL;
    TokenType type = lookup_type(lexeme);
    if (type == NO_TYPE) {
        type = IDENTIFIER;
    }
    if (type_category(type) == Literal) {
        // it must be either null/true/false because
        // int/float/string/char literals are not found in the lookup
        t = create_no_data_token(type, line);
    } else {
        t = create_lexeme_token(type, lexeme, line);
    }
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
        throw_fatal_error(line, "Lexer", "trailing '.' characters are not allowed as identifiers or floats\n");
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
            throw_fatal_error(line, "Lexer", "Parsed number lexeme was not a number\n");
        }

        Token* t = malloc(sizeof(Token));
        t->category = Literal;
        t->type = INT_LITERAL;
        t->data.int_t = u_val;
        t->line = line;
        return t;
    } else {
        char* fEnd;
        double f_val = strtod(lexeme, &fEnd);
        bool is_float = (fEnd == lexeme + strlen(lexeme)) && (*fEnd == '\0');

        free(lexeme);
        if (!is_float) {
            throw_fatal_error(line, "Lexer", "Parsed number lexeme was not a number\n");
        }

        Token* t = malloc(sizeof(Token));
        t->category = Literal;
        t->type = FLOAT_LITERAL;
        t->data.f64_value = f_val;
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
    throw_fatal_error(line, "Lexer", "unterminated string literal in lexeme\n");
    return NULL;
}

// Parse subsequent char literal
Token* get_char_token(FILE* fp, int line) {
    int char_content = fgetc(fp);
    if (char_content == EOF) throw_fatal_error(line, "Lexer", "no char or end single quote found before EOF\n");
    if (((char) char_content) == '\\') throw_fatal_error(line, "Lexer", "char content cannot be backslash\n");

    // get the closing quote
    int c_int = fgetc(fp);
    if (c_int == EOF || ((char) c_int) != '\'') throw_fatal_error(line, "Lexer", "no end single quote found\n");

    Token* t = malloc(sizeof(Token));
    t->line = line;
    t->category = Literal;
    t->type = CHAR_LITERAL;
    t->data.int_t = char_content;
    return t;
}

// See if the next character in the file stream matches c, consumes if yes, otherwise puts back
bool match_char(FILE* fp, char c) {
    int c_int = fgetc(fp);
    if (c_int != EOF && (char) c_int == c) {
        return true;
    }
    ungetc(c_int, fp);
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
            case '(': add_token(tokens, token_count, token_capacity, create_no_data_token(LPAREN, line)); break;
            case ')': add_token(tokens, token_count, token_capacity, create_no_data_token(RPAREN, line)); break;
            case '{': add_token(tokens, token_count, token_capacity, create_no_data_token(LBRACE, line)); break;
            case '}': add_token(tokens, token_count, token_capacity, create_no_data_token(RBRACE, line)); break;
            case '[': add_token(tokens, token_count, token_capacity, create_no_data_token(LBRACKET, line)); break;
            case ']': add_token(tokens, token_count, token_capacity, create_no_data_token(RBRACKET, line)); break;
            case ';': add_token(tokens, token_count, token_capacity, create_no_data_token(SEMICOLON, line)); break;
            case ',': add_token(tokens, token_count, token_capacity, create_no_data_token(COMMA, line)); break;
            case '~': add_token(tokens, token_count, token_capacity, create_no_data_token(TILDE, line)); break;
            case '+': add_token(tokens, token_count, token_capacity, create_no_data_token(PLUS, line)); break;
            case '-': add_token(tokens, token_count, token_capacity, create_no_data_token(MINUS, line)); break;
            case '*': add_token(tokens, token_count, token_capacity, create_no_data_token(STAR, line)); break;
            case '=': add_token(tokens, token_count, token_capacity, create_no_data_token(EQUAL, line)); break;
            case '%': add_token(tokens, token_count, token_capacity, create_no_data_token(MODULO, line)); break;

            // compounds operators
            case ':': add_token(tokens, token_count, token_capacity, create_no_data_token(match_char(fp, '=') ? WALRUS : COLON, line)); break;
            case '!': add_token(tokens, token_count, token_capacity, create_no_data_token(match_char(fp, '=') ? BANG_EQUAL : BANG, line)); break;
            case '<': add_token(tokens, token_count, token_capacity, create_no_data_token(match_char(fp, '=') ? LESS_EQUAL : LESS, line)); break;
            case '>': add_token(tokens, token_count, token_capacity, create_no_data_token(match_char(fp, '=') ? GREATER_EQUAL : GREATER, line)); break;
            case '\'': add_token(tokens, token_count, token_capacity, get_char_token(fp, line)); break;
            case '"': add_token(tokens, token_count, token_capacity, get_string_token(fp, c, line)); break;

            // comments
            case '/':
              {
                  if (match_char(fp, '/')) {
                      // comment out the rest of the line
                      while ((c_int = getc(fp)) != EOF && (char)c_int != '\n');
                      ++line;
                  } else {
                      add_token(tokens, token_count, token_capacity, create_no_data_token(FSLASH, line));
                  }
                  break;
              }
            default:
              {
                  if (isdigit(c_int)) {
                      add_token(tokens, token_count, token_capacity, get_number_token(fp, c, line));
                  } else if (isalpha(c_int) || c == '_') {
                      put_keyword_indentifier_token(tokens, token_count, token_capacity, fp, c, line);
                  } else {
                      throw_fatal_error(line, "Lexer", "unexpected token (%c)\n", c);
                  }
                  break;
              }
        }
    }
}
