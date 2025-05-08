#include "lexer.h"
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>

Category type_category(TokenType type) {
    switch (type) {
        case FUNC: case AND:
        case OR: case IF: case ELSE: case FOR:
        case WHILE: case PRINT: case RETURN: case RETURNS:
        case MUT: case NULL_KEYWORD:
            return Keyword;

        case IDENTIFIER:
            return Identifier;

        case U0_TYPE: case U8_TYPE: case U16_TYPE: case U32_TYPE:
        case U64_TYPE: case I8_TYPE: case I16_TYPE: case I32_TYPE:
        case I64_TYPE: case F64_TYPE: case BOOL_TYPE: case STRING_TYPE:
            return Primitive_type;

        case BOOL_LITERAL: case CHAR_LITERAL:
        case INT_LITERAL: case STRING_LITERAL: case FLOAT_LITERAL:
            return Literal;

        case LPAREN: case RPAREN: case LBRACE: case RBRACE:
        case LBRACKET: case RBRACKET: case COLON: case SEMICOLON:
        case COMMA: case TILDE:
            return Symbol;

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
        case NULL_KEYWORD:  return "null";

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

        // either an identifier or literal, as these are the two
        // categories that are ommitted due to not being constant
        default:            return "UNKNOWN";
    }
}

void free_token_data(Token* token) {
    if (token->category != Literal || token->type == STRING_LITERAL) {
        free(token->data.lexeme);
        return;
    }
}

void lex_except(const char* format, ...) {
    va_list args;
    va_start(args, format);
    printf("Lexer exception thrown: ");
    vprintf(format, args);
    va_end(args);
    exit(1);
}

bool is_valid_identifier(const char* lexeme) {
    if (lexeme[0] == '\0') return false;
    if (!isalpha(lexeme[0]) && lexeme[0] != '_') return false;
    for (int i = 1; lexeme[i] != '\0'; ++i) {
        if (!isalnum(lexeme[i]) && lexeme[i] != '_') {
            return false;
        }
    }
    return true;
}

// Get token type where lexeme is limited to possibly being:
// keyword, valid possible identifier name, symbol
TokenType get_keyword_identifier_symbol_type(const char* lexeme) {
    // this loop will only find non-literals and non-identifiers
    for (int i = 0; i < NO_TYPE; ++i) {
        TokenType type = (TokenType)i;
        if (strcmp(lexeme, tok_string(type)) == 0) {
            return type;
        }
    }

    // only works because if we are facing a literal it will
    // never be mistaken for identifier (except for true and false)
    if (strcmp(lexeme,"true")!=0 && strcmp(lexeme,"false")!=0 && is_valid_identifier(lexeme)) {
        return IDENTIFIER;
    }

    // must be a literal
    return NO_TYPE;
}

// Allocate a new Token and store lexeme in its char* segment.
// Only symbols should use this function directly
//  (though keywords and identifiers can be passed if it is certain of
//  their TokenType, which create_token will deduce for the client)
Token* alloc_token_lexeme_data(TokenType type, const char* lexeme, int line) {
    Token* t = (Token*) malloc(sizeof(Token)); // memory passed to the caller
    t->category = type_category(type);
    t->type = type;
    t->line = line;
    t->data.lexeme = strdup(lexeme);
    return t;
}

Token* create_literal_token(const char* lexeme, int line) {
    Token* t = (Token*) malloc(sizeof(Token)); // memory passed to the caller
    t->line = line;
    t->category = Literal;

    if (strcmp(lexeme, "true") == 0) {
        t->type = BOOL_LITERAL;
        t->data.int_t = 1; // true
        return t;
    } else if (strcmp(lexeme, "false") == 0) {
        t->type = BOOL_LITERAL;
        t->data.int_t = 0; // false
        return t;
    }

    int sz = strlen(lexeme); // does not include null byte
    if (lexeme[0] == '"' && lexeme[sz - 1] == '"') {
        t->type = STRING_LITERAL;

        // trim the quotation marks for the literal
        t->data.lexeme = (char*) malloc(sz - 1);
        // move past " and copy up to and including end quote
        strncpy(t->data.lexeme, lexeme + 1, sz - 1);
        t->data.lexeme[sz - 2] = '\0'; // replace end quote with null byte

        return t;
    }

    // check number literals:

    size_t num_len = strlen(lexeme);
    int base = 10;
    // determine base if literal specifies
    if (num_len >= 2) {
        if (lexeme[0] == '0') {
            if (lexeme[1] == 'x' || lexeme[1] == 'X') {
                base = 16;
            } else if (lexeme[1] == 'b' || lexeme[1] == 'B') {
                base = 2;
                lexeme += 2; // move past 0b...
            }
        }
    }

    char* iEnd;
    uint64_t u_val;
    u_val = strtoull(lexeme, &iEnd, base); // handles different bases for us
    bool is_num = (iEnd == lexeme + strlen(lexeme)) && (*iEnd == '\0');

    if (is_num) {
        t->type = INT_LITERAL;
        t->data.int_t = u_val;
        return t;
    }

    char* fEnd;
    double fValue = strtod(lexeme, &fEnd);
    bool is_float = (fEnd == lexeme + strlen(lexeme)) && (*fEnd == '\0');

    if (is_float) {
        t->type = FLOAT_LITERAL;
        t->data.f64_value = fValue;
        return t;
    }


    free_token_data(t);
    free(t);
    return NULL;
}

// Lexeme should be a keyword or an identifier
//  (because symbol types should be known easily by client)
// This will interpret the lexeme and find what the corresponding TokenType is
Token* create_token(const char* lexeme, int line) {
    TokenType type = get_keyword_identifier_symbol_type(lexeme);
    if (type != NO_TYPE) {
        return alloc_token_lexeme_data(type, lexeme, line);
    }

    Token* tok = create_literal_token(lexeme, line);
    if (tok != NULL) {
        return tok;
    }

    lex_except("Invalid token: %s\n", lexeme);
    return NULL;
}

void add_token(Token*** tokens, int* token_count, int* token_capacity, Token* token){
    if (token == NULL) return;
    if (*token_count >= *token_capacity) {
        *token_capacity *= 2;
        Token** temp = (Token**) realloc(*tokens, sizeof(Token*) * (*token_capacity));
        if (temp == NULL) {
            lex_except("Realloc Failed on add_token\n");
            return;
        }
        *tokens = temp;
        for (int i = *token_count; i < *token_capacity; ++i) {
            (*tokens)[i] = NULL;
        }
    }
    (*tokens)[*token_count] = token;
    ++(*token_count);
}

char* alloc_lex_buf(int capacity) {
    char* buf = (char*) malloc(capacity * sizeof(char));
    if (buf == NULL) {
        lex_except("Malloc failed in alloc_lex_buf\n");
        return NULL;
    }
    return buf;
}


// return the single char symbol, or one of the hard-coded compound symbols
char* get_symbol(FILE* fp, char c) {
    int len = 0;
    int capacity = 3; // longest symbol right now is 2, add 1 for null byte
    char* lexeme = alloc_lex_buf(capacity);
    lexeme[len++] = c;

    int next_char_int = fgetc(fp);
    if (next_char_int != EOF) {
        char next_c = (char) next_char_int;
        if ((c == ':' && next_c == '=') ||
            (c == '!' && next_c == '=') ||
            (c == '<' && next_c == '=') ||
            (c == '>' && next_c == '=')) {
            lexeme[len++] = next_c;
        } else {
            ungetc(next_char_int, fp);
        }
    }

    lexeme[len] = '\0';
    return lexeme;
}

char* get_number(FILE* fp, char c) {
    int len = 0;
    int capacity = 16;
    char* lexeme = alloc_lex_buf(capacity);
    lexeme[len++] = c;

    bool is_hex = false;
    bool is_binary = false;
    bool has_decimal = false;

    int next_char_int;
    // if it starts with a zero, we have to deduce the base
    if (c == '0') {
        next_char_int = fgetc(fp);
        if (next_char_int != EOF) {
            char next_c = (char)next_char_int;
            if (next_c == 'x' || next_c == 'X') {
                is_hex = true;
                lexeme[len++] = next_c;
            } else if (next_c == 'b' || next_c == 'B') {
                is_binary = true;
                lexeme[len++] = next_c;
            } else {
                // it is either just another integer, or an invalid character
                // either way we can simply un-get and continue, because these
                // will be addressed in the loop below
                ungetc(next_char_int, fp);
            }
        }
    }

    while (1) {
        next_char_int = fgetc(fp);
        if (next_char_int == EOF) break;
        char next_c = (char)next_char_int;

        bool valid = false;

        if (is_hex) {
            valid = isxdigit(next_c);
        } else if (is_binary) {
            valid = (next_c == '0' || next_c == '1');
        } else if (has_decimal) {
            if (isdigit(next_c)) {
                valid = true;
            }
        } else {
            if (isdigit(next_c)) {
                valid = true;
            } else if (next_c == '.' && !has_decimal) {
                valid = true;
                has_decimal = true;
            }
        }

        if (valid) {
            if (len >= capacity - 1) {
                capacity *= 2;
                char* temp = realloc(lexeme, capacity);
                if (!temp) {
                    free(lexeme);
                    lex_except("Realloc failed in get_number\n");
                    return NULL;
                }
                lexeme = temp;
            }
            lexeme[len++] = next_c;
        } else {
            ungetc(next_char_int, fp);
            break;
        }
    }

    if (len >= 1 && lexeme[len-1] == '.') {
        lex_except("Trailing '.' characters are not allowed as identifiers or floats\n");
    }
    lexeme[len] = '\0';
    return lexeme;
}

// get string literal
char* get_string(FILE* fp, char c) {
    if (c != '"') {
        lex_except("String literal should begin with '\"'\n");
    }
    bool escape_next = false;
    int next_char_int;
    int len = 0;
    int capacity = 16;
    char* lexeme = alloc_lex_buf(capacity);
    lexeme[len++] = c;
    while ((next_char_int = fgetc(fp)) != EOF) {
        char next_c = (char)next_char_int;

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
            return lexeme;
        }

        if (len >= capacity - 1) {
            capacity *= 2;
            char* temp = (char*) realloc(lexeme, capacity);
            if (!temp) {
                free(lexeme);
                lex_except("Realloc failed in get_string\n");
                return NULL;
            }
            lexeme = temp;
        }

        lexeme[len++] = next_c;
    }

    // we did not find closing quote
    free(lexeme);
    lex_except("Unterminated string literal in lexeme");
    return NULL;
}

char* get_identifier(FILE* fp, char c) {
    int len = 0;
    int capacity = 16;
    char* lexeme = alloc_lex_buf(capacity);
    lexeme[len++] = c;

    int next_char_int;
    while ((next_char_int = fgetc(fp)) != EOF) {
        char next_c = (char) next_char_int;

        if (isalnum(next_c) || next_c == '_') {
            // resize if necessary
            if (len >= capacity - 1) {
                capacity *= 2;
                char* temp = realloc(lexeme, capacity);
                if (!temp) {
                    free(lexeme);
                    lex_except("Realloc failed in get_identifier\n");
                    return NULL;
                }
                lexeme = temp;
            }
            lexeme[len++] = next_c;
        } else {
            ungetc(next_char_int, fp);
            break;
        }
    }

    lexeme[len] = '\0';
    return lexeme;
}



char* get_lexeme(FILE* fp, char c) {
    // From c that was just consumed by fp, this will return the full lexeme string that makes up the token based on the value of 'c'
    // Handles, numbers, string literals, identifiers, and single-char/compound symbols
    // Deduces ints in different forms: 42, 0xFF, 0b1010
    // floats must be in the form ((<int>)+) . (<int>)*

    if (c == '.') {
        lex_except("Leading '.' characters are not allowed as identifiers or floats\n");
        return NULL;
    }

    if (c == '"') {
        return get_string(fp, c);
    }

    if (isdigit(c)) {
        // we do not have any "negative" literals in this language
        return get_number(fp, c);
    }

    if (c == '_' || isalpha(c)) {
        return get_identifier(fp, c);
    }


    if (strchr("(){}[]:;,~!+-/*=%<>", c) != NULL) {
        // may be a compound symbol
        return get_symbol(fp, c);
    }

    lex_except("No valid lexeme format found for start char: %c\n", c);
    return NULL;
}

Token* get_char_token(FILE* fp, char c, int line) {
    if (c != '\'') {
        lex_except("Char literals must start and end with single quotes\n");
        return NULL;
    }
    // single quote will always represent start of char literal
    int ni = fgetc(fp);
    if (ni == EOF) lex_except("No closing single quote before EOF\n");

    int content = ni;
    if (((char) ni) == '\\') {
        lex_except("Char cannot contain backslash\n");
    }

    // get the closing quote
    ni = fgetc(fp);
    if (ni == EOF) lex_except("No closing single quote before EOF\n");
    if (((char) ni) == '\'') {
        Token* t = (Token*) malloc(sizeof(Token));
        t->line = line;
        t->category = Literal;
        t->type = CHAR_LITERAL;
        t->data.int_t = content;
        return t;
    }

    lex_except("Started with single quote but token is not a char literal\n");
    return NULL;
}

void lex_file(FILE* fp, Token*** tokens, int* token_count, int* token_capacity){
    if (*token_capacity == 0) {
        // initialize our lexer to default capacity and allocation
        *token_capacity = 100;
        *token_count = 0;
        *tokens = (Token**) malloc(sizeof(Token*) * (*token_capacity));
        // (*tokens)[token*, token*, ..., token*]
        for (int i = *token_count; i < *token_capacity; ++i) {
            (*tokens)[i] = NULL;
        }
    }

    int line = 1;

    int c_int;
    while ((c_int = getc(fp)) != EOF) {
        char c = (char) c_int;
        if (c == '#'){ // comments
            // move to the end of the line and go to next line
            while ((c = getc(fp)) != EOF && c != '\n');
            ++line;
            continue;
        }

        // we don't really have a lot of care for whitespace, semicolons and commas are primary
        // for separating statements (besides parentheses and content blocks {} [])
        if (isspace(c)) {
            if (c == '\n') ++line;
            continue;
        }

        switch (c) {
            // NOTE: STARTING SYMBOLS THAT ARE A PART OF COMPOUND OPERATORS SHOULD NOT EXIST HERE
            // ONLY ISOLATED SYMBOLS
            case '(': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(LPAREN, "(", line)); break;
            case ')': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(RPAREN, ")", line)); break;
            case '{': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(LBRACE, "{", line)); break;
            case '}': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(RBRACE, "}", line)); break;
            case '[': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(LBRACKET, "[", line)); break;
            case ']': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(RBRACKET, "]", line)); break;
            case ';': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(SEMICOLON, ";", line)); break;
            case ',': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(COMMA, ",", line)); break;
            case '~': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(TILDE, "~", line)); break;
            case '+': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(PLUS, "+", line)); break;
            case '-': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(MINUS, "-", line)); break;
            case '/': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(FSLASH, "/", line)); break;
            case '*': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(STAR, "*", line)); break;
            case '=': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(EQUAL, "=", line)); break;
            case '%': add_token(tokens, token_count, token_capacity, alloc_token_lexeme_data(MODULO, "%", line)); break;

            // compound operators, keywords, identifiers, and literals
            case '\'':
              {
                  Token* ch = get_char_token(fp, c, line);
                  add_token(tokens, token_count, token_capacity, ch);
                  break;
              }
            default:
              {
                  char* lexeme = get_lexeme(fp, c);
                  // expected that create_token will reallocate for lexeme copies if necessary
                  add_token(tokens, token_count, token_capacity, create_token(lexeme, line));
                  free(lexeme);
                  break;
              }
        }
    }
}
