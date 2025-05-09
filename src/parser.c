#include "ast.h"
#include "util.h"
#include "parser.h"

// === For easy movement through the tokens of the parser
// return token at the current position in the parser
static Token* current(Parser* p) {
    return (p->pos < p->size) ? p->tokens[p->pos] : NULL;
}

static int get_token_line(Parser* p) {
    return current(p) ? current(p)->line : -1;
}

// return token at the current position in the parser and then increment position
static Token* advance(Parser* p) {
    return (p->pos < p->size) ? p->tokens[p->pos++] : NULL;
}

// check if the type of the Token at the current position of the parser equals type
static bool match(Parser* p, TokenType type) {
    return current(p) && current(p)->type == type;
}

// consume an expected type character at current position and increment position
static void consume(Parser* p, TokenType type, const char* err) {
    if (match(p, type)) {
        advance(p);
        return;
    }
    throw_fatal_error(get_token_line(p), "Parser", "expected token: %s\n", err);
}
// ===

// allocate and initialize parser based on the result from the lexer
Parser* parser_init(Token** tokens, int count) {
    Parser* p = malloc(sizeof(Parser));
    p->tokens = tokens;
    p->pos = 0;
    p->size = count;
    p->symtab = symtab_create(NULL); // create the global scope symbol table
    return p;
}

// driver function for the parser that returns the root of the AST
ASTNode* parse_program(Parser* p) {
    // because we evaluate statements top down, even if something is not in
    // a function, we still have to add it as a statement, thus the root will
    // be a content block in itself

    ASTNode** root = NULL;
    int count = 0;

    // though the only possible executions is either a statement or function
    while (current(p) != NULL) {
        if (match(p, FUNC)) {
            ASTNode* func = parse_function(p);
            root = realloc(root, ++count * sizeof(ASTNode*));
            root[count-1] = func;
        } else if (match(p, SEMICOLON)) {
            // skip empty statements
            advance(p);
        } else {
            // it must be a statement
            ASTNode* stmt = parse_statement(p);
            root = realloc(root, ++count * sizeof(ASTNode*));
            root[count-1] = stmt;
        }
    }

    // root is the global content block
    return create_block(root, count);
}

ASTNode* parse_function(Parser* p) {
    consume(p, FUNC, "Expected 'func'");

    Token* name_tok = current(p);
    consume(p, IDENTIFIER, "Expected function name");

    // enter function scope
    symtab_enter_new_scope(&p->symtab);

    consume(p, LPAREN, "Expected '('");
    ASTNode** params = NULL;
    int param_count = 0;
    while (!match(p, RPAREN)) {
        // iterate throgh all parameters and add them to the list of params
        ASTNode* param = parse_param(p);
        params = realloc(params, ++param_count * sizeof(ASTNode*));
        params[param_count-1] = param;

        // if we see a comma, then we should loop again to check for another
        if (!match(p, COMMA)) break;

        // though if there is a lingering comma, we will still exit the loop upon loop condition
        advance(p);
    }
    consume(p, RPAREN, "Expected ')'");

    ASTNode* return_type = NULL;
    if (match(p, RETURNS)) {
        advance(p);
        consume(p, LPAREN, "Expected '('");
        return_type = parse_param(p);
        consume(p, RPAREN, "Expected ')'");
    } else {
        // void return type is implicit when ommitting "returns" clause
        return_type = create_type(false, U0_TYPE);
    }

    // add the function into the PARENT scope, not the current scope which is its own scope
    Symbol* func_sym = symtab_insert(p->symtab->parent, name_tok->data.lexeme, SYM_FUNCTION, return_type, NULL, false);
    if (func_sym == NULL) {
        // duplicate declaration
        throw_fatal_error(name_tok->line, "Parser", "duplicate function declaration (%s)\n", name_tok->data.lexeme);
    }

    // do not create another scope in the body
    ASTNode* body = parse_block(p, false);

    symtab_exit_scope(&p->symtab);

    return create_func_decl(return_type, name_tok->data.lexeme, params, param_count, body);
}

ASTNode* parse_param(Parser* p) {
    // my_param : u8
    if (!match(p, IDENTIFIER)) {
        throw_fatal_error(get_token_line(p), "Parser", "expected parameter name before ': <TYPE>'\n");
    }

    Token* name_tok = advance(p);
    consume(p, COLON, "Expected ':'");
    ASTNode* type = parse_type(p);

    ASTNode* param = create_param(type, name_tok->data.lexeme);

    Symbol* param_sym = symtab_insert(p->symtab, param->param.name, SYM_PARAMETER, param->param.type, param, false);
    if (param_sym == NULL) {
        // duplicate declaration
        throw_fatal_error(name_tok->line, "Parser", "duplicate variable declaration within parameters (%s)\n", param->param.name);
    }

    return param;
}

ASTNode* parse_block(Parser* p, bool create_scope) {
    if (create_scope) {
        symtab_enter_new_scope(&p->symtab);
    }

    consume(p, LBRACE, "Expected '{'");
    ASTNode** stmts = NULL;
    int count = 0;

    while (!match(p, RBRACE)) {
        // just like with params, iterate through all statements
        ASTNode* stmt = parse_statement(p);
        stmts = realloc(stmts, ++count * sizeof(ASTNode*));
        stmts[count-1] = stmt;
    }
    consume(p, RBRACE, "Expected '}'");

    if (create_scope) {
        symtab_exit_scope(&p->symtab);
    }

    return create_block(stmts, count);
}

ASTNode* parse_statement(Parser* p) {
    // check keywords
    if (match(p, RETURN)) {
        advance(p);
        ASTNode* expr = NULL;
        if (!match(p, SEMICOLON)) {
            expr = parse_expression(p);
        }
        consume(p, SEMICOLON, "Expected ';' after return");
        return create_return(expr);
    }

    if (match(p, PRINT)) {
        advance(p);
        ASTNode* expr = parse_expression(p);
        consume(p, SEMICOLON, "Expected ';' after print");
        return create_print(expr);
    }

    if (match(p, IF)) {
        advance(p);
        consume(p, LPAREN, "Expected '(' after if");
        ASTNode* cond = parse_expression(p);
        consume(p, RPAREN, "Expected ')' after if");
        ASTNode* then_block = parse_block(p, true); // create new scope
        ASTNode* else_block = NULL;

        if (match(p, ELSE)) {
            advance(p);
            else_block = parse_block(p, true); // new scope
        }
        return create_if(cond, then_block, else_block);
    }

    if (match(p, WHILE)) {
        advance(p);
        consume(p, LPAREN, "Expected '(' after while");
        ASTNode* cond = parse_expression(p);
        consume(p, RPAREN, "Expected ')' after while");
        ASTNode* body = parse_block(p, true);
        return create_while(cond, body);
    }

    if (match(p, FOR)) {
        advance(p);
        consume(p, LPAREN, "Expected '(' after for");

        symtab_enter_new_scope(&p->symtab); // new scope for init () and block {}

        ASTNode* init = NULL;
        if (match(p, MUT) || current(p)->category == Primitive_type) {
            init = parse_var_decl(p);
        } else if (match(p, IDENTIFIER)) {
            init = parse_expression(p);
            consume(p, SEMICOLON, "Expected ';' in for loop after initialization");
        } else {
            consume(p, SEMICOLON, "Expected ';' in for loop after initialization");
        }

        ASTNode* end = NULL;
        if (!match(p, SEMICOLON)) {
            end = parse_expression(p);
        }
        consume(p, SEMICOLON, "Expected ';' in for loop after condition");

        ASTNode* iter = NULL;
        if (!match(p, RPAREN)) {
            iter = parse_expression(p);
        }
        consume(p, RPAREN, "Expected ')' after for loop");

        // the loop body has its own scope though note that the initialization
        // vars will still be accessible from the parent scope in the block {}
        ASTNode* body = parse_block(p, true);

        symtab_exit_scope(&p->symtab); // exit loop init scope
        return create_for(init, end, iter, body);
    }

    if (match(p, LBRACE)) {
        // create new scope
        return parse_block(p, true);
    }

    // parse variable declaration (with possible initialization)
    if (match(p, MUT) || current(p)->category == Primitive_type) {
        return parse_var_decl(p);
    }

    // otherwise if it is not a keyword it must simply be an expression
    ASTNode* expr = parse_expression(p);
    consume(p, SEMICOLON, "Expected ';' after expression");
    return expr;
}

ASTNode* parse_var_decl(Parser* p) {
    ASTNode* type = parse_type(p);
    Token* name_tok = current(p);
    consume(p, IDENTIFIER, "Expected variable name");

    Symbol* sym = symtab_insert(p->symtab, name_tok->data.lexeme, SYM_VARIABLE, type, NULL, type->primitive.mut);
    if (sym == NULL) {
        // duplicate declaration
        throw_fatal_error(name_tok->line, "Parser", "duplicate variable declaration (%s)\n", name_tok->data.lexeme);
    }

    ASTNode* init = NULL;
    if (match(p, WALRUS)) {
        advance(p);
        init = parse_expression(p);
    }

    consume(p, SEMICOLON, "Expected ';' after variable declaration");
    return create_var_decl(type, name_tok->data.lexeme, init);
}

ASTNode* parse_expression(Parser* p) {
    // this is the top of the grammar, the lowest precedence
    return parse_assignment(p);
}

ASTNode* parse_assignment(Parser* p) {
    ASTNode* left = parse_logic_or(p);

    if (match(p, WALRUS)) {
        advance(p);
        if (left->type != NODE_IDENTIFIER) {
            // we should be looking at an identifier when assigning
            throw_fatal_error(get_token_line(p), "Parser", "invalid assignment target\n");
        }
        // now p is at the rhs of the assignment expression
        // do a recursive call for right associativity of assignments
        ASTNode* value = parse_assignment(p);

        ASTNode* assign = create_assign(left->identifier, value);

        // no longer need left node or its string data because it will be packed
        // into the assign node with value being what is being assigned
        free(left->identifier);
        free(left);
        return assign;
    }

    // just return this logical expression
    return left;
}

ASTNode* parse_logic_or(Parser* p) {
    ASTNode* left = parse_logic_and(p);

    while (match(p, OR)) {
        advance(p);
        ASTNode* right = parse_logic_and(p);
        left = create_bin_op(OR, left, right);
    }
    return left;
}

ASTNode* parse_logic_and(Parser* p) {
    ASTNode* left = parse_equality(p);

    while (match(p, AND)) {
        advance(p);
        ASTNode* right = parse_equality(p);
        left = create_bin_op(AND, left, right);
    }
    return left;
}

ASTNode* parse_equality(Parser* p) {
    ASTNode* left = parse_comparison(p);

    while (match(p, EQUAL) || match(p, BANG_EQUAL)) {
        TokenType op = advance(p)->type;
        ASTNode* right = parse_comparison(p);
        left = create_bin_op(op, left, right);
    }
    return left;
}

ASTNode* parse_comparison(Parser* p) {
    ASTNode* left = parse_additive(p);

    while (match(p, LESS) || match(p, LESS_EQUAL) || match(p, GREATER) || match(p, GREATER_EQUAL)) {
        TokenType op = advance(p)->type;
        ASTNode* right = parse_additive(p);
        left = create_bin_op(op, left, right);
    }
    return left;
}

ASTNode* parse_additive(Parser* p) {
    ASTNode* left = parse_multiplicative(p);

    while (match(p, PLUS) || match(p, MINUS)) {
        TokenType op = advance(p)->type;
        ASTNode* right = parse_multiplicative(p);
        left = create_bin_op(op, left, right);
    }
    return left;
}

ASTNode* parse_multiplicative(Parser* p) {
    ASTNode* left = parse_primary(p);

    while (match(p, STAR) || match(p, FSLASH) || match(p, MODULO)) {
        // get the current op that it is and then check right identifier
        TokenType op = advance(p)->type;
        ASTNode* right = parse_primary(p);
        left = create_bin_op(op, left, right);
    }
    return left;
}

ASTNode* parse_primary(Parser* p) {
    Token* tok = current(p);
    switch(tok->type) {
        case IDENTIFIER: {
            const char* name = tok->data.lexeme;
            Symbol* sym = symtab_lookup(p->symtab, tok->data.lexeme);
            if (sym == NULL) {
                // undefined variable
                throw_fatal_error(tok->line, "Parser", "undefined variable (%s)\n", name);
            }
            advance(p); // actually move past the identifier

            // check if function call on this identifier and handle
            if (match(p, LPAREN)) {
                advance(p);
                ASTNode** args = NULL;
                int arg_count = 0;

                while (!match(p, RPAREN)) {
                    ASTNode* arg = parse_expression(p);
                    args = realloc(args, ++arg_count * sizeof(ASTNode*));
                    args[arg_count-1] = arg;

                    if (!match(p, COMMA)) break;
                    advance(p);
                }
                consume(p, RPAREN, "Expected ')' after arguments");
                return create_func_call(name, args, arg_count);
            }
            // otherwise is just an identifier
            return create_identifier(name);
        }
        case INT_LITERAL:
        case FLOAT_LITERAL:
        case STRING_LITERAL:
        case CHAR_LITERAL:
        case TRUE:
        case FALSE:
        case NULL_LITERAL: {
            ASTNode* lit = create_literal(advance(p));
            return lit;
        }
        case LPAREN: {
            advance(p);
            ASTNode* expr = parse_expression(p);
            consume(p, RPAREN, "Expected ')' after expression");
            return expr;
        }
        case BANG:
        case MINUS: {
            TokenType op = advance(p)->type;
            ASTNode* operand = parse_primary(p);
            return create_unary_op(op, operand);
        }
        default:
            throw_fatal_error(get_token_line(p), "Parser", "unexpected token in expression: %s\n", tok_string(tok->type));
            exit(EXIT_FAILURE);
    }
}

ASTNode* parse_type(Parser* p) {
    bool mut = false;
    if (match(p, MUT)) {
        mut = true;
        advance(p);
    }
    Token* tok = current(p);
    if (tok->category != Primitive_type) {
        throw_fatal_error(get_token_line(p), "Parser", "expected type specifier instead of: %s\n", tok_string(tok->type));
    }

    advance(p);
    return create_type(mut, tok->type);
}

