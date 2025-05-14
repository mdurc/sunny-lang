#include "ast.h"
#include "parser.h"
#include "error.h"

#define _consume_go(_p, _type, _err, _go) \
    do { \
        if (!consume(_p, _type, _err)) goto _go; \
    } while (0)

#define _consume(_p, _type, _err) \
    do { \
        if (!consume(_p, _type, _err)) return NULL; \
    } while (0)

#define _parse_go(_var, _call, _go) \
    do { \
        _var = _call; \
        if (_var == NULL) goto _go; \
    } while (0)

#define _parse(_var, _call) \
    do { \
        _var = _call; \
        if (_var == NULL) return NULL; \
    } while (0)


// === For easy movement through the tokens of the parser
// return token at the current position in the parser
static Token* current(Parser* p) {
    if (p->pos < p->size) {
        return p->tokens[p->pos];
    }
    return NULL;
}

// return token at the current position in the parser and then increment position
static Token* advance(Parser* p) {
    if (p->pos < p->size) {
        p->last_line = p->tokens[p->pos]->row;
        return p->tokens[p->pos++];
    }
    return NULL;
}

// check if the type of the Token at the current position of the parser equals type
static bool match(Parser* p, TokenType type) {
    return current(p) && current(p)->type == type;
}

// consume an expected type character at current position and increment position
static bool consume(Parser* p, TokenType type, const char* err) {
    if (p->panic_mode) return false;

    if (match(p, type)) {
        advance(p);
        return true;
    }
    Token* curr = current(p);
    int row = p->last_line, col = -1, len = -1;
    if (curr) {
        row = curr->row;
        col = curr->col;
        len = curr->len;
    }
    p->panic_mode = true;
    error_report(p, row, col, len, "Parser", "%s\n", err);
    return false;
}

static bool is_sync_point(Token* tok) {
    if (!tok) return false;
    switch (tok->type) {
        case SEMICOLON:
        case RBRACE:
        case FUNC:
        case IF:
        case WHILE:
        case FOR:
        case RETURN:
        case PRINT:
        case LBRACE:
            return true;
        default:
            return false;
    }
}

static void synchronize(Parser* p) {
    if (!p->panic_mode) return;
    p->panic_mode = false;

    while (current(p) != NULL) {
        if (is_sync_point(current(p))) {
            if (current(p)->type == SEMICOLON) advance(p);
            return;
        }
        advance(p);
    }
}
// ===

void parser_add_p_token(Parser* p, Token* tok) {
    if (p->p_size >= p->p_capacity) {
        p->p_capacity = p->p_capacity ? p->p_capacity * 2 : 4;
        p->p_tokens = realloc(p->p_tokens, sizeof(Token*) * p->p_capacity);
    }
    p->p_tokens[p->p_size++] = tok;
}

void parser_add_diagnostic(Parser* p, DiagnosticSeverity severity,
        int line, int col, int len, const char* format, ...) {
    if (p->diag_count >= p->diag_capacity) {
        p->diag_capacity = p->diag_capacity ? p->diag_capacity * 2 : 4;
        p->diagnostics = realloc(p->diagnostics,
                sizeof(Diagnostic*) * p->diag_capacity);
    }

    Diagnostic* diag = malloc(sizeof(Diagnostic));
    diag->severity = severity;
    diag->row = line;
    diag->col = col;
    diag->len = len;

    va_list args;
    va_start(args, format);
    vasprintf(&diag->message, format, args); // allocates new message (char*)
    va_end(args);

    p->diagnostics[p->diag_count++] = diag;
}

void free_parser(Parser* p) {
    // free symbol table
    symtab_destroy_all(p->symtab);

    // free tokens
    for (int i = 0; i < p->size; i++) {
        free_token_data(p->tokens[i]);
        free(p->tokens[i]);
    }
    free(p->tokens);

    for (int i = 0; i < p->p_size; i++) {
        free_token_data(p->p_tokens[i]);
        free(p->p_tokens[i]);
    }
    if (p->p_tokens) free(p->p_tokens);

    // free diagnostics
    for (int i = 0; i < p->diag_count; i++) {
        free(p->diagnostics[i]->message);
        free(p->diagnostics[i]);
    }
    free(p->diagnostics);
}

// allocate and initialize parser based on the result from the lexer
Parser* parser_init(Token** tokens, int count) {
    Parser* p = malloc(sizeof(Parser));
    p->p_tokens = NULL;
    p->p_capacity = 0;
    p->p_size = 0;
    p->tokens = tokens;
    p->pos = 0;
    p->size = count;
    p->symtab = symtab_create(NULL); // create the global scope symbol table

    p->panic_mode = false;
    p->diagnostics = NULL;
    p->diag_count = 0;
    p->diag_capacity = 0;
    p->last_line = -1;
    p->errors = 0;
    p->warnings = 0;
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
        ASTNode* node = NULL;

        if (match(p, FUNC)) {
            node = parse_function(p);
        } else if (match(p, SEMICOLON)) {
            // skip empty statements
            advance(p);
            continue;
        } else {
            // it must be a statement
            node = parse_statement(p);
        }

        if (node) {
            root = realloc(root, ++count * sizeof(ASTNode*));
            root[count-1] = node;
        } else {
            if (!p->panic_mode) {
                fatal_error("Parser",
                        "statement returned as null while not in panic mode");
            }
            synchronize(p);
        }
    }

    // root is the global content block
    Token* root_token = create_lexeme_token(LBRACE, "{", -1, -1);
    parser_add_p_token(p, root_token);
    return create_block(root, count, p->symtab, root_token, p->symtab->scope_depth);
}

ASTNode* parse_function(Parser* p) {
    _consume(p, FUNC, "Expected 'func'"); // returns null on fail

    Token* func_tok = current(p);
    _consume(p, IDENTIFIER, "Expected function name"); // returns null on fail

    // enter function scope
    symtab_enter_new_scope(&p->symtab);

    _consume_go(p, LPAREN, "Expected '('", err_exit_func_scope);

    ASTNode** params = NULL;
    int param_count = 0;

    bool first_param = true;
    while (!match(p, RPAREN)) {
        if (!first_param) {
            _consume_go(p, COMMA, "Expected ',' between parameters",
                    err_free_func_params);
        }
        first_param = false;
        // iterate throgh all parameters and add them to the list of params
        ASTNode* param; _parse_go(param, parse_param(p, true), err_free_func_params);
        params = realloc(params, ++param_count * sizeof(ASTNode*));
        params[param_count-1] = param;

        if (param_count == 128) {
            warning_report(p, func_tok->row, func_tok->row,
                    func_tok->len, "Parser",
                    "Can't have more than 127 parameters\n");
        }

        // if this is not a comma, we have reached the end
        if (!match(p, COMMA)) break;
    }
    // If the next character is an IDENTIFIER they clearly missed the comma and mean to add another param
    _consume_go(p, RPAREN, match(p,IDENTIFIER) ?
            "Expected ',' to separate parameters" :
            "Expected ')'",
            err_free_func_params);

    ASTNode* return_param = NULL;
    if (match(p, RETURNS)) {
        advance(p);
        _consume_go(p, LPAREN, "Expected '('", err_free_func_params);
        _parse_go(return_param, parse_param(p, false), err_free_func_params);
        _consume_go(p, RPAREN, "Expected ')'", err_free_func_ret);
    } else {
        // void return type is implicit when ommitting "returns" clause
        Token* new_token = copy_token(func_tok);
        new_token->type = U0;
        parser_add_p_token(p, new_token);
        return_param = create_type(false, U0, new_token, p->symtab->scope_depth);
    }

    // do not create another scope in the body
    ASTNode* body; _parse_go(body, parse_block(p, false), err_free_func_ret);

    ASTNode* this_func = create_func_decl(return_param,
            func_tok->start, params, param_count, body, p->symtab,
            func_tok, p->symtab->scope_depth);

    // add the function into the current scope which is now the parent's scope because we just exitted.
    Symbol* func_sym = symtab_insert(p->symtab->parent, func_tok->start, this_func, true);

    if (func_sym == NULL) {
        // duplicate declaration
        p->panic_mode = true;
        error_report(p, func_tok->row, func_tok->col, func_tok->len,
                "Parser", "duplicate function declaration (%s)\n", func_tok->start);
        free_ast(this_func); // this already frees the return and params
        goto err_exit_func_scope; // so we can just jump right to the exit
    }

    symtab_exit_scope(&p->symtab);

    return this_func;

err_free_func_ret:
    free_ast(return_param);
err_free_func_params:
    for (int i = 0; i < param_count; ++i) { free_ast(params[i]); params[i] = NULL; }
    if (params) free(params);
err_exit_func_scope:
    symtab_exit_scope(&p->symtab);
    return NULL;
}

// is_param should only be false if this is the "returns" parameters
ASTNode* parse_param(Parser* p, bool is_param) {
    // my_param : u8
    Token* name_tok = current(p); // store name token here
    _consume(p, IDENTIFIER, "Expected parameter name\n");
    _consume(p, COLON, "Expected ':'\n");

    ASTNode* ast_type; _parse(ast_type, parse_type(p)); // returns null on failure
    ASTNode* param = create_param(ast_type, name_tok->start,
            name_tok, p->symtab->scope_depth);

    // only is_param's are initialized, not "returns" parameters
    Symbol* param_sym = symtab_insert(p->symtab, param->param.name, param, is_param);
    if (param_sym == NULL) {
        // duplicate declaration
        p->panic_mode = true;
        error_report(p, name_tok->row, name_tok->col, name_tok->len, "Parser",
                "duplicate variable declaration within parameters (%s)\n",
                param->param.name);

        // this should free both the ASTNode type within, and the ASTNode param itself
        free_ast(param);
        return NULL;
    }

    return param;
}

ASTNode* parse_block(Parser* p, bool create_scope) {
    if (create_scope) {
        symtab_enter_new_scope(&p->symtab);
    }

    Token* block_tok = current(p);
    _consume_go(p, LBRACE, "Expected '{'", err_close_block);
    ASTNode** stmts = NULL;
    int count = 0;

    while (!match(p, RBRACE)) {
        // just like with params, iterate through all statements
        ASTNode* stmt = parse_statement(p);
        if (stmt) {
            stmts = realloc(stmts, ++count * sizeof(ASTNode*));
            stmts[count-1] = stmt;
        } else if (current(p) != NULL) {
            synchronize(p);
        } else {
            // we reached the end of file
            goto err_clean_statements;
        }
    }

    _consume_go(p, RBRACE, "Expected '}'", err_clean_statements);

    ASTNode* this_block = create_block(stmts, count, p->symtab,
            block_tok, p->symtab->scope_depth);

    if (create_scope) {
        symtab_exit_scope(&p->symtab);
    }

    return this_block;

err_clean_statements:
    for (int i = 0; i < count; ++i) { free_ast(stmts[i]); stmts[i] = NULL; }
    if (stmts) free(stmts);
err_close_block:
    if (create_scope) { symtab_exit_scope(&p->symtab); }
    return NULL;
}

ASTNode* parse_if(Parser* p) {
    Token* if_tok = current(p);
    _consume(p, IF, "Expected if statement");
    _consume(p, LPAREN, "Expected '(' before if");
    ASTNode* cond; _parse(cond, parse_expression(p));
    if (!consume(p, RPAREN, "Expected ')' after if")) {
        free_ast(cond);
        return NULL;
    }
    ASTNode* then_block = parse_block(p, true); // create new scope
    if (then_block == NULL) {
        free_ast(cond);
        return NULL;
    }
    ASTNode* else_block = NULL;

    if (match(p, ELSE)) {
        advance(p);
        else_block = parse_block(p, true); // new scope
        if (else_block == NULL) {
            free_ast(then_block);
            free_ast(cond);
            return NULL;
        }
    }
    return create_if(cond, then_block, else_block, if_tok, p->symtab->scope_depth);
}

ASTNode* parse_for(Parser* p) {
    Token* for_tok = current(p);
    _consume(p, FOR, "Expected for statement");
    _consume(p, LPAREN, "Expected '(' after for");

    symtab_enter_new_scope(&p->symtab); // new scope for init () and block {}

    ASTNode* init = NULL;
    if (match(p, MUT) || (current(p)!=NULL && is_primitive(current(p)->type))) {
        _parse_go(init, parse_var_decl(p), err_for_cleanup);
    } else if (match(p, IDENTIFIER)) {
        _parse_go(init, parse_expression(p), err_for_cleanup);
        _consume_go(p, SEMICOLON, "Expected ';' in for loop after initialization",
                err_for_cleanup);
    } else {
        _consume_go(p, SEMICOLON, "Expected ';' in for loop after initialization",
                err_for_cleanup);
    }

    ASTNode* end = NULL;
    if (!match(p, SEMICOLON)) {
        _parse_go(end, parse_expression(p), err_for_init);
    }
    _consume_go(p, SEMICOLON, "Expected ';' in for loop after condition",
            err_for_end);

    ASTNode* iter = NULL;
    if (!match(p, RPAREN)) {
        _parse_go(iter, parse_expression(p), err_for_end);
    }
    _consume_go(p, RPAREN, "Expected ')' after for loop", err_for_iter);

    // the loop body has its own scope though note that the initialization
    // vars will still be accessible from the parent scope in the block {}
    ASTNode* body; _parse_go(body, parse_block(p, true), err_for_iter);

    ASTNode* this_for = create_for(init, end, iter, body, p->symtab,
            for_tok, p->symtab->scope_depth);

    symtab_exit_scope(&p->symtab); // exit loop init scope

    return this_for;

err_for_iter:
    free_ast(iter);
err_for_end:
    free_ast(end);
err_for_init:
    free_ast(init);
err_for_cleanup:
    symtab_exit_scope(&p->symtab);
    return NULL;
}

ASTNode* parse_statement(Parser* p) {
    Token* stmt_tok = current(p);
    if (stmt_tok == NULL) {
        p->panic_mode = true;
        error_report(p, p->last_line, -1, -1, "Parser", "expected statement\n");
        return NULL;
    }

    if (match(p, BREAK) || match(p, CONTINUE)) {
        // Checking that these are within a loop context is now a semantic issue and is thus in the type checker
        ASTNode* stmt = match(p,BREAK) ?
            create_break(stmt_tok, p->symtab->scope_depth) :
            create_continue(stmt_tok, p->symtab->scope_depth);

        advance(p);

        if (!consume(p, SEMICOLON, "Expected ';' after break/continue")) {
            free_ast(stmt);
            return NULL;
        }
        return stmt;
    }

    // check single expression ASTNode keywords
    if (match(p, RETURN)) {
        advance(p);
        ASTNode* expr = NULL;
        if (!match(p, SEMICOLON)) {
            _parse(expr, parse_expression(p));
        }
        if (!consume(p, SEMICOLON, "';' after return")) {
            if (expr) free_ast(expr);
            return NULL;
        }
        return create_return(expr, stmt_tok, p->symtab->scope_depth);
    }

    if (match(p, PRINT)) {
        advance(p);
        ASTNode* expr; _parse(expr, parse_expression(p));
        if (!consume(p, SEMICOLON, "Expected ';' after print")) {
            free_ast(expr);
            return NULL;
        }
        return create_print(expr, stmt_tok, p->symtab->scope_depth);
    }

    if (match(p, IF)) {
        return parse_if(p);
    }

    if (match(p, WHILE)) {
        advance(p);
        _consume(p, LPAREN, "Expected '(' after while");

        ASTNode* cond; _parse(cond, parse_expression(p));
        if (!consume(p, RPAREN, "Expected ')' after while")) {
            free_ast(cond);
            return NULL;
        }
        ASTNode* body = parse_block(p, true);
        if (body == NULL) {
            free_ast(cond);
            return NULL;
        }

        return create_while(cond, body, stmt_tok, p->symtab->scope_depth);
    }

    if (match(p, FOR)) {
        return parse_for(p);
    }

    if (match(p, LBRACE)) {
        // create new scope
        return parse_block(p, true);
    }

    // parse variable declaration (with possible initialization)
    if (match(p, MUT) || (current(p)!=NULL && is_primitive(current(p)->type))) {
        return parse_var_decl(p);
    }

    // otherwise if it is not a keyword it must simply be an expression
    ASTNode* expr; _parse(expr, parse_expression(p));
    if (!consume(p, SEMICOLON, "Expected ';' after expression")) {
        free_ast(expr);
        return NULL;
    }
    return expr;
}

ASTNode* parse_var_decl(Parser* p) {
    ASTNode* ast_type; _parse(ast_type, parse_type(p));

    ASTNode** decls = NULL;
    int count = 0;
    bool first = true;

    do {
        if (!first) {
            advance(p); // consume the comma that must exist after the loop condition
            // copy the type into a new type node for this new declaration
            // the type's line will always be constant
            ast_type = create_type(ast_type->is_mut, ast_type->token->type,
                    ast_type->token, p->symtab->scope_depth);
        }
        first = false;

        Token* name_tok = current(p);
        _consume_go(p, IDENTIFIER, "Expected variable name", err_decls);

        ASTNode* init = NULL;
        if (match(p, WALRUS)) {
            advance(p);
            _parse_go(init, parse_expression(p), err_decls);
        }

        ASTNode* decl = create_var_decl(ast_type, name_tok->start, init,
                name_tok, p->symtab->scope_depth);

        Symbol* sym = symtab_insert(p->symtab, name_tok->start, decl, init != NULL);
        if (sym == NULL) {
            p->panic_mode = true;
            error_report(p, name_tok->row, name_tok->col, name_tok->len,
                    "Parser", "duplicate variable declaration (%s)\n",
                    name_tok->start);
            free_ast(decl);
            goto err_decls;
        }

        decls = realloc(decls, ++count * sizeof(ASTNode*));
        decls[count-1] = decl;
    } while (match(p, COMMA));

    _consume_go(p, SEMICOLON, "Expected ';' after variable declaration", err_decls);

    if (count == 1) {
        ASTNode* single_var = decls[0];
        free(decls);
        return single_var;
    }

    // if we have multiple declarations on this line, then return it as a block of declarations
    // the scope will just be the same level scope as the current symtab
    Token* block_token = copy_token(ast_type->token);
    block_token->type = LBRACE; // match standard block type
    parser_add_p_token(p, block_token);
    return create_block(decls, count, p->symtab, block_token, p->symtab->scope_depth);

err_decls:
    for (int i = 0; i < count; i++) { free_ast(decls[i]); decls[i] = NULL; }
    if (decls) free(decls);
    // 'type' variable is free'd within freeing the variable declaration ast nodes
    return NULL;
}

ASTNode* parse_expression(Parser* p) {
    // this is the top of the grammar, the lowest precedence
    return parse_assignment(p);
}

ASTNode* parse_assignment(Parser* p) {
    ASTNode* left; _parse(left, parse_logic_or(p));

    if (match(p, WALRUS)) {
        advance(p);
        if (left->node_type != NODE_IDENTIFIER) {
            // we should be looking at an identifier when assigning
            p->panic_mode = true;
            error_report(p, left->token->row, left->token->col, left->token->len,
                    "Parser", "invalid assignment target\n");
            free_ast(left);
            return NULL;
        }
        // now p is at the rhs of the assignment expression
        // do a recursive call for right associativity of assignments
        ASTNode* value = parse_assignment(p);
        if (value == NULL) {
            free_ast(left);
            return NULL;
        }

        ASTNode* assign = create_assign(left->identifier, value,
                left->token, p->symtab->scope_depth);

        // no longer need left node or its string data because it will be packed
        // into the assign node with value being what is being assigned
        free_ast(left);
        return assign;
    }

    // just return this logical expression
    return left;
}

ASTNode* parse_logic_or(Parser* p) {
    ASTNode* left; _parse(left, parse_logic_and(p));

    while (match(p, OR)) {
        Token* l_logor = advance(p);
        ASTNode* right = parse_logic_and(p);
        if (right == NULL) {
            free_ast(left);
            return NULL;
        }
        left = create_bin_op(OR, left, right, l_logor, p->symtab->scope_depth);
    }
    return left;
}

ASTNode* parse_logic_and(Parser* p) {
    ASTNode* left; _parse(left, parse_equality(p));

    while (match(p, AND)) {
        Token* l_logand = advance(p);
        ASTNode* right = parse_equality(p);
        if (right == NULL) {
            free_ast(left);
            return NULL;
        }
        left = create_bin_op(AND, left, right, l_logand, p->symtab->scope_depth);
    }
    return left;
}

ASTNode* parse_equality(Parser* p) {
    ASTNode* left; _parse(left, parse_comparison(p));

    while (match(p, EQUAL) || match(p, BANG_EQUAL)) {
        Token* l_op = advance(p);
        TokenType op = l_op->type;
        ASTNode* right = parse_comparison(p);
        if (right == NULL) {
            free_ast(left);
            return NULL;
        }
        left = create_bin_op(op, left, right, l_op, p->symtab->scope_depth);
    }
    return left;
}

ASTNode* parse_comparison(Parser* p) {
    ASTNode* left; _parse(left, parse_additive(p));

    while (match(p, LESS) || match(p, LESS_EQUAL) || match(p, GREATER) || match(p, GREATER_EQUAL)) {
        Token* l_op = advance(p);
        TokenType op = l_op->type;
        ASTNode* right = parse_additive(p);
        if (right == NULL) {
            free_ast(left);
            return NULL;
        }
        left = create_bin_op(op, left, right, l_op, p->symtab->scope_depth);
    }
    return left;
}

ASTNode* parse_additive(Parser* p) {
    ASTNode* left; _parse(left, parse_multiplicative(p));

    while (match(p, PLUS) || match(p, MINUS)) {
        Token* l_op = advance(p);
        TokenType op = l_op->type;
        ASTNode* right = parse_multiplicative(p);
        if (right == NULL) {
            free_ast(left);
            return NULL;
        }
        left = create_bin_op(op, left, right, l_op, p->symtab->scope_depth);
    }
    return left;
}

ASTNode* parse_multiplicative(Parser* p) {
    ASTNode* left; _parse(left, parse_primary(p));
    while (match(p, STAR) || match(p, SLASH) || match(p, MODULO)) {
        // get the current op that it is and then check right identifier
        Token* l_op = advance(p);
        TokenType op = l_op->type;
        ASTNode* right = parse_primary(p);
        if (right == NULL) {
            free_ast(left);
            return NULL;
        }
        left = create_bin_op(op, left, right, l_op, p->symtab->scope_depth);
    }
    return left;
}

ASTNode* parse_func_call(Parser* p, const char* name, Token* tok) {
    _consume(p, LPAREN, "Expected '(' for function call args");
    ASTNode** args = NULL;
    int arg_count = 0;

    while (!match(p, RPAREN)) {
        ASTNode* arg; _parse_go(arg, parse_expression(p), err_args);
        args = realloc(args, ++arg_count * sizeof(ASTNode*));
        args[arg_count-1] = arg;

        if (arg_count == 128) {
            warning_report(p, arg->token->row, arg->token->col, arg->token->len,
                    "Parser", "Can't have more than 127 arguments\n");
        }

        if (!match(p, COMMA)) break;
        advance(p);
    }
    _consume_go(p, RPAREN, "Expected ')' after arguments", err_args);
    return create_func_call(name, args, arg_count, tok, p->symtab->scope_depth);

err_args:
    for (int i = 0; i < arg_count; i++) { free_ast(args[i]); args[i] = NULL; }
    free(args);
    return NULL;
}

ASTNode* parse_primary(Parser* p) {

    Token* tok = current(p);
    if (tok == NULL) {
        p->panic_mode = true;
        error_report(p, p->last_line, -1, -1, "Parser", "unexpected end of input\n");
        return NULL;
    }

    switch(tok->type) {
        case IDENTIFIER: {
            const char* name = tok->start;

            // Note: handling undefined indentifiers is left to the type-checker
            // which will have all information of the program.
            // This allows functions to be declared in any order.
            // Though variables will have to be in order.

            advance(p); // actually move past the identifier

            // check if function call on this identifier and handle
            if (match(p, LPAREN)) {
                return parse_func_call(p, name, tok);
            }
            // otherwise is just an identifier
            return create_identifier(name, tok, p->symtab->scope_depth);
        }
        case INT_LITERAL:
        case FLOAT_LITERAL:
        case STRING_LITERAL:
        case CHAR_LITERAL:
        case TRUE:
        case FALSE:
        case NULL_: {
            return create_literal(advance(p), p->symtab->scope_depth);
        }
        case LPAREN: {
            advance(p);
            ASTNode* expr; _parse(expr, parse_expression(p));
            if (!consume(p, RPAREN, "Expected ')' after expression")) {
                free_ast(expr);
                return NULL;
            }
            return expr;
        }
        case BANG:
        case MINUS: {
            Token* ad = advance(p);
            if (ad == NULL) return NULL;
            TokenType op = ad->type;
            ASTNode* operand; _parse(operand, parse_primary(p));
            return create_unary_op(op, operand, tok, p->symtab->scope_depth);
        }
        default:
            p->panic_mode = true;
            error_report(p, tok->row, tok->col, tok->len,
                    "Parser", "unexpected token in expression: %s\n",
                    tok_string(tok->type));
            return NULL;
    }
}

ASTNode* parse_type(Parser* p) {
    bool mut = false;
    if (match(p, MUT)) {
        mut = true;
        advance(p);
    }
    Token* tok = current(p);
    if (tok == NULL || !is_primitive(tok->type) /* || tok->type == U0 */) {
        // used to also disallow tok->type == U0 but I think it can be helpful
        // to assign variables to u0 to see if a function is void or not.
        // this can always be added back to simply ban the use of u0 as a type
        // while it still existing as the return type of "void" functions
        if (tok) {
            p->panic_mode = true;
            error_report(p, tok->row, tok->col, tok->len,
                    "Parser", "expected type specifier instead of: %s\n",
                    tok_string(tok->type));
        } else {
            p->panic_mode = true;
            error_report(p, p->last_line, -1, -1, "Parser",
                    "expected type specifier instead of: %s\n",
                    tok_string(tok->type));
        }
        return NULL;
    }

    advance(p); // consume the type
    return create_type(mut, tok->type, tok, p->symtab->scope_depth);
}

