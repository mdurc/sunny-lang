#include "type_checker.h"
#include "error.h"

bool can_implicit_cast(TokenType from, TokenType to) {
    if (from == to) return true;
    if (from == STRING && to == STRING) return true;
    if (is_numeric(from) && is_numeric(to)) return true;

    if (from == BOOL && is_numeric(to)) return true;
    if (is_numeric(from) && to == BOOL) return true;

    if (from == CHAR_LITERAL && is_numeric(to)) return true;
    return false;
}

void typecheck_program(ASTNode* program, Parser* parser) {
    if (program->node_type != NODE_BLOCK) return;

    TypeChecker context;
    context.symtab = parser->symtab;
    context.parser = parser;
    context.current_return_type = EOF_;
    context.in_loop = false;


    for (int i = 0; i < program->block.stmt_count; i++) {
        ASTNode* stmt = program->block.statements[i];
        typecheck_node(stmt, &context);
    }
}

void typecheck_node(ASTNode* node, TypeChecker* ctx) {
    if (!node) {
        fatal_error("TypeChecker", "null node was provided to type check\n");
    }
    switch (node->node_type) {
        case NODE_FUNC_CALL:
            typecheck_function_call(node, ctx);
            break;
        case NODE_FUNC_DECL:
            typecheck_func_decl(node, ctx);
            break;
        case NODE_VAR_DECL:
            typecheck_var_decl(node, ctx);
            break;
        case NODE_IDENTIFIER:
            resolve_identifier_type(node, ctx);
        case NODE_BLOCK:
            {
                SymbolTable* previous_symtab = ctx->symtab;
                ctx->symtab = node->block.symtab;
                for (int i = 0; i < node->block.stmt_count; i++) {
                    typecheck_node(node->block.statements[i], ctx);
                }
                ctx->symtab = previous_symtab;
                break;
            }
        case NODE_IF:
            typecheck_if(node, ctx);
            break;
        case NODE_WHILE:
            typecheck_while(node, ctx);
            break;
        case NODE_FOR:
            typecheck_for(node, ctx);
            break;
        case NODE_ASSIGN:
            typecheck_assign(node, ctx);
            break;
        case NODE_BIN_OP:
            typecheck_bin_op(node, ctx);
            break;
        case NODE_UNARY_OP:
            typecheck_unary_op(node, ctx);
            break;
        case NODE_LITERAL:
            resolve_literal_type(node, ctx);
            break;
        case NODE_PRINT:
            typecheck_print(node, ctx);
            break;
        case NODE_RETURN:
            typecheck_return(node, ctx);
            break;
        case NODE_BREAK:
        case NODE_CONTINUE:
            if (!ctx->in_loop) {
                error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                        "TypeChecker", "'%s' statement outside of a loop\n",
                         node->node_type == NODE_BREAK ? "break" : "continue");
            }
            break;
        default:
            // ERROR -- there must have been some context-sensitive rule
            // that was broken (i.e. break/continue outside of a loop
            // or return outside of a function)
            fatal_error("TypeChecker", "unknown starting statement here in context\n");
            break;
    }
}

bool check_all_paths_return(ASTNode* node, TypeChecker* ctx, const char* return_var, bool assigned) {
    if (!node) return assigned;

    switch (node->node_type) {
        case NODE_BLOCK:
            for (int i = 0; i < node->block.stmt_count; i++) {
                assigned = check_all_paths_return(node->block.statements[i], ctx, return_var, assigned);
            }
            return assigned;
        case NODE_IF: {
            bool then_ok = check_all_paths_return(node->if_stmt.then_block, ctx, return_var, assigned);
            bool else_ok = node->if_stmt.else_block
                          ? check_all_paths_return(node->if_stmt.else_block, ctx, return_var, assigned)
                          : false;
            return then_ok && else_ok;
        }
        case NODE_ASSIGN:
            if (strcmp(node->var_decl.name, return_var) == 0) {
                return true;
            }
            return assigned;

        case NODE_RETURN:
            // we already typechecked return statements within function declaration
            return true;
        case NODE_FOR:
        case NODE_WHILE:
            check_all_paths_return(node->for_stmt.body, ctx, return_var, assigned);
            return assigned;
        default:
            return assigned;
    }
}

void typecheck_func_decl(ASTNode* node, TypeChecker* ctx) {
    SymbolTable* previous_symtab = ctx->symtab;
    ctx->symtab = node->func_decl.symtab; // scope of the params and contents of the function

    // type check the return type
    ASTNode* return_param_node = node->func_decl.return_param;
    TokenType return_type = EOF_;
    if (return_param_node->node_type == NODE_PARAM) {
        return_type = return_param_node->param.type->token->type;
        Symbol* sym = symtab_lookup(ctx->symtab, return_param_node->param.name);
        if (!sym) {
            error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                    "TypeChecker", "Return variable '%s' not declared\n",
                    return_param_node->param.name);
            ctx->symtab = previous_symtab;
        }
    } else if (return_param_node->node_type == NODE_PRIMITIVE && return_param_node->token->type == U0) {
        return_type = return_param_node->token->type;
    } else {
        error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker", "Malformed return type for function '%s'\n",
                node->func_decl.name);
        ctx->symtab = previous_symtab;
    }

    TokenType prev_return_type = ctx->current_return_type;
    ctx->current_return_type = return_type;

    // type check body
    typecheck_node(node->func_decl.body, ctx);
    // this typechecking will assert that all returns are returning the correct type
    // so we don't have to consider that when evaluating return paths

    // validate return variable usage for non-void functions
    if (return_type != U0) {
        const char* return_var_name = return_param_node->param.name;
        bool all_paths_valid = check_all_paths_return(node->func_decl.body, ctx, return_var_name, false);
        if (!all_paths_valid) {
            error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                    "TypeChecker", "Not all code paths assign return variable '%s' or contain explicit return\n",
                    return_var_name);
        }
    }

    node->resolved_state.token_type = return_type;
    // restore context
    ctx->current_return_type = prev_return_type;
    ctx->symtab = previous_symtab;
}

// check arguments to assert that the correct number and types are provided
void typecheck_function_call(ASTNode* call, TypeChecker* ctx) {
    Symbol* sym = symtab_lookup(ctx->symtab, call->func_decl.name);
    if (!sym) {
        error_report(ctx->parser, call->token->row, call->token->col, call->token->len,
                "TypeChecker", "Undeclared function (%s)\n", call->func_decl.name);
        return;
    }

    ASTNode* func_decl_ast = sym->ast_node;
    if (!func_decl_ast || func_decl_ast->node_type != NODE_FUNC_DECL) {
        error_report(ctx->parser, call->token->row, call->token->col, call->token->len,
                "TypeChecker", "Invalid function declaration\n");
        return;
    }

    int expected_args = func_decl_ast->func_decl.param_count;
    int actual_args = call->func_decl.param_count;
    if (actual_args != expected_args) {
        error_report(ctx->parser, call->token->row, call->token->col, call->token->len,
                "TypeChecker", "Function '%s' expects %d arguments, got %d\n",
                call->func_decl.name, expected_args, actual_args);
    }

    // function call reuses func_decl struct
    for (int i = 0; i < actual_args; i++) {
        ASTNode* arg = call->func_decl.params[i];
        typecheck_node(arg, ctx);

        ASTNode* param = func_decl_ast->func_decl.params[i];
        TokenType expected_type = param->param.type->token->type;

        if (!can_implicit_cast(arg->resolved_state.token_type, expected_type)) {
            error_report(ctx->parser, call->token->row, call->token->col, call->token->len,
                    "TypeChecker",
                    "Argument %d type mismatch in '%s': expected %s, got %s\n",
                    i + 1, call->func_decl.name,
                    tok_string(expected_type),
                    tok_string(arg->resolved_state.token_type));
        }
    }

    ASTNode* return_param_node = func_decl_ast->func_decl.return_param;
    if (return_param_node->node_type == NODE_PARAM) {
        call->resolved_state.token_type = return_param_node->param.type->token->type;
    } else {
        call->resolved_state.token_type = U0;
    }
}

void typecheck_var_decl(ASTNode* node, TypeChecker* ctx) {
    // check initialization type
    ASTNode* expr = node->var_decl.init_value;
    typecheck_assign_expr(node, expr, ctx);
}

// decl_node is the ast node associated to the variables declaration
void typecheck_assign_expr(ASTNode* decl_node, ASTNode* expr, TypeChecker* ctx) {
    if (expr == NULL) return;

    TokenType decl_type = decl_node->var_decl.var_type->token->type;

    // resolve the initialized type
    typecheck_node(expr, ctx);

    TokenType rhs_type = expr->resolved_state.token_type;

    if (!can_implicit_cast(rhs_type, decl_type)) {
        error_report(ctx->parser, expr->token->row, expr->token->col, expr->token->len,
                "TypeChecker",
                "Variable '%s' type mismatch: declared %s, initialized/assigned with %s\n",
                decl_node->var_decl.name, tok_string(decl_type),
                tok_string(rhs_type));
        return;
    }
}


void resolve_identifier_type(ASTNode* node, TypeChecker* ctx) {
    Symbol* sym = symtab_lookup(ctx->symtab, node->identifier);
    if (!sym) {
        error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker", "Undeclared variable '%s'\n",
                node->identifier);
        return; // resolved type stays as EOF_
    }
    // we access the ast node that is a part of the symbol table, which must
    // have come from the variables declaration, which must have an associated primitive type
    TokenType decl_type = sym->ast_node->var_decl.var_type->token->type;
    node->resolved_state.token_type = decl_type;
}

void typecheck_if(ASTNode* node, TypeChecker* ctx) {
    typecheck_node(node->if_stmt.cond, ctx);
    TokenType resolved_t = node->if_stmt.cond->resolved_state.token_type;
    if (!can_implicit_cast(resolved_t, BOOL)) {
        error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker",
                "Condition of 'if' statement must be a boolean or a non-zero integer, got %s\n",
                 tok_string(resolved_t));
    }

    typecheck_node(node->if_stmt.then_block, ctx);

    if (node->if_stmt.else_block) {
        typecheck_node(node->if_stmt.else_block, ctx);
    }

    // no resolved type
}

void typecheck_while(ASTNode* node, TypeChecker* ctx) {
    typecheck_node(node->if_stmt.cond, ctx);
    TokenType resolved_t = node->if_stmt.cond->resolved_state.token_type;
    if (!can_implicit_cast(resolved_t, BOOL)) {
        error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker",
                "Condition of 'while' loop must be a boolean, got %s\n",
                 tok_string(resolved_t));
    }

    bool was_in_loop = ctx->in_loop;
    ctx->in_loop = true;
    typecheck_node(node->if_stmt.then_block, ctx);
    ctx->in_loop = was_in_loop;

    // no resolved type
}

void typecheck_for(ASTNode* node, TypeChecker* ctx) {
    SymbolTable* previous_symtab = ctx->symtab;
    ctx->symtab = node->for_stmt.symtab; // scope of the for loop (..){..}

    ASTNode* init_node = node->for_stmt.init_expr;
    if (init_node) typecheck_node(init_node, ctx);

    ASTNode* end_node = node->for_stmt.end_expr;
    if(end_node) {
        typecheck_node(end_node, ctx);
        TokenType resolved_t = node->for_stmt.end_expr->resolved_state.token_type;
        if (!can_implicit_cast(resolved_t, BOOL)) {
            error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                    "TypeChecker", "Condition of 'for' loop must be a boolean, got %s\n",
                    tok_string(resolved_t));
        }
    }

    ASTNode* iter_node = node->for_stmt.iter_expr;
    if (iter_node) typecheck_node(iter_node, ctx);

    bool was_in_loop = ctx->in_loop;
    ctx->in_loop = true;
    typecheck_node(node->for_stmt.body, ctx);
    ctx->in_loop = was_in_loop;

    // no resolved type, so we can just restore context
    ctx->symtab = previous_symtab;
}

void typecheck_assign(ASTNode* node, TypeChecker* ctx) {
    Symbol* sym = symtab_lookup(ctx->symtab, node->var_decl.name);
    if (!sym) {
        error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker",
                "Undeclared variable '%s' in assignment\n",
                node->var_decl.name);
        return;
    }

    // check mutability from declaration AST node
    ASTNode* decl_node = sym->ast_node;
    ASTNode* type_node = decl_node->var_decl.var_type;

    if (!type_node->is_mut) {
        if (sym->is_initialized) {
            error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                    "TypeChecker",
                    "Can only assign once to immutable variable '%s'\n",
                    decl_node->var_decl.name);
            return;
        } else {
            sym->is_initialized = true;
        }
    }

    // Now we go back to the "assign" ast node
    ASTNode* expr = node->var_decl.init_value;
    if (expr == NULL) {
        error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker",
                "Requires an expression to assign to identifier %s\n",
                node->var_decl.name);
        return;
    }

    // we can then treat this as a variable assignment
    typecheck_assign_expr(decl_node, expr, ctx);

    node->resolved_state.token_type = BOOL;
}

void resolve_literal_type(ASTNode* node, TypeChecker* ctx) {
    switch (node->token->type) {
        case INT_LITERAL:
        case CHAR_LITERAL:
            node->resolved_state.token_type = INT_LITERAL;
            break;
        case FLOAT_LITERAL:
            node->resolved_state.token_type = F64;
            break;
        case STRING_LITERAL:
            node->resolved_state.token_type = STRING;
            break;
        case TRUE:
        case FALSE:
            node->resolved_state.token_type = BOOL;
            break;
        case NULL_:
            node->resolved_state.token_type = U0;
            break;
        default:
            error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                    "TypeChecker", "Unknown literal type for resolution\n");
            node->resolved_state.token_type = EOF_;
            break;
    }
}

void typecheck_print(ASTNode* node, TypeChecker* ctx) {
    ASTNode* operand = node->unary_op.operand;
    typecheck_node(operand, ctx);

    if (is_numeric(operand->resolved_state.token_type) ||
            operand->resolved_state.token_type == BOOL ||
            operand->resolved_state.token_type == STRING) {
        return; // OK
    }

    error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
            "TypeChecker",
            "Cannot print an expression with type %s\n",
            tok_string(operand->resolved_state.token_type));
}

void typecheck_return(ASTNode* node, TypeChecker* ctx) {
    if (ctx->current_return_type == EOF_) {
        error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker", "'return' statement outside of a function\n");
        return;
    }

    if (node->unary_op.operand) {
        typecheck_node(node->unary_op.operand, ctx);
        TokenType t1 = node->unary_op.operand->resolved_state.token_type;
        TokenType t2 = ctx->current_return_type;
        if (!can_implicit_cast(t1, t2)) {
            error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                    "TypeChecker", "Return type mismatch: expected %s, got %s\n",
                    tok_string(ctx->current_return_type),
                    tok_string(node->unary_op.operand->resolved_state.token_type));
        }
    } else if (ctx->current_return_type != U0) {
        error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker",
                "Function expected to return %s, but 'return' statement has no value\n",
                tok_string(ctx->current_return_type));
    }
}

void type_error(TypeChecker* ctx, bool bin_op, ASTNode* node, const char* requirement, TokenType lt, TokenType rt) {
    error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
            "TypeChecker",
            "Operands of '%s' must be %s, got %s and %s\n",
            tok_string(bin_op ? node->bin_op.op : node->unary_op.op),
            requirement, tok_string(lt), tok_string(rt));
}


void typecheck_bin_op(ASTNode* node, TypeChecker* ctx) {
    ASTNode* left = node->bin_op.left;
    ASTNode* right = node->bin_op.right;

    typecheck_node(left, ctx);
    typecheck_node(right, ctx);

    TokenType left_type = left->resolved_state.token_type;
    TokenType right_type = right->resolved_state.token_type;
    TokenType op = node->bin_op.op;
    TokenType result_type = EOF_;

    switch (op) {
        // Arithmetic operations
        case PLUS:
        case MINUS:
        case STAR:
        case SLASH: {
            if (!is_numeric(left_type) || !is_numeric(right_type)) {
                type_error(ctx, true, node, "numeric", left_type, right_type);
                break;
            }
            result_type = is_float(left_type) ? left_type : right_type;
            break;
        }
        case MODULO:
            if (!is_integer(left_type) || !is_integer(right_type)) {
                type_error(ctx, true, node, "integers", left_type, right_type);
                break;
            }
            result_type = left_type;
            break;
        case GREATER:
        case GREATER_EQUAL:
        case LESS:
        case LESS_EQUAL: {
            if (!is_numeric(left_type)) {
                type_error(ctx, true, node, "numeric", left_type, right_type);
                break;
            }
            result_type = BOOL;
            break;
        }
        case EQUAL:
        case BANG_EQUAL: {
            if ((left_type != BOOL && !is_numeric(left_type)) ||
                    (right_type != BOOL && !is_numeric(right_type))) {
                type_error(ctx, true, node, "valid and same type for equality", left_type, right_type);
                break;
            }
            result_type = BOOL;
            break;
        }
        case AND:
        case OR: {
            if ((left_type != BOOL && !is_numeric(left_type)) ||
                    (right_type != BOOL && !is_numeric(right_type))) {
                type_error(ctx, true, node, "boolean", left_type, right_type);
                break;
            }
            result_type = BOOL;
            break;
        }

        default:
            error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker", "Unknown binary operator %s\n", tok_string(op));
            break;
    }

    node->resolved_state.token_type = result_type;
}

void typecheck_unary_op(ASTNode* node, TypeChecker* ctx) {
    ASTNode* operand = node->unary_op.operand;
    typecheck_node(operand, ctx);

    TokenType op = node->unary_op.op;
    TokenType operand_type = operand->resolved_state.token_type;
    TokenType result_type = EOF_;

    switch (op) {
        case MINUS:
            if (!is_numeric(operand_type)) {
                type_error(ctx, false, node, "numeric", operand_type, EOF_);
                break;
            }

            result_type = operand_type;

            if (is_integer(operand_type)) {
                result_type = operand_type;
            } else {
                result_type = F64;
            }
            break;

        case BANG:
            if (operand_type != BOOL && !is_numeric(operand_type)) {
                type_error(ctx, false, node, "boolean", operand_type, EOF_);
                break;
            }
            result_type = BOOL;
            break;

        default:
            error_report(ctx->parser, node->token->row, node->token->col, node->token->len,
                "TypeChecker", "Unknown unary operator %s\n", tok_string(op));
            break;
    }

    node->resolved_state.token_type = result_type;
}
