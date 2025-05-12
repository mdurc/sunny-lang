#include "type_checker.h"
#include "error.h"

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
        fatal_error(-1, "TypeChecker", "Null node was provided to type check");
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
                error_report(node->line, "TypeChecker", "'%s' statement outside of a loop\n",
                             node->node_type == NODE_BREAK ? "break" : "continue");
                ctx->parser->errors++;
            }
            break;
        default:
            // ERROR -- there must have been some context-sensitive rule
            // that was broken (i.e. break/continue outside of a loop
            // or return outside of a function)
            fatal_error(node->line, "TypeChecker", "unknown starting statement here in context\n");
            break;
    }
}

static TokenType resolve_integer_literal(struct CheckedState* state) {
    if (state->int_sign == true) {
        // sign bit is on, thus it is negative, meaning signed integer
        if (state->i <= (uint64_t)INT8_MAX) return I8;
        if (state->i <= (uint64_t)INT16_MAX) return I16;
        if (state->i <= (uint64_t)INT32_MAX) return I32;
        if (state->i <= (uint64_t)INT64_MAX) return I64;
    } else {
        if (state->i <= UINT8_MAX) return U8;
        if (state->i <= UINT16_MAX) return U16;
        if (state->i <= UINT32_MAX) return U32;
        if (state->i <= UINT64_MAX) return U64;
    }

    fatal_error(-1, "TypeChecker", "Too large of an unsigned integer: %llu\n", state->i);
    return EOF_;
}

static int get_bit_width(TokenType type) {
    switch (type) {
        case U8:
        case I8:  return 8;
        case U16:
        case I16: return 16;
        case U32:
        case I32: return 32;
        case U64:
        case I64: return 64;
        default: return 0;
    }
}

static bool is_compatible(TokenType from, TokenType to) {
    if (from == to) return true;
    if (from == STRING || to == STRING) return false;

    uint64_t mask = 0;
    switch (from) {
        case U8:    mask = (1ULL << U16) | (1ULL << U32) | (1ULL << U64)
                            | (1ULL << I16) | (1ULL << I32) | (1ULL << I64)
                            | (1ULL << F64); break;
        case U16:   mask = (1ULL << U32) | (1ULL << U64) | (1ULL << I32)
                            | (1ULL << I64) | (1ULL << F64); break;
        case U32:   mask = (1ULL << U64) | (1ULL << I64) | (1ULL << F64); break;
        case U64:   mask = (1ULL << F64); break;
        case I8:    mask = (1ULL << U16) | (1ULL << U32) | (1ULL << U64)
                            | (1ULL << I16) | (1ULL << I32) | (1ULL << I64)
                            | (1ULL << F64); break;
        case I16:   mask = (1ULL << U32) | (1ULL << U64) | (1ULL << I32)
                            | (1ULL << I64) | (1ULL << F64); break;
        case I32:   mask = (1ULL << U64) | (1ULL << I64) | (1ULL << F64); break;
        case I64:   mask = (1ULL << F64); break;
        case F64:   break;
        case BOOL:  mask = (1ULL << U8) | (1ULL << U16) | (1ULL << U32)
                            | (1ULL << U64) | (1ULL << I8) | (1ULL << I16)
                            | (1ULL << I32) | (1ULL << I64) | (1ULL << F64);
                            break;
        case CHAR_LITERAL: mask = (1ULL << U8) | (1ULL << U16) | (1ULL << U32)
                            | (1ULL << U64) | (1ULL << I8) | (1ULL << I16)
                            | (1ULL << I32) | (1ULL << I64) | (1ULL << F64)
                            | (1ULL << BOOL);
                            break;
        default:         return false;
    }

    return (mask & (1ULL << to)) != 0;
}

static bool check_all_paths_return(ASTNode* node, TypeChecker* ctx, const char* return_var, bool assigned) {
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
        return_type = return_param_node->param.type->token_type;
        Symbol* sym = symtab_lookup(ctx->symtab, return_param_node->param.name);
        if (!sym) {
            error_report(node->line, "TypeChecker", "Return variable '%s' not declared\n", return_param_node->param.name);
            ctx->parser->errors++;
            ctx->symtab = previous_symtab;
            return;
        }
    } else if (return_param_node->node_type == NODE_PRIMITIVE && return_param_node->token_type == U0) {
        return_type = return_param_node->token_type;
    } else {
        error_report(node->line, "TypeChecker", "Malformed return type for function '%s'\n", node->func_decl.name);
        ctx->parser->errors++;
        ctx->symtab = previous_symtab;
        return;
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
            error_report(node->line, "TypeChecker",
                       "Not all code paths assign return variable '%s' or contain explicit return\n",
                       return_var_name);
            ctx->parser->errors++;
        }
    }

    // restore context
    ctx->current_return_type = prev_return_type;
    ctx->symtab = previous_symtab;
}

// check arguments to assert that the correct number and types are provided
void typecheck_function_call(ASTNode* call, TypeChecker* ctx) {
    Symbol* sym = symtab_lookup(ctx->symtab, call->func_decl.name);
    if (!sym) {
        error_report(call->line, "TypeChecker", "Undeclared function (%s)\n", call->func_decl.name);
        ctx->parser->errors++;
        return;
    }

    ASTNode* func_decl_ast = sym->ast_node;
    if (!func_decl_ast || func_decl_ast->node_type != NODE_FUNC_DECL) {
        error_report(call->line, "TypeChecker", "Invalid function declaration\n");
        ctx->parser->errors++;
        return;
    }

    int expected_args = func_decl_ast->func_decl.param_count;
    int actual_args = call->func_decl.param_count;
    if (actual_args != expected_args) {
        error_report(call->line, "TypeChecker", "Function '%s' expects %d arguments, got %d\n",
                     call->func_decl.name, expected_args, actual_args);
        ctx->parser->errors++;
        return;
    }

    // function call reuses func_decl struct
    for (int i = 0; i < actual_args; i++) {
        ASTNode* arg = call->func_decl.params[i];
        typecheck_node(arg, ctx);

        ASTNode* param = func_decl_ast->func_decl.params[i];
        TokenType expected_type = param->param.type->token_type;

        if (arg->resolved_state.token_type != expected_type) {
            error_report(call->line, "TypeChecker",
                        "Argument %d type mismatch in '%s': expected %s, got %s\n",
                        i + 1, call->func_decl.name,
                        tok_string(expected_type),
                        tok_string(arg->resolved_state.token_type));
            ctx->parser->errors++;
            return;
        }
    }

    ASTNode* return_param_node = func_decl_ast->func_decl.return_param;
    if (return_param_node->node_type == NODE_PARAM) {
        call->resolved_state.token_type = return_param_node->param.type->token_type;
    } else {
        error_report(call->line, "TypeChecker", "Invalid return type for function '%s'\n", call->func_decl.name);
        ctx->parser->errors++;
        return;
    }
}

void typecheck_var_decl(ASTNode* node, TypeChecker* ctx) {
    // check initializauion type
    ASTNode* expr = node->var_decl.init_value;
    typecheck_assign_expr(node, expr, ctx);
}

// decl_node is the ast node associated to the variables declaration
void typecheck_assign_expr(ASTNode* decl_node, ASTNode* expr, TypeChecker* ctx) {
    if (expr == NULL) return;

    TokenType decl_type = decl_node->var_decl.var_type->token_type;

    // resolve the initialized type
    typecheck_node(expr, ctx);

    TokenType rhs_type = expr->resolved_state.token_type;

    if (!is_compatible(rhs_type, decl_type)) {
        error_report(decl_node->line, "TypeChecker",
                "Variable '%s' type mismatch: declared %s, initialized/assigned with %s\n",
                decl_node->var_decl.name, tok_string(decl_type), tok_string(rhs_type));
        ctx->parser->errors++;
    }
}


void resolve_identifier_type(ASTNode* node, TypeChecker* ctx) {
    Symbol* sym = symtab_lookup(ctx->symtab, node->identifier);
    if (!sym) {
        error_report(node->line, "TypeChecker", "Undeclared variable '%s'\n", node->identifier);
        return; // resolved type stays as EOF_
    }
    // we access the ast node that is a part of the symbol table, which must
    // have come from the variables declaration, which must have an associated primitive type
    node->resolved_state.token_type = sym->ast_node->var_decl.var_type->token_type;
}

void typecheck_if(ASTNode* node, TypeChecker* ctx) {
    typecheck_node(node->if_stmt.cond, ctx);
    TokenType resolved_t = node->if_stmt.cond->resolved_state.token_type;
    if (resolved_t != BOOL) {
        error_report(node->line, "TypeChecker",
                "Condition of 'if' statement must be a boolean or a non-zero integer, got %s\n",
                 tok_string(resolved_t));
        ctx->parser->errors++;
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
    if (resolved_t != BOOL) {
        error_report(node->line, "TypeChecker", "Condition of 'while' loop must be a boolean, got %s\n",
                     tok_string(resolved_t));
        ctx->parser->errors++;
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

    typecheck_node(node->for_stmt.init_expr, ctx);

    typecheck_node(node->for_stmt.end_expr, ctx);
    TokenType resolved_t = node->for_stmt.end_expr->resolved_state.token_type;
    if (resolved_t != BOOL) {
        error_report(node->line, "TypeChecker", "Condition of 'for' loop must be a boolean, got %s\n",
                tok_string(resolved_t));
        ctx->parser->errors++;
    }

    typecheck_node(node->for_stmt.iter_expr, ctx);

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
        error_report(node->line, "TypeChecker",
                "Undeclared variable '%s' in assignment\n",
                node->var_decl.name);
        ctx->parser->errors++;
        return;
    }

    // check mutability from declaration AST node
    ASTNode* decl_node = sym->ast_node;
    ASTNode* type_node = decl_node->var_decl.var_type;

    if (!type_node->is_mut) {
        if (sym->is_initialized) {
            error_report(node->line, "TypeChecker",
                    "Cannot assign again to immutable variable '%s'\n",
                    decl_node->var_decl.name);
            ctx->parser->errors++;
            return;
        } else {
            sym->is_initialized = true;
        }
    }

    // Now we go back to the "assign" ast node
    ASTNode* expr = node->var_decl.init_value;
    if (expr == NULL) {
        error_report(node->line, "TypeChecker",
                "Requires an expression to assign to identifier %s\n",
                node->var_decl.name);
        ctx->parser->errors++;
        return;
    }

    // we can then treat this as a variable assignment
    int err_count = ctx->parser->errors;

    typecheck_assign_expr(decl_node, expr, ctx);

    if (ctx->parser->errors == err_count) {
        node->resolved_state.i = 1; // true
    } else {
        node->resolved_state.i = 0; // false
    }

    node->resolved_state.token_type = BOOL;
}

static void type_error(TypeChecker* ctx, ASTNode* node, const char* requirement, TokenType lt, TokenType rt) {
    error_report(node->line, "TypeChecker",
               "Operands of '%s' must be %s, got %s and %s\n",
               tok_string(node->bin_op.op), requirement, tok_string(lt), tok_string(rt));
    ctx->parser->errors++;
}

static bool is_numeric(TokenType t) {
    switch(t) {
        case INT_LITERAL: case FLOAT_LITERAL:
        case U8: case U16: case U32: case U64: case I8:
        case I16: case I32: case I64: case F64:
            return true;
        default: return false;
    }
}

static bool is_integer(TokenType t) {
    return t != F64 && t != FLOAT_LITERAL && is_numeric(t);
}

static bool is_float(TokenType t) {
    return t != F64 && t != FLOAT_LITERAL;
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

    #define ARITH_OP_INT(OP) \
        node->resolved_state.i = left->resolved_state.i OP right->resolved_state.i; \
        int bit_width = get_bit_width(left_type); \
        if (bit_width > 0) { \
            uint64_t max_unsigned = (1ULL << bit_width) - 1; \
            uint64_t max_signed = (1ULL << (bit_width - 1)) - 1; \
            uint64_t truncated = node->resolved_state.i & max_unsigned; \
            node->resolved_state.int_sign = (truncated > max_signed); \
            node->resolved_state.i = truncated; \
        } \
        result_type = resolve_integer_literal(&node->resolved_state); \

    #define ARITH_OP(OP) \
        if (is_integer(left_type)) { \
            ARITH_OP_INT(OP) \
        } else { \
            node->resolved_state.f = left->resolved_state.f OP right->resolved_state.f; \
            result_type = F64; \
        }

    #define NUMERIC_COMPARE(OP, TYPE) \
        node->resolved_state.i = left->resolved_state.TYPE OP right->resolved_state.TYPE;
    // store in i as a BOOL

    #define CHECK_DIV_ZERO(RIGHT, TYPE) \
        if (RIGHT->resolved_state.TYPE == 0) { \
            error_report(node->line, "TypeChecker", "Division by zero"); \
            ctx->parser->errors++; \
            return; \
        }

    switch (op) {
        // Arithmetic operations
        case PLUS:
        case MINUS:
        case STAR:
        case SLASH: {
            if (is_numeric(left_type) && left_type == right_type) {
                switch (op) {
                    case PLUS:  ARITH_OP(+) break;
                    case MINUS: ARITH_OP(-) break;
                    case STAR:  ARITH_OP(*) break;
                    case SLASH:
                        {
                            if (is_integer(right_type)) {
                                CHECK_DIV_ZERO(right, i);
                            } else {
                                CHECK_DIV_ZERO(right, f);
                            }
                            ARITH_OP(/) break;
                        }
                    default: break;
                }
            } else {
                type_error(ctx, node, "numeric and same type", left_type, right_type);
            }
            break;
        }
        case MODULO:
            if (is_integer(left_type) && is_integer(right_type)) {
                CHECK_DIV_ZERO(right, i);
                ARITH_OP_INT(%)
            } else {
                type_error(ctx, node, "integers", left_type, right_type);
            }
            break;
        case GREATER:
        case GREATER_EQUAL:
        case LESS:
        case LESS_EQUAL: {
            result_type = BOOL;
            if (is_numeric(left_type) && left_type == right_type) {
                if (is_integer(left_type)) {
                    switch (op) {
                        case GREATER: NUMERIC_COMPARE(>, i) break;
                        case GREATER_EQUAL: NUMERIC_COMPARE(>=, i) break;
                        case LESS: NUMERIC_COMPARE(<, i) break;
                        case LESS_EQUAL: NUMERIC_COMPARE(<=, i) break;
                        default: break;
                    }
                } else {
                    switch (op) {
                        case GREATER: NUMERIC_COMPARE(>, f) break;
                        case GREATER_EQUAL: NUMERIC_COMPARE(>=, f) break;
                        case LESS: NUMERIC_COMPARE(<, f) break;
                        case LESS_EQUAL: NUMERIC_COMPARE(<=, f) break;
                        default: break;
                    }
                }
            } else {
                type_error(ctx, node, "numeric and same type for comparison", left_type, right_type);
            }
            break;
        }
        case EQUAL:
        case BANG_EQUAL: {
            result_type = BOOL;
            if (left_type == right_type && left_type != EOF_) {
                bool match;
                if (is_integer(left_type) || left_type == BOOL) {
                    match = left->resolved_state.i == right->resolved_state.i;
                } else if (!is_float(left_type)) {
                    match = left->resolved_state.f == right->resolved_state.f;
                } else {
                    match = true;
                }

                node->resolved_state.i = (match == (op == EQUAL)) ? 1 : 0;
            } else {
                type_error(ctx, node, "same type for equality", left_type, right_type);
            }
            break;
        }
        case AND:
        case OR: {
            result_type = BOOL;
            if (left_type == BOOL && right_type == BOOL) {
                bool lval = left->resolved_state.i;
                bool rval = right->resolved_state.i;
                node->resolved_state.i = (op == AND ? lval && rval : lval || rval) ? 1 : 0;
            } else {
                type_error(ctx, node, "boolean", left_type, right_type);
            }
            break;
        }

        default:
            error_report(node->line, "TypeChecker", "Unknown binary operator %s\n", tok_string(op));
            ctx->parser->errors++;
            break;
    }

    #undef ARITH_OP_INT
    #undef ARITH_OP
    #undef NUMERIC_COMPARE
    #undef CHECK_DIV_ZERO

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
            if (is_numeric(operand_type)) {
                if (is_integer(operand_type)) {
                    node->resolved_state.int_sign = !node->resolved_state.int_sign;
                    result_type = resolve_integer_literal(&node->resolved_state);
                } else {
                    node->resolved_state.f = -operand->resolved_state.f;
                    result_type = F64;
                }
            } else {
                type_error(ctx, node, "numeric", operand_type, EOF_);
            }
            break;

        case BANG:
            if (operand_type == BOOL) {
                result_type = BOOL;
                if (node->resolved_state.i != 0) {
                    node->resolved_state.i = 0;
                } else {
                    node->resolved_state.i = 1;
                }
            } else {
                type_error(ctx, node, "boolean", operand_type, EOF_);
            }
            break;

        default:
            error_report(node->line, "TypeChecker", "Unknown unary operator %s\n", tok_string(op));
            ctx->parser->errors++;
            break;
    }

    node->resolved_state.token_type = result_type;
}

void resolve_literal_type(ASTNode* node, TypeChecker* ctx) {
    switch (node->token_type) {
        case INT_LITERAL:
        case CHAR_LITERAL:
            node->resolved_state.i = node->literal.i;
            node->resolved_state.int_sign = false; // isolated literals are always positive
            node->resolved_state.token_type = resolve_integer_literal(&node->resolved_state);
            break;
        case FLOAT_LITERAL:
            node->resolved_state.token_type = F64;
            node->resolved_state.f = node->literal.f;
            break;
        case STRING_LITERAL:
            node->resolved_state.token_type = STRING;
            node->resolved_state.s = node->literal.s;
            break;
        case TRUE:
        case FALSE:
            node->resolved_state.token_type = BOOL;
            node->resolved_state.i = node->literal.i;
            break;
        case NULL_:
            node->resolved_state.token_type = U0;
            node->resolved_state.i = node->literal.i;
            break;
        default:
            error_report(node->line, "TypeChecker", "Unknown literal type for resolution\n");
            ctx->parser->errors++;
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
        // OK
    } else if (operand->resolved_state.token_type == EOF_) {
        error_report(node->line, "TypeChecker", "Cannot print an expression with unresolved type\n");
        ctx->parser->errors++;
    }

    // no resolved type or state
}

void typecheck_return(ASTNode* node, TypeChecker* ctx) {
    if (ctx->current_return_type == EOF_) {
        error_report(node->line, "TypeChecker", "'return' statement outside of a function\n");
        ctx->parser->errors++;
        return;
    }

    if (node->unary_op.operand) {
        typecheck_node(node->unary_op.operand, ctx);
        if (node->unary_op.operand->resolved_state.token_type != ctx->current_return_type) {
            error_report(node->line, "TypeChecker", "Return type mismatch: expected %s, got %s\n",
                         tok_string(ctx->current_return_type),
                         tok_string(node->unary_op.operand->resolved_state.token_type));
            ctx->parser->errors++;
        }
    } else if (ctx->current_return_type != U0) {
        error_report(node->line, "TypeChecker",
                "Function expected to return %s, but 'return' statement has no value\n",
                     tok_string(ctx->current_return_type));
        ctx->parser->errors++;
    }
}
