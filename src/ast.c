#include "ast.h"
#include "error.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ASTNode* create_func_decl(ASTNode* return_param, const char* name,
        ASTNode** params, int param_count, ASTNode* body, SymbolTable* symtab,
        Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_FUNC_DECL;
    // the token for func will be the func name because that is where the LSP should go to
    assert("AST", token->type == IDENTIFIER, "func decl token is not type IDENTIFIER");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->func_decl.return_param = return_param;
    node->func_decl.name = strdup(name);
    node->func_decl.params = params;
    node->func_decl.param_count = param_count;
    node->func_decl.body = body;
    node->func_decl.symtab = symtab;
    return node;
}

ASTNode* create_func_call(const char* name, ASTNode** args, int arg_count, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_FUNC_CALL;
    assert("AST", token->type == IDENTIFIER, "func call token is not type IDENTIFIER");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    // reuse func_decl space
    node->func_decl.name = strdup(name);
    node->func_decl.params = args;
    node->func_decl.param_count = arg_count;
    return node;
}

ASTNode* create_identifier(const char* name, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_IDENTIFIER;
    assert("AST", token->type == IDENTIFIER, "identifer token is not type IDENTIFIER");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->identifier = strdup(name);
    return node;
}

ASTNode* create_type(bool mut, TokenType type, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_PRIMITIVE;
    assert("AST", token->type == type, "token type and type did not match");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->is_mut = mut;
    return node;
}

ASTNode* create_param(ASTNode* type, const char* name, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_PARAM;
    assert("AST", token->type == IDENTIFIER, "param token type is not IDENTIFIER");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->param.type = type;
    node->param.name = strdup(name);
    return node;
}

ASTNode* create_block(ASTNode** statements, int count, SymbolTable* symtab, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_BLOCK;
    assert("AST", token->type == LBRACE, "block token type is not LBRACE");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->block.statements = statements;
    node->block.stmt_count = count;
    node->block.symtab = symtab;
    return node;
}

ASTNode* create_print(ASTNode* expr, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_PRINT;
    assert("AST", token->type == PRINT, "print token type is not PRINT");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->unary_op.operand = expr;
    return node;
}

ASTNode* create_if(ASTNode* cond, ASTNode* then_block, ASTNode* else_block, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_IF;
    assert("AST", token->type == IF, "if token type is not IF");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->if_stmt.cond = cond;
    node->if_stmt.then_block = then_block;
    node->if_stmt.else_block = else_block;
    return node;
}

ASTNode* create_while(ASTNode* cond, ASTNode* body, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_WHILE;
    assert("AST", token->type == WHILE, "while token type is not WHILE");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    // reuse if_stmt but only with condition and body
    node->if_stmt.cond = cond;
    node->if_stmt.then_block = body;
    return node;
}

ASTNode* create_for(ASTNode* init, ASTNode* end, ASTNode* iter, ASTNode* body, SymbolTable* symtab, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_FOR;
    assert("AST", token->type == FOR, "for token type is not FOR");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->for_stmt.init_expr = init;
    node->for_stmt.end_expr = end;
    node->for_stmt.iter_expr = iter;
    node->for_stmt.body = body;
    node->for_stmt.symtab = symtab;
    return node;
}

ASTNode* create_var_decl(ASTNode* type, const char* name, ASTNode* init, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_VAR_DECL;
    assert("AST", token->type == IDENTIFIER, "var decl token type is not IDENTIFIER");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->var_decl.var_type = type;
    node->var_decl.name = strdup(name);
    node->var_decl.init_value = init;
    return node;
}

ASTNode* create_assign(const char* name, ASTNode* value, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_ASSIGN;
    assert("AST", token->type == IDENTIFIER, "assign token type is not IDENTIFIER");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    // reuse var_decl but without type
    node->var_decl.name = strdup(name);
    node->var_decl.init_value = value;
    return node;
}

ASTNode* create_bin_op(TokenType op, ASTNode* left, ASTNode* right, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_BIN_OP;
    assert("AST", token->type == op, "bin op token type does not match op token type");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->bin_op.op = op;
    node->bin_op.left = left;
    node->bin_op.right = right;
    return node;
}

ASTNode* create_unary_op(TokenType op, ASTNode* operand, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_UNARY_OP;
    assert("AST", token->type == op, "unary op token type does not match op token type");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->unary_op.op = op;
    node->unary_op.operand = operand;
    return node;
}


ASTNode* create_return(ASTNode* expr, Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_RETURN;
    assert("AST", token->type == RETURN, "return token type is not RETURN");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    node->unary_op.operand = expr;
    return node;
}

ASTNode* create_literal(Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_LITERAL;
    node->token = token;
    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;

    switch(token->type) {
        case CHAR_LITERAL:
        case INT_LITERAL:
            node->literal.i = token->data.int_val;
            break;
        case FLOAT_LITERAL:
            node->literal.f = token->data.float_val;
            break;
        case STRING_LITERAL:
            node->literal.s = strdup(token->data.str_val);
            break;
        case TRUE:
            node->literal.i = 1;
            break;
        case FALSE:
        case NULL_:
            node->literal.i = 0;
            break;
        default:
            fatal_error("AST", "Invalid literal type: %d\n", token->type);
            exit(EXIT_FAILURE);
    }
    return node;
}

ASTNode* create_break(Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_BREAK;
    assert("AST", token->type == BREAK, "break token type is not BREAK");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    return node;
}

ASTNode* create_continue(Token* token, int scope) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->node_type = NODE_CONTINUE;
    assert("AST", token->type == CONTINUE, "continue token type is not CONTINUE");
    node->token = token;

    node->scope_depth = scope;
    node->resolved_state.token_type = EOF_;
    return node;
}

void print_ast(ASTNode* node, int indent) {
    if (!node) return;
    for (int i = 0; i < indent; i++) printf("  ");

    switch (node->node_type) {
        case NODE_FUNC_DECL:
            printf("Function: %s at line %d\n", node->func_decl.name, node->token->row);
            printf("%*sReturn Type:\n", (indent+1)*2, "");
            print_ast(node->func_decl.return_param, indent+2);
            printf("%*sParameters (%d):\n", (indent+1)*2, "", node->func_decl.param_count);
            for (int i = 0; i < node->func_decl.param_count; i++) {
                print_ast(node->func_decl.params[i], indent+2);
            }
            printf("%*sBody:\n", (indent+1)*2, "");
            print_ast(node->func_decl.body, indent+2);
            break;
        case NODE_BLOCK:
            printf("Block (%d statements) at line %d:\n", node->block.stmt_count, node->token->row);
            for (int i = 0; i < node->block.stmt_count; i++) {
                print_ast(node->block.statements[i], indent+1);
            }
            break;
        case NODE_BIN_OP:
            printf("Binary Operation: %s at line %d\n", tok_string(node->bin_op.op), node->token->row);
            print_ast(node->bin_op.left, indent+1);
            print_ast(node->bin_op.right, indent+1);
            break;
        case NODE_UNARY_OP:
            printf("Unary Operation: %s at line %d\n", tok_string(node->unary_op.op), node->token->row);
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_LITERAL:
            if (node->token->type == INT_LITERAL)
                printf("Integer Literal: %lld at line %d\n", node->literal.i, node->token->row);
            else if (node->token->type == CHAR_LITERAL)
                printf("Char Literal (ascii): %lld at line %d\n", node->literal.i, node->token->row);
            else if (node->token->type == FLOAT_LITERAL)
                printf("Float Literal: %f at line %d\n", node->literal.f, node->token->row);
            else if (node->token->type == STRING_LITERAL)
                printf("String Literal: \"%s\" at line %d\n", node->literal.s, node->token->row);
            else if (node->token->type == TRUE)
                printf("Boolean Literal: true at line %d\n", node->token->row);
            else if (node->token->type == FALSE)
                printf("Boolean Literal: false at line %d\n", node->token->row);
            else if (node->token->type == NULL_)
                printf("Null Literal at line %d\n", node->token->row);
            break;
        case NODE_IDENTIFIER:
            printf("Identifier: %s at line %d\n", node->identifier, node->token->row);
            break;
        case NODE_PRIMITIVE:
            printf("Primitive: %s%s at line %d\n", (node->is_mut ? "mut ":""), tok_string(node->token->type), node->token->row);
            break;
        case NODE_PARAM:
            printf("Parameter: %s at line %d\n", node->param.name, node->token->row);
            printf("%*sType:\n", (indent+1)*2, "");
            print_ast(node->param.type, indent+2);
            break;
        case NODE_BREAK:
            printf("Break Statement at line %d\n", node->token->row);
            break;
        case NODE_CONTINUE:
            printf("Continue Statement at line %d\n", node->token->row);
            break;
        case NODE_RETURN:
            printf("Return Statement at line %d:\n", node->token->row);
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_PRINT:
            printf("Print Statement at line %d:\n", node->token->row);
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_IF:
            printf("If Statement at line %d:\n", node->token->row);
            printf("%*sCondition:\n", (indent+1)*2, "");
            print_ast(node->if_stmt.cond, indent+2);
            printf("%*sThen Block:\n", (indent+1)*2, "");
            print_ast(node->if_stmt.then_block, indent+2);
            if (node->if_stmt.else_block) {
                printf("%*sElse Block:\n", (indent+1)*2, "");
                print_ast(node->if_stmt.else_block, indent+2);
            }
            break;
        case NODE_FOR:
            printf("For Loop at line %d:\n", node->token->row);
            printf("%*sInitialization:\n", (indent+1)*2, "");
            print_ast(node->for_stmt.init_expr, indent+2);
            printf("%*sEnd Condition:\n", (indent+1)*2, "");
            print_ast(node->for_stmt.end_expr, indent+2);
            printf("%*sIteration:\n", (indent+1)*2, "");
            print_ast(node->for_stmt.iter_expr, indent+2);
            printf("%*sBody:\n", (indent+1)*2, "");
            print_ast(node->for_stmt.body, indent+2);
            break;
        case NODE_WHILE:
            printf("While Loop at line %d:\n", node->token->row);
            printf("%*sCondition:\n", (indent+1)*2, "");
            print_ast(node->if_stmt.cond, indent+2);
            printf("%*sThen Block:\n", (indent+1)*2, "");
            print_ast(node->if_stmt.then_block, indent+2);
            break;
        case NODE_ASSIGN:
            printf("Variable Assignment at line %d:\n", node->token->row);
            printf("%*sName: %s\n", (indent+1)*2, "", node->var_decl.name);
            if (node->var_decl.init_value) {
                printf("%*sNew Value:\n", (indent+1)*2, "");
                print_ast(node->var_decl.init_value, indent+2);
            } else {
                printf("%*sNew Value: %s\n", (indent+1)*2, "", "NULL");
            }
            break;
        case NODE_VAR_DECL:
            printf("Variable Declaration at line %d:\n", node->token->row);
            printf("%*sName: %s\n", (indent+1)*2, "", node->var_decl.name);
            printf("%*sType:\n", (indent+1)*2, "");
            print_ast(node->var_decl.var_type, indent+2);
            if (node->var_decl.init_value) {
                printf("%*sInitial Value:\n", (indent+1)*2, "");
                print_ast(node->var_decl.init_value, indent+2);
            } else {
                printf("%*sInitial Value: %s\n", (indent+1)*2, "", "NULL");
            }
            break;
        case NODE_FUNC_CALL:
            printf("Function Call: %s at line %d\n", node->func_decl.name, node->token->row);
            printf("%*sArguments (%d) at line %d:\n", (indent+1)*2, "", node->func_decl.param_count, node->token->row);
            for (int i = 0; i < node->func_decl.param_count; i++) {
                print_ast(node->func_decl.params[i], indent+2);
            }
            break;
        default:
            fatal_error("AST", "Unknown Node Type for AST print: %d\n", node->node_type);
    }
}

void free_ast(ASTNode* node) {
    if (!node) return;

    switch (node->node_type) {
        case NODE_FUNC_DECL:
            free(node->func_decl.name);
            free_ast(node->func_decl.return_param);
            for (int i = 0; i < node->func_decl.param_count; i++)
                free_ast(node->func_decl.params[i]);
            if (node->func_decl.params) {
                free(node->func_decl.params);
            }
            free_ast(node->func_decl.body);
            break;

        case NODE_FUNC_CALL:
            free(node->func_decl.name);
            for (int i = 0; i < node->func_decl.param_count; i++)
                free_ast(node->func_decl.params[i]);
            if (node->func_decl.params) {
                free(node->func_decl.params);
            }
            break;

        case NODE_BLOCK:
            for (int i = 0; i < node->block.stmt_count; i++)
                free_ast(node->block.statements[i]);
            if(node->block.statements) {
                free(node->block.statements);
            }
            break;

        case NODE_PARAM:
            free_ast(node->param.type);
            free(node->param.name);
            break;

        case NODE_UNARY_OP:
        case NODE_RETURN:
        case NODE_PRINT:
            free_ast(node->unary_op.operand);
            break;

        case NODE_IF:
            free_ast(node->if_stmt.cond);
            free_ast(node->if_stmt.then_block);
            free_ast(node->if_stmt.else_block);
            break;

        case NODE_WHILE:
            free_ast(node->if_stmt.cond);
            free_ast(node->if_stmt.then_block);
            break;

        case NODE_FOR:
            free_ast(node->for_stmt.iter_expr);
            free_ast(node->for_stmt.init_expr);
            free_ast(node->for_stmt.end_expr);
            free_ast(node->for_stmt.body);
            break;

        case NODE_VAR_DECL:
            free_ast(node->var_decl.var_type);
            free(node->var_decl.name);
            free_ast(node->var_decl.init_value);
            break;

        case NODE_ASSIGN:
            free(node->var_decl.name);
            free_ast(node->var_decl.init_value);
            break;

        case NODE_BIN_OP:
            free_ast(node->bin_op.left);
            free_ast(node->bin_op.right);
            break;

        case NODE_LITERAL:
            if (node->token->type == STRING_LITERAL) {
                free(node->literal.s);
            }
            break;

        case NODE_IDENTIFIER:
            free(node->identifier);
            break;

        case NODE_BREAK:
        case NODE_CONTINUE:
        case NODE_PRIMITIVE:
            // No dynamic memory to free for ints, bools, or floats
            break;

        default:
            fatal_error("AST", "Warning: Unknown node type %d in free_ast\n", node->node_type);
            break;
    }

    free(node);
}
