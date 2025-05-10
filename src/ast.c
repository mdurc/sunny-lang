#include "ast.h"
#include "error.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ASTNode* create_func_decl(ASTNode* return_type, const char* name,
        ASTNode** params, int param_count, ASTNode* body, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FUNC_DECL;
    node->line = line;
    node->func_decl.return_type = return_type;
    node->func_decl.name = strdup(name);
    node->func_decl.params = params;
    node->func_decl.param_count = param_count;
    node->func_decl.body = body;
    return node;
}

ASTNode* create_func_call(const char* name, ASTNode** args, int arg_count, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FUNC_CALL;
    node->line = line;
    // reuse func_decl space
    node->func_decl.name = strdup(name);
    node->func_decl.params = args;
    node->func_decl.param_count = arg_count;
    return node;
}

ASTNode* create_identifier(const char* name, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_IDENTIFIER;
    node->line = line;
    node->identifier = strdup(name);
    return node;
}

ASTNode* create_type(bool mut, TokenType type, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_PRIMITIVE;
    node->line = line;
    node->primitive.mut = mut;
    node->primitive.type_spec = type;
    return node;
}

ASTNode* create_param(ASTNode* type, const char* name, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_PARAM;
    node->line = line;
    node->param.type = type;
    node->param.name = strdup(name);
    return node;
}

ASTNode* create_block(ASTNode** statements, int count, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BLOCK;
    node->line = line;
    node->block.statements = statements;
    node->block.stmt_count = count;
    return node;
}

ASTNode* create_print(ASTNode* expr, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_PRINT;
    node->line = line;
    node->unary_op.operand = expr;
    return node;
}

ASTNode* create_if(ASTNode* cond, ASTNode* then_block, ASTNode* else_block, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_IF;
    node->line = line;
    node->if_stmt.cond = cond;
    node->if_stmt.then_block = then_block;
    node->if_stmt.else_block = else_block;
    return node;
}

ASTNode* create_while(ASTNode* cond, ASTNode* body, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_WHILE;
    node->line = line;
    // reuse if_stmt but only with condition and body
    node->if_stmt.cond = cond;
    node->if_stmt.then_block = body;
    return node;
}

ASTNode* create_for(ASTNode* init, ASTNode* end, ASTNode* iter, ASTNode* body, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FOR;
    node->line = line;
    node->for_stmt.init_expr = init;
    node->for_stmt.end_expr = end;
    node->for_stmt.iter_expr = iter;
    node->for_stmt.body = body;
    return node;
}

ASTNode* create_var_decl(ASTNode* type, const char* name, ASTNode* init, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_VAR_DECL;
    node->line = line;
    node->var_decl.var_type = type;
    node->var_decl.name = strdup(name);
    node->var_decl.init_value = init;
    return node;
}

ASTNode* create_assign(const char* name, ASTNode* value, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_ASSIGN;
    node->line = line;
    // reuse var_decl but without type
    node->var_decl.name = strdup(name);
    node->var_decl.init_value = value;
    return node;
}

ASTNode* create_bin_op(TokenType op, ASTNode* left, ASTNode* right, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BIN_OP;
    node->line = line;
    node->bin_op.op = op;
    node->bin_op.left = left;
    node->bin_op.right = right;
    return node;
}

ASTNode* create_unary_op(TokenType op, ASTNode* operand, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_UNARY_OP;
    node->line = line;
    node->unary_op.op = op;
    node->unary_op.operand = operand;
    return node;
}


ASTNode* create_return(ASTNode* expr, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_RETURN;
    node->line = line;
    node->unary_op.operand = expr;
    return node;
}

ASTNode* create_literal(Token* token, int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_LITERAL;
    node->line = line;
    node->token = token;

    switch(token->type) {
        case CHAR_LITERAL:
        case INT_LITERAL:
            node->literal.i = token->data.int_t;
            break;
        case FLOAT_LITERAL:
            node->literal.f = token->data.f64_value;
            break;
        case STRING_LITERAL:
            node->literal.s = strdup(token->data.lexeme);
            break;
        case TRUE:
            node->literal.i = 1;
            break;
        case FALSE:
        case NULL_LITERAL:
            node->literal.i = 0;
            break;
        default:
            fatal_error(token->line, "AST", "Invalid literal type: %d\n", token->type);
            exit(EXIT_FAILURE);
    }
    return node;
}

ASTNode* create_break(int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BREAK;
    node->line = line;
    return node;
}

ASTNode* create_continue(int line) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_CONTINUE;
    node->line = line;
    return node;
}

void print_ast(ASTNode* node, int indent) {
    if (!node) return;
    for (int i = 0; i < indent; i++) printf("  ");

    switch (node->type) {
        case NODE_FUNC_DECL:
            printf("Function: %s at line %d\n", node->func_decl.name, node->line);
            printf("%*sReturn Type:\n", (indent+1)*2, "");
            print_ast(node->func_decl.return_type, indent+2);
            printf("%*sParameters (%d):\n", (indent+1)*2, "", node->func_decl.param_count);
            for (int i = 0; i < node->func_decl.param_count; i++) {
                print_ast(node->func_decl.params[i], indent+2);
            }
            printf("%*sBody:\n", (indent+1)*2, "");
            print_ast(node->func_decl.body, indent+2);
            break;
        case NODE_BLOCK:
            printf("Block (%d statements) at line %d:\n", node->block.stmt_count, node->line);
            for (int i = 0; i < node->block.stmt_count; i++) {
                print_ast(node->block.statements[i], indent+1);
            }
            break;
        case NODE_BIN_OP:
            printf("Binary Operation: %s at line %d\n", tok_string(node->bin_op.op), node->line);
            print_ast(node->bin_op.left, indent+1);
            print_ast(node->bin_op.right, indent+1);
            break;
        case NODE_UNARY_OP:
            printf("Unary Operation: %s at line %d\n", tok_string(node->unary_op.op), node->line);
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_LITERAL:
            if (node->token->type == INT_LITERAL)
                printf("Integer Literal: %lld at line %d\n", node->literal.i, node->line);
            else if (node->token->type == CHAR_LITERAL)
                printf("Char Literal (ascii): %lld at line %d\n", node->literal.i, node->line);
            else if (node->token->type == FLOAT_LITERAL)
                printf("Float Literal: %f at line %d\n", node->literal.f, node->line);
            else if (node->token->type == STRING_LITERAL)
                printf("String Literal: \"%s\" at line %d\n", node->literal.s, node->line);
            else if (node->token->type == TRUE)
                printf("Boolean Literal: true at line %d\n", node->line);
            else if (node->token->type == FALSE)
                printf("Boolean Literal: false at line %d\n", node->line);
            else if (node->token->type == NULL_LITERAL)
                printf("Null Literal at line %d\n", node->line);
            break;
        case NODE_IDENTIFIER:
            printf("Identifier: %s at line %d\n", node->identifier, node->line);
            break;
        case NODE_PRIMITIVE:
            printf("Primitive: %s%s at line %d\n", (node->primitive.mut ? "mut ":""), tok_string(node->primitive.type_spec), node->line);
            break;
        case NODE_PARAM:
            printf("Parameter: %s at line %d\n", node->param.name, node->line);
            printf("%*sType:\n", indent*2, "");
            print_ast(node->param.type, indent+1);
            break;
        case NODE_BREAK:
            printf("Break Statement at line %d\n", node->line);
            break;
        case NODE_CONTINUE:
            printf("Continue Statement at line %d\n", node->line);
            break;
        case NODE_RETURN:
            printf("Return Statement at line %d:\n", node->line);
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_PRINT:
            printf("Print Statement at line %d:\n", node->line);
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_IF:
            printf("If Statement at line %d:\n", node->line);
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
            printf("For Loop at line %d:\n", node->line);
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
            printf("While Loop at line %d:\n", node->line);
            printf("%*sCondition:\n", (indent+1)*2, "");
            print_ast(node->if_stmt.cond, indent+2);
            printf("%*sThen Block:\n", (indent+1)*2, "");
            print_ast(node->if_stmt.then_block, indent+2);
            break;
        case NODE_ASSIGN:
            printf("Variable Assignment at line %d:\n", node->line);
            printf("%*sName: %s\n", (indent+1)*2, "", node->var_decl.name);
            if (node->var_decl.init_value) {
                printf("%*sNew Value:\n", (indent+1)*2, "");
                print_ast(node->var_decl.init_value, indent+2);
            } else {
                printf("%*sNew Value: %s\n", (indent+1)*2, "", "NULL");
            }
            break;
        case NODE_VAR_DECL:
            printf("Variable Declaration at line %d:\n", node->line);
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
            printf("Function Call: %s at line %d\n", node->func_decl.name, node->line);
            printf("%*sArguments (%d) at line %d:\n", (indent+1)*2, "", node->func_decl.param_count, node->line);
            for (int i = 0; i < node->func_decl.param_count; i++) {
                print_ast(node->func_decl.params[i], indent+2);
            }
            break;
        default:
            fatal_error(node->line, "AST", "Unknown Node Type for AST print: %d\n", node->type);
    }
}

void free_ast(ASTNode* node) {
    if (!node) return;


    switch (node->type) {
        case NODE_FUNC_DECL:
            free(node->func_decl.name);
            free_ast(node->func_decl.return_type);
            for (int i = 0; i < node->func_decl.param_count; i++)
                free_ast(node->func_decl.params[i]);
            free(node->func_decl.params);
            free_ast(node->func_decl.body);
            break;

        case NODE_FUNC_CALL:
            free(node->func_decl.name);
            for (int i = 0; i < node->func_decl.param_count; i++)
                free_ast(node->func_decl.params[i]);
            free(node->func_decl.params);
            break;

        case NODE_BLOCK:
            for (int i = 0; i < node->block.stmt_count; i++)
                free_ast(node->block.statements[i]);
            free(node->block.statements);
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
            fatal_error(node->line, "AST", "Warning: Unknown node type %d in free_ast\n", node->type);
            break;
    }

    free(node);
}
