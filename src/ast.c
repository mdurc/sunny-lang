#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ASTNode* create_func_decl(ASTNode* return_type, const char* name,
        ASTNode** params, int param_count, ASTNode* body) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FUNC_DECL;
    node->func_decl.return_type = return_type;
    node->func_decl.name = strdup(name);
    node->func_decl.params = params;
    node->func_decl.param_count = param_count;
    node->func_decl.body = body;
    return node;
}

ASTNode* create_func_call(const char* name, ASTNode** args, int arg_count) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FUNC_CALL;
    // reuse func_decl space
    node->func_decl.name = strdup(name);
    node->func_decl.params = args;
    node->func_decl.param_count = arg_count;
    return node;
}

ASTNode* create_identifier(const char* name) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_IDENTIFIER;
    node->identifier = strdup(name);
    return node;
}

ASTNode* create_type(bool mut, TokenType type) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_PRIMITIVE;
    node->primitive.mut = mut;
    node->primitive.type_spec = type;
    return node;
}

ASTNode* create_param(ASTNode* type, const char* name) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_PARAM;
    node->param.type = type;
    node->param.name = strdup(name);
    return node;
}

ASTNode* create_block(ASTNode** statements, int count) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BLOCK;
    node->block.statements = statements;
    node->block.stmt_count = count;
    return node;
}

ASTNode* create_print(ASTNode* expr) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_PRINT;
    node->unary_op.operand = expr;
    return node;
}

ASTNode* create_if(ASTNode* cond, ASTNode* then_block, ASTNode* else_block) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_IF;
    node->if_stmt.cond = cond;
    node->if_stmt.then_block = then_block;
    node->if_stmt.else_block = else_block;
    return node;
}

ASTNode* create_while(ASTNode* cond, ASTNode* body) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_WHILE;
    // reuse if_stmt but only with condition and body
    node->if_stmt.cond = cond;
    node->if_stmt.then_block = body;
    return node;
}

ASTNode* create_for(ASTNode* init, ASTNode* end, ASTNode* iter, ASTNode* body) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FOR;
    node->for_stmt.init_expr = init;
    node->for_stmt.end_expr = end;
    node->for_stmt.iter_expr = iter;
    node->for_stmt.body = body;
    return node;
}

ASTNode* create_var_decl(ASTNode* type, const char* name, ASTNode* init) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_VAR_DECL;
    node->var_decl.var_type = type;
    node->var_decl.name = strdup(name);
    node->var_decl.init_value = init;
    return node;
}

ASTNode* create_assign(const char* name, ASTNode* value) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_ASSIGN;
    // reuse var_decl but without type
    node->var_decl.name = strdup(name);
    node->var_decl.init_value = value;
    return node;
}

ASTNode* create_bin_op(TokenType op, ASTNode* left, ASTNode* right) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BIN_OP;
    node->bin_op.op = op;
    node->bin_op.left = left;
    node->bin_op.right = right;
    return node;
}

ASTNode* create_unary_op(TokenType op, ASTNode* operand) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_UNARY_OP;
    node->unary_op.op = op;
    node->unary_op.operand = operand;
    return node;
}


ASTNode* create_return(ASTNode* expr) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_RETURN;
    node->unary_op.operand = expr;
    return node;
}

ASTNode* create_literal(Token* token) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_LITERAL;
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
        case FALSE:
        case NULL_LITERAL:
            node->literal.i = 0;
            break;
        default:
            fprintf(stderr, "Invalid literal type: %d\n", token->type);
            exit(EXIT_FAILURE);
    }
    return node;
}

void print_ast(ASTNode* node, int indent) {
    if (!node) return;
    for (int i = 0; i < indent; i++) printf("  ");

    switch (node->type) {
        case NODE_FUNC_DECL:
            printf("Function: %s\n", node->func_decl.name);
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
            printf("Block (%d statements):\n", node->block.stmt_count);
            for (int i = 0; i < node->block.stmt_count; i++) {
                print_ast(node->block.statements[i], indent+1);
            }
            break;
        case NODE_BIN_OP:
            printf("Binary Operation: %s\n", tok_string(node->bin_op.op));
            print_ast(node->bin_op.left, indent+1);
            print_ast(node->bin_op.right, indent+1);
            break;
        case NODE_UNARY_OP:
            printf("Unary Operation: %s\n", tok_string(node->unary_op.op));
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_LITERAL:
            if (node->token->type == INT_LITERAL)
                printf("Integer Literal: %lld\n", node->literal.i);
            else if (node->token->type == CHAR_LITERAL)
                printf("Char Literal (ascii): %lld\n", node->literal.i);
            else if (node->token->type == FLOAT_LITERAL)
                printf("Float Literal: %f\n", node->literal.f);
            else if (node->token->type == STRING_LITERAL)
                printf("String Literal: \"%s\"\n", node->literal.s);
            else if (node->token->type == TRUE)
                printf("Boolean Literal: true\n");
            else if (node->token->type == FALSE)
                printf("Boolean Literal: false\n");
            else if (node->token->type == NULL_LITERAL)
                printf("Null Literal\n");
            break;
        case NODE_IDENTIFIER:
            printf("Identifier: %s\n", node->identifier);
            break;
        case NODE_PRIMITIVE:
            printf("Primitive: %s%s\n", (node->primitive.mut ? "mut ":""), tok_string(node->primitive.type_spec));
            break;
        case NODE_PARAM:
            printf("Parameter: %s\n", node->param.name);
            printf("%*sType:\n", indent*2, "");
            print_ast(node->param.type, indent+1);
            break;
        case NODE_RETURN:
            printf("Return Statement:\n");
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_PRINT:
            printf("Print Statement:\n");
            print_ast(node->unary_op.operand, indent+1);
            break;
        case NODE_IF:
            printf("If Statement:\n");
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
            printf("For Loop:\n");
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
            printf("While Loop:\n");
            printf("%*sCondition:\n", (indent+1)*2, "");
            print_ast(node->if_stmt.cond, indent+2);
            printf("%*sThen Block:\n", (indent+1)*2, "");
            print_ast(node->if_stmt.then_block, indent+2);
            break;
        case NODE_ASSIGN:
            printf("Variable Assignment:\n");
            printf("%*sName: %s\n", (indent+1)*2, "", node->var_decl.name);
            if (node->var_decl.init_value) {
                printf("%*sNew Value:\n", (indent+1)*2, "");
                print_ast(node->var_decl.init_value, indent+2);
            } else {
                printf("%*sNew Value: %s\n", (indent+1)*2, "", "NULL");
            }
            break;
        case NODE_VAR_DECL:
            printf("Variable Declaration:\n");
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
            printf("Function Call: %s\n", node->func_decl.name);
            printf("%*sArguments (%d):\n", (indent+1)*2, "", node->func_decl.param_count);
            for (int i = 0; i < node->func_decl.param_count; i++) {
                print_ast(node->func_decl.params[i], indent+2);
            }
            break;
        default:
            printf("Unknown Node Type: %d\n", node->type);
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

        case NODE_PRIMITIVE:
            // No dynamic memory to free for ints, bools, or floats
            break;

        default:
            fprintf(stderr, "Warning: Unknown node type %d in free_ast\n", node->type);
            break;
    }

    free(node);
}
