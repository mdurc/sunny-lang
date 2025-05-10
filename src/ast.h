#ifndef AST_H
#define AST_H

#include "lexer.h"

typedef enum {
    NODE_FUNC_DECL,
    NODE_FUNC_CALL,
    NODE_IDENTIFIER,
    NODE_PRIMITIVE,
    NODE_PARAM,
    NODE_BLOCK,
    NODE_RETURN,
    NODE_BREAK,
    NODE_CONTINUE,
    NODE_PRINT,
    NODE_IF,
    NODE_WHILE,
    NODE_FOR,
    NODE_VAR_DECL,
    NODE_ASSIGN,
    NODE_BIN_OP,
    NODE_UNARY_OP,
    NODE_LITERAL,
} NodeType;

typedef struct ASTNode ASTNode;

struct ASTNode {

    NodeType type;
    Token* token;

    union {
        // function declaration
        struct {
            ASTNode* return_type;
            char* name;
            ASTNode** params;
            int param_count;
            ASTNode* body;
        } func_decl;

        // content block
        struct {
            ASTNode** statements;
            int stmt_count;
        } block;

        // binary operation
        struct {
            ASTNode *left, *right;
            TokenType op;
        } bin_op;

        // unary operation
        struct {
            ASTNode *operand;
            TokenType op;
        } unary_op;

        // literal values
        union {
            uint64_t i;
            double f;
            char* s;
        } literal;

        // identifier
        char* identifier;

        // type
        struct {
            bool mut;
            TokenType type_spec;
        } primitive;

        // function parameter
        struct {
            ASTNode* type;
            char* name;
        } param;

        // control structures
        struct {
            ASTNode *cond, *then_block, *else_block;
        } if_stmt;

        struct {
            ASTNode *init_expr;
            ASTNode *end_expr;
            ASTNode *iter_expr;
            ASTNode* body;
        } for_stmt;

        // variable declaration
        struct {
            ASTNode* var_type;
            char* name;
            ASTNode* init_value;
        } var_decl;
    };
};

ASTNode* create_func_call(const char* name, ASTNode** args, int arg_count);
ASTNode* create_func_decl(ASTNode* return_type, const char* name, ASTNode** params, int param_count, ASTNode* body);
ASTNode* create_block(ASTNode** statements, int count);
ASTNode* create_bin_op(TokenType op, ASTNode* left, ASTNode* right);
ASTNode* create_unary_op(TokenType op, ASTNode* operand);
ASTNode* create_literal(Token* token);
ASTNode* create_identifier(const char* name);
ASTNode* create_type(bool mut, TokenType type);
ASTNode* create_param(ASTNode* type, const char* name);
ASTNode* create_return(ASTNode* expr);
ASTNode* create_print(ASTNode* expr);
ASTNode* create_if(ASTNode* cond, ASTNode* then_block, ASTNode* else_block);
ASTNode* create_for(ASTNode* init, ASTNode* end, ASTNode* iter, ASTNode* body);
ASTNode* create_var_decl(ASTNode* type, const char* name, ASTNode* init);

void print_ast(ASTNode* node, int indent);
void free_ast(ASTNode* node);

ASTNode* create_assign(const char* name, ASTNode* value);
ASTNode* create_while(ASTNode* cond, ASTNode* body);
#endif
