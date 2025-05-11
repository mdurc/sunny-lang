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
    int line;
    TokenType resolved_type; // changed by type checker after AST construction

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

ASTNode* create_func_call(const char* name, ASTNode** args, int arg_count, int line);
ASTNode* create_func_decl(ASTNode* return_type, const char* name, ASTNode** params, int param_count, ASTNode* body, int line);
ASTNode* create_block(ASTNode** statements, int count, int line);
ASTNode* create_bin_op(TokenType op, ASTNode* left, ASTNode* right, int line);
ASTNode* create_unary_op(TokenType op, ASTNode* operand, int line);
ASTNode* create_literal(Token* token, int line);
ASTNode* create_identifier(const char* name, int line);
ASTNode* create_type(bool mut, TokenType type, int line);
ASTNode* create_param(ASTNode* type, const char* name, int line);
ASTNode* create_return(ASTNode* expr, int line);
ASTNode* create_print(ASTNode* expr, int line);
ASTNode* create_if(ASTNode* cond, ASTNode* then_block, ASTNode* else_block, int line);
ASTNode* create_for(ASTNode* init, ASTNode* end, ASTNode* iter, ASTNode* body, int line);
ASTNode* create_var_decl(ASTNode* type, const char* name, ASTNode* init, int line);
ASTNode* create_assign(const char* name, ASTNode* value, int line);
ASTNode* create_while(ASTNode* cond, ASTNode* body, int line);
ASTNode* create_break(int line);
ASTNode* create_continue(int line);

void print_ast(ASTNode* node, int indent);
void free_ast(ASTNode* node);

#endif
