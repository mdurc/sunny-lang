#ifndef TYPE_CHECKER_H
#define TYPE_CHECKER_H

#include "symbol_table.h"
#include "parser.h"

typedef struct {
    SymbolTable* symtab;
    TokenType current_return_type;
    Parser* parser;
    bool in_loop;
} TypeChecker;


void typecheck_program(ASTNode* program, Parser* parser);
void typecheck_node(ASTNode* node, TypeChecker* ctx);
void typecheck_func_decl(ASTNode* node, TypeChecker* ctx);
void typecheck_function_call(ASTNode* call, TypeChecker* ctx);
void typecheck_var_decl(ASTNode* node, TypeChecker* ctx);
void typecheck_assign_expr(ASTNode* node, ASTNode* expr, TypeChecker* ctx);
void resolve_identifier_type(ASTNode* node, TypeChecker* ctx);

void typecheck_if(ASTNode* node, TypeChecker* ctx);
void typecheck_while(ASTNode* node, TypeChecker* ctx);
void typecheck_for(ASTNode* node, TypeChecker* ctx);
void typecheck_assign(ASTNode* node, TypeChecker* ctx);
void typecheck_bin_op(ASTNode* node, TypeChecker* ctx);
void typecheck_unary_op(ASTNode* node, TypeChecker* ctx);
void resolve_literal_type(ASTNode* node, TypeChecker* ctx);
void typecheck_print(ASTNode* node, TypeChecker* ctx);
void typecheck_return(ASTNode* node, TypeChecker* ctx);

bool check_all_paths_return(ASTNode* node, TypeChecker* ctx, const char* return_var, bool assigned);
void type_error(TypeChecker* ctx, bool bin_op, ASTNode* node, const char* requirement, TokenType lt, TokenType rt);
#endif
