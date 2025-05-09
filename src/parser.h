#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"
#include "symbol_table.h"

typedef struct {
    Token** tokens;
    int pos;
    int size;
    SymbolTable* symtab;
} Parser;

Parser* parser_init(Token** tokens, int count);
ASTNode* parse_program(Parser* parser);
ASTNode* parse_function(Parser* parser);
ASTNode* parse_statement(Parser* parser);
ASTNode* parse_var_decl(Parser* p);
ASTNode* parse_expression(Parser* parser);

ASTNode* parse_type(Parser* p);
ASTNode* parse_param(Parser* p);
ASTNode* parse_block(Parser* p, bool create_scope);
ASTNode* parse_assignment(Parser* p);

ASTNode* parse_primary(Parser* p);
ASTNode* parse_multiplicative(Parser* p);
ASTNode* parse_additive(Parser* p);
ASTNode* parse_comparison(Parser* p);
ASTNode* parse_equality(Parser* p);
ASTNode* parse_logic_and(Parser* p);
ASTNode* parse_logic_or(Parser* p);
#endif
