#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"
#include "symbol_table.h"

typedef enum {
    DIAG_ERROR = 1,
    DIAG_WARNING
} DiagnosticSeverity;

typedef struct {
    int row, col, len;
    char* message;
    DiagnosticSeverity severity;
} Diagnostic;

typedef struct {
    // extra tokens that may need to be added by parser
    Token** p_tokens;
    int p_size, p_capacity;

    Token** tokens;
    int pos;
    int size;
    SymbolTable* symtab;

    bool panic_mode;
    Diagnostic** diagnostics;
    int diag_count;
    int diag_capacity;
    int last_line;

    int errors, warnings;
} Parser;

void parser_add_diagnostic(Parser* p, DiagnosticSeverity severity, int line, int col, int len, const char* format, ...);
void free_parser(Parser* p);
Parser* parser_init(Token** tokens, int count);

ASTNode* parse_program(Parser* parser);
ASTNode* parse_function(Parser* parser);
ASTNode* parse_func_call(Parser* p, const char* name, Token* tok);
ASTNode* parse_if(Parser* p);
ASTNode* parse_for(Parser* p);
ASTNode* parse_statement(Parser* parser);
ASTNode* parse_var_decl(Parser* p);
ASTNode* parse_expression(Parser* parser);

ASTNode* parse_type(Parser* p);
ASTNode* parse_param(Parser* p, bool is_param);
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
