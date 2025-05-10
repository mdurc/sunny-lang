#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "ast.h"

typedef enum {
    SYM_VARIABLE,
    SYM_FUNCTION,
    SYM_PARAMETER,
    SYM_TYPE
} SymbolKind;

// linked list symbol table (fast insert slow access)
typedef struct Symbol {
    char* name;
    SymbolKind kind;
    ASTNode* type;
    ASTNode* decl_node;
    bool is_mutable;
    int scope_depth;
    struct Symbol* next;
    struct Symbol* prev;
} Symbol;

typedef struct SymbolTable {
    struct Symbol* head;
    int scope_depth;
    struct SymbolTable* parent; // parent scope
} SymbolTable;

SymbolTable* symtab_create(SymbolTable* parent);
void symtab_destroy(SymbolTable* st);
void symtab_destroy_all(SymbolTable* st);
Symbol* symtab_insert(SymbolTable* st, const char* name, SymbolKind kind, ASTNode* type, ASTNode* decl_node, bool is_mutable);
Symbol* symtab_lookup(SymbolTable* st, const char* name);
Symbol* symtab_lookup_current(SymbolTable* st, const char* name);
void symtab_enter_new_scope(SymbolTable** root);
void symtab_exit_scope(SymbolTable** root);

#endif
