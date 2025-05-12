#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "ast.h"

// linked list symbol table (fast insert slow access)
typedef struct Symbol {
    char* name;
    ASTNode* ast_node;
    bool is_initialized;
    int scope_depth;
    struct Symbol* next;
    struct Symbol* prev;
} Symbol;

typedef struct SymbolTable {
    struct Symbol* head;
    int scope_depth;
    struct SymbolTable* parent; // parent scope
    struct SymbolTable** children; // children scope
    int children_count;
    int children_capacity;
} SymbolTable;

SymbolTable* symtab_create(SymbolTable* parent);
void symtab_destroy(SymbolTable* st);
void symtab_destroy_all(SymbolTable* root);
Symbol* symtab_insert(SymbolTable* st, const char* name, ASTNode* ast_node, bool is_initialized);
Symbol* symtab_lookup(SymbolTable* st, const char* name);
Symbol* symtab_lookup_current(SymbolTable* st, const char* name);
void symtab_enter_new_scope(SymbolTable** root);
void symtab_exit_scope(SymbolTable** root);
void symtab_print_current(SymbolTable* st);
void symtab_print(SymbolTable* st);

#endif
