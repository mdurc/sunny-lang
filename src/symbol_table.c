#include "symbol_table.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

SymbolTable* symtab_create(SymbolTable* parent) {
    SymbolTable* st = malloc(sizeof(SymbolTable));
    st->head = NULL;
    st->parent = parent;
    st->scope_depth = parent ? parent->scope_depth + 1 : 0;
    return st;
}

void symtab_destroy(SymbolTable* st) {
    Symbol* current = st->head;
    // free linked list within table
    while (current) {
        Symbol* next = current->next;
        free(current->name);
        free(current);
        current = next;
    }
    // free the table
    free(st);
}

void symtab_destroy_all(SymbolTable* st) {
    if (st == NULL) return;
    SymbolTable* next;
    while (st != NULL) {
        next = st->parent;
        symtab_destroy(st);
        st = next;
    }
}

// Caller will have to explicitly check for null and throw exception
Symbol* symtab_insert(SymbolTable* st, const char* name, SymbolKind kind,
                     ASTNode* type, ASTNode* decl_node, bool is_mutable) {
    // check for existing symbol in current scope
    if (symtab_lookup_current(st, name)) {
        return NULL;
    }

    Symbol* sym = malloc(sizeof(Symbol));
    sym->name = strdup(name);
    sym->kind = kind;
    sym->type = type;
    sym->decl_node = decl_node;
    sym->is_mutable = is_mutable;
    sym->scope_depth = st->scope_depth;

    // prepend this new symbol to the front of linked list
    // complements the move-to-front optimization on lookup
    sym->next = st->head;
    sym->prev = NULL;

    if (st->head) {
        st->head->prev = sym;
    }
    st->head = sym;
    return sym;
}

// search within current and all parent scopes
Symbol* symtab_lookup(SymbolTable* st, const char* name) {
    SymbolTable* current = st;
    while (current) {
        Symbol* sym = current->head;
        while (sym) {
            if (strcmp(sym->name, name) == 0) {
                // move to front optimization
                if (sym != current->head) {
                    if (sym->prev) sym->prev->next = sym->next;
                    if (sym->next) sym->next->prev = sym->prev;

                    sym->next = current->head;
                    sym->prev = NULL;
                    if (current->head) current->head->prev = sym;
                    current->head = sym;
                }

                return sym;
            }
            sym = sym->next;
        }
        // look in the outer scope
        current = current->parent;
    }
    return NULL;
}

// search within the current scope only
Symbol* symtab_lookup_current(SymbolTable* st, const char* name) {
    Symbol* sym = st->head;
    while (sym) {
        if (strcmp(sym->name, name) == 0) {
            // move-to-front
            if (sym != st->head) {
                if (sym->prev) sym->prev->next = sym->next;
                if (sym->next) sym->next->prev = sym->prev;

                sym->next = st->head;
                sym->prev = NULL;
                if (st->head) st->head->prev = sym;
                st->head = sym;
            }
            return sym;
        }
        sym = sym->next;
    }
    return NULL;
}

// make a child scope from root then modify root to now point to that sub-scope
void symtab_enter_new_scope(SymbolTable** root) {
    SymbolTable* new_scope = symtab_create(*root);
    *root = new_scope;
}

// modify root by bringing it to outer scope (if not outmost) and destroy inner scope
void symtab_exit_scope(SymbolTable** root) {
    if (*root && (*root)->parent) {
        SymbolTable* old = *root;
        *root = old->parent;
        symtab_destroy(old);
    }
}

