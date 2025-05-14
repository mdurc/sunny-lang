#include "symbol_table.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

SymbolTable* symtab_create(SymbolTable* parent) {
    SymbolTable* st = malloc(sizeof(SymbolTable));
    st->head = NULL;
    st->parent = parent;
    st->scope_depth = parent ? parent->scope_depth + 1 : 0;
    st->children = NULL;
    st->children_count = 0;
    st->children_capacity = 0;
    return st;
}

void symtab_destroy(SymbolTable* st) {
    Symbol* current = st->head;
    while (current) {
        Symbol* next = current->next;
        if (current->prev) current->prev->next = NULL; // no dangling pointers
        current->prev = NULL;
        free(current->name);
        free(current);
        current = next;
    }
    free(st->children);
    free(st);
}

void symtab_destroy_all(SymbolTable* root) {
    if (!root) return;
    for (int i = 0; i < root->children_count; i++) {
        symtab_destroy_all(root->children[i]);
    }
    symtab_destroy(root);
}

static void collect_unique_depths(SymbolTable* node, int** depths, int* size, int* capacity) {
    if (!node) return;
    const int current_depth = node->scope_depth;
    for (int i = 0; i < *size; i++) {
        if ((*depths)[i] == current_depth) goto process_children;
    }
    if (*size >= *capacity) {
        *capacity = (*capacity ? *capacity * 2 : 4);
        *depths = realloc(*depths, *capacity * sizeof(int));
    }
    (*depths)[(*size)++] = current_depth;

process_children:
    for (int i = 0; i < node->children_count; i++) {
        collect_unique_depths(node->children[i], depths, size, capacity);
    }
}

// expects the caller to free the memory
int* symtab_get_reachable_depths(SymbolTable* root, int* out_count) {
    int* depths = NULL;
    int size = 0, capacity = 0;
    if (root) collect_unique_depths(root, &depths, &size, &capacity);
    if (size > 0) {
        int* tmp = realloc(depths, size * sizeof(int));
        if (tmp) depths = tmp;
    }
    *out_count = size;
    return depths;
}

// Caller will have to explicitly check for null and throw exception
Symbol* symtab_insert(SymbolTable* st, const char* name, ASTNode* ast_node, bool is_initialized) {
    // check for existing symbol in current scope
    if (symtab_lookup_current(st, name)) {
        return NULL;
    }

    Symbol* sym = malloc(sizeof(Symbol));
    sym->name = strdup(name);
    sym->ast_node = ast_node;
    sym->is_initialized = is_initialized;
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
    SymbolTable* parent = *root;
    SymbolTable* new_scope = symtab_create(parent);

    if (parent) {
        if (parent->children_count >= parent->children_capacity) {
            parent->children_capacity = parent->children_capacity ?
                parent->children_capacity * 2 : 4;
            parent->children = realloc(parent->children,
                parent->children_capacity * sizeof(SymbolTable*));
        }
        parent->children[parent->children_count++] = new_scope;
    }
    *root = new_scope;
}

// modify root by bringing it to outer scope (if not outmost) and destroy inner scope
void symtab_exit_scope(SymbolTable** root) {
    if (*root && (*root)->parent) {
        *root = (*root)->parent;
    }
}

void symtab_print(SymbolTable* st) {
    printf("\nSymbol Table at Current and Parent Scopes:\n");
    SymbolTable* current = st;
    int count = 1;
    while (current) {
        printf("\tScope %d:\n", count++);
        Symbol* sym = current->head;
        while (sym) {
            printf("\t\t%s\n", sym->name);
            sym = sym->next;
        }
        current = current->parent;
    }
}

void symtab_print_current(SymbolTable* st) {
    printf("\nSymbol Table at Current Scope:\n");
    if (st == NULL) {
        printf("\tTable is a null pointer\n");
        return;
    }
    Symbol* current = st->head;
    while (current) {
        printf("\t%s\n", current->name);
        current = current->next;
    }
}
