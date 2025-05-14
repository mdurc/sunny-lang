#include "json_exporter.h"
#include <stdio.h>

static void json_escape(FILE* output, const char* str) {
    putc('"', output);
    for (const char* c = str; *c; c++) {
        switch (*c) {
            case '"': fputs("\\\"", output); break;
            case '\\': fputs("\\\\", output); break;
            case '\b': fputs("\\b", output); break;
            case '\f': fputs("\\f", output); break;
            case '\n': fputs("\\n", output); break;
            case '\r': fputs("\\r", output); break;
            case '\t': fputs("\\t", output); break;
            default: putc(*c, output);
        }
    }
    putc('"', output);
}

static void export_range(FILE* output, int row, int col, int len) {
    fprintf(output, "\"range\":{"
        "\"start\":{\"line\":%d,\"character\":%d},"
        "\"end\":{\"line\":%d,\"character\":%d}}",
        row, col, row, col + len);
}

static void export_ast_node(FILE* output, ASTNode* node) {
    if (!node) {
        fprintf(output, "null");
        return;
    }

    switch(node->node_type) {
        case NODE_FUNC_DECL:
            fprintf(output, "{\"name\":");
            json_escape(output, node->func_decl.name);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);
            fprintf(output, "}"); // done with function data

            for (int i = 0; i < node->func_decl.param_count; i++) {
                fprintf(output, ",");
                export_ast_node(output, node->func_decl.params[i]);
            }

            if (node->token->type != U0) {
                fprintf(output, ",");
                export_ast_node(output, node->func_decl.return_param);
            }

            fprintf(output, ",");
            export_ast_node(output, node->func_decl.body);
            break;
        case NODE_BLOCK:
            if (node->block.stmt_count == 0) {
                fprintf(output, "null");
            }
            for (int i = 0; i < node->block.stmt_count; i++) {
                if (i > 0) fprintf(output, ",");
                export_ast_node(output, node->block.statements[i]);
            }
            break;
        case NODE_BIN_OP:
            export_ast_node(output, node->bin_op.left);
            fprintf(output, ",");
            export_ast_node(output, node->bin_op.right);
            break;
        case NODE_UNARY_OP:
            export_ast_node(output, node->unary_op.operand);
            break;
        case NODE_LITERAL:
            fprintf(output, "{\"name\":");
            json_escape(output, node->token->start);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);

            fprintf(output, ",\"literalType\":\"%s\"", tok_string(node->token->type));
            fprintf(output, "}");
            break;
        case NODE_IDENTIFIER:
            fprintf(output, "{\"name\":");
            json_escape(output, node->identifier);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);
            fprintf(output, "}");
            break;
        case NODE_PRIMITIVE:
            fprintf(output, "{\"name\":");
            json_escape(output, node->token->start);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);

            //fprintf(output, ",\"isMut\":%s", node->is_mut ? "true" : "false");
            fprintf(output, "}");
            break;
        case NODE_PARAM:
            fprintf(output, "{\"name\":");
            json_escape(output, node->param.name);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);
            fprintf(output, "}");
            fprintf(output, ",");
            export_ast_node(output, node->param.type);
            break;
        case NODE_RETURN:
            if (node->unary_op.operand) {
                export_ast_node(output, node->unary_op.operand);
            } else {
                fprintf(output, "{\"name\":\"return\"");
                fprintf(output, ",\"scope\": %d,", node->scope_depth);
                export_range(output, node->token->row, node->token->col, node->token->len);
                fprintf(output, "}");
            }
            break;
        case NODE_PRINT:
            if (node->unary_op.operand) {
                export_ast_node(output, node->unary_op.operand);
            } else {
                fprintf(output, "{\"name\":\"print\"");
                fprintf(output, ",\"scope\": %d,", node->scope_depth);
                export_range(output, node->token->row, node->token->col, node->token->len);
                fprintf(output, "}");
            }
            break;
        case NODE_IF:
            export_ast_node(output, node->if_stmt.cond);
            fprintf(output, ",");
            export_ast_node(output, node->if_stmt.then_block);
            if (node->if_stmt.else_block) {
                fprintf(output, ",");
                export_ast_node(output, node->if_stmt.else_block);
            }
            break;
        case NODE_FOR:
        {
            bool print_first = false;
            if (node->for_stmt.init_expr) {
                print_first = true;
                export_ast_node(output, node->for_stmt.init_expr);
            }

            if (node->for_stmt.end_expr) {
                if (print_first) fprintf(output, ",");
                print_first = true;
                export_ast_node(output, node->for_stmt.end_expr);
            }

            if (node->for_stmt.iter_expr) {
                if (print_first) fprintf(output, ",");
                print_first = true;
                export_ast_node(output, node->for_stmt.iter_expr);
            }

            if (print_first) fprintf(output, ",");
            export_ast_node(output, node->for_stmt.body);
        }
            break;
        case NODE_WHILE:
            export_ast_node(output, node->if_stmt.cond);
            fprintf(output, ",");
            export_ast_node(output, node->if_stmt.then_block);
            break;
        case NODE_ASSIGN:
            fprintf(output, "{\"name\":");
            json_escape(output, node->var_decl.name);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);
            fprintf(output, "}");

            if (node->var_decl.init_value) {
                fprintf(output, ",");
                export_ast_node(output, node->var_decl.init_value);
            }
            break;
        case NODE_VAR_DECL:
            fprintf(output, "{\"name\":");
            json_escape(output, node->var_decl.name);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);
            fprintf(output, "}");

            fprintf(output, ",");
            export_ast_node(output, node->var_decl.var_type);

            if (node->var_decl.init_value) {
                fprintf(output, ",");
                export_ast_node(output, node->var_decl.init_value);
            }
            break;
        case NODE_FUNC_CALL:
            fprintf(output, "{\"name\":");
            json_escape(output, node->func_decl.name);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);
            fprintf(output, "}");

            for (int i = 0; i < node->func_decl.param_count; i++) {
                fprintf(output, ",");
                export_ast_node(output, node->func_decl.params[i]);
            }
            break;
        default:
            fprintf(output, "{\"name\":");
            json_escape(output, node->token->start);
            fprintf(output, ",\"scope\": %d,", node->scope_depth);
            export_range(output, node->token->row, node->token->col, node->token->len);
            fprintf(output, "}");
            break;
    }
}

static void export_symbol(FILE* output, Symbol* sym, SymbolTable* tab) {
    fprintf(output, "{");
    json_escape(output, "name");
    fprintf(output, ":");
    json_escape(output, sym->name);

    fprintf(output, ",");
    json_escape(output, "reachable_scopes");
    fprintf(output, ":[");

    int depth_count;
    int* depths = symtab_get_reachable_depths(tab, &depth_count);
    for (int i=0; i<depth_count; ++i) {
        if (i > 0) fprintf(output, ",");
        fprintf(output, "%d", depths[i]);
    }
    free(depths);

    fprintf(output, "]");

    if (sym->ast_node) {
        switch(sym->ast_node->node_type) {
            case NODE_VAR_DECL:
                fprintf(output, ",\"type\":\"%s%s\"",
                        sym->ast_node->var_decl.var_type->is_mut ? "mut " : "",
                        tok_string(sym->ast_node->var_decl.var_type->token->type));
                fprintf(output, ",");
                export_range(output, sym->ast_node->token->row, sym->ast_node->token->col, sym->ast_node->token->len);
                break;
            case NODE_FUNC_DECL:
                if (sym->ast_node->func_decl.return_param->token->type == U0) {
                    // void
                    fprintf(output, ",\"type\":\"func->%s%s\"",
                            sym->ast_node->func_decl.return_param->is_mut ? "mut " : "",
                            tok_string(sym->ast_node->func_decl.return_param->token->type));
                } else {
                    fprintf(output, ",\"type\":\"func->%s%s\"",
                            sym->ast_node->func_decl.return_param->param.type->is_mut ? "mut " : "",
                            tok_string(sym->ast_node->func_decl.return_param->param.type->token->type));
                }
                fprintf(output, ",");
                export_range(output, sym->ast_node->token->row, sym->ast_node->token->col, sym->ast_node->token->len);
                break;
            case NODE_PARAM:
                fprintf(output, ",\"type\":\"param:%s%s\"",
                        sym->ast_node->param.type->is_mut ? "mut " : "",
                        tok_string(sym->ast_node->param.type->token->type));
                fprintf(output, ",");
                export_range(output, sym->ast_node->token->row, sym->ast_node->token->col, sym->ast_node->token->len);
                break;
            default:
                break;
        }
    }
    fprintf(output, "}");
}

static void export_symbol_table(FILE* output, SymbolTable* tab) {
    bool first_symbol = true;
    Symbol* sym = tab->head;
    while (sym) {
        if (!first_symbol) fprintf(output, ",");
        first_symbol = false;
        export_symbol(output, sym, tab);
        sym = sym->next;
    }
}

static void export_symtab(FILE* output, SymbolTable* tab, bool* first) {
    if (!tab) return;
    for (int i = 0; i < tab->children_count; i++) {
        export_symtab(output, tab->children[i], first);
    }
    if (tab->head != NULL) {
        if (!*first) {
            fprintf(output, ",");
        } else {
            *first = false;
        }
        export_symbol_table(output, tab);
    }
}

static void export_diagnostic(FILE* output, Diagnostic* diag) {
    fprintf(output, "{");
    export_range(output, diag->row, diag->col, diag->len);
    fprintf(output, ",\"severity\":%d", diag->severity);
    fprintf(output, ",\"source\":\"sunny-lsp-compiler\"");
    fprintf(output, ",\"message\":");
    json_escape(output, diag->message);
    fprintf(output, "}");
}

void export_context_to_json(Parser* parser, ASTNode* ast, FILE* output) {
    fprintf(output, "{");
    fprintf(output, "\"symbols\":[");
    bool first = true;
    export_symtab(output, parser->symtab, &first);
    fprintf(output, "],");

    // ast export:
    fprintf(output, "\"ast\":");
    fprintf(output, "[");
    export_ast_node(output, ast);
    fprintf(output, "],");

    // with the diagnostics
    fprintf(output, "\"diagnostics\":[");
    for (int i = 0; i < parser->diag_count; i++) {
        if (i > 0) fprintf(output, ",");
        export_diagnostic(output, parser->diagnostics[i]);
    }
    fprintf(output, "]");

    fprintf(output, "}");
}
