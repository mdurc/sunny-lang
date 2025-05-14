#ifndef JSON_EXPORTER_H
#define JSON_EXPORTER_H

#include "../ast.h"
#include "../symbol_table.h"
#include "../parser.h"

void export_context_to_json(Parser* parser, ASTNode* ast, FILE* output);

#endif
