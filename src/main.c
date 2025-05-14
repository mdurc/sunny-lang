#include "exporter/json_exporter.h"
#include "lexer.h"
#include "parser.h"
#include "type_checker.h"
#include <assert.h>
#include <stdlib.h>

void cleanup(Parser* parser, ASTNode* ast) {
    free_ast(ast);
    free_parser(parser);
    free(parser);
}

bool compile(const char* filename, bool export_json) {
    FILE* fp = fopen(filename, "r");
    if (fp == NULL){
        fprintf(stderr, "Invalid input file: %s\n", filename);
        exit(EXIT_FAILURE);
    }

    Token** tokens = NULL;
    int count = 0;
    lex_file(fp, &tokens, &count);
    fclose(fp);

    Parser* parser = parser_init(tokens, count);
    ASTNode* ast = parse_program(parser);
    typecheck_program(ast, parser);

    if (parser->errors > 0) {
        fprintf(stderr, "Compilation failed with %d errors and %d warnings.\n", parser->errors, parser->warnings);
        if (export_json) {
            export_context_to_json(parser, ast, stdout);
        }
        cleanup(parser, ast);
        return false;
    }

    if (export_json) {
        export_context_to_json(parser, ast, stdout);
    } else {
        printf("Generated AST:\n");
        print_ast(ast, 0);
    }

    cleanup(parser, ast);
    return true;
}

int main(int argc, char** argv) {
    bool export_json = false;
    char* filename = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--export-json") == 0) {
            export_json = true;
        } else {
            filename = argv[i];
        }
    }

    if (!filename) {
        fprintf(stderr, "Usage: %s [--export-json] input.code\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    compile(filename, export_json);

    return 0;
}
