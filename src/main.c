
#include "error.h"
#include "lexer.h"
#include "parser.h"
#include <assert.h>
#include <stdlib.h>
//#include "code_gen.h"

int main(int argc, char** argv){
    if (argc != 2){
        fprintf(stderr, "Usage: ./a.out input.code\n");
        exit(EXIT_FAILURE);
    }
    FILE* fp = fopen(argv[1], "r");
    if (fp == NULL){
        fprintf(stderr, "Invalid input file: %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }

    Token** tokens = NULL;
    int token_count = 0;
    int capacity = 0;
    lex_file(fp, &tokens, &token_count, &capacity);
    fclose(fp);

    Parser* parser = parser_init(tokens, token_count);
    ASTNode* ast = parse_program(parser);

    if (parser->errors > 0) {
        fprintf(stderr, "Compilation failed with %d errors and %d warnings.\n", parser->errors, parser->warnings);
        symtab_destroy_all(parser->symtab);
        free_ast(ast);
        for (int i = 0; i < token_count; i++) {
            free_token_data(tokens[i]);
            free(tokens[i]);
        }
        free(tokens);
        free(parser);
        exit(EXIT_FAILURE);
    }

    printf("Generated AST:\n");
    print_ast(ast, 0);

    symtab_destroy_all(parser->symtab);
    free_ast(ast);
    for (int i = 0; i < token_count; i++) {
        free_token_data(tokens[i]);
        free(tokens[i]);
    }
    free(tokens);
    free(parser);
    return 0;
}
