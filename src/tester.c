#include "lexer.h"
#include <assert.h>
#include <stdlib.h>

// used for easily making my own tests and checking the lexer quickly
void run_test(const char* test_str) {
    printf("======\n");
    size_t string_length = strlen(test_str);

    FILE* fp = fmemopen((void*)test_str, string_length, "r");
    if (fp == NULL) {
        perror("fmemopen failed");
        exit(EXIT_FAILURE);
    }

    Token** tokens = NULL;
    int token_count = 0;
    int capacity = 0;
    lex_file(fp, &tokens, &token_count, &capacity);
    fclose(fp);

    for (int i = 0; i < token_count; i++){
        if (tokens[i] == NULL) {
            printf("null token\n");
        } else {
            if (tokens[i]->type == INT_LITERAL) {
                printf("Int: %lld\n", (int64_t)tokens[i]->data.int_t);
            } else if (tokens[i]->type == FLOAT_LITERAL) {
                printf("Float: %f\n", tokens[i]->data.f64_value);
            } else if (tokens[i]->type == STRING_LITERAL) {
                printf("Str: %s\n", tokens[i]->data.lexeme);
            } else {
                printf("Key/Ident: %s\n", tokens[i]->data.lexeme);
            }
        }
    }

    for (int i = 0; i < token_count; i++) {
        free_token_data(tokens[i]);
        free(tokens[i]);
    }
    free(tokens);

    printf("======\n\n");
}

void test_number_parsing() {
    printf("####### Identifiers #######\n");
    run_test("abc");
    run_test("a123_b");
    run_test("_underscore123");

    printf("####### Integers #######\n");
    run_test("0");
    run_test("123");
    run_test("-987");

    printf("####### Floats #######\n");
    run_test("3.14");
    run_test("-0.001");

    printf("####### Hexadecimal #######\n");
    run_test("0x1F");
    run_test("0XABCDEF");

    printf("####### Binary #######\n");
    run_test("0b1010");
    run_test("0B1101");

    printf("####### Negative Numbers #######\n");
    run_test("-42");
    run_test("-0xFF");

    printf("###### Minus Unary Operator ######\n");
    run_test("-identifier");
    //run_test("-.99");  // throws exception
}

int main(int argc, char** argv){

    //test_number_parsing();

    //run_test("u8 hi := 3 -2;");
    run_test("u8 h := 3;");
    //run_test("u8 my_number := 13 >= 3 < 1 <= 3 >= 2 != 1;");
    //run_test("f64 hi := 3.14;");
    //run_test("func ADD(u8 x, u8 y) { }");
    //run_test("if (true) { }");

    return 0;
}

