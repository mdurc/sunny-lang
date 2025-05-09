#include "util.h"

void throw_warning(int line, const char* component, const char* msg, ...) {
    va_list args;
    va_start(args, msg);
    if (line == -1) {
        printf("Warning in %s: ", component);
    } else {
        printf("[line %d] Warning in %s: ", line, component);
    }
    vprintf(msg, args);
    va_end(args);
}

void throw_fatal_error(int line, const char* component, const char* msg, ...) {
    va_list args;
    va_start(args, msg);
    if (line == -1) {
        fprintf(stderr, "[FATAL] %s: ", component);
    } else {
        fprintf(stderr, "[FATAL][line %d] %s: ", line, component);
    }
    vfprintf(stderr, msg, args);
    va_end(args);
    exit(EXIT_FAILURE);
}
