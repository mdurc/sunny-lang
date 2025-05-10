#include "error.h"
#include <stdio.h>
#include <stdlib.h>

void error_report(int line, const char *component, const char *format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(stderr, "[Error]");
    if (line != -1) {
        fprintf(stderr, "[line %d] ", line);
    }
    fprintf(stderr, "in %s: ", component);
    vfprintf(stderr, format, args);
    va_end(args);
}

void warning_report(int line, const char *component, const char *format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(stderr, "[Warning]");
    if (line != -1) {
        fprintf(stderr, "[line %d] ", line);
    }
    fprintf(stderr, "in %s: ", component);
    vfprintf(stderr, format, args);
    va_end(args);
}

void fatal_error(int line, const char *component, const char *format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(stderr, "[Fatal Error]");
    if (line != -1) {
        fprintf(stderr, "[line %d] ", line);
    }
    fprintf(stderr, "in %s: ", component);
    vfprintf(stderr, format, args);
    va_end(args);

    exit(EXIT_FAILURE);
}
