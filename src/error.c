#include "error.h"
#include <stdio.h>
#include <stdlib.h>

void error_report(Parser* p, int line, int col, int len, const char *component, const char *format, ...) {
    va_list args;
    va_start(args, format);
    char* message;
    vasprintf(&message, format, args);
    va_end(args);

    parser_add_diagnostic(p, DIAG_ERROR, line, col, len, "[%s] %s", component, message);

    fprintf(stderr, "[Error]");
    if (line != -1) fprintf(stderr, "[line %d] ", line);
    fprintf(stderr, "in %s: %s", component, message);
    free(message);
    p->errors++;
}

void warning_report(Parser* p, int line, int col, int len, const char *component, const char *format, ...) {
    va_list args;
    va_start(args, format);
    char* message;
    vasprintf(&message, format, args);
    va_end(args);

    parser_add_diagnostic(p, DIAG_WARNING, line, col, len,
                        "[%s] %s", component, message);

    fprintf(stderr, "[Warning]");
    if (line != -1) fprintf(stderr, "[line %d] ", line);
    fprintf(stderr, "in %s: %s", component, message);
    free(message);
    p->warnings++;
}

void fatal_error(const char *component, const char *format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(stderr, "[Fatal Error]");
    fprintf(stderr, "in %s: ", component);
    vfprintf(stderr, format, args);
    va_end(args);

    exit(EXIT_FAILURE);
}

void assert(const char* component, bool eq, const char* reason) {
    if (eq == false) {
        fatal_error(component, "assert statement failed -- %s", reason);
    }
}
