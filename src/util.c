#include "util.h"

void throw_error(int line, const char* component, const char* msg, ...) {
    va_list args;
    va_start(args, msg);
    printf("[line %d] Error in %s: ", line, component);
    vprintf(msg, args);
    va_end(args);
    exit(1);
}
