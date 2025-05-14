#ifndef ERROR_H
#define ERROR_H

#include "parser.h"
#include <stdarg.h>

void error_report(Parser* p, int line, int col, int len, const char *component, const char *format, ...);
void warning_report(Parser* p, int line, int col, int len, const char *component, const char *format, ...);
void fatal_error(const char *component, const char *format, ...);
void assert(const char* component, bool eq, const char* reason);

#endif
