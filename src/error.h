#ifndef ERROR_H
#define ERROR_H

#include <stdarg.h>

void error_report(int line, const char *component, const char *format, ...);
void warning_report(int line, const char *component, const char *format, ...);
void fatal_error(int line, const char *component, const char *format, ...);

#endif
