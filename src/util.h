#ifndef UTIL_H
#define UTIL_H

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

void throw_warning(int line, const char* component, const char* msg, ...);
void throw_fatal_error(int line, const char* component, const char* msg, ...);

#endif
