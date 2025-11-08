#ifndef LEVEL3_FORMATTER_H
#define LEVEL3_FORMATTER_H

#include "level2_buffer.h"

typedef struct {
    Buffer output_buf;
    int indent_level;
} Formatter;

void format_init(Formatter* f);
void format_output(Formatter* f, const char* text);
void format_escape(Formatter* f, const char* special);

#endif
