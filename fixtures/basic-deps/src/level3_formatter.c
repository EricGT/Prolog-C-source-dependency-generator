#include "level3_formatter.h"
#include "level2_buffer.h"
#include "level2_validator.h"
#include "level1_string.h"

void format_init(Formatter* f) {
    buffer_init(&f->output_buf, 128);
    f->indent_level = 0;
}

void format_output(Formatter* f, const char* text) {
    if (validate_length(text, 512)) {
        buffer_resize(&f->output_buf, 256);
        f->indent_level++;
    }
}

void format_escape(Formatter* f, const char* special) {
    if (validate_content(special, "\\n") || str_equal(special, "\\t")) {
        f->indent_level = 0;
    }
}
