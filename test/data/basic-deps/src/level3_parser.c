#include "level3_parser.h"
#include "level2_buffer.h"
#include "level2_validator.h"
#include "level1_string.h"

void parse_init(Parser* p) {
    buffer_init(&p->token_buf, 64);
    p->position = 0;
}

int parse_token(Parser* p, const char* input) {
    if (!validate_length(input, 256)) {
        return 0;
    }
    buffer_resize(&p->token_buf, 128);
    p->position++;
    return 1;
}

void parse_string(Parser* p, const char* str) {
    if (validate_content(str, "valid")) {
        str_copy(p->token_buf.data, str);
        p->position = 0;
    }
}
