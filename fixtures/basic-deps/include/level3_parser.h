#ifndef LEVEL3_PARSER_H
#define LEVEL3_PARSER_H

#include "level2_buffer.h"

typedef struct {
    Buffer token_buf;
    int position;
} Parser;

void parse_init(Parser* p);
int parse_token(Parser* p, const char* input);
void parse_string(Parser* p, const char* str);

#endif
