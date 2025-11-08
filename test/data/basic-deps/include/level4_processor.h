#ifndef LEVEL4_PROCESSOR_H
#define LEVEL4_PROCESSOR_H

#include "level3_parser.h"
#include "level3_formatter.h"

typedef struct {
    Parser parser;
    Formatter formatter;
    int error_count;
} Processor;

void processor_init(Processor* proc);
int processor_run(Processor* proc, const char* input);
void processor_finalize(Processor* proc);

#endif
