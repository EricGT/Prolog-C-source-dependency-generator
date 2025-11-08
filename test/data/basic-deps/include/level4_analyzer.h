#ifndef LEVEL4_ANALYZER_H
#define LEVEL4_ANALYZER_H

#include "level3_parser.h"
#include "level3_formatter.h"

typedef struct {
    Parser parser;
    Formatter formatter;
    int stats[10];
} Analyzer;

void analyzer_setup(Analyzer* a);
void analyzer_check(Analyzer* a, const char* data);
void analyzer_report(Analyzer* a);

#endif
