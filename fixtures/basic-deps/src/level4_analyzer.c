#include "level4_analyzer.h"
#include "level3_parser.h"
#include "level3_formatter.h"

void analyzer_setup(Analyzer* a) {
    parse_init(&a->parser);
    format_init(&a->formatter);
    for (int i = 0; i < 10; i++) {
        a->stats[i] = 0;
    }
}

void analyzer_check(Analyzer* a, const char* data) {
    if (parse_token(&a->parser, data)) {
        a->stats[0]++;
    }
    parse_string(&a->parser, data);
}

void analyzer_report(Analyzer* a) {
    format_output(&a->formatter, "report");
    format_escape(&a->formatter, "\\t");
}
