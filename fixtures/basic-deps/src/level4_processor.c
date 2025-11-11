#include "level4_processor.h"
#include "level3_parser.h"
#include "level3_formatter.h"

void processor_init(Processor* proc) {
    parse_init(&proc->parser);
    format_init(&proc->formatter);
    proc->error_count = 0;
}

int processor_run(Processor* proc, const char* input) {
    int success = parse_token(&proc->parser, input);
    if (success) {
        format_output(&proc->formatter, input);
        return 1;
    }
    proc->error_count++;
    return 0;
}

void processor_finalize(Processor* proc) {
    parse_string(&proc->parser, "done");
    format_escape(&proc->formatter, "\\n");
}
