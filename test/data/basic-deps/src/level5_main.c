#include "level4_processor.h"
#include "level4_analyzer.h"
#include "level3_formatter.h"
#include "level2_validator.h"

static Processor g_processor;
static Analyzer g_analyzer;

void initialize(void) {
    processor_init(&g_processor);
    analyzer_setup(&g_analyzer);
}

void process(const char* data) {
    processor_run(&g_processor, data);
    analyzer_check(&g_analyzer, data);
}

void cleanup(void) {
    processor_finalize(&g_processor);
    analyzer_report(&g_analyzer);
}

void error_handler(const char* msg) {
    Formatter f;
    format_init(&f);
    format_escape(&f, msg);
    validate_content(msg, "error");
}

int main(int argc, char** argv) {
    initialize();

    if (argc > 1) {
        process(argv[1]);
    }

    cleanup();
    return 0;
}
