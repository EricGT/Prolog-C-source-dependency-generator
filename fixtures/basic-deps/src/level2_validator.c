#include "level2_validator.h"
#include "level1_string.h"
#include "level1_math.h"

int validate_length(const char* str, int max_len) {
    int len = str_len(str);
    int clamped = min(len, max_len);
    return (len == clamped);
}

int validate_content(const char* str, const char* expected) {
    int matches = str_equal(str, expected);
    int len = str_len(str);
    return matches && (len > 0);
}
