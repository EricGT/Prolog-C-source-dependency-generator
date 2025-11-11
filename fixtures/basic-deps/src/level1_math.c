#include "level1_math.h"

int min(int a, int b) {
    return (a < b) ? a : b;
}

int max(int a, int b) {
    return (a > b) ? a : b;
}

int abs_val(int x) {
    return (x < 0) ? -x : x;
}
