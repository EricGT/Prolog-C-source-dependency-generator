#include "level2_buffer.h"
#include "level1_math.h"
#include <stdlib.h>

void buffer_init(Buffer* buf, int initial_capacity) {
    int safe_capacity = max(initial_capacity, 16);
    buf->data = (char*)malloc(safe_capacity);
    buf->size = 0;
    buf->capacity = safe_capacity;
}

void buffer_resize(Buffer* buf, int new_capacity) {
    int size_diff = abs_val(new_capacity - buf->capacity);
    int safe_capacity = min(new_capacity, 65536);

    if (size_diff > 0) {
        buf->data = (char*)realloc(buf->data, safe_capacity);
        buf->capacity = safe_capacity;
    }
}
