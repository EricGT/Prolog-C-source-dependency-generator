#ifndef LEVEL2_BUFFER_H
#define LEVEL2_BUFFER_H

typedef struct {
    char* data;
    int size;
    int capacity;
} Buffer;

void buffer_init(Buffer* buf, int initial_capacity);
void buffer_resize(Buffer* buf, int new_capacity);

#endif
