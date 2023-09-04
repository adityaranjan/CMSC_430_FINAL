#ifndef RUNTIME_H
#define RUNTIME_H
int64_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

// in words
// #define heap_size 10000

// NOTE: actually the double of active heap size (this includes both from-space and to-space)!
#define heap_size 2

extern int64_t *heap;
#endif /* RUNTIME_H */
