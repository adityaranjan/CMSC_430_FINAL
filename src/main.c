#include <stdio.h>
#include <stdlib.h>
#include "values.h"
#include "print.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();
val_t *heap;

void error_exit()
{
  printf("err\n");
  exit(1);
}

void raise_error()
{
  return error_handler();
}

int main(int argc, char** argv)
{
  in = stdin;
  out = stdout;
  error_handler = &error_exit;

  heap = malloc(8 * heap_size);
  int64_t *from_space = heap;
  int64_t *to_space = heap + (heap_size / 2);
  int64_t h_size = (heap_size / 2);

  val_t result;

  // alloc ptr, from space, to space, heap size (n/2)
  result = entry(heap, from_space, to_space, h_size);

  print_result(result);
  if (val_typeof(result) != T_VOID)
    putchar('\n');

  free(heap);
  return 0;
}
