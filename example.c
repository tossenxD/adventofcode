#include <stdio.h>
#include <stdlib.h>
#include "parsing.h"
#include <string.h>

int main(void) {
  char (*arr)[C] = allocate_lines();
  int n = parse(arr);

  for (int i = 0; i < n; ++i) {
    printf ("%s\n", arr[i]);
  }

  free(arr);
  return 0;
}
