#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parsing.h"

int main(void) {
  char (*arr)[C] = allocate_lines();
  int i, j, len, tmp, retval = 0, n = get_lines(arr);

  /* @@ PART 1 @@ */

  for (i = 0; i < n; i++) {
    // parse array of integers
    int *num_arr = number_arr(arr[i]);
    if (num_arr == NULL) { continue; }
    len = num_arr[0];
    // downwards propagate
    for (;;) {
      tmp = 0;
      for (j = 1; j < len; j++) {
        tmp += num_arr[j];
      }
      if (!tmp) { break; }
      len--;
      for (j = 1; j < len; j++) {
        num_arr[j] = num_arr[j+1] - num_arr[j];
      }
    }
    // forward propagate
    tmp = num_arr[0];
    while (j < tmp) {
      retval += num_arr[j++];
    }
    free(num_arr);
  }
  printf("part 1: %d\n", retval);

  /* @@ PART 2 @@ */

  free(arr);
  return 0;
}
