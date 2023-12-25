#include <stdio.h>
#include <stdlib.h>
#include "parsing.h"
#include <string.h>

int main(void) {
  char (*arr)[C] = allocate_lines();
  int n = get_lines(arr);

  int *num_arr = number_arr(arr[0]);

  for (int i = 1; i < num_arr[0]; i++) {
    printf ("%d ", num_arr[i]);
  } printf ("\n");

  free(num_arr);

  for (int i = 0; i < n; ++i) {
    printf ("%s\n", arr[i]);
  }

  free(arr);
  return 0;
}
