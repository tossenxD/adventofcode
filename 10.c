#include <stdio.h>
#include <stdlib.h>
#include "parsing.h"

enum {N, E, S, W};

int main(void) {
  char (*arr)[C] = allocate_lines();
  int i, j, x, y, d, len, retval = 0, n = get_lines(arr);
  for (len = 0; arr[0][len] != '\0'; len++);

  /* @@ PART 1 @@ */
  for (i = 0; i < n; i++) {
    for (j = 0; j < len; j++) {
      if (arr[i][j] != 'S') { continue; }
      x = i, y = j;
      // Find initial pipe
      if (j!=len&&(arr[i][j+1]=='-'||arr[i+1][j]=='J'||arr[i+1][j]=='7')) {
        d = E;
      }
      else if (j!=0&&(arr[i][j-1]=='-'||arr[i-1][j]=='L'||arr[j-1][j]=='F')) {
        d = W;
      }
      else if (i!=0&&(arr[i-1][j]=='|'||arr[i-1][j]=='7'||arr[i-1][j]=='F')) {
        d = N;
      } else {
        d = S;
      }
      // Iterate pipe-loop
      for (;;) {
        if (d == N) {
          d = (arr[--x][y] == '|') ? N :
              (arr[x][y] == '7') ? W : E;
        }
        else if (d == E) {
          d = (arr[x][++y] == '-') ? E :
              (arr[x][y] == '7') ? S : N;
        }
        else if (d == S) {
          d = (arr[++x][y] == '|') ? S :
              (arr[x][y] == 'L') ? E : W;
        }
        else {
          d = (arr[x][--y] == '-') ? W :
              (arr[x][y] == 'L') ? N : S;
        }
        retval++;
        if (x == i && y == j) { break; }
      }
    }
  }
  printf("Part 1: %d\n", retval / 2);

  free(arr);
  return 0;
}
