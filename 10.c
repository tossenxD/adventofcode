#include <stdio.h>
#include <stdlib.h>
#include "parsing.h"

enum { N, E, S, W };

int main(void) {
  char (*arr)[C] = allocate_lines();
  int i, j, x, y, d, p, len, retval, tmp, n = get_lines(arr);
  for (len = 0; arr[0][len] != '\0'; len++) {}
  int mps[n][len];
  for (i = 0; i < n; i++) {
    for (j = 0; j < len; j++) {
      mps[i][j] = -1;
    }
  }

  /* @@ PART 1 @@ */
  for (i = 0; i < n; i++) {
    for (j = 0; j < len; j++) {
      if (arr[i][j] != 'S') { continue; }
      // Find initial pipe
      if (i < n-1 && (arr[i+1][j] == '|' || arr[i+1][j] == 'L' ||
                      arr[i+1][j] == 'J')) {
        d = S;
      } else if (j < len-1 && (arr[i][j+1] == '-' || arr[i][j+1] == '7' ||
                               arr[i][j+1] == 'J')) {
        d = E;
      } else {
        d = N;
      }
      // Iterate pipe-loop
      x = i, y = j, retval = 0;
      do { retval++;
        mps[x][y] = d; /* used for part2 */
        if (d == N) {
          d = arr[--x][y] == '|' ? N : arr[x][y] == '7' ? W : E;
        } else if (d == E) {
          d = arr[x][++y] == '-' ? E : arr[x][y] == '7' ? S : N;
        } else if (d == S) {
          d = arr[++x][y] == '|' ? S : arr[x][y] == 'L' ? E : W;
        } else {
          d = arr[x][--y] == '-' ? W : arr[x][y] == 'L' ? N : S;
        }
      } while (x != i || y != j);
    }
  }
  printf("Part 1: %d\n", retval / 2);

  /* @@ PART 2 @@ */
  // find a pivot pipe - store in i and j
  for (x = 0, i = 0; i < n; i++) {
    for (j = 0; j < len; j++) {
      if (mps[i][j] > -1) { x++; break; }
    }
    if (x) { break; }
  }
  // p is # of pipes in the pipe-loop
  p = retval, d = mps[i][j], retval = 0, tmp = 0, n = i, len = j;
  // find area using shoelace formula - store in retval
  for (;;) {
    // find next point - store in x and y
    if (d == W) {
      for (x = i, y = j-1; mps[x][y] == d; y--) { }
    } else if (d == E) {
      for (x = i, y = j+1; mps[x][y] == d; y++) { }
    } else if (d == N) {
      for (x = i-1, y = j; mps[x][y] == d; x--) { }
    } else if (d == S) {
      for (x = i+1, y = j; mps[x][y] == d; x++) { }
    }
    // compute determinant sum (use tmp since we don't know loop-orientation)
    retval += (i * y) - (j * x);
    tmp -= (i * y) - (j * x);
    // find new direction or terminate loop
    if (x == n && y == len) {
      break;
    } else {
      i = x, j = y, d = mps[i][j];
    }
  } retval = (retval < 0 ? tmp : retval) / 2;
  // Use Pick's theorem to compute interior points
  retval -= (p / 2) - 1;
  printf("Part 2: %d\n", retval);

  // pretty printer for debugging
  /*
  for (i = 0; i < n; i++) {
    for (j = 0; j < len; j++) {
      char c = mps[i][j] == -1 ? '.' : mps[i][j] == W ? 'W' :
               mps[i][j] == S ? 'S' : mps[i][j] == N ? 'N' : 'E';
      printf("%c", c);
    } printf("\n");
  }
  */

  free(arr);
  return 0;
}
