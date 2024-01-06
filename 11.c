#include <stdio.h>
#include <stdlib.h>
#include "parsing.h"

int main(void) {
  char (*arr)[C] = allocate_lines();
  long long int i, j, x, y, b, g, len, retval, n = get_lines(arr);
  for (len = 0; arr[0][len] != '\0'; len++) {}
  struct { int fst; int snd; } glx[len * n];
  g = 0;

  // Expand universe vertically
  for (j = 0; j < len; j++) {
    for (i = 0, b = 0; !b && i < n; i++) {
      b = (arr[i][j] == '#');
    }
    if (!b) {
      for (i = 0; i < n; i++) {
        arr[i][j] = '!';
      }
    }
  }

  /* @@ PART 1 @@ */

  // Find coordinates of galaxies
  for (i = 0, y = 0; i < n; i++, y++) {
    for (j = 0, x = 0, b = 0; j < len; j++, x++) {
      if (arr[i][j] == '!') { x++; }
      else if (arr[i][j] == '#') {
        glx[g].fst = y;
        glx[g++].snd = x;
        b = 1;
      }
    } if (!b) { y++; };
  }
  // Compute distances
  for (i = 0, retval = 0; i < g-1; i++) {
    for (j = i+1; j < g; j++) {
      y = glx[i].fst - glx[j].fst;
      x = glx[i].snd - glx[j].snd;
      retval += y < 0 ? -y : y;
      retval += x < 0 ? -x : x;
    }
  }

  printf("Part 1: %lld\n", retval);

  /* @@ PART 2 @@ */

  g = 0, retval = 0;
  // Find coordinates of galaxies
  for (i = 0, y = 0; i < n; i++, y++) {
    for (j = 0, x = 0, b = 0; j < len; j++, x++) {
      if (arr[i][j] == '!') { x+=1000000-1; }
      else if (arr[i][j] == '#') {
        glx[g].fst = y;
        glx[g++].snd = x;
        b = 1;
      }
    } if (!b) { y+=1000000-1; };
  }
  // Compute distances
  for (i = 0; i < g-1; i++) {
    for (j = i+1; j < g; j++) {
      y = glx[i].fst - glx[j].fst;
      x = glx[i].snd - glx[j].snd;
      retval += y < 0 ? -y : y;
      retval += x < 0 ? -x : x;
    }
  }

  printf("Part 2: %lld\n", retval);

  free(arr);
  return 0;
}
