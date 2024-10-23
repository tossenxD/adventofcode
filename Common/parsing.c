#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parsing.h"
#include <ctype.h>

/* @@ PRIVATE FUNCTIONS @@ */

void consume(int i, char *line) { memmove(line, line+i, strlen(line)); }

/* @@ EXPORTED FUNCTIONS @@ */

// [SUCCESS] ptr to uninitialized array of strings
// [ERROR] exits
void *allocate_lines() {
  void *lines = malloc(sizeof(char[L][C]));
  if (lines == NULL) {
    printf("[ERROR] failed to allocate memory for parsing\n");
    exit(-1);
  } return lines;
}

// [SUCCESS] number of lines read
// [ERROR] exits
int get_lines(char (*arr)[C]) {
  FILE *f = fopen("input.txt", "r");
  if (f == NULL) {
    printf("[ERROR] missing input file\n");
    free(arr);
    exit(-1);
  }
  int i = 0;
  while (fgets(arr[i], C, f)) {
    arr[i][strlen(arr[i]) - 1] = '\0';
    i++;
  }
  fclose(f);
  return i;
}

// consumes spaces
void spaces(char *line) {
  int i = 0;
  while (isspace(line[i])) { i++; };
  consume(i, line);
}

// [SUCCESS] digit read
// [FAILURE] -1
int digit(char *line) {
  int retval = isdigit(line[0]) ? line[0] - '0' : -1;
  if (retval != -1) { consume(1, line); }
  return retval;
}

// [SUCCESS] integer read
// [FAILURE] -0xDEADBEEF
// consumes spaces
int number(char *line) {
  int sign = line[0] == '-';
  int i = sign;
  int retval = 0;
  while (isdigit(line[i])) {
    retval *= 10;
    retval += line[i++] - '0';
  }
  if (!i || (i == 1 && sign)) {
    retval = -0xDEADBEEF;
  } else {
    if (sign) { retval = 0 - retval; }
    consume(i, line);
    spaces(line);
  }
  return retval;
}

// [SUCCESS] 0
// [FAILURE] -1
int character(char c, char *line) {
  if (line[0] == c) {
    consume(1, line);
    return 0;
  } else { return -1; }
}

// [SUCCESS] 0
// [FAILURE] -1
// consumes spaces
int symbol(char *symb, char *line) {
  int len = strlen(symb);
  if (strncmp(symb, line, len) == 0) {
    consume(len, line);
    spaces(line);
    return 0;
  } else { return -1; }
}

// [SUCCESS] pointer to array of integers read - length stored in index 0
// [FAILURE] NULL ptr - should not be freed
// [ERROR] exits
// consumes spaces
int *number_arr(char *line) {
  int n = number(line);
  if (n == -0xDEADBEEF) {
    return NULL;
  }
  int i = 1;
  int len = 10;
  int *arr = malloc(len * sizeof(int));
  if (arr == NULL) {
    printf("[ERROR] failed to allocate memory to hold numbers\n");
    exit(-1);
  }
  while (n != -0xDEADBEEF) {
    arr[i] = n;
    spaces(line);
    n = number(line);
    if (++i == len && n != -0xDEADBEEF) {
      len *= 2;
      arr = realloc(arr, len * sizeof(int));
      if (arr == NULL) {
        printf("[ERROR] failed to allocate memory to hold numbers\n");
        exit(-1);
      }
    }
  }
  arr[0] = i;
  if (i != len) {
    arr = realloc(arr, i * sizeof(int));
    if (arr == NULL) {
      printf("[ERROR] failed to allocate memory to hold numbers\n");
      exit(-1);
    }
  }
  return arr;
}
