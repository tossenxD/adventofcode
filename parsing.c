#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parsing.h"
#include <ctype.h>

/* @@ PRIVATE FUNCTIONS @@ */

void consume(int i, char *line) { memmove(line, line+i, strlen(line)); }

/* @@ EXPORTED FUNCTIONS @@ */

void *allocate_lines() {
  void *lines = malloc(sizeof(char[L][C]));
  if (lines == NULL) {
    printf("[ERROR] failed to allocate memory for parsing\n");
    exit(-1);
  } return lines;
}

int get_lines(char (*arr)[C]) {
  FILE *f = fopen("input.txt", "r");
  if (f == NULL) {
    printf("[ERROR] missing input file\n");
    free(arr);
    exit(-1);
  }
  int i = 0;
  while (fgets(arr[i], C, f)) {
    arr[i][strlen(arr[i]) - 1] = '\0'; // remove newlines
    i++;
  }
  fclose(f);
  return i; // on success, return number of lines read
}

void spaces(char *line) {
  int i = 0;
  while (isspace(line[i])) { i++; };
  consume(i, line);
}

int digit(char *line) {
  int retval = isdigit(line[0]) ? line[0] - '0' : -1;
  if (retval != -1) { consume(1, line); }
  return retval; // on success, returns digit read
}

int number(char *line) {
  int i = 0;
  int retval = 0;
  while (isdigit(line[i])) {
    retval *= 10;
    retval += line[i++] - '0';
  } if (i == 0) { retval = -1; }
  else { consume(i, line); }
  return retval; // on success, returns integer read
}

int character(char c, char *line) {
  if (line[0] == c) {
    consume(1, line);
    return 0; // on success, returns 0
  } else { return -1; }
}

int symbol(char *symb, char *line) {
  int len = strlen(symb);
  if (strncmp(symb, line, len) == 0) {
    consume(len, line);
    return 0; // on success, returns 0
  } else { return -1; }
}

int *number_arr(char *line) {
  int i = 1;
  int len = 10;
  int *arr = malloc(len * sizeof(int));
  if (arr == NULL) {
    printf("[ERROR] failed to allocate memory to hold numbers\n");
    exit(-1);
  }
  int n = number(line);
  while (n != -1) {
    arr[i] = n;
    spaces(line); // consumes tailing spaces
    n = number(line);
    if (++i == len && n != -1) {
      len *= 2;
      arr = realloc(arr, len * sizeof(int));
      if (arr == NULL) {
        printf("[ERROR] failed to allocate memory to hold numbers\n");
        exit(-1);
      }
    }
  }
  arr[0] = i; // stores length at index 0
  if (i != len) {
    arr = realloc(arr, i * sizeof(int));
    if (arr == NULL) {
      printf("[ERROR] failed to allocate memory to hold numbers\n");
      exit(-1);
    }
  }
  return arr;
}
