#ifndef PARSING
#define PARSING

#define L 200 // number of lines
#define C 200 // number characters per line

void *allocate_lines(void);
int get_lines(char (*)[C]);
void spaces(char *);
int digit(char *);
int number(char *);
int character(char, char *);
int symbol(char *, char *);
int *number_arr(char *);

#endif
