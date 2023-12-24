#ifndef PARSING
#define PARSING

#define L 200 // number of lines
#define C 200 // number characters per line

void *allocate_lines(void);
int parse(char (*)[C]);
int digit(char *);
int digits(char *);
void spaces(char *);
int symbol(char *, char *);

#endif
