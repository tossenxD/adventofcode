CC = gcc
CFLAGS = -Wall -std=c11 -g -ggdb

HDAYS = 1 2 3 4 5 6 7 8
CDAYS = 9 10

.PHONY: $(HDAYS) $(CDAYS)

all: $(HDAYS) $(CDAYS)

$(HDAYS):
	stack ghc -- $@.hs

$(CDAYS):
	$(CC) $(CFLAGS) $@.c parsing.c -o $@

clean:
	rm -rf *.o *.hi *.out $(HDAYS) $(CDAYS)
