HDAYS = 01 02 03 04 05 06 07 08
CDAYS = 09 10 11
CPPDAYS = 12

WHOLEMONTH = $(HDAYS) $(CDAYS) $(CPPDAYS)

.PHONY: $(WHOLEMONTH)

all: $(WHOLEMONTH)

$(HDAYS):
	stack ghc -- $@.hs

$(CDAYS):
	gcc -Wall -std=c11 -g -ggdb $@.c Common/parsing.c -o $@

$(CPPDAYS):
	g++ -Wall -g -ggdb $@.cpp Common/parsing.cpp -o $@

clean:
	find -type f -name "*.o" -delete
	find -type f -name "*.hi" -delete
	find -type f -name "*~" -delete
	rm -f $(WHOLEMONTH)
