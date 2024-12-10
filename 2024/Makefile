CPPDAYS = 01 02 03 04 05 06

WHOLEMONTH = $(CPPDAYS)

.PHONY: $(WHOLEMONTH)

all: $(WHOLEMONTH)

$(CPPDAYS):
	g++ -Wall -std=c++20 -g -ggdb $@.cpp Common/parsing.cpp -o $@

clean:
	find -type f -name "*.o" -delete
	find -type f -name "*.hi" -delete
	find -type f -name "*~" -delete
	rm -f $(WHOLEMONTH)
