#ifndef PARSING
#define PARSING
#include <vector>
#include <string>

/* Internally, a parser consists of the vector of `n`-lenghted strings called
   `lines`, s.t. `lines[i][j]` denotes the `j`th character of the `i`th line
   of the given (on construction) input file.
*/
class Parser { 
    std::vector<std::string> lines;
    size_t i, j, m;
    template<class RETTYPE>
    RETTYPE incr(size_t jj, RETTYPE retval);
public:
    Parser(std::string fname);
    std::size_t num_lines();
    std::size_t len_line();
    bool parse_char(char c);
    char parse_some_char();
    std::size_t parse_chars(char c);
    int parse_int();
    bool parse_empty_line();
    bool parse_eof();
    void print_state();
};

#endif
