#ifndef PARSING
#define PARSING
#include <vector>
#include <string>
#include <regex>

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
    // Constructor
    Parser(std::string fname);

    // Internal state
    std::size_t num_lines();
    std::size_t len_line();
    bool        is_eof();
    bool        is_letter();
    void        print_state();
    void        reset_state();
    std::vector<std::string> data();

    // Parsers
    bool        parse_char(char c);
    std::size_t parse_chars(char c);
    char        parse_some_char();
    int         parse_int();
    long long   parse_i64();
    bool        parse_empty_line();

    // Functional applications
    void        apply_regex_sum(std::regex re, std::function<void(std::string)> lambda);
};

#endif
