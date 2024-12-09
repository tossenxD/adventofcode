#include "parsing.hpp"
#include <string>
#include <iostream>
#include <vector>
#include <fstream>

using std::string;
using std::size_t;

/* Parser constructor. */
Parser::Parser(string fname) : lines(100), i(0), j(0) {
    // Open input file
    std::ifstream file(fname);
    if (!file.is_open())
        throw std::invalid_argument("input file does not exist!\n");

    // Read input file into (dynamically located) vector
    m = 0;
    string line;
    while (!std::getline(file, line).eof()) {
        size_t s = lines.size();
        if (m >= s) { lines.resize(s*2); }
        lines[m++] = line;
    }
    file.close();

    // Sanitize vector size based on number of lines in input file
    if (m == 0)
        throw std::invalid_argument("input file is empty!\n");
    lines.resize(m);
}

/* Private function that increments the parser by `jj` at row-level. */
template<class RETTYPE>
RETTYPE Parser::incr(size_t jj, RETTYPE retval) {
    if (i >= m) { throw std::runtime_error("expected input to be longer!\n"); }
    j += jj;
    if (j >= lines[i].length()) { j = 0; i++; }
    return retval;
}

/* Public parser functions. */
size_t Parser::num_lines() { return m;                 }
size_t Parser::len_line()  { return lines[i].length(); }

bool Parser::parse_char(char c) {
    return lines[i][j] == c ? incr<bool>(1, true) : false;
}

char Parser::parse_some_char() {
    char retval = lines[i][j];
    return incr<char>(1, retval);
}

size_t Parser::parse_chars(char c) {
    size_t retval = 0;
    while (j <  lines[i].length() && lines[i][j] == c) { j++; retval++; }
    if    (j == lines[i].length())                     { i++; j = 0;    }
    return retval;
}

int Parser::parse_int() {
    if (lines[i][j] < '1' || lines[i][j] > '9') {
        return 0xDEADBEEF;
    } else {
        const string l = lines[i].substr(j);
        size_t ii;
        int b = std::stoi(l, &ii);
        return incr<int>(ii, b);
    }
}

bool Parser::parse_empty_line() {
    if (i>= m || len_line() == 0) {
        i++;
        return true;
    } else
        return false;
}

bool Parser::is_eof() {
    return i >= m;
}

bool Parser::is_letter() {
    char c = lines[i][j];
    return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

void Parser::print_state() {
    std::cout << "parser state:\n\ti\t: " << i << "\n\tj\t: "
              << j << "\n\tm\t: " << m << "\n\tinput\t:\n";
    for (string & line : lines)
        std::cout << line << "\n";
}

void Parser::reset_state() {
    i = 0; j = 0;
}

std::vector<string> Parser::data() {
    return lines;
}

void Parser::apply_regex_sum(std::regex re, std::function<void(std::string)> lambda) {
    string substr = lines[i].substr(j);
    std::smatch match;
    while (regex_search(substr, match, re)) {
        lambda(match.str());
        substr = match.suffix().str();
    }
    j = 0; i++;
}
