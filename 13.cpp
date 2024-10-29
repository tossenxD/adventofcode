#include "Common/parsing.hpp"
#include <iostream>
#include <cstdint>
#include <vector>

typedef uint32_t Row;
typedef std::vector<Row> Pattern;

void print_Pattern(size_t n, Pattern in) {
    for (size_t i = 0; i < in.size(); i++) {
        for (size_t j = 0; j < n; j++) {
            printf("%d ", (in[i] >> j) & 1);
        }
        printf("\n");
    }
}

size_t transpose(size_t n, Pattern in, Pattern& out) {
    if (out.size() != n)
        throw std::runtime_error("Unfit dimensions!\n");
    size_t m = in.size();
    for (size_t i = 0; i < n; i++) {
        Row row = 0;
        for (size_t j = 0; j < m; j++)
            row |= ((in[j] >> i) & 1) << j;
        out[i] = row;
    }
    return m;
}

int count_reflections_h(Pattern in) {
    int sum = 0;
    for (size_t i = 1; i < in.size(); i++) {
        size_t ii_off = std::min(i, in.size()-i);
        bool reflects = true;
        for (size_t ii = i - ii_off, iii = 0; ii < i; ii++, iii++)
            reflects = reflects && (in[ii] == in[i + ii_off - 1 - iii]);
        if (reflects)
            sum += i;
    }
    return sum;
}

int count_reflections_v(size_t n, Pattern in) {
    Pattern t_in(n, 0);
    transpose(n, in, t_in);
    return count_reflections_h(t_in);
}

int count_reflections(size_t n, Pattern in) {
    int h = count_reflections_h(in);
    return (h) ? h * 100 : count_reflections_v(n, in);
}

int main() {
    Parser in("in/13.in");
    int sum = 0;
    while(!in.parse_eof()) {
        Pattern pattern;
        size_t n = in.len_line();
        do {
            Row row = 0;
            for (size_t i = 0; i < n; i++) {
                if (in.parse_char('#'))
                    row |= 1 << i;
                else if (!in.parse_char('.'))
                    throw std::runtime_error("unmatched symbol!\n");
            }
            pattern.push_back(row);
        } while (!in.parse_empty_line());
        sum += count_reflections(n, pattern);
    }
    std::cout << sum << std::endl;
}