#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <vector>

int main() {
    /* PARSING */
    Parser in("in/09.in");      
    Position bounds(in.num_lines(), in.len_line());
    std::vector<std::vector<u8>> map(bounds.first, std::vector<u8>(bounds.second));
    for (auto &row : map)
        for (auto &point : row)
            point = in.parse_digit();

    /* PART 1 */
    std::cout << "part1: " << std::endl;

    /* PART 2 */
    std::cout << "part2: " << std::endl;
}
