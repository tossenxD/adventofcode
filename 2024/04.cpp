#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <vector>
#include <string>

int main() {
    /* PART 1 */
    Parser in("in/04.in");
    u64 sum = 0;
    const std::vector<std::string> lines = in.data();
    for (st i = 0; i < in.num_lines(); i++) {
        for (st j = 0; j < in.len_line(); j++) {
            if (lines[i][j] == 'X') {
                if (j >= 3 &&
                    lines[i][j-1] == 'M' && lines[i][j-2] == 'A' && lines[i][j-3] == 'S')
                    sum++;
                if (j >= 3 && i >= 3 &&
                    lines[i-1][j-1] == 'M' && lines[i-2][j-2] == 'A' && lines[i-3][j-3] == 'S')
                    sum++;
                if (i >= 3 &&
                    lines[i-1][j] == 'M' && lines[i-2][j] == 'A' && lines[i-3][j] == 'S')
                    sum++;
                if (j < in.len_line()-3 && i >= 3 &&
                    lines[i-1][j+1] == 'M' && lines[i-2][j+2] == 'A' && lines[i-3][j+3] == 'S')
                    sum++;
                if (j < in.len_line()-3 &&
                    lines[i][j+1] == 'M' && lines[i][j+2] == 'A' && lines[i][j+3] == 'S')
                    sum++;
                if (j < in.len_line()-3 && i < in.num_lines()-3 &&
                    lines[i+1][j+1] == 'M' && lines[i+2][j+2] == 'A' && lines[i+3][j+3] == 'S')
                    sum++;
                if (i < in.num_lines()-3 &&
                    lines[i+1][j] == 'M' && lines[i+2][j] == 'A' && lines[i+3][j] == 'S')
                    sum++;
                if (j >= 3 && i < in.num_lines()-3 &&
                    lines[i+1][j-1] == 'M' && lines[i+2][j-2] == 'A' && lines[i+3][j-3] == 'S')
                    sum++;
            }
        }
    }
    std::cout << "part1: " << sum << std::endl;

    /* PART 2 */
    sum = 0;
    for (st i = 1; i < in.num_lines()-1; i++) {
        for (st j = 1; j < in.len_line()-1; j++) {
            if (lines[i][j] == 'A') {
                if ( ( (lines[i-1][j-1] == 'M' && lines[i+1][j+1] == 'S') ||
                       (lines[i-1][j-1] == 'S' && lines[i+1][j+1] == 'M')
                     ) &&
                     ( (lines[i+1][j-1] == 'M' && lines[i-1][j+1] == 'S') ||
                       (lines[i+1][j-1] == 'S' && lines[i-1][j+1] == 'M')
                     ) )
                    sum++;
            }
        }
    }
    std::cout << "part2: " << sum << std::endl;
}
