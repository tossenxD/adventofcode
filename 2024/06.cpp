#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <regex>
#include <string>
#include <map>
#include <set>

enum Direction { N, E, S, W };

int main() {
    /* PART 1 */
    Parser in("in/06.in");
    Direction dir;
    std::pair<u32,u32> pos;
    u8 obs[in.num_lines()][in.len_line()]; // bit 1 is obstacles and bit 2 traversed path
    // parse input
    for (st i = 0; i < in.num_lines(); i++) {
        for (st j = 0; j < in.len_line(); j++) {
            if (in.parse_char('#'))
                obs[i][j] = 1;
            else {
                obs[i][j] = 0;
                if (!in.parse_char('.')) {
                    pos = {i, j};
                    if      (in.parse_char('^'))
                        dir = N;
                    else if (in.parse_char('<'))
                        dir = E;
                    else if (in.parse_char('v'))
                        dir = S;
                    else
                        dir = W;
                }
            }
        }
    }
    // start loop
    while (!(     pos.first == 0  && dir == N  ||  pos.second == in.len_line()-1  && dir == E
             ||   pos.second == 0 && dir == W  ||  pos.first  == in.num_lines()-1 && dir == S )) {
    }
}
