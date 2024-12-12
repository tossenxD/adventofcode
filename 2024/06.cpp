#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <vector>

#define TURN(dir) ((dir == N) ? E : (dir == E) ? S : (dir == S) ? W : N)

#define ENCODE_DIR(dir) ((dir == N) ? 4 : (dir == E) ? 8 : (dir == S) ? 16 : 32)

#define ADVANCE(pos, dir) ((dir == N) ? std::make_pair( pos.first-1u, pos.second    ) : \
                           (dir == E) ? std::make_pair( pos.first,    pos.second+1u ) : \
                           (dir == S) ? std::make_pair( pos.first+1u, pos.second    ) : \
                                        std::make_pair( pos.first,    pos.second-1u ) )

#define IS_ABOUT_TO_EXIT(pos, n, m, dir) (                             \
     (pos.first == 0  && dir == N) || (pos.second == m-1 && dir == E)  \
  || (pos.second == 0 && dir == W) || (pos.first  == n-1 && dir == S))

bool detect_cycle(std::vector<std::vector<u8>> grid, st n, st m, Position pos, Direction dir) {
    while (!IS_ABOUT_TO_EXIT(pos, n, m, dir)) {
        // Mark current position and direction, and compute next position
        grid[pos.first][pos.second] |= ENCODE_DIR(dir);
        Position npos = ADVANCE(pos, dir);
	if ((grid[npos.first][npos.second] & 1)) // obstacle, so turn 90 degrees to the right
            dir = TURN(dir);
        else {
            // Check for cycle
            if ((grid[npos.first][npos.second] & ENCODE_DIR(dir)))
                return true;
            // Take a step
            pos = npos;
        }
    }
    return false;
}

int main() {
    /* PARSING */
    Parser in("in/06.in");
    Direction dir{};
    Position pos{};
    st n = in.num_lines();
    st m = in.len_line();
    // bit 1 is obstacles and bit 2 traversed path
    std::vector<std::vector<u8>> grid(n, std::vector<u8>(m));
    // parse input
    for (u32 i = 0; i < n; i++) {
        for (u32 j = 0; j < m; j++) {
            if (in.parse_char('#'))
                grid[i][j] = 1;
            else if (!in.parse_char('.')) {
                pos = std::make_pair(i, j);
                if      (in.parse_char('^'))
                    dir = N;
                else if (in.parse_char('<'))
                    dir = E;
                else if (in.parse_char('v'))
                    dir = S;
                else if (in.parse_char('>'))
                    dir = W;
            }
        }
    }
    // save position and direction for part 2
    Position  start_pos = pos;
    Direction start_dir = dir;

    /* PART 1 */
    // begin traversal loop
    while (!IS_ABOUT_TO_EXIT(pos, n, m, dir)) {
        Position npos = ADVANCE(pos, dir);
	if (grid[npos.first][npos.second] & 1) // obstacle, so we turn 90 degrees to the right
            dir = TURN(dir);
        else {
            pos = npos;
            grid[npos.first][npos.second] |= 2;
        }
    }
    // count number of traversed spaces in grid
    u32 sum = 0;
    for (st i = 0; i < n; i++)
        for (st j = 0; j < m; j++)
            if (grid[i][j] & 2)
                sum++;
    std::cout << "part1: " << sum << std::endl;

    /* PART 2 */
    sum = 0;
    pos = start_pos;
    dir = start_dir;
    // The idea is to utilize the byte-encoding of the position grid s.t. bit 3, 4, 5, and 6
    // marks an earlier traversal of that position by the guard while facing direction N, E, S,
    // and W, respectively. The algorithm is to follow the traveral from part 1, except we place an
    // obstacle in front of the guard before each step. We then run a cycle-detection algorithm on
    // the new grid s.t. if the guard ever end up in an earlier position while facing the some
    // direction, he is stuck in a cycle and we mark that grid as a possibility (sum++).
    // An obstacle is only placed if it is the first time the guard is about to arrive at a given
    // point, which is checked using bit 7.
    while (!IS_ABOUT_TO_EXIT(pos, n, m, dir)) {
        // mark current position and direction
        grid[pos.first][pos.second] |= ENCODE_DIR(dir);
        // find new position
        Position npos = ADVANCE(pos, dir);
        if (grid[npos.first][npos.second] == 1) // obstacle, so turn 90 degrees to the right
            dir = TURN(dir);
        else {
            // check if it next position has already been stepped on earlier
            if (!(grid[npos.first][npos.second] & 64)) {
                // place an obstacle, mark point, and run cycle detection procedure
                grid[npos.first][npos.second] |= 65;
                if (detect_cycle(grid, n, m, pos, dir))
                    sum++;
                // remove opstacle
                grid[npos.first][npos.second] &= 254;
            }
            // take a step
            pos = npos;
        }
    }

    std::cout << "part2: " << sum << std::endl;
}
