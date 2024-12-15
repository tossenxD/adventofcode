#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <vector>
#include <set>

void compute_trailhead_score(const std::vector<std::vector<u8>> map,
                             const st i, const st j, std::set<Position> &set) {
    if (map[i][j] == 9u) {
        set.insert(std::make_pair(i,j));
    }
    else {
        const u8 perfel = map[i][j]+1;
        if (i-1 < map.size() && map[i-1][j] == perfel)     // north
            compute_trailhead_score(map, i-1, j, set);
        if (j-1 < map[i].size() && map[i][j-1] == perfel)  // west
            compute_trailhead_score(map, i, j-1, set);
        if (j+1 < map[i].size() && map[i][j+1] == perfel)  // east
            compute_trailhead_score(map, i, j+1, set);
        if (i+1 < map.size() && map[i+1][j] == perfel)     // south
            compute_trailhead_score(map, i+1, j, set);
    }
}

u32 compute_trailhead_rating(const std::vector<std::vector<u8>> map, const st i, const st j) {
    if (map[i][j] == 9u)
        return 1u;
    else {
        const u8 perfel = map[i][j]+1;
        u32 retval = 0u;
        if (i-1 < map.size() && map[i-1][j] == perfel)     // north
            retval += compute_trailhead_rating(map, i-1, j);
        if (j-1 < map[i].size() && map[i][j-1] == perfel)  // west
            retval += compute_trailhead_rating(map, i, j-1);
        if (j+1 < map[i].size() && map[i][j+1] == perfel)  // east
            retval += compute_trailhead_rating(map, i, j+1);
        if (i+1 < map.size() && map[i+1][j] == perfel)     // south
            retval += compute_trailhead_rating(map, i+1, j);
        return retval;
    }
}

int main() {
    /* PARSING */
    Parser in("in/10.in");      
    Position bounds(in.num_lines(), in.len_line());
    std::vector<std::vector<u8>> map(bounds.first, std::vector<u8>(bounds.second));
    for (auto &row : map)
        for (auto &point : row)
            point = in.parse_digit();

    /* PART 1 */
    st sum = 0;
    for (st i = 0; i < bounds.first; i++)
        for (st j = 0; j < bounds.second; j++)
            if (map[i][j] == 0) {
                std::set<Position> set;
                compute_trailhead_score(map, i, j, set);
                sum += set.size();
            }
    std::cout << "part1: " << sum << std::endl;

    /* PART 2 */
    sum = 0;
    for (st i = 0; i < bounds.first; i++)
        for (st j = 0; j < bounds.second; j++)
            if (map[i][j] == 0)
                sum += compute_trailhead_rating(map, i, j);
    std::cout << "part2: " << sum << std::endl;
}
