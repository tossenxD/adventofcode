#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdint>

std::int32_t count(std::int32_t y, std::vector<std::int32_t> v) {
    std::int32_t c = 0;
    for (auto x : v)
        if (x == y)
            c++;
    return c;
}

int main() {
    /* PART 1 */
    Parser in("in/01.in");
    std::size_t n = in.num_lines();
    std::vector<std::int32_t> ll(n);
    std::vector<std::int32_t> rl(n);

    for(std::size_t idx = 0; idx < n; idx++) {
        ll[idx] = in.parse_int();
        in.parse_chars(' ');
        rl[idx] = in.parse_int();
    }
    std::sort(ll.begin(), ll.end());
    std::sort(rl.begin(), rl.end());

    std::int32_t total_distance = 0;
    for(std::size_t idx = 0; idx < n; idx++)
        total_distance += DISTANCE(ll[idx], rl[idx]);

    std::cout << "part1: " << total_distance << "\n";

    /* PART 2 */
    std::int32_t similarity_score = 0;
    std::int32_t id_scope = ll[0];
    std::int32_t id_count = count(id_scope, rl);

    for (auto id : ll) {
        if (id != id_scope) {
            id_scope = id;
            id_count = count(id_scope, rl);
        }
        similarity_score += id_scope * id_count;
    }

    std::cout << "part2: " << similarity_score << "\n";
}
