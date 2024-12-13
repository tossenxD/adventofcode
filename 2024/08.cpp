#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <vector>
#include <set>

struct Antenna {
    char frequency;
    Position position;
    Antenna(char f, u32 x, u32 y) : frequency(f), position(std::make_pair(x,y)) {};
};

int main() {
    /* PARSING */
    Parser in("in/08.in");
    std::vector<Antenna> ants{};
    Position bound = std::make_pair(in.num_lines(), in.len_line());
    for (st i = 0; i < bound.first; i++)
        for (st j = 0; j < bound.second; j++)
            if (!in.parse_char('.'))
                ants.push_back(Antenna(in.parse_some_char(), i, j));

    /* PART 1 */
    std::set<Position> antis{}; // set of antinodes
    for (st i = 0; i < ants.size() - 1; i++) {
        for (st j = i + 1; j < ants.size(); j++) { // check all unique pairs of antennas
            Antenna a0 = ants[i];
            Antenna a1 = ants[j];
            if (a0.frequency == a1.frequency) { // compute antinodes for identical frequencies
                Position diffs = std::make_pair(a1.position.first  - a0.position.first,
                                                a1.position.second - a0.position.second);
                Position p0 = std::make_pair(a0.position.first  - diffs.first,
                                             a0.position.second - diffs.second);
                Position p1 = std::make_pair(a1.position.first  + diffs.first,
                                             a1.position.second + diffs.second);
                if (p0.first < bound.first && p0.second < bound.second) // unsigned bound check
                    antis.insert(p0);
                if (p1.first < bound.first && p1.second < bound.second)
                    antis.insert(p1);
            }
        }
    }
    std::cout << "part1: " << antis.size() << std::endl;

    /* PART 2 */                         // close to part 1, except we compute antinodes in loop
    for (st i = 0; i < ants.size() - 1; i++) {
        for (st j = i + 1; j < ants.size(); j++) { // check all unique pairs of antennas
            Antenna a0 = ants[i];
            Antenna a1 = ants[j];
            if (a0.frequency == a1.frequency) { // compute antinodes for identical frequencies
                Position diffs = std::make_pair(a1.position.first  - a0.position.first,
                                                a1.position.second - a0.position.second);
                for (Position p0 = a0.position;
                     p0.first < bound.first && p0.second < bound.second;
                     p0 = std::make_pair(p0.first - diffs.first, p0.second - diffs.second))
                    antis.insert(p0);
                for (Position p1 = a1.position;
                     p1.first < bound.first && p1.second < bound.second;
                     p1 = std::make_pair(p1.first + diffs.first, p1.second + diffs.second))
                    antis.insert(p1);
            }
        }
    }
    std::cout << "part2: " << antis.size() << std::endl;
}
