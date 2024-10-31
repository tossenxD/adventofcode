#include "Common/parsing.hpp"
#include <iostream>
#include <cstdint>
#include <vector>

// A reflector dish space is either empty or contains one of the two rock types
enum Space { RoundedRock, CubicRock, Empty };

// A reflector dish is a 2D grid of reflector dish spaces
using Grid = std::vector<std::vector<Space>>;

// A dish configuration is the coordinates of its rounded rocks
using Point  = std::pair<size_t, size_t>;
using Config = std::vector<Point>;

class Dish {
    size_t m, n;
    Grid grid;
    std::vector<Config> configs;
public:
    Dish(Parser in)
        : m(in.num_lines())
        , n(in.len_line())
        , grid(m, std::vector<Space>(n))
        {
            for(size_t j = 0; j < m; j++)
                for(size_t i = 0; i < n; i++)
                    grid[j][i] = in.parse_char('#') ? CubicRock   :
                                 in.parse_char('O') ? RoundedRock :
                                 in.parse_char('.') ? Empty       :
                                 throw std::runtime_error("Cannot parse!\n");
        }

    size_t tilt_cycle () {
        // North
        {
        size_t empty_spaces[n] = {0};
        for(size_t j = 0; j < m; j++) {
            for(size_t i = 0; i < n; i++) {
                switch(grid[j][i]) {
                    case RoundedRock:
                        grid[j][i] = Empty;
                        grid[j - empty_spaces[i]][i] = RoundedRock;
                        break;
                    case CubicRock:
                        empty_spaces[i] = 0;
                        break;
                    case Empty:
                        empty_spaces[i] += 1;
                        break;
                }
            }
        }
        }
        // West
        for(size_t j = 0; j < m; j++) {
            size_t empty_space = 0;
            for(size_t i = 0; i < n; i++) {
                switch(grid[j][i]) {
                    case RoundedRock:
                        grid[j][i] = Empty;
                        grid[j][i - empty_space] = RoundedRock;
                        break;
                    case CubicRock:
                        empty_space = 0;
                        break;
                    case Empty:
                        empty_space += 1;
                        break;
                }
            }
        }
        // South
        {
        size_t empty_spaces[n] = {0};
        for(size_t j = m-1; j < m; j--) {
            for(size_t i = 0; i < n; i++) {
                switch(grid[j][i]) {
                    case RoundedRock:
                        grid[j][i] = Empty;
                        grid[j + empty_spaces[i]][i] = RoundedRock;
                        break;
                    case CubicRock:
                        empty_spaces[i] = 0;
                        break;
                    case Empty:
                        empty_spaces[i] += 1;
                        break;
                }
            }
        }
        }
        // East (and store configuration)
        Config config;
        for(size_t j = 0; j < m; j++) {
            size_t empty_space = 0;
            for(size_t i = n-1; i < n; i--) {
                switch(grid[j][i]) {
                    case RoundedRock: {
                        grid[j][i] = Empty;
                        grid[j][i + empty_space] = RoundedRock;
                        Point p(j,i);
                        config.push_back(p);
                        break;
                    }
                    case CubicRock:
                        empty_space = 0;
                        break;
                    case Empty:
                        empty_space += 1;
                        break;
                }
            }
        }
        // Check whether the tilt-cycle has converged to a config-cycle
        for (size_t i = 0; i < configs.size(); i++) {
            if (configs[i] == config)
                return i;
        }
        configs.push_back(config);
        return 0;
    }

    // Performs remaining tilt-cycles after a configuration-cycle has occoured
    size_t cyclic_tilt_cycle(size_t current, size_t remaining) {
        size_t ind = current + (remaining % (configs.size() - current));
        Config config = configs[ind-1];
        size_t sum = 0;
        for (Point p : config)
            sum += m - p.first;
        return sum;
    }

    // Prints a dish with symbols
    void print() {
        for(size_t j = 0; j < m; j++) {
            for(size_t i = 0; i < n; i++)
                std::cout << ((grid[j][i] == CubicRock)   ? '#' :
                              (grid[j][i] == RoundedRock) ? 'O' : '.');
            std::cout << "\n";
        }
        std::cout << "\n";
    }
};

int main() {
    /** Part 1 **/
    Parser in1("in/14.in");
    const size_t n = in1.len_line(), m = in1.num_lines();
    int sum = 0;

    size_t empty_spaces[n] = {0};
    for(size_t j = 0; j < m; j++) {
        for(size_t i = 0; i < n; i++) {
            switch(in1.parse_some_char()) {
                case '#':
                    empty_spaces[i] = 0;
                    break;
                case 'O':
                    sum += m - j + empty_spaces[i];
                    break;
                case '.':
                    empty_spaces[i] += 1;
                    break;
                default:
                    throw std::runtime_error("Cannot parse!\n");
            }
        }
    }
    std::cout << "part1: " << sum << "\n";

    /** Part 2 **/
    Parser in2("in/14.in");
    Dish dish(in2);
    size_t reps = 1000000000;
    for (size_t i = 0; i < reps; i++) {
        size_t c = dish.tilt_cycle();
        if (c) {
            size_t retval = dish.cyclic_tilt_cycle(c, reps - i);
            std::cout << "part2: " << retval << "\n";
            break;
        }
    }
}