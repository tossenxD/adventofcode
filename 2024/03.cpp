#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <regex>
#include <string>

int main() {
    /* PART 1 */
    Parser in("in/03.in");
    u64 sum = 0;
    std::regex re1("mul\\([0-9]{1,3},[0-9]{1,3}\\)", std::regex_constants::extended);
    for (st i = 0; i < in.num_lines(); i++) {
        in.apply_regex_sum(re1, [&sum](std::string str) {
            u64 digit_count;
            u64 n0 = std::stol(str.substr(4), &digit_count);
            u64 n1 = std::stol(str.substr(5+digit_count));
            sum += n0 * n1;
        });
    }
    std::cout << "part1: " << sum << std::endl;

    /* PART 2 */
    in.reset_state();
    sum = 0;
    std::regex re2("mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)",
                   std::regex_constants::extended);
    bool enabled = true;
    for (st i = 0; i < in.num_lines(); i++) {
        in.apply_regex_sum(re2, [&sum, &enabled](std::string str) {
            if (str == "do()")
                enabled = true;
            else if (str == "don't()")
                enabled = false;
            else if (enabled) {
                u64 digit_count;
                u64 n0 = std::stol(str.substr(4), &digit_count);
                u64 n1 = std::stol(str.substr(5+digit_count));
                sum += n0 * n1;
            }
        });
    }
    std::cout << "part2: " << sum << std::endl;
}
