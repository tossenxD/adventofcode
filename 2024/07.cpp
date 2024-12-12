#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <regex>
#include <vector>
#include <string>

// generic integral concat function, which may be useful?
template<typename T>
std::enable_if_t<std::is_integral_v<T>, T> concat(T a, T b, T r) {
    for (u64 d = r; d <= b; d *= r)
        a *= r;
    return a * r + b;
}

class Calibration {
    u64 result{};
    std::vector<u64> terms{};
public:
    void push_term (u64 t) { terms.push_back(t); }
    void set_result(u64 r) { result = r;         }

    u64 part1() {
        // create an operator bitmask s.t. 0 is `+` and 1 is `*`
        u32 ops_count = terms.size() - 1;
        u32 ops = 0xFFFFFFFF << ops_count;
        for (;;) {
            u64 eq_res = terms[0];
            for (u32 i = 0; i < ops_count; i++) {
                if (ops & (1u << i))
                    eq_res *= terms[i+1];
                else
                    eq_res += terms[i+1];
            }
            if (eq_res == result)
                return result;
            if (ops == 0xFFFFFFFF)
                return 0ull;
            ops++;
        }
    }

    u64 part2() {
        // create an operator pairbitmask s.t. 0 is `+`, 1 is `*`, 2 is '||', and 3 is dummy
        u64 ops_count = terms.size() - 1;
        u64 ops = 0xFFFFFFFFFFFFFFFF << 2*ops_count;
        for (;;) {
            u64 eq_res = terms[0];
            for (u64 i = 0; i < ops_count; i++) {
                u64 op = (ops >> 2*i) & 3u;
                if      (op == 0)
                    eq_res += terms[i+1];
                else if (op == 1)
                    eq_res *= terms[i+1];
                else if (op == 2)
                    eq_res = concat<u64>(eq_res, terms[i+1], 10ull);
                else {/*(op == 3)*/
                    eq_res = 0;
                    break;
                }
            }
            if (eq_res == result)
                return result;
            if (ops == 0xFFFFFFFFFFFFFFFF)
                return 0ull;
            ops++;
        }
    }
};

int main() {
    /* PARSING */
    Parser in("in/07.in");
    std::vector<Calibration> calibrations(in.num_lines());
    for (st i = 0; i < in.num_lines(); i++) {
        calibrations[i].set_result(static_cast<u64>(in.parse_i64()));
        std::regex re("[0-9]+", std::regex_constants::extended);
        in.apply_regex_sum(re, [&calibrations, i](std::string str) {
            calibrations[i].push_term(static_cast<u64>(std::stol(str)));
        });
    }

    /* PART 1 */
    u64 total_result = 0;
    for (Calibration calibration : calibrations)
        total_result += calibration.part1();
    std::cout << "part1: " << total_result << std::endl;

    /* PART 2 */
    total_result = 0;
    for (Calibration calibration : calibrations)
        total_result += calibration.part2();
    std::cout << "part2: " << total_result << std::endl;
}
