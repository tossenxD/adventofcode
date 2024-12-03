#include "Common/parsing.hpp"
#include "Common/datastruct.hpp"
#include <iostream>
#include <vector>
#include <cstdint>

#define DISTANCE(x, y) ((x - y >= 0) ? x - y : y - x)

typedef std::vector<i32> Report;

std::vector<i32> parse_report(Parser& p) {
    std::vector<i32> report{};
    do { report.push_back(p.parse_int()); }
    while (!p.is_eof() && p.parse_char(' '));
    return report;
}

bool is_safe_lvls(i32 lvl0, i32 lvl1, bool is_increasing) {
    i32 dist = DISTANCE(lvl0, lvl1);
    return (is_increasing == (lvl0 < lvl1)) && (0 < dist) && (dist < 4);
}

bool is_safe_report(Report report) {
    bool is_safe = true;
    bool is_increasing = report[0] < report[1];
    for (st i = 0; i < report.size()-1; i++)
        is_safe = is_safe && is_safe_lvls(report[i], report[i+1], is_increasing);
    return is_safe;
}

int main() {
    /* PART 1 */
    Parser in("in/02.in");
    i32 safe_count = 0;
    while (!in.is_eof()) {
        Report report = parse_report(in);
        if (is_safe_report(report))
            safe_count++;
    }
    std::cout << "part1: " << safe_count << std::endl;

    /* PART 2 */
    in.reset_state();
    safe_count = 0;
    while (!in.is_eof()) {
        Report report = parse_report(in);
        if (is_safe_report( Report(report.begin()+1, report.end()) ))
            safe_count++;
        else {
            bool is_safe = true;
            bool is_increasing = report[0] < report[1];
            for (st i = 0; i < report.size()-1 && is_safe; i++) {
                is_safe = is_safe && is_safe_lvls(report[i], report[i+1], is_increasing);
                if (!is_safe)
                    report.erase(report.begin()+i+1);
            }
            if (is_safe || is_safe_report(report))
                safe_count++;
        }
    }
    std::cout << "part2: " << safe_count << std::endl;
}
