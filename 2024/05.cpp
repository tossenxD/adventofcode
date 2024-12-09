#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <regex>
#include <string>
#include <map>
#include <set>

/* The solution is designed around a mapping from pages to sets of pages, s.t. the page-set
   asociated with a given page indicates conditions for pages that must come after the given page.
   The problem is then divided into constructing actual left-hand-sides and conditioned
   right-hand-sides for each page, and checking if these sides satisfy the ordering conditions.
 */

typedef u32 Page;

bool verify_page_ordering(Page p, std::map<Page, std::set<Page>> ordermap, std::vector<Page> lhs) {
    bool c = true;
    auto itr = ordermap.find(p);
    if (itr != ordermap.end()) {
        std::set<Page> rhs = itr->second;
        for (auto pl : lhs) {
            c = c && (!rhs.contains(pl)); // fail if a rhs-page is to the lhs
        }
    }
    return c;
}

int main() {
    /* PART 1 */
    Parser in("in/05.in");
    std::map<Page, std::set<Page>> ordermap {};
    while (!in.parse_empty_line()) {
        Page lp = static_cast<Page>(in.parse_int());
        in.parse_char('|');
        Page rp = static_cast<Page>(in.parse_int());
        auto itr = ordermap.find(lp);
        if (itr == ordermap.end())
            ordermap[lp] = {rp};
        else {
            auto ps = itr->second;
            ps.insert(rp);
            ordermap[lp] = ps;
        }
    }

    u32 sum = 0;
    std::regex re("[0-9]+", std::regex_constants::extended);
    while (!in.is_eof()) {
        std::vector<Page> lhs{};
        bool c = true;
        in.apply_regex_sum(re, [&sum, &lhs, &ordermap, &c](std::string str) {
            Page p = std::stol(str);
            c = c && verify_page_ordering(p, ordermap, lhs);
            lhs.push_back(p);
        });
        sum += c ? lhs[lhs.size() / 2] : 0;
    }
    std::cout << "part1: " << sum << std::endl;

    /* PART 2 */
    in.reset_state();
    while (!in.parse_empty_line()) // skip page ordering for part 2
        in.parse_some_char();

    sum = 0;
    while (!in.is_eof()) {
        std::vector<Page> lhs{};
        bool c = true;
        in.apply_regex_sum(re, [&sum, &lhs, &ordermap, &c](std::string str) {
            Page p = std::stol(str);
            c = c && verify_page_ordering(p, ordermap, lhs);
            lhs.push_back(p);
        });
        // Until this point, we do as in part 1. Then, if the conditions does not hold,
        // we repeatedly try to verify ordering conditions similarly to before, except that
        // any two pages failed to satisfy the order condition within an update is swapped.
        if (!c) {
            for (st i = 1; i < lhs.size(); i++) {
                Page p = lhs[i];
                auto itr = ordermap.find(p);
                if (itr != ordermap.end()) {
                    std::set<Page> rhs = itr->second;
                    for (st j = 0; j < i; j++) {
                        if (rhs.contains(lhs[j])) {
                            lhs[i] = lhs[j];
                            lhs[j] = p;
                            i = 1;
                            break;
                        }
                    }
                }
            }
            sum += lhs[lhs.size() / 2];
        }
    }
    std::cout << "part2: " << sum << std::endl;
}
