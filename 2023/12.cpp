#include "Common/parsing.hpp"
#include <iostream>
#include <cstdint>

/* Part 1 is solved using a bitwise-optimized bruteforce, using 64-bit unsigned
   integers to represent both groups and conditions, row-wise.
*/

/** DATA STRUCTURES **/

using std::size_t;
using std::uint64_t;

class Conds {
    // A condition is represented by 0 ('.'), 1 ('#'), and 3 ('?').
    uint64_t cs;
public:
    Conds() : cs(0) {}
    Conds(uint64_t cs_init) : cs(cs_init) {}

    void set_operational(size_t i) { cs &= ~((uint64_t) 3 << (uint64_t) (2*i)); }
    void set_damaged    (size_t i) { cs &= ~((uint64_t) 3 << (uint64_t) (2*i));
                                     cs |=   (uint64_t) 1 << (uint64_t) (2*i);  }
    void set_unknown    (size_t i) { cs |=   (uint64_t) 3 << (uint64_t) (2*i) ; }

    bool is_operational (size_t i) { return ((cs >> (2*i)) & 3) == 0; }
    bool is_damaged     (size_t i) { return ((cs >> (2*i)) & 3) == 1; }
    bool is_unknown     (size_t i) { return ((cs >> (2*i)) & 3) == 3; }

    Conds clone() { return Conds(cs); }

    void print() {
        for (int i = 0; i < 32; i++)
            std::cout << ((cs >> (2*i)) & 3) << " ";
        std::cout << std::endl;
    }
};

class Groups {
    // A group is represented by four bits, so max groupsize is 15.
    uint64_t gs;
public:
    Groups() : gs(0) {}

    void set_group(uint64_t g, size_t i) {
        gs &= ~((uint64_t) 255 << (uint64_t) (8*i));
        gs |= ((g & 255) << (8*i));
    }

    uint64_t get_group(size_t i) { return ((gs >> (8*i)) & 255); }
    uint64_t get_groups()        { return gs;                   }

    void print() {
        for (int i = 0; i < 8; i++)
            std::cout << ((gs >> (8*i)) & 255) << " ";
        std::cout << std::endl;
    }
};

/** PART 1 HELPERS **/

Groups count_groups(Conds conds) {
    Groups groups;
    uint64_t group = 1;
    for (size_t i = 0, count = 0; i <= 31; i++) {
        if (conds.is_damaged(i)) {
            if (conds.is_damaged(i+1)) {
                group++;
            } else {
                groups.set_group(group, count++);
                group = 1;
            }
        }
    }
    return groups;
}

int sum_of_row(size_t i, Conds conds, Groups groups) {
    // Base case
    if (i >= 32) {
        return (int) (count_groups(conds).get_groups() == groups.get_groups());
    }
    // Recursive cases
    else if (conds.is_unknown(i)) {
        Conds conds1 = conds.clone();
        conds.set_operational(i);
        conds1.set_damaged(i);
        return sum_of_row(i+1, conds, groups) + sum_of_row(i+1, conds1, groups);
    }
    else {
        return sum_of_row(i+1, conds, groups);
    }
}

/** MAIN **/

int main() {
    Parser in("in/12.in");
    int sum = 0;
    for (size_t i = 0; i < in.num_lines(); i++) {
        /* Parse the input one row at a time. Parsing generates two integers,
           representing condition records (by two bits s.t. '.' = 0, '#' = 1,
           and '?' = 3) and contiguous groups (by four bits), respectively.
        */
        Conds conds;
        size_t j = 0;
        bool parsing = true;
        do {
            if      (in.parse_char('.')) { j++;                    }
            else if (in.parse_char('#')) { conds.set_damaged(j++); }
            else if (in.parse_char('?')) { conds.set_unknown(j++); }
            else if (in.parse_char(' ')) { parsing = false;        }
            else { throw std::runtime_error("Unexpected symbol!\n"); }
        } while (parsing);

        Groups groups;
        j = 0;
        do {
            groups.set_group(in.parse_int(), j++);
        } while (in.parse_char(','));

        /* Find the sum of arrangments of the row, recursively.
         */
        sum += sum_of_row(0, conds, groups);
    }
    std::cout << sum << "\n";
}
