#include "Common/parsing.hpp"
#include <string>
#include <iostream>
#include <cstdint>
#include <vector>
#include <array>

typedef std::string Key;
typedef uint8_t Val;
typedef std::pair<Key, Val> KeyVal;
typedef std::vector<KeyVal> Queue;

// Implements a box as a Queue of lenses
class Box {
    Queue queue;
public:
    void remove(Key key) {
        for (Queue::iterator i = queue.begin(); i != queue.end(); i++)
            if ((*i).first == key) {
                queue.erase(i);
                break;
            }
    }

    void insert(KeyVal keyval) {
        size_t i = 0;
        while (i < queue.size()) {
            if (queue[i].first == keyval.first) {
                queue[i].second = keyval.second;
                break;
            }
            i++;
        }
        if (i == queue.size())
            queue.push_back(keyval);
    }

    size_t focusing_power(size_t box_num) {
        size_t fp = 0;
        for (size_t slot = 0; slot < queue.size(); slot++)
            fp += (box_num+1) * (slot+1) * queue[slot].second;
        return fp;
    }
};

int main() {
    /* PART 1*/
    Parser in1("in/15.in");
    size_t sum = 0;
    while (!in1.is_eof()) {
        size_t val = 0;
        do { val = ((val + (size_t) in1.parse_some_char()) * 17) % 256; }
        while (!(in1.is_eof() || in1.parse_char(',')));
        sum += val;
    }
    std::cout << "part1: " << sum << "\n";

    /* PART 2*/
    Parser in2("in/15.in");
    std::array<Box, 256> boxes;
    while (!in2.is_eof()) {
        // Parse box number and label
        size_t box_num = 0;
        Key key = "";
        do {
            char c = in2.parse_some_char();
            key.push_back(c);
            box_num = ((box_num + (size_t) c) * 17) % 256;
        }
        while (in2.is_letter());

        // Remove the lens
        if (in2.parse_char('-')) {
            boxes[box_num].remove(key);
        }
        // Insert the lense
        else if (in2.parse_char('=')) {
            Val val = (Val) in2.parse_int();
            KeyVal keyval(key, val);
            boxes[box_num].insert(keyval);
        }
        // Parsing error
        else
            throw std::runtime_error("Parsing expected [ '-' | '=' ]!\n");

        // Remove the (possibly) seperating ','
        if (!in2.is_eof())
            in2.parse_char(',');
    }

    // Compute total focusing power amongst lenses
    size_t fp_sum = 0;
    for (size_t box_num = 0; box_num < 256; box_num++)
        fp_sum += boxes[box_num].focusing_power(box_num);

    std::cout << "part2: " << fp_sum << "\n";
}