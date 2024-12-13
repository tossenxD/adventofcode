#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <vector>

typedef u16 ID_t;
#define freespace 0xFFFF

int main() {
    /* PARSING */
    Parser in("in/09.in");
    std::vector<ID_t> disk(1048576); // assume that the disk is <= 1 megablock
    ID_t id = 0;
    u32 idx = 0;
    for (st k = 0; k < in.len_line(); k++) {
        bool odd = static_cast<bool>(k & 1u);
        for (u32 bsize = static_cast<u32>(in.parse_some_char()) - 48u; bsize > 0; bsize--)
            disk[idx++] = odd ? freespace : id;
        id += (odd) ? 0u : 1u;
    }
    disk.resize(idx);

    /* Part 1 */
    u64 checksum = 0; // idx points at the end+1 and idx0 at the begining
    for (u32 idx0 = 0; idx0 < idx; idx0++) {
        if (disk[idx0] != freespace)
            checksum += disk[idx0] * idx0;
        else {
            while (disk[--idx] == freespace) {}
            checksum += disk[idx] * idx0;
        }
    }
    std::cout << "part1: " << checksum << std::endl;

    /* PART 2 */
    checksum = 0;
    std::cout << "part2: " << checksum << std::endl;
}
