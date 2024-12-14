#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <vector>

typedef u16 ID_t;
#define freespace 0xFFFF

struct Section {
    u16 size{};
    ID_t id{};
};

int main() {
    /* PARSING */
    Parser in("in/09.in");
    std::vector<ID_t> disk(1048576); // assume that the disk is <= 1 megablock
    ID_t id = 0;
    u32 idx = 0;
    st numsec = in.len_line();
    for (st k = 0; k < numsec; k++) {
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
    in.reset_state();
    id = 0;
    std::vector<Section> sections(numsec); // represent disk as sections rather than blocks
    for (st k = 0; k < numsec; k++) {      // and represent it in reverse order
        sections[numsec-1 - k].size = static_cast<u32>(in.parse_some_char()) - 48u;
        sections[numsec-1 - k].id = (static_cast<bool>(k & 1u)) ? freespace : id++;
    }

    for (st k = 0; k < sections.size(); k++) {
        for (st kk = sections.size()-1; kk > k; kk--) {
            if (sections[kk].id == freespace && sections[k].id != freespace
                                             && sections[kk].size >= sections[k].size) {
                u16 sizediff = sections[kk].size - sections[k].size;
                sections[kk].id = sections[k].id;
                sections[k].id = freespace;
                if (sizediff != 0) { // ineffecient way to handle excess space
                    sections[kk].size = sections[k].size;
                    sections.insert(sections.begin()+kk, Section{sizediff, freespace});
                }
                break;
            }
        }
    }

    u64 blockIdx = 0;
    for (auto sec = sections.rbegin(); sec != sections.rend(); sec++){
        if (sec->id == freespace)
            blockIdx += sec->size;
        else
            for (u32 s = sec->size; s > 0; s--)
                checksum += sec->id * blockIdx++;
    }

    std::cout << "part2: " << checksum << std::endl;
}
