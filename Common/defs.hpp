#ifndef DEFS
#define DEFS
#include <cstdint>

/** Types **/

// Integrals
typedef std::int8_t    i8;
typedef std::int16_t  i16;
typedef std::int32_t  i32;
typedef std::int64_t  i64;

typedef std::uint8_t   u8;
typedef std::uint16_t u16;
typedef std::uint32_t u32;
typedef std::uint64_t u64;
typedef std::size_t    st;

// Positioning
enum Direction { N, E, S, W };
typedef std::pair<u32,u32> Position;

/** Macros **/

#define DISTANCE(x, y) ((x > y) ? x - y : y - x)

#endif
