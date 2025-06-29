export module autofront;

import std;

export import :common;
export import :lex;

static_assert(std::numeric_limits<std::uint8_t>::digits == 8uz);
