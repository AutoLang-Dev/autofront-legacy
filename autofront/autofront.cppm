export module autofront;

import std;

export import :common;
export import :lex;
export import :utf;
export import :i18n;

static_assert(std::numeric_limits<std::uint8_t>::digits == 8uz);
