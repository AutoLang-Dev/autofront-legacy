export module autofront:common;

import std;

namespace ranges = std::ranges;
namespace views  = std::views;

export namespace autofront
{

struct source_position
{
    std::size_t lineno = 1uz;
    std::size_t colno  = 1uz;
};

struct comment
{
    enum struct kinds : bool
    {
        line,
        stream
    };
    kinds kind;
    source_position start;
    source_position end;
    std::string text;
};

struct error_entry
{
    source_position where;
    std::string message;
    bool internal = false;
    bool fallback = false;
    std::source_location from;
};

auto is_start(char ch)
{
    return std::isalpha(ch) || ch == '_';
}

auto is_continue(char ch)
{
    return is_start(ch) || std::isdigit(ch);
}

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

}

template <>
struct std::formatter<autofront::source_position, char> : public std::formatter<std::string_view, char>
{
    auto format(autofront::source_position loc, auto&& ctx) const
    {
        return std::format_to(ctx.out(), "{}:{}", loc.lineno, loc.colno);
    }
};
