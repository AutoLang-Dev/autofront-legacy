import std;
import autofront;

using namespace std::literals;
namespace ranges = std::ranges;
namespace views  = std::views;

namespace
{

[[maybe_unused]] auto display_width(std::string_view sv)
{
    auto mangled_str = std::format(".{}", sv);
    auto max_width   = mangled_str.size();
    auto padding_str = std::format("{:>{}}", mangled_str, max_width);
    return max_width - padding_str.find_first_not_of(' ') - 1;
}

auto ltrim(std::string_view str) -> std::string_view
{
    auto start = 0uz;
    while (start < str.size() && std::isspace(static_cast<unsigned char>(str[start]))) {
        ++start;
    }
    return str.substr(start);
}
auto rtrim(std::string_view str) -> std::string_view
{
    auto end = str.size();
    while (end > 0 && std::isspace(static_cast<unsigned char>(str[end - 1]))) {
        --end;
    }
    return str.substr(0, end);
}
auto trim(std::string_view str) -> std::string_view
{
    return rtrim(ltrim(str));
}

auto utf32_to_utf8(std::u32string_view sv) -> std::string
{
    auto result = std::string{};
    for (auto ch : sv) {
        auto buf = std::array<char, 4uz>{};
        auto end = autofront::utf::utf32_to_utf8(ch, buf.begin());
        result.append_range(std::string_view{buf.begin(), end});
    }
    return result;
}

auto print_error(std::span<const std::string_view> lines, const autofront::error_entry& error)
{
    auto&& [where, message, internal, fallback, from] = error;
    std::print("{}: {}", where, message);
    if (internal || fallback) {
        std::print(" (");
        if (internal) {
            std::print("i");
        }
        if (fallback) {
            std::print("f");
        }
        std::print(")");
    }
    std::println();
    if (where.lineno != 0uz) {
        std::println("{}| {}", where.lineno, lines[where.lineno - 1uz]);
        auto pos_hint_width = std::formatted_size("{}| ", where.lineno);
        if (where.colno != 0uz) {
            std::println("{:>{}}", "^", pos_hint_width + where.colno);
        }
    }
    std::println("this error was emitted by autofront/{}:{}:{} (function {})",
                 from.file_name(),
                 from.line(),
                 from.column(),
                 from.function_name());
    std::println();
}

}

auto main() -> int
{
    auto logo =
        R"(  ___          _           _                          
 / _ \        | |         | |                         
/ /_\ \ _   _ | |_   ___  | |      __ _  _ __    __ _ 
|  _  || | | || __| / _ \ | |     / _` || '_ \  / _` |
| | | || |_| || |_ | (_) || |____| (_| || | | || (_| |
\_| |_/ \__,_| \__| \___/ \_____/ \__,_||_| |_| \__, |
                                                 __/ |
                                                |___/)"sv;
    std::println("{}", logo);
    std::println("AutoFront Test Lexer Repl. Type \".exit\" to exit.");

    for (auto source = std::string{};;) {
        std::print("> ");
        auto line = std::string{};
        std::getline(std::cin, line);
        if (!std::cin) break;
        source += line;
        if (!line.empty() && line.back() == '\\') {
            source.back() = '\n';
            continue;
        }
        if (trim(source) == ".exit"sv) break;

        auto lines = source | std::views::split('\n') | std::views::transform([](auto r) {
                         return std::string_view{r};
                     })
                     | std::ranges::to<std::vector>();

        auto source_utf32 =
            std::string_view{source} | autofront::views::from_utf8_to_utf32 | std::ranges::to<std::u32string>();
        auto result = autofront::lex_all(source_utf32);

        if (result) {
            auto&& tokens     = result.value();
            auto max_pos1_wid = 0uz;
            auto max_pos2_wid = 0uz;
            auto max_lex_wid  = 0uz;
            for (auto&& token : tokens) {
                max_pos1_wid = std::max(max_pos1_wid, std::formatted_size("{}", token.pos));
                max_pos2_wid = std::max(max_pos2_wid, std::formatted_size("{}", token.span().end.colno));
                max_lex_wid  = std::max(max_lex_wid, std::formatted_size("{}", token.type));
            }
            for (auto&& token : tokens) {
                std::println("{:>{}}~{:<{}} | {:<{}} | {:?}",
                             std::format("{}", token.pos),
                             max_pos1_wid,
                             token.span().end.colno,
                             max_pos2_wid,
                             std::format("{}", token.type),
                             max_lex_wid,
                             utf32_to_utf8(token.view));
            }
        } else {
            print_error(lines, result.error());
        }
        source.clear();
    }
}
