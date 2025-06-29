import std;
import autofront;

using namespace std::literals;
namespace ranges = std::ranges;
namespace views  = std::views;

namespace
{

auto display_width(std::string_view sv)
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

    auto in_comment            = false;
    auto current_comment       = std::string{};
    auto current_comment_start = autofront::source_position{};
    auto tokens                = std::vector<autofront::token>{};
    auto comments              = std::vector<autofront::comment>{};
    auto errors                = std::vector<autofront::error_entry>{};

    auto lines = std::deque<std::string>{};

    for (auto lineno = 1uz;; ++lineno) {
        auto& line = lines.emplace_back();
        std::print("> ");
        std::getline(std::cin, line);
        if (!std::cin) break;
        if (trim(line) == ".exit"sv) break;
        auto s = autofront::lex_line(line,
                                     lineno,
                                     in_comment,
                                     current_comment,
                                     current_comment_start,
                                     tokens,
                                     comments,
                                     errors);

        auto max_lineno_len = 0uz;
        auto max_colno_len  = 0uz;
        auto max_token_len  = 0uz;
        for (auto&& [view, pos, _] : tokens) {
            max_lineno_len = std::max(max_lineno_len, std::formatted_size("{}", pos.lineno));
            max_colno_len  = std::max(max_colno_len, std::formatted_size("{}", pos.colno));
            max_token_len  = std::max(max_token_len, display_width(std::format("{:?}", view)));
        }
        for (auto&& [view, pos, type] : tokens) {
            std::println("({:>{}}:{:<{}}): {:<{}?} ({}),",
                         pos.lineno,
                         max_lineno_len,
                         pos.colno,
                         max_colno_len,
                         view,
                         max_token_len,
                         autofront::_as<std::string>(type));
        }
        tokens.clear();

        if (!s) {
            std::println();
            std::println("===!!!Error!!!===");
            auto stop = false;
            for (auto&& [where, message, internal, fallback] : errors) {
                stop |= fallback;
                std::print("{}: {:?}", where, message);
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
            }
            errors.clear();
            if (stop) {
                break;
            } else {
                continue;
            }
        }

        if (in_comment) {
            std::println();
            std::println("in_comment = {}", in_comment);
            std::println("current_comment = {:?}", current_comment);
            std::println("current_comment_start = {}", current_comment_start);
        }

        if (!comments.empty()) {
            std::println();
            std::println("Comments:");
            for (auto&& [kind, start, end, text] : comments) {
                std::println("{}~{}: {:?} ({}),",
                             start,
                             end,
                             text,
                             kind == autofront::comment::kinds::line ? "line" : "stream");
            }
            comments.clear();
        }
    }
}
