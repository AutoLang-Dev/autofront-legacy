import std;
import autofront;

using namespace std::literals;
namespace ranges = std::ranges;
namespace views  = std::views;

namespace
{

using namespace autofront;

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

auto print_tree(const token_tree& tree) -> void
{
    constexpr auto indent_step = 2uz;

    auto delim_map = std::flat_map<token_tree::delimiter, std::string_view>{
        {token_tree::delimiter::angle,   "<>"},
        {token_tree::delimiter::brace,   "{}"},
        {token_tree::delimiter::bracket, "[]"},
        {token_tree::delimiter::none,    {}  },
        {token_tree::delimiter::paren,   "()"},
    };

    constexpr auto green  = "\e[32m"sv;
    constexpr auto yellow = "\e[33m"sv;
    constexpr auto blue   = "\e[34m"sv;
    constexpr auto purple = "\e[35m"sv;
    constexpr auto white  = "\e[37m"sv;

    auto brace_color = [&](std::size_t indent) {
        assert_(indent % indent_step == 0uz, "indent is not an integer multiple of indent_step");
        auto layers = (indent / indent_step) % 3uz;
        if (layers == 0uz) return yellow;
        if (layers == 1uz) return purple;
        if (layers == 2uz) return blue;
        std::unreachable();
    };

    auto vis = [&](this auto&& vis, const token_tree& t, std::size_t iw = 0uz) -> std::size_t {
        if (auto token = t.get_token()) {
            return iw + std::formatted_size("{} ", token->type());
        } else if (auto group = t.get_group()) {
            auto delta_width = group->delim() == token_tree::delimiter::none ? 0uz : indent_step;
            auto ret         = iw;
            for (auto&& child : group->without_delim()) {
                ret = std::max(ret, vis(child, iw + delta_width));
            }
            return ret;
        } else {
            throw std::runtime_error{"unknown node"};
        }
    };
    auto max_wid = vis(tree);

    auto print = [&](this auto&& print, const token_tree& tree, std::size_t indent_width = 0uz) -> void {
        auto indent = std::string(indent_width, ' ');
        if (auto token = tree.get_token()) {
            auto fill = max_wid - indent_width - std::formatted_size("{} ", token->type());
            std::println("{}{}{} {}{} {}",
                         indent,
                         green,
                         std::format("{}", token->type()),
                         std::string(fill, '.'),
                         white,
                         utf32_to_utf8(token->text()));
        } else if (auto group = tree.get_group()) {
            auto delta_width = group->delim() == token_tree::delimiter::none ? 0uz : indent_step;
            auto delims      = delim_map[group->delim()];
            if (!delims.empty()) {
                std::println("{}{}{}{}", indent, brace_color(indent_width), delims.front(), white);
            }
            for (auto&& child : group->without_delim()) {
                print(child, indent_width + delta_width);
            }
            if (!delims.empty()) {
                std::println("{}{}{}{}", indent, brace_color(indent_width), delims.back(), white);
            }
        } else {
            std::println("{}(None)", indent);
        }
    };
    print(tree, 0uz);
}

auto print_error(std::span<const std::string_view> lines, const error_entry& error)
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
[[maybe_unused]]
auto compile(std::string_view source_utf8)
{
    auto lines = source_utf8 | std::views::split('\n') | std::views::transform([](auto r) {
                     return std::string_view{r};
                 })
                 | std::ranges::to<std::vector>();


    auto source = source_utf8 | autofront::views::from_utf8_to_utf32 | std::ranges::to<std::u32string>();

    auto lex = lex_all(source);

    if (!lex) {
        print_error(lines, std::move(lex).error());
        return;
    }

    auto tokens = std::move(lex).value();

    preprocess(tokens);

    auto errors = std::vector<error_entry>{};

    auto tree = build_token_tree(tokens, errors);
    if (!errors.empty()) {
        for (auto&& error : errors) {
            print_error(lines, error);
        }
        return;
    }
    print_tree(tree);
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
    std::println("AutoFront Test of token_tree. Enter any code.");
    std::println(">>>");

    auto source = std::string{};
    for (auto line = std::string{};;) {
        std::getline(std::cin, line);
        source += line;
        if (!std::cin) break;
        source += '\n';
    }
    compile(source);
}
