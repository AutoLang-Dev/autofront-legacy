import std;
import autofront;

using namespace std::literals;
namespace ranges = std::ranges;
namespace views  = std::views;
namespace fs     = std::filesystem;

auto display_width(std::string_view sv)
{
    auto mangled_str = std::format(".{}", sv);
    auto max_width   = mangled_str.size();
    auto padding_str = std::format("{:>{}}", mangled_str, max_width);
    return max_width - padding_str.find_first_not_of(' ') - 1;
}

using autofront::lexeme;
using autofront::token;
using namespace autofront;
namespace i18n = autofront::i18n;

auto utf32_to_utf8(std::u32string_view sv) -> std::string
{
    auto result = std::string{};
    for (auto ch : sv) {
        auto buf = std::array<char, 4uz>{};
        auto end = utf::utf32_to_utf8(ch, buf.begin());
        result.append_range(std::string_view{buf.begin(), end});
    }
    return result;
}

auto print_error(std::ostream& out, std::span<const std::string_view> lines, const error_entry& error) -> void
{
    auto&& [where, message, internal, fallback, from] = error;
    std::print(out, "{}: {}", where, message);
    if (internal || fallback) {
        std::print(out, " (");
        if (internal) {
            std::print(out, "i");
        }
        if (fallback) {
            std::print(out, "f");
        }
        std::print(out, ")");
    }
    std::println(out);
    if (where.lineno != 0uz) {
        std::println(out, "{}| {}", where.lineno, lines[where.lineno - 1uz]);
        auto pos_hint_width = std::formatted_size("{}| ", where.lineno);
        if (where.colno != 0uz) {
            std::println(out, "{:>{}}", "^", pos_hint_width + where.colno);
        }
    }

    out << i18n::emitted_by(from.file_name(), from.line(), from.column(), from.function_name());

    // std::println(out,
    //              "this error was emitted by autofront/{}:{}:{} (function {})",
    //              from.file_name(),
    //              from.line(),
    //              from.column(),
    //              from.function_name());
    std::println(out);
}

class compilation_error : public std::exception
{
public:
    std::string msg;

    compilation_error(std::string msg) : msg{std::move(msg)} {}

    auto what() const noexcept -> const char* override
    {
        return msg.c_str();
    }
};

struct compilation
{
    std::string filename;
    std::string target_triple;
    std::string_view source;

    auto compile() -> std::string
    {
        auto lines = source | std::views::split('\n') | std::views::transform([](auto r) {
                         return std::string_view{r};
                     })
                     | std::ranges::to<std::vector>();

        auto src = source | autofront::views::from_utf8_to_utf32 | std::ranges::to<std::u32string>();

        auto lex = lex_all(src);

        if (!lex) {
            auto out = std::ostringstream{};
            print_error(out, lines, std::move(lex).error());
            throw compilation_error{std::move(out).str()};
        }

        auto tokens = std::move(lex).value();

        preprocess(tokens);

        auto result = build_token_tree(tokens);

        if (!result) {
            auto out = std::ostringstream{};
            for (auto&& error : result.error()) {
                print_error(out, lines, error);
            }
            throw compilation_error{std::move(out).str()};
        }

        auto tree = std::move(result).value();

        auto pr = autofront::parse(tree);
        if (!pr) {
            auto out = std::ostringstream{};
            print_error(out, lines, std::move(pr).error());
            throw compilation_error{std::move(out).str()};
        }

        auto tu = std::move(pr).value();

        auto emitter          = codegen::ast_llvm::emitter{};
        emitter.filename      = filename;
        emitter.target_triple = target_triple;

        try {
            emitter.emit(tu);
            return std::move(emitter.ctx).str();
        } catch (codegen::ast_llvm::emitting_exception& e) {
            auto err = error_entry{
                .where   = e.span().value_or(source_span{}).start,
                .message = e.what(),
                .from    = e.loc(),
            };
            auto out = std::ostringstream{};
            print_error(out, lines, err);
            throw compilation_error{std::move(out).str()};
        }
    }
};


auto main(int argc, char** argv) -> int
{
    tr.set_zh();

    if (argc < 2) {
        std::cerr << i18n::too_few_arg() << std::endl;
        return 1;
    }
    if (argc > 2) {
        std::cerr << i18n::too_many_arg() << std::endl;
        return 1;
    }

    auto filename = std::string{argv[1]};
    auto path  = fs::path{filename};

    if (!fs::exists(path)) {
        std::cerr << i18n::not_exist(filename) << std::endl;
        return 1;
    }

    if (!fs::is_regular_file(path)) {
        std::cerr << i18n::not_regular_file(filename) << std::endl;
        return 1;
    }

    auto fin    = std::ifstream{filename};
    auto buffer = std::stringstream{};
    buffer << fin.rdbuf();

    auto code = buffer.view();
    if(code.empty()) return 0;

    auto c = compilation{
        .filename      = path.filename(),
        .target_triple = "",
        .source        = code,
    };
    try {
        auto ir = c.compile();
        std::print("{}", ir);
    } catch (std::exception& e) {
        std::print(std::cerr, "{}", e.what());
        return 1;
    }
}
