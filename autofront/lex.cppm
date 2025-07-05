export module autofront:lex;

import std;
import :common;

using namespace std::literals;

export namespace autofront
{

inline namespace from_cpp2 // 直接复制的 cpp2 代码，后续再改
{

enum struct lexeme : std::uint8_t
{
    SlashEq,
    Slash,
    LeftShiftEq,
    LeftShift,
    Spaceship,
    LessEq,
    Less,
    RightShiftEq,
    RightShift,
    GreaterEq,
    Greater,
    PlusPlus,
    PlusEq,
    Plus,
    MinusMinus,
    MinusEq,
    Arrow,
    Minus,
    LogicalOrEq,
    LogicalOr,
    PipeEq,
    Pipe,
    LogicalAndEq,
    LogicalAnd,
    MultiplyEq,
    Multiply,
    ModuloEq,
    Modulo,
    AmpersandEq,
    Ampersand,
    CaretEq,
    Caret,
    TildeEq,
    Tilde,
    EqualComparison,
    Assignment,
    NotEqualComparison,
    Not,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Scope,
    Colon,
    Semicolon,
    Comma,
    Dot,
    DotDot,
    Ellipsis,
    EllipsisLess,
    EllipsisEqual,
    QuestionMark,
    At,
    Dollar,
    FloatLiteral,
    BinaryLiteral,
    DecimalLiteral,
    HexadecimalLiteral,
    StringLiteral,
    CharacterLiteral,
    UserDefinedLiteralSuffix,
    Keyword,
    Cpp1MultiKeyword,
    Cpp2FixedType,
    Identifier,
    None = 127
};

auto is_literal(lexeme l) -> bool
{
    switch (l) {
        break;
    case lexeme::FloatLiteral:
    case lexeme::BinaryLiteral:
    case lexeme::DecimalLiteral:
    case lexeme::HexadecimalLiteral:
    case lexeme::StringLiteral:
    case lexeme::CharacterLiteral:
        return true;
        break;
    default:
        return false;
    }
}

auto close_paren_type(lexeme l) -> lexeme
{
    switch (l) {
        break;
    case lexeme::LeftBrace:
        return lexeme::RightBrace;
        break;
    case lexeme::LeftBracket:
        return lexeme::RightBracket;
        break;
    case lexeme::LeftParen:
        return lexeme::RightParen;
        break;
    default:
        return lexeme::None;
    }
}

auto is_operator(lexeme l) -> bool
{
    return l <= lexeme::Not;
}

}

}

template <>
struct std::formatter<autofront::lexeme, char> : public std::formatter<std::string_view, char>
{
    auto format(const autofront::lexeme& l, auto&& ctx) const;
};

#define AUTOFRONT_LEXEME_TO_STRING(l)                                                                                  \
    case lexeme::l:                                                                                                    \
        return std::format_to(ctx.out(), #l);

auto std::formatter<autofront::lexeme, char>::format(const autofront::lexeme& l, auto&& ctx) const
{
    using autofront::lexeme;
    switch (l) {
        AUTOFRONT_LEXEME_TO_STRING(SlashEq);
        AUTOFRONT_LEXEME_TO_STRING(Slash);
        AUTOFRONT_LEXEME_TO_STRING(LeftShiftEq);
        AUTOFRONT_LEXEME_TO_STRING(LeftShift);
        AUTOFRONT_LEXEME_TO_STRING(Spaceship);
        AUTOFRONT_LEXEME_TO_STRING(LessEq);
        AUTOFRONT_LEXEME_TO_STRING(Less);
        AUTOFRONT_LEXEME_TO_STRING(RightShiftEq);
        AUTOFRONT_LEXEME_TO_STRING(RightShift);
        AUTOFRONT_LEXEME_TO_STRING(GreaterEq);
        AUTOFRONT_LEXEME_TO_STRING(Greater);
        AUTOFRONT_LEXEME_TO_STRING(PlusPlus);
        AUTOFRONT_LEXEME_TO_STRING(PlusEq);
        AUTOFRONT_LEXEME_TO_STRING(Plus);
        AUTOFRONT_LEXEME_TO_STRING(MinusMinus);
        AUTOFRONT_LEXEME_TO_STRING(MinusEq);
        AUTOFRONT_LEXEME_TO_STRING(Arrow);
        AUTOFRONT_LEXEME_TO_STRING(Minus);
        AUTOFRONT_LEXEME_TO_STRING(LogicalOrEq);
        AUTOFRONT_LEXEME_TO_STRING(LogicalOr);
        AUTOFRONT_LEXEME_TO_STRING(PipeEq);
        AUTOFRONT_LEXEME_TO_STRING(Pipe);
        AUTOFRONT_LEXEME_TO_STRING(LogicalAndEq);
        AUTOFRONT_LEXEME_TO_STRING(LogicalAnd);
        AUTOFRONT_LEXEME_TO_STRING(MultiplyEq);
        AUTOFRONT_LEXEME_TO_STRING(Multiply);
        AUTOFRONT_LEXEME_TO_STRING(ModuloEq);
        AUTOFRONT_LEXEME_TO_STRING(Modulo);
        AUTOFRONT_LEXEME_TO_STRING(AmpersandEq);
        AUTOFRONT_LEXEME_TO_STRING(Ampersand);
        AUTOFRONT_LEXEME_TO_STRING(CaretEq);
        AUTOFRONT_LEXEME_TO_STRING(Caret);
        AUTOFRONT_LEXEME_TO_STRING(TildeEq);
        AUTOFRONT_LEXEME_TO_STRING(Tilde);
        AUTOFRONT_LEXEME_TO_STRING(EqualComparison);
        AUTOFRONT_LEXEME_TO_STRING(Assignment);
        AUTOFRONT_LEXEME_TO_STRING(NotEqualComparison);
        AUTOFRONT_LEXEME_TO_STRING(Not);
        AUTOFRONT_LEXEME_TO_STRING(LeftBrace);
        AUTOFRONT_LEXEME_TO_STRING(RightBrace);
        AUTOFRONT_LEXEME_TO_STRING(LeftParen);
        AUTOFRONT_LEXEME_TO_STRING(RightParen);
        AUTOFRONT_LEXEME_TO_STRING(LeftBracket);
        AUTOFRONT_LEXEME_TO_STRING(RightBracket);
        AUTOFRONT_LEXEME_TO_STRING(Scope);
        AUTOFRONT_LEXEME_TO_STRING(Colon);
        AUTOFRONT_LEXEME_TO_STRING(Semicolon);
        AUTOFRONT_LEXEME_TO_STRING(Comma);
        AUTOFRONT_LEXEME_TO_STRING(Dot);
        AUTOFRONT_LEXEME_TO_STRING(DotDot);
        AUTOFRONT_LEXEME_TO_STRING(Ellipsis);
        AUTOFRONT_LEXEME_TO_STRING(EllipsisLess);
        AUTOFRONT_LEXEME_TO_STRING(EllipsisEqual);
        AUTOFRONT_LEXEME_TO_STRING(QuestionMark);
        AUTOFRONT_LEXEME_TO_STRING(At);
        AUTOFRONT_LEXEME_TO_STRING(Dollar);
        AUTOFRONT_LEXEME_TO_STRING(FloatLiteral);
        AUTOFRONT_LEXEME_TO_STRING(BinaryLiteral);
        AUTOFRONT_LEXEME_TO_STRING(DecimalLiteral);
        AUTOFRONT_LEXEME_TO_STRING(HexadecimalLiteral);
        AUTOFRONT_LEXEME_TO_STRING(StringLiteral);
        AUTOFRONT_LEXEME_TO_STRING(CharacterLiteral);
        AUTOFRONT_LEXEME_TO_STRING(UserDefinedLiteralSuffix);
        AUTOFRONT_LEXEME_TO_STRING(Keyword);
        AUTOFRONT_LEXEME_TO_STRING(Cpp1MultiKeyword);
        AUTOFRONT_LEXEME_TO_STRING(Cpp2FixedType);
        AUTOFRONT_LEXEME_TO_STRING(Identifier);
    case lexeme::None:
        return std::format_to(ctx.out(), "(NONE)");
    default:
        return std::format_to(ctx.out(), "INTERNAL-ERROR");
    }
}

export namespace autofront{

struct token
{
    std::string_view view;
    source_position pos;
    lexeme type;
};

auto lex_line(std::string_view line,
              std::size_t lineno,
              bool& in_comment,
              std::string& current_comment,
              source_position& current_comment_start,
              std::vector<token>& tokens,
              std::vector<comment>& comments,
              std::vector<error_entry>& errors) -> bool // 模仿 cppfront
{
    auto origial_size = tokens.size();
    auto colno        = 1uz;
    auto it           = line.begin();
    auto bound        = line.end();

    auto pos = [&] {
        return source_position{.lineno = lineno, .colno = colno};
    };
    auto prev = [&](std::size_t n) {
        [[assume(n < colno)]];
        return source_position{.lineno = lineno, .colno = colno - n};
    };
    auto next = [&](std::size_t n) {
        // [[assume(...)]];
        return source_position{.lineno = lineno, .colno = colno + n};
    };

    auto peek = [&](std::size_t n) {
        auto it2 = ranges::next(it, n, bound);
        if (it2 == bound) {
            return char{};
        }
        return *it2;
    };

    auto peeks = std::array<char, 4uz>{};
    for (auto i = 0uz; i < peeks.size(); ++i) {
        peeks[i] = peek(i);
    }

    auto parse_escape = [&](decltype(it) first, char right) -> std::pair<decltype(it), std::string>
    // pre(*first == '\\')
    {
        auto after_first = ranges::next(first);
        if ("'\"?\\abfnrtv"sv.contains(*after_first)) {
            return {ranges::next(first, 2uz), {}};
        } else if ("oxun"sv.contains(*after_first)) {
            auto after_after = ranges::next(after_first);
            if (*after_after != '{') {
                return {after_after, std::format("expected a \"{{\", but {:?} found", *after_after)};
            }
            for (; after_after < bound && *after_after != '}'; ++after_after) {
                if (after_after + 1uz == bound) {
                    return {after_after + 1uz, R"(expected a "}" before the end of line)"};
                }
                if (*after_after == right) {
                    return {after_after, std::format(R"(expected a "}}" before the closing "{}")", right)};
                }
            }
            return {ranges::next(after_after), {}};
        }
        return {first, std::format("unexpected a {:?}, not a legal escape sequence", *after_first)};
    };

    auto iter_next = [&](std::size_t n = 1uz) {
        colno += n;
        ranges::advance(it, n);
        // ranges::shift_left(peeks, 1uz);
        std::shift_left(peeks.begin(), peeks.end(), n);
        // for (auto&& [p, i] : peeks | views::enumerate | views::reverse | views::take(n)) {
        for (auto i = peeks.size() - 1uz; auto&& p : peeks | views::reverse | views::take(n)) {
            peeks[i] = peek(i);
            --i;
        }
    };

    auto store = [&](std::size_t n, lexeme type) {
        // [[assume(n <= ranges::distance(it, bound))]];
        if (n == 0uz) return;
        tokens.push_back({
            .view = {it, n},
            .pos  = pos(),
            .type = type
        });
        iter_next(n - 1uz);
    };
    auto store2 = [&](decltype(it) last, lexeme type) {
        [[assume(last < bound)]]; // 之后会换成并非随机访问迭代器，不过仍然支持比较
        if (last == it) return;
        tokens.push_back({
            .view = {it, last},
            .pos  = pos(),
            .type = type
        });
        iter_next(ranges::distance(it, last) - 1uz);
    };

    for (; it != bound; iter_next()) {
        if (in_comment) {
            if (peeks[0] == '*' && peeks[1] == '/') {
                iter_next();
                current_comment += "*/"sv;
                comments.push_back({
                    .kind  = comment::kinds::stream,
                    .start = current_comment_start,
                    .end   = next(1uz),
                    .text  = std::move(current_comment),
                });
                in_comment = false;
            } else {
                current_comment += peeks[0];
            }
        } else {
            switch (peeks[0]) {
            case '/':
                if (peeks[1] == '*') {
                    current_comment       = "/*";
                    current_comment_start = pos();
                    in_comment            = true;
                    iter_next();
                } else if (peeks[1] == '/') {
                    auto start = pos();
                    auto text  = std::string_view{it, bound};
                    iter_next(ranges::distance(it, ranges::prev(bound)));
                    comments.push_back({
                        .kind  = comment::kinds::line,
                        .start = start,
                        .end   = next(1uz),
                        .text  = std::string{text},
                    });
                } else if (peeks[1] == '/') {
                    store(2uz, lexeme::SlashEq);
                } else {
                    store(1uz, lexeme::Slash);
                }
                break;
            case '<':
                if (peeks[1] == '<') {
                    if (peeks[2] == '=') {
                        store(3uz, lexeme::LeftShiftEq);
                    } else {
                        store(2uz, lexeme::LeftShift);
                    }
                } else if (peeks[1] == '=') {
                    if (peeks[2] == '>') {
                        store(3uz, lexeme::Spaceship);
                    } else {
                        store(2uz, lexeme::LessEq);
                    }
                } else {
                    store(1uz, lexeme::Less);
                }
                break;
            case '>':
                // 这是为了简化尖括号对的解析
                // if (peeks[1] == '>') {
                //     if (peeks[2] == '=') {
                //         store(3uz, lexeme::RightShiftEq);
                //     } else {
                //         store(2uz, lexeme::RightShift);
                //     }
                // } else {
                //     if (peeks[1] == '=') {
                //         store(2uz, lexeme::GreaterEq);
                //     } else {
                /*      */ store(1uz, lexeme::Greater);
                //     }
                // }
                break;
            case '+':
                if (peeks[1] == '+') {
                    store(2uz, lexeme::PlusPlus);
                } else if (peeks[1] == '=') {
                    store(2uz, lexeme::PlusEq);
                } else {
                    store(1uz, lexeme::Plus);
                }
                break;
            case '-':
                if (peeks[1] == '-') {
                    store(2uz, lexeme::MinusMinus);
                } else if (peeks[1] == '=') {
                    store(2uz, lexeme::MinusEq);
                } else if (peeks[1] == '>') {
                    store(2uz, lexeme::Arrow);
                } else {
                    store(1uz, lexeme::Minus);
                }
                break;
            case '|':
                if (peeks[1] == '|') {
                    if (peeks[2] == '=') {
                        store(3uz, lexeme::LogicalOrEq);
                    } else {
                        store(2uz, lexeme::LogicalOr);
                    }
                } else if (peeks[1] == '=') {
                    store(2uz, lexeme::PipeEq);
                } else {
                    store(1uz, lexeme::Pipe);
                }
                break;
            case '&':
                if (peeks[1] == '&') {
                    if (peeks[2] == '=') {
                        store(3uz, lexeme::LogicalAndEq);
                    } else {
                        store(2uz, lexeme::LogicalAnd);
                    }
                } else if (peeks[1] == '=') {
                    store(2uz, lexeme::AmpersandEq);
                } else {
                    store(1uz, lexeme::Ampersand);
                }
                break;
            case '*':
                if (peeks[1] == '=') {
                    store(2uz, lexeme::MultiplyEq);
                } else {
                    store(1uz, lexeme::MultiplyEq);
                }
                break;
            case '%':
                if (peeks[1] == '=') {
                    store(2uz, lexeme::ModuloEq);
                } else {
                    store(1uz, lexeme::Modulo);
                }
                break;
            case '^':
                if (peeks[1] == '=') {
                    store(2uz, lexeme::CaretEq);
                } else {
                    store(1uz, lexeme::Caret);
                }
                break;
            case '~':
                if (peeks[1] == '=') {
                    store(2uz, lexeme::TildeEq);
                } else {
                    store(1uz, lexeme::Tilde);
                }
                break;
            case '=':
                if (peeks[1] == '=') {
                    store(2uz, lexeme::EqualComparison);
                } else {
                    store(1uz, lexeme::Assignment);
                }
                break;
            case '!':
                if (peeks[1] == '=') {
                    store(2uz, lexeme::NotEqualComparison);
                } else {
                    store(1uz, lexeme::Not);
                }
                break;
            case '.':
                if (peeks[1] == '.') {
                    if (peeks[2] == '.') {
                        store(3uz, lexeme::Ellipsis);
                    } else if (peeks[2] == '<') {
                        store(3uz, lexeme::EllipsisLess);
                    } else if (peeks[2] == '=') {
                        store(3uz, lexeme::EllipsisEqual);
                    } else if (peeks[2] == '|') {
                        // store(3uz, lexeme:: );
                        std::println(std::cerr, "To be impl");
                        std::abort();
                    } else {
                        store(2uz, lexeme::DotDot);
                    }
                } else {
                    store(1uz, lexeme::Dot);
                }
                break;
            case ':':
                if (peeks[1] == ':') {
                    store(2uz, lexeme::Scope);
                } else {
                    store(1uz, lexeme::Colon);
                }
                break;
            case '{':
                store(1uz, lexeme::LeftBrace);
                break;
            case '}':
                store(1uz, lexeme::RightBrace);
                break;
            case '(':
                store(1uz, lexeme::LeftParen);
                break;
            case ')':
                store(1uz, lexeme::RightParen);
                break;
            case '[':
                store(1uz, lexeme::LeftBracket);
                break;
            case ']':
                store(1uz, lexeme::RightBracket);
                break;
            case ';':
                store(1uz, lexeme::Semicolon);
                break;
            case ',':
                store(1uz, lexeme::Comma);
                break;
            case '?':
                store(1uz, lexeme::QuestionMark);
                break;
            case '@':
                store(1uz, lexeme::At);
                break;
            case '$':
                store(1uz, lexeme::Dollar);
                break;
            case '0':
                if ("bB"sv.contains(peeks[1])) {
                    auto is_bin = [](char digit) {
                        return "01"sv.contains(digit);
                    };
                    if (is_bin(peeks[2])) {
                        auto iter = ranges::next(it, 3uz, bound);
                        for (; iter < bound && (is_bin(*iter) || *iter == '\''); ++iter);
                        store2(iter, lexeme::BinaryLiteral);
                    } else {
                        errors.push_back({
                            .where = pos(),
                            .message =
                                std::format("binary literal cannot be empty (0{} must be followed by binary digits)",
                                            peeks[1]),
                            .from = std::source_location::current(),
                        });
                        return false; // 实际上可以视情况继续分析
                    }
                } else if ("xX"sv.contains(peeks[1])) {
                    if (std::isxdigit(peeks[2])) {
                        auto iter = ranges::next(it, 3uz, bound);
                        for (; iter < bound && (std::isxdigit(*iter) || *iter == '\''); ++iter);
                        store2(iter, lexeme::HexadecimalLiteral);
                    } else {
                        errors.push_back({
                            .where   = pos(),
                            .message = std::format(
                                "hexadecimal literal cannot be empty (0{} must be followed by hexadecimal digits)",
                                peeks[1]),
                            .from = std::source_location::current(),
                        });
                        return false;
                    }
                }
                [[fallthrough]];
            default:
                if (peeks[0] == 'n' && peeks[1] == 'o' && peeks[2] == 't' && std::isspace(peeks[3])) {
                    store(3uz, lexeme::Not);
                } else if (std::isdigit(peeks[0])) { // 数字字面量，需要完善
                    auto iter = ranges::next(it, 1uz, bound);
                    for (; iter < bound && (std::isdigit(*iter) || *iter == '\''); ++iter);
                    if ((*iter != '.' || (iter + 1 == bound && std::isdigit(iter[1])) && !"eEfF"sv.contains(*iter))) {
                        if (iter < bound && "uU"sv.contains(*iter)) ++iter;
                        if (iter < bound && "lL"sv.contains(*iter)) ++iter;
                        if (iter < bound && "lL"sv.contains(*iter)) ++iter;
                        store2(iter, lexeme::DecimalLiteral);
                    } else {
                        if (*iter == '.') {
                            ++iter;
                            if (iter == bound || !std::isdigit(*iter)) {
                                errors.push_back({
                                    .where   = pos(),
                                    .message = "a floating point literal must have at least one digit "
                                               "after the decimal point (can be '.0')",
                                    .from    = std::source_location::current(),
                                });
                                return false;
                            }
                            for (; iter < bound && (std::isdigit(*iter) || *iter == '\''); ++iter);
                        }
                        if (iter < bound && "eE"sv.contains(*iter)) {
                            ++iter;
                            if (iter < bound && "-+"sv.contains(*iter)) ++iter;
                            for (; iter < bound && (std::isdigit(*iter) || *iter == '\''); ++iter);
                        }
                        if (iter < bound && "fFlL"sv.contains(*iter)) ++iter;
                        store2(iter, lexeme::FloatLiteral);
                    }
                } else if (peeks[0] == '"') { // 字符串字面量
                    auto iter = ranges::next(it, 1uz, bound);
                    while (iter < bound && *iter != '"') {
                        if (*iter == '\\') {
                            auto [escape_end, msg] = parse_escape(iter, '"');
                            if (!msg.empty()) {
                                errors.push_back({
                                    .where   = next(ranges::distance(it, escape_end)),
                                    .message = std::move(msg),
                                    .from    = std::source_location::current(),
                                });
                                return false;
                            }
                            iter = escape_end;
                        } else {
                            ++iter;
                        }
                    }
                    if (iter == bound) {
                        errors.push_back({
                            .where   = next(ranges::distance(it, iter)),
                            .message = std::format("string literal {:?} is missing its closing \"",
                                                   std::string_view{it, iter}),
                            .from    = std::source_location::current(),
                        });
                        return false;
                    }
                    store2(ranges::next(iter), lexeme::StringLiteral);
                } else if (peeks[0] == '\'') { // 字符字面量
                    if (peeks[1] == '\'') {
                        errors.push_back({.where = pos(), .message = "character literal is empty"});
                        return false;
                    }
                    auto missing = 0uz;
                    if (peeks[1] == '\\') {
                        auto [escape_end, msg] = parse_escape(std::next(it), '\'');
                        if (!msg.empty()) {
                            errors.push_back({
                                .where   = next(ranges::distance(it, escape_end)),
                                .message = std::move(msg),
                                .from    = std::source_location::current(),
                            });
                            return false;
                        }
                        if (escape_end < bound && *escape_end == '\'') {
                            ++escape_end;
                            store2(escape_end, lexeme::CharacterLiteral);
                            continue;
                        } else {
                            missing = ranges::distance(it, escape_end);
                        }
                    }
                    if (missing || peeks[1] == char{} || peeks[2] != '\'') {
                        errors.push_back({
                            .where   = next(missing ? missing : 2uz),
                            .message = "character literal is missing its closing \"'\"",
                            .from    = std::source_location::current(),
                        });
                        return false;
                    }
                    store(3uz, lexeme::CharacterLiteral);
                } else if (is_start(peeks[0])) { // 没有考虑字面量后缀，需要完善
                    auto iter = ranges::next(it, 1uz, bound);
                    for (; iter < bound && is_continue(*iter); ++iter);
                    store2(iter, lexeme::Identifier);
                } else if (!std::isspace(peeks[0])) {
                    errors.push_back({
                        .where    = pos(),
                        .message  = std::format("unexcepted text {:?}", peeks[0]),
                        .fallback = true, // a noisy fallback error message // from cppfront
                        .from     = std::source_location::current(),
                    });
                    return false;
                }
            }
        }
    }

    if (in_comment) {
        current_comment += '\n';
    }
    return tokens.size() != origial_size;
}

// AutoFront 同样需要括号跟踪器
// 但与 Cpp2 不同的是，该括号跟踪器是为了更好地解析模板尖括号，并抽象出一些利于后续分析的结构
namespace braces_tracker
{

struct brace;

struct bracket;

struct paren;

struct angle;

using node = std::variant<token, brace, bracket, paren, angle>;

using node_list = std::vector<node>;

struct brace
{
    token left;
    token right;
    node_list nodes;
};

struct bracket
{
    token left;
    token right;
    node_list nodes;
};

struct paren
{
    token left;
    token right;
    node_list nodes;
};

struct angle
{
    token left;
    token right;
    node_list nodes;
};

}

}

namespace autofront
{

export namespace braces_tracker
{

template <typename T>
concept BraceStructure = std::same_as<T, brace> || std::same_as<T, bracket> || std::same_as<T, paren>;

template <BraceStructure Brace>
struct brace_structure_trait;

template <>
struct brace_structure_trait<brace>
{
    static constexpr auto left  = lexeme::LeftBrace;
    static constexpr auto right = lexeme::RightBrace;
};

template <>
struct brace_structure_trait<bracket>
{
    static constexpr auto left  = lexeme::LeftBracket;
    static constexpr auto right = lexeme::RightBracket;
};

template <>
struct brace_structure_trait<paren>
{
    static constexpr auto left  = lexeme::LeftParen;
    static constexpr auto right = lexeme::RightParen;
};

}

namespace braces_tracker
{

template <BraceStructure Brace>
using trait = brace_structure_trait<Brace>;

namespace
{

template <typename T>
concept BraceOrGlobal = BraceStructure<T> || std::same_as<T, node_list>;

template <BraceOrGlobal Brace>
auto track_braces(const token*& it, const token* last, token left, std::vector<error_entry>& errors) -> Brace
// pre(it < last)
{
    auto angles     = std::vector<std::size_t>{};
    auto nodes      = node_list{};
    auto last_token = std::numeric_limits<std::size_t>::max();
    for (; it < last; ++it) {
        auto [view, pos, type] = *it;
        if (type == lexeme::LeftBrace || type == lexeme::LeftBracket || type == lexeme::LeftParen) {
            auto left = *it;
            ++it;
            if (type == lexeme::LeftBrace) {
                nodes.emplace_back(track_braces<brace>(it, last, left, errors));
            } else if (type == lexeme::LeftBracket) {
                nodes.emplace_back(track_braces<bracket>(it, last, left, errors));
            } else if (type == lexeme::LeftParen) {
                nodes.emplace_back(track_braces<paren>(it, last, left, errors));
            }
        } else if (type == lexeme::RightBrace || type == lexeme::RightBracket || type == lexeme::RightParen) {
            if constexpr (std::same_as<Brace, node_list>) {
                errors.push_back({
                    .where    = pos,
                    .message  = std::format("unexpected `{}`", type),
                    .fallback = true,
                    .from     = std::source_location::current(),
                });
                nodes.emplace_back(*it);
            } else {
                if (type == trait<Brace>::right) {
                    return {
                        .left  = left,
                        .right = *it,
                        .nodes = nodes,
                    };
                } else {
                    errors.push_back({
                        .where    = pos,
                        .message  = std::format("expected `{}`, but `{}` found",
                                               trait<Brace>::right,
                                               type),
                        .fallback = true,
                        .from     = std::source_location::current(),
                    });
                    --it;
                    return {
                        .left  = left,
                        .right = {.view = "(None)", .pos = {}, .type = lexeme::None},
                        .nodes = nodes,
                    };
                }
            }
        } else {
            if (type == lexeme::Greater && !angles.empty()) {
                auto i = angles.back();
                angles.pop_back();
                auto in_angle = node_list{};
                in_angle.append_range(nodes | views::drop(i + 1uz));
                nodes.erase(nodes.begin() + (i + 1uz), nodes.end());
                auto left = std::get<token>(nodes.back());
                nodes.pop_back();
                nodes.emplace_back(angle{
                    .left  = left,
                    .right = *it,
                    .nodes = std::move(in_angle),
                });
            } else {
                auto has_merged = false;
                if (last_token != std::numeric_limits<std::size_t>::max() && last_token + 1uz == nodes.size()) {
                    auto& prev_token = std::get<token>(nodes.back());
                    if (prev_token.view.end() == view.begin()) {
                        auto merge = [&](lexeme prev, lexeme current, lexeme merged) {
                            if (prev_token.type == prev && type == current) {
                                prev_token.view = {prev_token.view.begin(), view.end()};
                                prev_token.type = merged;
                                has_merged      = true;
                            }
                        };
                        merge(lexeme::Greater, lexeme::Greater, lexeme::RightShift);
                        merge(lexeme::Greater, lexeme::Assignment, lexeme::GreaterEq);
                        merge(lexeme::RightShift, lexeme::Assignment, lexeme::RightShiftEq);
                    }
                }
                if (!has_merged) {
                    if (type == lexeme::Less) {
                        angles.emplace_back(nodes.size());
                    } else if (type == lexeme::Semicolon) {
                        angles.clear();
                    }
                    last_token = nodes.size();
                    nodes.emplace_back(*it);
                }
            }
        }
    }
    if constexpr (std::same_as<Brace, node_list>) {
        return nodes;
    } else {
        auto back  = std::prev(last);
        auto pos   = back->pos;
        pos.colno += back->view.size();
        errors.push_back({
            .where    = std::prev(last)->pos,
            .message  = std::format("expected `{}`, but not found", trait<Brace>::right),
            .fallback = true,
        });
        return {
            .left  = left,
            .right = {.view = "(Error)", .pos = pos, .type = lexeme::None},
            .nodes = std::move(nodes),
        };
    }
}

}

}

export auto track_braces(std::span<const token> tokens, std::vector<error_entry>& errors) -> braces_tracker::node_list
{
    using namespace braces_tracker;
    auto first = tokens.begin().base();
    auto last  = tokens.end().base();
    return track_braces<node_list>(first, last, {.view = "(Internal-Error)", .pos = {}, .type = lexeme::None}, errors);
}

}

// template <>
// struct std::formatter<autofront::token, char> : public std::formatter<std::string_view, char>
// {
//     auto format(const autofront::token& token, auto&& ctx) const
//     {
//         return std::format_to(ctx.out(), "{}", token.view);
//     }
// };

// template <>
// struct std::formatter<autofront::comment, char> : public std::formatter<std::string_view, char>
// {
//     auto format(const autofront::comment& comment, auto&& ctx) const
//     {
//         return std::format_to(ctx.out(), "{}", comment.text);
//     }
// };

// template <>
// struct std::formatter<autofront::error_entry, char> : public std::formatter<std::string_view, char>
// {
//     auto format(const autofront::error_entry& err, auto&& ctx) const
//     {
//         return std::format_to(ctx.out(), "{}: error: {}", err.where, err.message);
//     }
// };
