export module autofront:ast;

import std;
import :common;
import :lex;
import :utf;

template <typename T>
struct is_variant
{
    static constexpr auto value = false;
};

template <typename... Ts>
struct is_variant<std::variant<Ts...>>
{
    static constexpr auto value = true;
};

template <typename T>
struct is_optional
{
    static constexpr auto value = false;
};

template <typename T>
struct is_optional<std::optional<T>>
{
    static constexpr auto value = true;
};

export namespace autofront
{

namespace ast
{

using token = token_tree::token;

template <typename Node>
concept node = requires {
    typename Node::ptr;
    typename Node::vec;
};

#define DEF_ALIASES(T)                                                                                                 \
    using ptr = indirect<T>;                                                                                           \
    using opt = std::optional<ptr>;                                                                                    \
    using vec = std::vector<ptr>;                                                                                      \
    static auto uniq(auto&&... args) -> ptr                                                                            \
    {                                                                                                                  \
        return indirect<T>{std::in_place, std::forward<decltype(args)>(args)...};                                      \
    }

struct if_expr;
struct while_expr;
struct return_expr;
struct break_expr;
struct continue_expr;
struct block_expr;
struct paren_expr;
struct call_expr;
struct bin_expr;
struct chain_expr;

struct underscore
{
    DEF_ALIASES(underscore)
};

struct name
{
    DEF_ALIASES(name)

    std::u32string str;

    static auto need_semi() -> bool
    {
        return true;
    }
};

struct type_name
{
    DEF_ALIASES(type_name)

    name::ptr name;
};

struct pattern
{
    DEF_ALIASES(pattern)

    sum<name, underscore> pat;
};

struct lit_bool
{
    DEF_ALIASES(lit_bool)

    bool value;
};

struct lit_float
{
    DEF_ALIASES(lit_float)

    std::u32string value;
    std::u32string suffix;
};

struct lit_int
{
    enum struct base : std::uint8_t
    {
        bin = 2,
        oct = 8,
        dec = 10,
        hex = 16,
    };

    DEF_ALIASES(lit_int)

    std::uintmax_t value;
    std::u32string suffix;
    base b;
};

struct lit_str
{
    DEF_ALIASES(lit_str)

    std::u32string value;
    std::u32string suffix;
};

struct lit_char
{
    DEF_ALIASES(lit_char)

    char32_t value;
    std::u32string suffix;
};

struct lit_expr
{
    DEF_ALIASES(lit_expr)

    sum<lit_bool, lit_float, lit_int, lit_str, lit_char> lit;

    static auto need_semi() -> bool
    {
        return true;
    }
};

struct operato
{
    DEF_ALIASES(operato)

    std::u32string op;
};

struct any_expr
{
    DEF_ALIASES(any_expr)

    sum<name,
        lit_expr,
        block_expr,
        if_expr,
        while_expr,
        return_expr,
        break_expr,
        continue_expr,
        paren_expr,
        call_expr,
        bin_expr,
        chain_expr>
        expr;

    auto need_semi(this auto&& self) -> bool
    {
        return self.expr.visit([](auto&& x) -> bool {
            return x->need_semi();
        });
    }
};

struct expr_stmt
{
    DEF_ALIASES(expr_stmt)

    any_expr::ptr expr;
};

struct local
{
    DEF_ALIASES(local)

    pattern::ptr pat;
    type_name::opt type;
    any_expr::opt init;
};

struct any_stmt
{
    DEF_ALIASES(any_stmt)

    sum<local, any_expr, expr_stmt> stmt;
};

struct block_expr
{
    DEF_ALIASES(block_expr)

    any_stmt::vec stmts;

    static auto need_semi() -> bool
    {
        return false;
    }
};

struct else_expr
{
    DEF_ALIASES(else_expr)

    sum<block_expr, if_expr> body;
};

struct if_expr
{
    DEF_ALIASES(if_expr)

    any_expr::ptr cond;
    block_expr::ptr tru;
    else_expr::opt fls;

    static auto need_semi() -> bool
    {
        return false;
    }
};

struct while_expr
{
    DEF_ALIASES(while_expr)

    any_expr::ptr cond;
    block_expr::ptr body;

    static auto need_semi() -> bool
    {
        return false;
    }
};

struct return_expr
{
    DEF_ALIASES(return_expr)

    any_expr::opt expr;

    static auto need_semi() -> bool
    {
        return false;
    }
};

struct break_expr
{
    DEF_ALIASES(break_expr)

    static auto need_semi() -> bool
    {
        return false;
    }
};

struct continue_expr
{
    DEF_ALIASES(continue_expr)

    static auto need_semi() -> bool
    {
        return false;
    }
};

struct paren_expr
{
    DEF_ALIASES(paren_expr)

    any_expr::ptr expr;

    static auto need_semi() -> bool
    {
        return true;
    }
};

struct call_expr
{
    DEF_ALIASES(call_expr)

    any_expr::ptr callee;
    any_expr::vec args;

    static auto need_semi() -> bool
    {
        return true;
    }
};

struct bin_expr
{
    DEF_ALIASES(bin_expr)

    any_expr::ptr lhs;
    operato::ptr op;
    any_expr::ptr rhs;

    static auto need_semi() -> bool
    {
        return true;
    }
};

struct chain_expr
{
    DEF_ALIASES(chain_expr)

    std::vector<sum<any_expr, operato>> chain;

    static auto need_semi() -> bool
    {
        return true;
    }
};

struct asm_block
{
    DEF_ALIASES(asm_block)

    std::vector<std::vector<std::u32string>> asms;
};

struct parameter
{
    DEF_ALIASES(parameter)

    pattern::ptr pat;
    type_name::ptr type;
};

struct fn_sign
{
    DEF_ALIASES(fn_sign)

    parameter::vec paras;
    type_name::ptr ret;
};

struct fn_decl
{
    DEF_ALIASES(fn_decl)

    name::ptr name;
    fn_sign::ptr sign;
    optsum<block_expr, asm_block> body;
};

struct trans_unit
{
    DEF_ALIASES(trans_unit)

    fn_decl::vec fns;
};

// 接下来这一部分递归宏来自 https://www.scs.stanford.edu/~dm/blog/va-opt.html

#define EMPTY_PARENS ()

#define EXPAND(...) EXPAND4(EXPAND4(EXPAND4(EXPAND4(__VA_ARGS__))))
#define EXPAND4(...) EXPAND3(EXPAND3(EXPAND3(EXPAND3(__VA_ARGS__))))
#define EXPAND3(...) EXPAND2(EXPAND2(EXPAND2(EXPAND2(__VA_ARGS__))))
#define EXPAND2(...) EXPAND1(EXPAND1(EXPAND1(EXPAND1(__VA_ARGS__))))
#define EXPAND1(...) __VA_ARGS__

#define FOR_EACH(macro, obj, ...) __VA_OPT__(EXPAND(FOR_EACH_HELPER(macro, obj, __VA_ARGS__)))
#define FOR_EACH_HELPER(macro, obj, arg, ...)                                                                          \
    macro(obj, arg) __VA_OPT__(FOR_EACH_AGAIN EMPTY_PARENS(macro, obj, __VA_ARGS__))
#define FOR_EACH_AGAIN() FOR_EACH_HELPER

#define NAIVE_PRINT_FIELD(obj, tag) naive_print_field(#tag, obj->tag);

#define NAIVE_PRINT_DEF(T, ...)                                                                                        \
    auto naive_print(const T::ptr& node) -> void                                                                       \
    {                                                                                                                  \
        print("{} ", #T);                                                                                              \
        indently_print([&] -> void {                                                                                   \
            FOR_EACH(NAIVE_PRINT_FIELD, node, __VA_ARGS__)                                                             \
        });                                                                                                            \
    }

struct naive_printer : indent_printer
{
    template <typename T>
    auto naive_print(const std::vector<T>& vec)
    {
        if (vec.empty()) {
            print("[]");
            return;
        }
        println("[");
        {
            auto _ = indent();

            for (auto&& x : vec) {
                naive_print(x);
                println(",");
            }
        }
        print("]");
    }

    template <typename... Ts>
    auto naive_print(const std::variant<Ts...>& x) -> void
    {
        x.visit([&](auto& x) {
            naive_print(x);
        });
    }

    template <typename T>
    auto naive_print(const std::optional<T>& x) -> void
    {
        if (x) {
            naive_print(*x);
        } else {
            print("null");
        }
    }

    template <typename T>
    auto naive_print_field(std::string_view tag, T& x) -> void
    {
        print("{}: ", tag);
        naive_print(x);
        println(",");
    }

    auto naive_print(std::monostate)
    {
        print("null");
    }

    auto naive_print(bool flag)
    {
        print("{}", flag);
    }

    auto naive_print(std::uintmax_t x)
    {
        print("{}", x);
    }

    auto naive_print(std::u32string_view sv)
    {
        print("{:?}", utf::utf32_to_utf8(sv));
    }

    auto naive_print(lit_int::base base)
    {
        print("{}", std::to_underlying(base));
    }

    auto naive_print(char32_t ch)
    {
        naive_print(std::u32string_view{&ch, 1uz});
    }

    NAIVE_PRINT_DEF(underscore)

    NAIVE_PRINT_DEF(name, str)

    NAIVE_PRINT_DEF(pattern, pat)

    NAIVE_PRINT_DEF(parameter, pat, type)

    NAIVE_PRINT_DEF(type_name, name)

    NAIVE_PRINT_DEF(operato, op)

    NAIVE_PRINT_DEF(fn_sign, paras, ret)

    NAIVE_PRINT_DEF(if_expr, cond, tru, fls)

    NAIVE_PRINT_DEF(else_expr, body)

    NAIVE_PRINT_DEF(while_expr, cond, body)

    NAIVE_PRINT_DEF(return_expr, expr)

    NAIVE_PRINT_DEF(break_expr)

    NAIVE_PRINT_DEF(continue_expr)

    NAIVE_PRINT_DEF(paren_expr, expr)

    NAIVE_PRINT_DEF(call_expr, callee, args)

    NAIVE_PRINT_DEF(bin_expr, lhs, op, rhs)

    NAIVE_PRINT_DEF(chain_expr, chain)

    NAIVE_PRINT_DEF(lit_bool, value)

    NAIVE_PRINT_DEF(lit_float, value, suffix)

    NAIVE_PRINT_DEF(lit_int, value, suffix)

    NAIVE_PRINT_DEF(lit_str, value, suffix)

    NAIVE_PRINT_DEF(lit_char, value, suffix)

    NAIVE_PRINT_DEF(lit_expr, lit)

    NAIVE_PRINT_DEF(any_expr, expr)

    NAIVE_PRINT_DEF(expr_stmt, expr)

    NAIVE_PRINT_DEF(local, pat, type, init)

    NAIVE_PRINT_DEF(any_stmt, stmt)

    NAIVE_PRINT_DEF(block_expr, stmts)

    NAIVE_PRINT_DEF(asm_block, asms)

    NAIVE_PRINT_DEF(fn_decl, name, sign, body)

    auto naive_print(const trans_unit::ptr& tu)
    {
        naive_print(tu->fns);
        println();
    }

    auto operator()(const trans_unit::ptr& d)
    {
        naive_print(d);
    }
};

struct pretty_printer : indent_printer
{
    auto pretty_println(auto&& e)
    {
        pretty_print(e);
        println(",");
    }

    auto pretty_print(std::string_view tag, auto&& x)
    {
        print("{}: ", tag);
        pretty_print(x);
        println(",");
    }

    template <typename T>
    auto pretty_print(const std::vector<T>& vec)
    {
        if (vec.empty()) {
            print("[]");
            return;
        }
        println("[");
        {
            auto _ = indent();

            for (auto&& x : vec) {
                pretty_print(x);
                println(",");
            }
        }
        print("]");
    }

    template <typename... Ts>
    auto pretty_print(const std::variant<Ts...>& x) -> void
    {
        x.visit([&](auto& x) -> void {
            pretty_print(x);
        });
    }

    template <typename T>
    auto pretty_print(const std::optional<T>& x) -> void
    {
        if (x) {
            pretty_print(*x);
        } else {
            print("{{}}");
        }
    }

    auto pretty_print(std::monostate)
    {
        print("{{}}");
    }

    auto pretty_print(bool flag)
    {
        print("{}", flag);
    }

    auto pretty_print(char32_t ch)
    {
        print("Unicode(U+{:X})", static_cast<std::uint32_t>(ch));
    }

    auto pretty_print(std::integral auto x)
    {
        print("{}", x);
    }

    auto pretty_print(lit_int::base base)
    {
        print("Base({})", std::to_underlying(base));
    }

    auto pretty_print(std::u32string_view sv)
    {
        print("{:?}", utf::utf32_to_utf8(sv));
    }

    auto pretty_print(const underscore::ptr&)
    {
        print("Underscore {{}}");
    }

    auto pretty_print(const name::ptr& n)
    {
        print("Name");
        print("({:?})", utf::utf32_to_utf8(n->str));
    }

    auto pretty_print(const pattern::ptr& p)
    {
        print("Pattern ");
        indently_print([&] {
            pretty_println(p->pat);
        });
    }

    auto pretty_print(const type_name::ptr& t)
    {
        print("Type ");
        indently_print([&] {
            pretty_println(t->name);
        });
    }

    auto pretty_print(const operato::ptr& t)
    {
        print("Operator ");
        indently_print([&] {
            pretty_println(t->op);
        });
    }

    auto pretty_print(const lit_bool::ptr& e)
    {
        print("LitBool ");
        indently_print([&] {
            pretty_print("value", e->value);
        });
    }

    auto pretty_print(const lit_float::ptr& e)
    {
        print("LitFloat ");
        indently_print([&] {
            pretty_print("value", e->value);
            pretty_print("suffix", e->suffix);
        });
    }

    auto pretty_print(const lit_int::ptr& e)
    {
        print("LitInt ");
        indently_print([&] {
            pretty_print("value", e->value);
            pretty_print("suffix", e->suffix);
            pretty_print("base", e->b);
        });
    }

    auto pretty_print(const lit_str::ptr& e)
    {
        print("LitStr ");
        indently_print([&] {
            pretty_print("value", e->value);
            pretty_print("suffix", e->suffix);
        });
    }

    auto pretty_print(const lit_char::ptr& e)
    {
        print("LitChar ");
        indently_print([&] {
            pretty_print("value", e->value);
            pretty_print("suffix", e->suffix);
        });
    }

    auto pretty_print(const lit_expr::ptr& e)
    {
        print("Lit ");
        indently_print([&] {
            pretty_println(e->lit);
        });
    }

    auto pretty_print(const any_expr::ptr& e)
    {
        pretty_print(e->expr);
    }

    auto pretty_print(const expr_stmt::ptr& s)
    {
        print("ExprStmt ");
        indently_print([&] {
            pretty_println(s->expr);
        });
    }

    auto pretty_print(const local::ptr& l)
    {
        print("Local ");
        indently_print([&] {
            pretty_print("pat", l->pat);
            pretty_print("type", l->type);
            pretty_print("init", l->init);
        });
    }

    auto pretty_print(const any_stmt::ptr& s)
    {
        pretty_print(s->stmt);
    }

    auto pretty_print(const block_expr::ptr& e)
    {
        print("BlockExpr ");
        pretty_print(e->stmts);
    }

    auto pretty_print(const else_expr::ptr& e)
    {
        pretty_print(e->body);
    }

    auto pretty_print(const if_expr::ptr& e)
    {
        print("IfExpr ");
        indently_print([&] {
            pretty_print("cond", e->cond);
            pretty_print("body", e->tru);
        });
    }

    auto pretty_print(const while_expr::ptr& e)
    {
        print("WhileExpr ");
        indently_print([&] {
            pretty_print("cond", e->cond);
            pretty_print("body", e->body);
        });
    }

    auto pretty_print(const return_expr::ptr& e)
    {
        print("ReturnExpr ");
        indently_print([&] {
            pretty_println(e->expr);
        });
    }

    auto pretty_print(const break_expr::ptr&)
    {
        print("BreakExpr {{}}");
    }

    auto pretty_print(const continue_expr::ptr&)
    {
        print("ContinueExpr {{}}");
    }

    auto pretty_print(const paren_expr::ptr& e)
    {
        print("ParenExpr ");
        indently_print([&] {
            pretty_println(e->expr);
        });
    }

    auto pretty_print(const call_expr::ptr& e)
    {
        print("CallExpr ");
        indently_print([&] {
            pretty_print("callee", e->callee);
            pretty_print("args", e->args);
        });
    }

    auto pretty_print(const bin_expr::ptr& e)
    {
        print("BinExpr ");
        indently_print([&] {
            pretty_print("lhs", e->lhs);
            pretty_print("op", e->op);
            pretty_print("rhs", e->rhs);
        });
    }

    auto pretty_print(const chain_expr::ptr& e)
    {
        print("ChainExpr ");
        indently_print([&] {
            pretty_println(e->chain);
        });
    }

    auto pretty_print(const parameter::ptr& p)
    {
        print("Parameter ");
        indently_print([&] {
            pretty_print("pat", p->pat);
            pretty_print("type", p->type);
        });
    }

    auto pretty_print(const asm_block::ptr& b)
    {
        print("AsmBlock ");
        indently_print([&] {
            pretty_println(b->asms);
        });
    }

    auto pretty_print(const fn_sign::ptr& s)
    {
        print("FnSign ");
        indently_print([&] {
            pretty_print("paras", s->paras);
            pretty_print("ret", s->ret);
        });
    }

    auto pretty_print(const fn_decl::ptr& d)
    {
        print("FnDecl ");
        indently_print([&] {
            pretty_print("name", d->name);
            pretty_print("sign", d->sign);
            pretty_print("body", d->body);
        });
    }

    auto pretty_print(const trans_unit::ptr& tu)
    {
        print("TransUnit ");
        pretty_print(tu->fns);
    }
};

};

}
