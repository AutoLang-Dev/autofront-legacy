export module autofront:codegen;

import std;
import :ast;

export namespace autofront::codegen::ast_llvm
{

class emitting_exception : public std::runtime_error
{
private:
    std::optional<source_span> span_;
    std::source_location loc_;

public:
    emitting_exception(std::string_view msg,
                       std::optional<source_span> span,
                       std::source_location loc = std::source_location::current())
        : runtime_error{std::string{msg}}, span_{span}, loc_{loc}
    {
    }
    emitting_exception(std::string_view msg, std::source_location loc = std::source_location::current())
        : emitting_exception{msg, std::nullopt, loc}
    {
    }

    emitting_exception(std::string_view msg,
                       source_span span,
                       std::source_location loc = std::source_location::current())
        : emitting_exception{msg, std::optional{span}, loc}
    {
    }

    auto span() const
    {
        return span_;
    }

    auto loc() const
    {
        return loc_;
    }
};

enum struct type : std::uint8_t
{
    Unit,
    Bool,
    Int,
    Char,
    Never,
};

auto type_to_name(type ty) -> std::string_view
{
    if (ty == (type::Unit)) return "Unit"sv;
    if (ty == (type::Bool)) return "Bool"sv;
    if (ty == (type::Int)) return "Int"sv;
    if (ty == (type::Char)) return "Char"sv;
    if (ty == (type::Never)) return "Never"sv;
    std::unreachable();
}

auto valuable(type ty) -> bool
{
    if (ty == (type::Unit)) return false;
    if (ty == (type::Never)) return false;
    return true;
}

struct value
{
    enum struct kind : std::uint8_t
    {
        local,
        global,
        literal,
    };

    std::string id;
    type ty;
    kind cata;

    auto emit() const -> std::string
    {
        if (cata == kind::local) return std::format("%{}", id);
        if (cata == kind::global) return std::format("@{}", id);
        return id;
    }

    static auto local(std::string id, type ty) -> value
    {
        return {
            .id   = std::move(id),
            .ty   = ty,
            .cata = kind::local,
        };
    }

    static auto global(std::string id, type ty) -> value
    {
        return {
            .id   = std::move(id),
            .ty   = ty,
            .cata = kind::global,
        };
    }

    static auto literal(std::string val, type ty) -> value
    {
        return {
            .id   = std::move(val),
            .ty   = ty,
            .cata = kind::literal,
        };
    }

    static auto unit() -> value
    {
        return {
            .id   = {},
            .ty   = type::Unit,
            .cata = kind::literal,
        };
    }

    static auto never() -> value
    {
        return {
            .id   = {},
            .ty   = type::Never,
            .cata = kind::literal,
        };
    }
};

struct symbol
{
    std::string name;
    value val;
};

struct funciton
{
    std::string name;
    std::vector<symbol> paras;
    type ret;
};

struct context
{
public:
    using scope_env = std::map<std::string, symbol>;

private:
    std::map<std::string, funciton> fn_decls_;
    std::deque<scope_env> scopes_;
    std::size_t id_counter_{};
    std::ostringstream code_;
    std::size_t indent_{};
    bool new_line_ = true;

    struct entering_guard
    {
    private:
        context& ctx_;

    public:
        entering_guard(context& ctx) : ctx_{ctx}
        {
            ctx_.scopes_.emplace_back();
        }

        ~entering_guard()
        {
            ctx_.scopes_.pop_back();
        }
    };

    struct indent_guard
    {
    private:
        context& ctx_;

    public:
        indent_guard(context& ctx) : ctx_{ctx}
        {
            ++ctx_.indent_;
        }

        ~indent_guard()
        {
            --ctx_.indent_;
        }
    };

    friend entering_guard;

public:
    auto clear() -> void
    {
        scopes_.clear();
        id_counter_ = 0uz;
        code_.str(""sv);
    }

    auto enter() -> entering_guard
    {
        return {*this};
    }

    auto indent() -> indent_guard
    {
        return {*this};
    }

    auto scope() -> scope_env&
    {
        return scopes_.back();
    }

    auto add(symbol sym) -> void
    {
        auto key     = sym.name;
        auto [it, s] = scope().emplace(std::move(key), std::move(sym));
        if (!s) {
            throw emitting_exception{i18n::redefinition(it->first)};
        }
    }

    auto find(std::string_view name) -> symbol*
    {
        for (auto& s : scopes_ | std::views::reverse) {
            // 理论上这里应该用异构查找，但出于未知原因，
            // 标准库没做 std::string 和 std::string_view 的异构比较
            if (auto it = s.find(std::string{name}); it != s.end()) {
                return &it->second;
            }
        }
        return nullptr;
    }

    auto gen_id() -> std::string
    {
        return std::format(".{}", id_counter_++);
    }

    auto gen_id(std::string_view sv) -> std::string
    {
        return std::format("{}.{}", sv, id_counter_++);
    }

    auto print_indent()
    {
        if (new_line_ && indent_) {
            std::print(code_, "  ");
            new_line_ = false;
        }
    }

    auto print_comment_indent()
    {
        if (new_line_) {
            if (indent_) {
                std::print(code_, "  ");
                new_line_ = false;
            }
        } else std::print(code_, " ");
    }

    template <typename... Args>
    auto emitf(std::format_string<Args...> fmt, Args&&... args) -> void
    {
        print_indent();
        std::print(code_, fmt, std::forward<Args>(args)...);
    }

    auto emit(auto&& x) -> void
    {
        print_indent();
        std::print(code_, "{}", x);
    }

    auto emit_label(std::string_view label) -> void
    {
        std::println(code_, "{}:", label);
    }

    auto new_line() -> void
    {
        std::println(code_);
        new_line_ = true;
    }

    template <typename... Args>
    auto commentf(std::format_string<Args...> fmt, Args&&... args) -> void
    {
        print_comment_indent();
        std::print(code_, "; ");
        std::println(code_, fmt, std::forward<Args>(args)...);
        new_line_ = true;
    }

    auto comment(auto&& x) -> void
    {
        print_comment_indent();
        std::println(code_, "; {}", x);
        new_line_ = true;
    }

    auto view() const -> std::string_view
    {
        return code_.view();
    }

    auto str() const -> std::string
    {
        return code_.str();
    }

    auto str() && -> std::string
    {
        return code_.str();
    }

    auto add(funciton fn)
    {
        auto paras = fn.paras | views::transform([](symbol& s) {
                         return s.val.ty;
                     })
                     | ranges::to<std::vector>();
        fn_decls_.emplace(fn.name, fn);
    }

    auto fns() const -> const std::map<std::string, funciton>&
    {
        return fn_decls_;
    }

    auto get_fn_sign(std::string_view name) -> std::optional<funciton>
    {
        // 不用异构查找的原因如上
        auto it = fn_decls_.find(std::string{name});
        if (it == fn_decls_.end()) return std::nullopt;
        return it->second;
    }
};

struct emitter
{
public:
    context ctx;
    std::string filename;
    std::string target_triple;

private:
    std::string last_label;
    
    type this_fn_ret_ = type::Never;

    auto gen_or(std::string_view m)
    {
        if (m.empty()) return ctx.gen_id();
        return std::string{m};
    }

    auto gen_or(std::string_view m, std::string_view sv)
    {
        if (m.empty()) return ctx.gen_id(sv);
        return std::string{m};
    }

    template <typename Key = decltype(i18n::type_mismatch)>
    static auto
    throw_type_mismatch(type ty1, type ty2, Key key = {}, std::source_location loc = std::source_location::current())
    {
        auto tyn1 = type_to_name(ty1);
        auto tyn2 = type_to_name(ty2);
        auto msg  = key(tyn1, tyn2);
        throw emitting_exception{std::move(msg), loc};
    }

    static auto implicity_convertible_to(type from, type to)
    {
        return from == type::Never || from == to;
    }

    static auto has_common_type(type ty1, type ty2) -> bool
    {
        return ty1 == type::Never || ty2 == type::Never || ty1 == ty2;
    }

    static auto common_type(type ty1, type ty2, auto key) -> type
    {
        if (ty1 == type::Never)return ty2;
        if (ty2 == type::Never) return ty1;
        if (ty1 == ty2) return ty1;
        throw_type_mismatch(ty1, ty2, key);
        std::unreachable();
    }

    static auto deref(auto&& ptr, std::source_location loc = std::source_location::current()) -> auto&&
    {
        // 它曾经用于防止解引用空指针，现在有 indirect 防止
        // if (!ptr) {
        //     throw emitting_exception{i18n::deref_null(), loc};
        // }
        return *ptr;
    }

    static auto tr(type ty) -> std::string
    {
        if (ty == type::Unit) return "void";
        if (ty == type::Bool) return "i1";
        if (ty == type::Int) return "i32";
        if (ty == type::Char) return "i21";
        if (ty == type::Never) return "void";
        std::unreachable();
    }

    static auto tr(const ast::name::ptr& name) -> std::string
    {
        return utf::utf32_to_utf8(deref(name).str);
    }

    static auto tr(const ast::underscore::ptr&) -> std::string
    {
        return "_"s;
    }

    static auto tr(const ast::type_name::ptr& ty) -> type
    {
        auto& [n]  = deref(ty);
        auto& [tn] = deref(n);
        if (tn == U"Unit") return type::Unit;
        if (tn == U"Bool") return type::Bool;
        if (tn == U"Int") return type::Int;
        if (tn == U"Char") return type::Char;
        if (tn == U"Never") return type::Never;
        throw emitting_exception{i18n::unknown_type(utf::utf32_to_utf8(tn))};
    }

    static auto tr(const ast::pattern::ptr& pat) -> std::string
    {
        return deref(pat).pat.visit([&](auto& n) {
            return tr(n);
        });
    }

    auto tr(const ast::parameter::ptr& para, std::size_t i) -> symbol
    {
        auto& [pat, type] = deref(para);

        auto id = std::format("arg.{}", i);
        auto ty = tr(type);
        return {
            .name = tr(pat),
            .val  = value::local(std::move(id), ty),
        };
    }

    auto tr(const ast::name::ptr& name, const ast::fn_sign::ptr& sign) -> funciton
    {
        auto& [paras, ret] = deref(sign);

        auto na = tr(name);
        auto ps = std::vector<symbol>{};
        for (auto i = 0uz; auto& para : paras) {
            auto arg = tr(para, i++);
            ps.push_back(std::move(arg));
        }
        auto rt = tr(ret);
        return {
            .name  = std::move(na),
            .paras = std::move(ps),
            .ret   = rt,
        };
    }

    auto new_bb(std::string_view label)
    {
        ctx.emit_label(label);
        last_label = label;
    }
    
    auto emit(const ast::local::ptr& l, std::string_view to = {}) -> value
    {
        auto& [pat, tyn, init] = deref(l);

        auto vna      = tr(pat);
        auto ty       = type::Unit;
        auto id       = std::string{};
        auto init_val = value::unit();
        if (init) {
            auto er = emit(*init, to);
            if (tyn) {
                ty = tr(*tyn);
                if (!implicity_convertible_to(er.ty, ty)) {
                    throw_type_mismatch(er.ty, ty);
                }
            } else ty = er.ty;
            init_val = std::move(er);
        } else if (tyn) {
            ty       = tr(*tyn);
            init_val = value::literal("0", ty);
        } else std::unreachable();
        if (!implicity_convertible_to(ty, type::Unit)) {
            id = gen_or(to);
            ctx.emitf("%{} = add {} {}, 0", id, tr(ty), init_val.emit());
            ctx.new_line();
        }
        if (vna != "_"sv) {
            ctx.add(symbol{
                .name = std::move(vna),
                .val  = value::local(std::move(id), ty),
            });
        }
        return value::unit();
    }

    auto emit(const ast::name::ptr& n) -> value
    {
        auto na  = tr(n);
        auto sym = ctx.find(na);
        if (!sym) throw emitting_exception(i18n::undeclared_ident(na));
        return sym->val;
    }

    auto emit(const ast::lit_bool::ptr& l, std::string_view = {}) -> value
    {
        auto val = deref(l).value;
        return value::literal(std::format("{}", val), type::Bool);
    }

    auto emit(const ast::lit_float::ptr&, std::string_view = {}) -> value
    {
        throw emitting_exception{i18n::not_impl()};
    }

    auto emit(const ast::lit_int::ptr& l, std::string_view = {}) -> value
    {
        auto& lit = deref(l);
        if (!lit.suffix.empty()) {
            throw emitting_exception{i18n::not_impl()};
        }

        auto val = lit.value;
        if (val > std::numeric_limits<std::int32_t>::max()) {
            throw emitting_exception{i18n::int_lit_out_of_lim()};
        }

        return value::literal(std::format("{}", val), type::Int);
    }

    auto emit(const ast::lit_str::ptr&, std::string_view = {}) -> value
    {
        throw emitting_exception{i18n::not_impl()};
    }

    auto emit(const ast::lit_char::ptr& l, std::string_view = {}) -> value
    {
        auto& lit = deref(l);
        if (!lit.suffix.empty()) {
            throw emitting_exception{i18n::not_impl()};
        }

        auto cp = static_cast<std::uint32_t>(lit.value);
        return value::literal(std::format("{}", cp), type::Char);
    }

    auto emit(const ast::lit_expr::ptr& e, std::string_view to = {}) -> value
    {
        return deref(e).lit.visit([&](auto& x) {
            return emit(x, to);
        });
    }

    auto emit(const ast::block_expr::ptr& e, std::string_view to = {}) -> value
    {
        auto& stmts = deref(e).stmts;
        auto ret    = value::unit();
        if (stmts.empty()) return ret;
        for (auto _ = ctx.enter(); auto& stmt : stmts) {
            if (!implicity_convertible_to(ret.ty, type::Unit)) {
                throw_type_mismatch(ret.ty, type::Unit);
            }
            auto to_arg = std::string_view{};
            if (&stmt == &stmts.back()) to_arg = to;
            auto er = emit(stmt, to_arg);
            if (ret.ty != type::Never) ret = er;
        }
        return ret;
    }

    auto emit(const ast::else_expr::ptr& e, std::string_view to = {}) -> value
    {
        return deref(e).body.visit([&](auto& x) {
            return emit(x, to);
        });
    }

    auto emit(const ast::if_expr::ptr& e, std::string_view to = {}) -> value
    {
        auto& [condn, trun, flsn] = deref(e);

        auto cond = emit(condn);
        if (!implicity_convertible_to(cond.ty, type::Bool)) {
            throw_type_mismatch(cond.ty, type::Bool);
        }

        auto trul = ctx.gen_id();
        auto flsl = ctx.gen_id();
        auto merl = ctx.gen_id();
        ctx.emitf("br i1 {}, label %{}, label %{}", cond.emit(), trul, flsl);
        ctx.new_line();

        auto tru = value::unit();
        new_bb(trul);
        tru = emit(trun, to);
        trul = last_label;
        ctx.emitf("br label %{}", merl);
        ctx.new_line();

        auto fls = value::unit();
        new_bb(flsl);
        if (!flsn) fls = value::unit();
        else fls = emit(*flsn, to);
        flsl = last_label;
        ctx.emitf("br label %{}", merl);
        ctx.new_line();

        new_bb(merl);
        auto mer_ty = common_type(tru.ty, fls.ty, i18n::if_incompatible);
        auto mer = value::local(std::string{to}, mer_ty);
        if (valuable(mer.ty) && to.empty()) {
            mer.id = ctx.gen_id();
            ctx.emitf("{} = phi {} [{}, %{}], [{}, %{}]", mer.emit(), tr(mer.ty), tru.emit(), trul, fls.emit(), flsl);
            ctx.new_line();
        }

        return mer;
    }

    auto emit(const ast::while_expr::ptr& e, std::string_view = {}) -> value
    {
        auto& [condn, bodyn] = deref(e);

        auto cond = emit(condn);
        if (!implicity_convertible_to(cond.ty, type::Bool)) {
            throw_type_mismatch(cond.ty, type::Bool);
        }

        auto condl = ctx.gen_id();
        auto bodyl = ctx.gen_id();
        auto endl  = ctx.gen_id();
        ctx.emitf("br label %{}", condl);
        ctx.new_line();
        new_bb(condl);
        ctx.emitf("br i1 {}, label %{}, label %{}", cond.emit(), bodyl, endl);
        ctx.new_line();

        new_bb(bodyl);
        auto body = emit(bodyn);
        if (!implicity_convertible_to(body.ty, type::Unit)) {
            throw_type_mismatch(cond.ty, type::Unit);
        }
        ctx.emitf("br label %{}", condl);
        ctx.new_line();

        return value::unit();
    }

    auto emit_return(const value& ret)
    {
        if (ret.ty == type::Never) {
            ctx.emit("unreachable");
        } else {
            if (!implicity_convertible_to(ret.ty, this_fn_ret_)) {
                throw_type_mismatch(ret.ty, this_fn_ret_);
            }
            if (ret.ty == type::Unit) {
                ctx.emit("ret void");
            } else {
                ctx.emitf("ret {} {}", tr(ret.ty), ret.emit());
            }
        }
        ctx.new_line();
    }

    auto emit(const ast::return_expr::ptr& e, std::string_view to = {}) -> value
    {
        auto ret = value::unit();
        if (auto& expr = deref(e); expr.expr) {
            ret = emit(*expr.expr, to);
        }
        emit_return(ret);
        return value::never();
    }

    auto emit(const ast::break_expr::ptr&, std::string_view = {}) -> value
    {
        return value::never();
    }

    auto emit(const ast::continue_expr::ptr&, std::string_view = {}) -> value
    {
        return value::never();
    }

    auto emit(const ast::paren_expr::ptr& e, std::string_view = {}) -> value
    {
        return emit(deref(e).expr);
    }

    auto emit(const ast::call_expr::ptr& e, std::string_view to = {}) -> value
    {
        auto& [calleen, argsn] = deref(e);

        auto& callee = deref(calleen);
        auto fn      = std::string{};
        if (std::holds_alternative<ast::name::ptr>(callee.expr)) {
            auto& name = std::get<ast::name::ptr>(callee.expr);
            fn         = tr(name);
        } else {
            auto val = emit(calleen);
            throw emitting_exception{i18n::not_callable(type_to_name(val.ty))};
        }

        auto fsymo = ctx.get_fn_sign(fn);
        if (!fsymo) {
            throw emitting_exception{i18n::undeclared_ident(fn)};
        }

        auto fsym = std::move(fsymo).value();
        if (fsym.paras.size() != argsn.size()) {
            throw emitting_exception{i18n::bad_args_count(fn, fsym.paras.size(), argsn.size())};
        }
        auto args = std::vector<value>{};
        for (auto&& [p, a] : views::zip(fsym.paras, argsn)) {
            auto val = emit(a);
            if (!implicity_convertible_to(val.ty, p.val.ty)) {
                throw_type_mismatch(val.ty, p.val.ty);
            }
            args.push_back(val);
        }
        auto as = args | views::filter([](value& arg) {
                      return valuable(arg.ty);
                  })
                  | views::transform([](value& arg) {
                        return std::format("{} {}", tr(arg.ty), arg.emit());
                    })
                  | views::join_with(", "sv) | ranges::to<std::string>();

        auto ret = value::local({}, fsym.ret);
        if (valuable(fsym.ret)) {
            ret.id = gen_or(to);
            ctx.emitf("{} = call {} @{}({})", ret.emit(), tr(fsym.ret), fn, as);
        } else {
            ctx.emitf("call void @{}({})", fn, as);
            if (fsym.ret == type::Never) ctx.emit(" noreturn");
        }
        ctx.new_line();
        return ret;
    }

    auto emit(const ast::bin_expr::ptr&, std::string_view = {}) -> value
    {
        throw emitting_exception{i18n::not_impl()};
    }

    auto emit(const ast::chain_expr::ptr&, std::string_view = {}) -> value
    {
        throw emitting_exception{i18n::not_impl()};
    }

    auto emit(const ast::any_expr::ptr& e, std::string_view = {}) -> value
    {
        return deref(e).expr.visit([&](auto& x) {
            return emit(x);
        });
    }

    auto emit(const ast::expr_stmt::ptr& e, std::string_view = {}) -> value
    {
        auto val = emit(deref(e).expr);
        if (val.ty == type::Never) return value::never();
        return value::unit();
    }

    auto emit(const ast::any_stmt::ptr& stmt, std::string_view = {}) -> value
    {
        return deref(stmt).stmt.visit([&](auto&& x) {
            return emit(x);
        });
    }

    auto emit_fn_body(const ast::block_expr::ptr& body) -> void
    {
        auto _   = ctx.enter();
        auto ret = emit(body);
        emit_return(ret);
    }

    auto emit_fn_body(const ast::asm_block::ptr& body) -> void
    {
        auto& [asms] = deref(body);
        for (auto& line : asms) {
            for (auto& tok : line) {
                ctx.emit(utf::utf32_to_utf8(tok));
            }
            ctx.new_line();
        }
    }

    auto emit_fn_body(std::monostate) -> void {}

    auto emit(const ast::fn_decl::ptr& fn) -> void
    {
        auto& [name, sign, body] = deref(fn);

        auto has_body = !std::holds_alternative<std::monostate>(body);

        auto kw   = has_body ? "define"sv : "declare"sv;
        auto na   = tr(name);
        auto fsym = deref(ctx.get_fn_sign(na));
        auto rt   = tr(fsym.ret);
        auto ps   = fsym.paras | views::filter([](symbol& para) {
                      return valuable(para.val.ty);
                  })
                  | views::transform([](symbol& para) {
                        return std::format("{} {}", tr(para.val.ty), para.val.emit());
                    })
                  | views::join_with(", "sv) | ranges::to<std::string>();

        ctx.emitf("{} {} @{}({})", kw, rt, na, ps);
        if (fsym.ret == type::Never) ctx.emit(" noreturn");

        if (has_body) {
            ctx.emit(" {");
            ctx.new_line();
            {
                auto _ = ctx.enter();
                auto _ = ctx.indent();
                for (auto& para : fsym.paras) {
                    ctx.add(para);
                }
                this_fn_ret_ = fsym.ret;
                body.visit([&](auto& x) {
                    emit_fn_body(x);
                });
            }
            ctx.emit("}");
        }
        ctx.new_line();
    }

public:
    auto emit(const ast::trans_unit::ptr& tu) -> std::string_view
    {
        ctx.clear();

        ctx.comment(i18n::ast_llvm_emit_tu_1());
        ctx.comment(i18n::ast_llvm_emit_tu_2());
        ctx.comment(i18n::ast_llvm_emit_tu_3("https://github.com/AutoLang-Dev/autofront"));
        ctx.new_line();

        if (!filename.empty()) {
            ctx.emitf("source_filename = {:?}", filename);
            ctx.new_line();
        }
        if (!target_triple.empty()) {
            ctx.emitf("target triple = {:?}", target_triple);
            ctx.new_line();
        }
        ctx.new_line();

        auto& [fns] = deref(tu);
        for (auto& fn : fns) {
            auto& [name, sign, _] = deref(fn);
            auto fsym             = tr(name, sign);
            ctx.add(std::move(fsym));
        }
        for (auto& fn : fns) {
            emit(fn);
            ctx.new_line();
        }

        return ctx.view();
    }
};
}
