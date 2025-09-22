export module autofront:parse;

import std;
import :common;
import :lex;
import :ast;

template <typename>
struct is_unique_ptr
{
    static constexpr auto value = false;
};

template <typename T, typename D>
struct is_unique_ptr<std::unique_ptr<T, D>>
{
    static constexpr auto value = true;
};

template <typename T>
constexpr auto is_unique_ptr_v = is_unique_ptr<T>::value;

export namespace autofront
{

class parsing_exception : public std::runtime_error
{
    using runtime_error::runtime_error;
};

template <typename T>
struct parsing_success
{
    T value;
};

struct parsing_failure
{
    error_entry error;
};

template <typename T>
struct parsing_promise;

template <typename T>
struct [[nodiscard]] parsing
{
    using promise_type = parsing_promise<T>;

    unique_coro<promise_type> handle_;

    auto promise() const noexcept -> promise_type&
    {
        return handle_.promise();
    }

    parsing(promise_type::handle_t handle) : handle_{handle} {}

    auto resume(const token_tree::group* nodes, std::size_t idx = 0uz) const noexcept -> void
    {
        promise().nodes_   = nodes;
        promise().cur_idx_ = idx;
        handle_.resume();
    }

    parsing()          = default;
    parsing(parsing&&) = default;

    auto operator=(parsing&&) -> parsing& = default;

    auto is_stateful() const noexcept -> bool
    {
        return static_cast<bool>(handle_);
    }

    auto finished() const noexcept -> bool
    {
        if (!is_stateful()) return false;
        if (promise().exception_) return true;
        return !std::holds_alternative<std::monostate>(promise().result_);
    }

    auto has_value() const noexcept -> bool
    {
        if (!is_stateful()) return false;
        if (promise().exception_) return false;
        return std::holds_alternative<parsing_success<T>>(promise().result_);
    }

    auto has_error() const noexcept -> bool
    {
        if (!is_stateful()) return false;
        if (promise().exception_) return false;
        return std::holds_alternative<parsing_failure>(promise().result_);
    }

    auto take_result() const -> std::expected<T, error_entry>
    {
        if (!is_stateful()) {
            throw parsing_exception{i18n::stateless_parsing()};
        }
        if (auto& e = promise().exception_) {
            std::rethrow_exception(std::move(e));
        }
        if (!finished()) {
            throw parsing_exception{i18n::unfinished_parsing()};
        }
        if (has_value()) {
            return std::move(std::get<parsing_success<T>>(promise().result_).value);
        } else {
            return std::unexpected{std::move(std::get<parsing_failure>(promise().result_).error)};
        }
    }
};

struct has_next_t
{
} has_next;

template <typename... Args>
struct uniqueing
{
    std::tuple<Args...> args;
};

template <typename... Args>
auto uniq(Args&&... args) -> uniqueing<Args&&...>
{
    return {
        .args = std::tuple<Args&&...>{std::forward<Args>(args)...},
    };
}

struct failure
{
    std::string msg;
    std::optional<std::source_location> loc;
};

template <typename... Args>
auto fail(std::string msg) -> failure
{
    return {
        .msg = std::move(msg),
    };
}

template <typename... Args>
auto fail(std::source_location loc, std::string msg) -> failure
{
    return {
        .msg = std::move(msg),
        .loc = loc,
    };
}

template <typename T>
struct subtreeing
{
    parsing<T> parsing;
};

template <typename T>
auto subtree(parsing<T> parsing) -> subtreeing<T>
{
    return {.parsing = std::move(parsing)};
}

struct suspend_or_not
{
private:
    bool flag_;

public:
    suspend_or_not(bool flag) : flag_{flag} {}
    suspend_or_not(std::suspend_always) : flag_{true} {}
    suspend_or_not(std::suspend_never) : flag_{false} {}

    constexpr auto await_ready() const noexcept -> bool
    {
        return !flag_;
    }

    constexpr auto await_suspend(std::coroutine_handle<>) const noexcept -> void {}

    constexpr auto await_resume(this auto&& self) noexcept -> void {}
};

template <typename T>
struct parsing_promise
{
    using handle_t = std::coroutine_handle<parsing_promise>;

    std::exception_ptr exception_;
    std::variant<std::monostate, parsing_success<T>, parsing_failure> result_;
    const token_tree::group* nodes_;
    std::size_t cur_idx_;

    auto peek_node(std::size_t i = 0uz) -> const token_tree*
    {
        if (i > -1uz - cur_idx_) {
            return nullptr;
        }
        return nodes_->children(cur_idx_ + i);
    }

    auto current_pos() -> source_position
    {
        auto child = peek_node();
        if (child) return child->span().start;
        return nodes_->close().start;
    }

    auto get_return_object() -> parsing<T>
    {
        return parsing<T>{handle_t::from_promise(*this)};
    }

    auto return_value(T ret)
    {
        result_ = parsing_success<T>{std::forward<decltype(ret)>(ret)};
    }

    template <typename U>
        requires std::constructible_from<T, U&&>
    auto return_value(U&& ret) -> void
    {
        result_ = parsing_success<T>{
            .value = T{std::forward<U>(ret)},
        };
    }

    template <typename... Args>
        requires requires(Args... args) {
            requires is_unique_ptr_v<T>;
            typename T::element_type;
            std::make_unique<typename T::element_type>(std::forward<Args>(args)...);
        } // && std::constructible_from<typename T::element_type, Args...>
    auto return_value(uniqueing<Args...> f) -> void
    {
        result_ = parsing_success<T>{
            .value = std::apply(
                [](Args... args) {
                    return std::make_unique<typename T::element_type>(std::forward<Args>(args)...);
                },
                std::move(f.args)),
        };
    }

    auto return_value(parsing_failure error) -> void
    {
        result_ = std::move(error);
    }

    auto return_value(failure f, std::source_location l = std::source_location::current()) -> void
    {
        auto error = error_entry{
            .where   = current_pos(),
            .message = std::move(f.msg),
            .from    = f.loc.value_or(l),
        };
        result_ = parsing_failure{.error = std::move(error)};
    }

    auto initial_suspend() const noexcept -> std::suspend_always
    {
        return {};
    }

    auto final_suspend() const noexcept -> std::suspend_always
    {
        return {};
    }

    auto unhandled_exception() -> void
    {
        exception_ = std::current_exception();
    }

    auto await_transform(this_promise_t) -> just_awaitable<parsing_promise&>
    {
        return {*this};
    }

    auto await_transform(has_next_t) -> just_awaitable<bool>
    {
        return {cur_idx_ < nodes_->children().size()};
    }

    auto await_transform(failure f, std::source_location l = std::source_location::current())
        -> just_awaitable<error_entry>
    {
        auto error = error_entry{
            .where   = current_pos(),
            .message = std::move(f.msg),
            .from    = f.loc.value_or(l),
        };
        return {std::move(error)};
    }

    template <typename... Args>
        requires is_unique_ptr_v<T>
    auto await_transform(uniqueing<Args...> f)
    {
        return just_awaitable{
            std::apply(
                [](Args... args) {
                    return std::make_unique<T>(std::forward<Args>(args)...);
                },
                std::move(f.args)),
        };
    }

    template <typename U>
    struct parsing_transformed_awaitable
    {
        parsing<U> parsing_;
        parsing_promise& promise_;

        auto await_ready() const -> bool
        {
            if (!parsing_.is_stateful()) {
                throw parsing_exception{i18n::await_stateless_parsing()};
            }
            return false;
        }

        auto await_suspend(handle_t handle) const -> bool
        {
            assert_(parsing_.is_stateful(), i18n::await_stateless_parsing());
            assert_(&promise_ == &handle.promise(), i18n::unequal_coro());

            auto&& pp = parsing_.promise();
            parsing_.resume(promise_.nodes_, promise_.cur_idx_);

            assert_(parsing_.finished(), i18n::unfinished_parsing());

            if (parsing_.has_error()) {
                promise_.result_ = std::move(std::get<parsing_failure>(pp.result_));
                return true;
            }
            if (pp.exception_) {
                promise_.exception_ = std::move(pp.exception_);
                return true;
            }
            return false;
        }

        auto await_resume() noexcept -> U
        {
            assert_(parsing_.has_value(), i18n::failed_parsing());

            auto&& pp = parsing_.promise();
            if (promise_.nodes_ == pp.nodes_) {
                promise_.cur_idx_ = pp.cur_idx_;
            } else {
                if (promise_.cur_idx_ < promise_.nodes_->size()) {
                    ++promise_.cur_idx_;
                }
            }

            return std::move(std::get<parsing_success<U>>(pp.result_).value);
        }
    };

    template <typename U>
    auto await_transform(parsing<U> p) -> parsing_transformed_awaitable<U>
    {
        return {
            .parsing_ = std::move(p),
            .promise_ = *this,
        };
    }

    template <typename U>
    struct subtreeing_transformed_awaitable
    {
        parsing<U> parsing_;
        parsing_promise& promise_;
        std::source_location loc_;

        auto await_ready() const -> bool
        {
            if (!parsing_.is_stateful()) {
                throw parsing_exception{i18n::await_stateless_parsing()};
            }

            return false;
        }

        auto await_suspend(handle_t handle) const -> bool
        {
            assert_(parsing_.is_stateful(), i18n::await_stateless_parsing());
            assert_(&promise_ == &handle.promise(), i18n::unequal_coro());

            auto subtree = promise_.nodes_->children(promise_.cur_idx_);
            assert_(subtree->get_kind() != token_tree::kind::none, i18n::expected_nntt());
            auto group = subtree->get_group();
            if (!group) {
                auto error = error_entry{
                    .where   = promise_.current_pos(),
                    .message = i18n::expected_group(subtree->get_token()->type()),
                    .from    = loc_,
                };
                promise_.result_ = parsing_failure{
                    .error = std::move(error),
                };
            }
            auto&& pp = parsing_.promise();
            parsing_.resume(group);

            assert_(parsing_.finished(), i18n::unfinished_parsing());

            if (parsing_.has_error()) {
                promise_.result_ = std::move(std::get<parsing_failure>(pp.result_));
                return true;
            }
            if (pp.exception_) {
                promise_.exception_ = std::move(pp.exception_);
                return true;
            }
            return false;
        }

        auto await_resume() noexcept -> U
        {
            assert_(parsing_.has_value(), i18n::failed_parsing());

            auto&& pp = parsing_.promise();
            if (promise_.nodes_ == pp.nodes_) {
                promise_.cur_idx_ = pp.cur_idx_;
            } else {
                if (promise_.cur_idx_ < promise_.nodes_->size()) {
                    ++promise_.cur_idx_;
                }
            }

            return std::move(std::get<parsing_success<U>>(pp.result_).value);
        }
    };

    template <typename U>
    auto await_transform(subtreeing<U> p, std::source_location l = std::source_location::current())
        -> subtreeing_transformed_awaitable<U>
    {
        return {
            .parsing_ = std::move(p.parsing),
            .promise_ = *this,
            .loc_     = l,
        };
    }
};

auto peek_node(std::size_t n = 0uz, std::source_location loc = std::source_location::current())
    -> parsing<const token_tree*>
{
    auto node = (co_await this_promise).peek_node(n);
    if (!node) co_return fail(loc, i18n::reached_end());
    co_return node;
}

auto is_token_of(lexeme l) -> parsing<bool>
{
    auto node  = co_await peek_node();
    auto token = node->get_token();
    if (!token) co_return false;
    co_return token->type() == l;
}

auto is_token() -> parsing<bool>
{
    auto node = co_await peek_node();
    co_return node->get_kind() == token_tree::kind::token;
}

auto is_group_of(token_tree::delimiter delim) -> parsing<bool>
{
    auto node  = co_await peek_node();
    auto group = node->get_group();
    if (!group) co_return false;
    co_return group->delim() == delim;
}

auto is_group() -> parsing<bool>
{
    auto node = co_await peek_node();
    co_return node->get_kind() == token_tree::kind::group;
}

auto peek_token(std::size_t i = 0uz, std::source_location loc = std::source_location::current())
    -> parsing<const token_tree::token*>
{
    auto node = co_await peek_node(i, loc);
    if (!node) co_return nullptr;
    if (auto tok = node->get_token()) {
        co_return tok;
    }
    co_return nullptr;
}

auto peek_lexeme(std::size_t i = 0uz, std::source_location loc = std::source_location::current()) -> parsing<lexeme>
{
    auto node = co_await peek_node(i, loc);
    if (node->get_kind() == token_tree::kind::group) {
        co_return lexeme::Group;
    }
    if (node->get_kind() == token_tree::kind::none) {
        co_return lexeme::None;
    }
    co_return node->get_token()->type();
}

auto current_pos() -> parsing<source_position>
{
    auto&& promise = co_await this_promise;
    co_return promise.current_pos();
}

auto next(std::size_t n = 1uz) -> parsing<unit>
{
    auto&& promise = co_await this_promise;
    auto group     = promise.nodes_;
    assert_(group, i18n::expected_nnn());
    if (group->size() - promise.cur_idx_ >= n) {
        promise.cur_idx_ += n;
    } else {
        promise.cur_idx_ = group->size();
    }
    co_return unit{};
}

auto parse_token(std::source_location loc = std::source_location::current()) -> parsing<const token_tree::token*>
{
    auto tok = co_await peek_token();
    if (!tok) {
        co_return fail(loc, i18n::expected_token_group_found());
    }
    co_await next();
    co_return tok;
}

auto parse_token(lexeme l, std::source_location loc = std::source_location::current())
    -> parsing<const token_tree::token*>
{
    auto tok = co_await peek_token(0uz, loc);
    if (!tok) {
        co_return fail(loc, i18n::expected_token_group_found2(l));
    }
    if (auto ty = tok->type(); ty != l) {
        co_return fail(loc, i18n::expected_found(l, ty));
    }
    co_await next();
    co_return tok;
}

auto is_end() -> parsing<bool>
{
    auto&& promise = co_await this_promise;
    co_return !(promise.cur_idx_ < promise.nodes_->size());
}

auto parse_end(std::source_location loc = std::source_location::current()) -> parsing<unit>
{
    if (co_await is_end()) {
        co_return unit{};
    }
    if (co_await is_token()) {
        co_return fail(loc, i18n::unexpected(co_await peek_lexeme()));
    }
    co_return fail(loc, i18n::unexpected_group());
}

auto parse_bound(std::source_location loc = std::source_location::current()) -> parsing<unit>
{
    if (co_await is_end()) {
        co_return unit{};
    }
    if (co_await is_token()) {
        if (is_close_paren(co_await peek_lexeme())) {
            co_return unit{};
        }
        co_return fail(loc, i18n::unexpected(co_await peek_lexeme()));
    }
    co_return fail(loc, i18n::unexpected_group());
}

auto nothing() -> parsing<unit>
{
    co_return unit{};
}

template <typename T>
auto parse_group(parsing<T> p, token_tree::delimiter delim) -> parsing<T>
{
    static const auto delim_map = std::flat_map<token_tree::delimiter, std::string_view>{
        {token_tree::delimiter::angle,   "<>"sv},
        {token_tree::delimiter::brace,   "{}"sv},
        {token_tree::delimiter::bracket, "[]"sv},
        {token_tree::delimiter::none,    {}    },
        {token_tree::delimiter::paren,   "()"sv},
    };

    assert_(p.is_stateful(), i18n::stateless_parsing());

    auto node  = co_await peek_node();
    auto group = node->get_group();
    if (!group) {
        auto sv = delim_map.at(delim);
        co_return fail(i18n::expected_group_of(sv.front(), sv.back()));
    }

    co_return co_await subtree(std::move(p));
}

template <typename T>
auto parse_brace(parsing<T> p) -> parsing<T>
{
    return parse_group(std::move(p), token_tree::delimiter::brace);
}

template <typename T>
auto parse_bracket(parsing<T> p) -> parsing<T>
{
    return parse_group(std::move(p), token_tree::delimiter::bracket);
}

template <typename T>
auto parse_paren(parsing<T> p) -> parsing<T>
{
    return parse_group(std::move(p), token_tree::delimiter::paren);
}

template <typename T>
auto parse_angle(parsing<T> p) -> parsing<T>
{
    return parse_group(std::move(p), token_tree::delimiter::angle);
}

auto parse_underscore() -> parsing<ast::underscore::ptr>
{
    co_await parse_token(lexeme::Underscore);
    co_return uniq();
}

auto parse_name() -> parsing<ast::name::ptr>
{
    auto tok = co_await parse_token(lexeme::Ident);
    co_return uniq(tok->str());
}

auto is_pattern() -> parsing<bool>
{
    auto l = co_await peek_lexeme();
    co_return l == lexeme::Ident || l == lexeme::Underscore;
}

auto parse_pattern() -> parsing<ast::pattern::ptr>
{
    if (co_await is_token_of(lexeme::Underscore)) {
        co_return uniq(co_await parse_underscore());
    }
    co_return uniq(co_await parse_name());
}

auto parse_type_name() -> parsing<ast::type_name::ptr>
{
    auto name = co_await parse_name();
    co_return uniq(std::move(name));
}

auto parse_parameter() -> parsing<ast::parameter::ptr>
{
    auto pat = co_await parse_pattern();
    co_await parse_token(lexeme::Colon);
    auto type = co_await parse_type_name();
    co_return uniq(std::move(pat), std::move(type));
}

auto parse_parameter_list() -> parsing<ast::parameter::vec>
{
    auto paras = ast::parameter::vec{};
    co_await parse_token(lexeme::LeftParen);
    while (co_await is_pattern()) {
        paras.emplace_back(co_await parse_parameter());
        if (co_await is_token_of(lexeme::Comma)) {
            co_await parse_token(lexeme::Comma);
        } else break;
    }
    co_await parse_token(lexeme::RightParen);
    co_await parse_end();
    co_return paras;
}

auto parse_fn_sign() -> parsing<ast::fn_sign::ptr>
{
    auto paras = co_await parse_paren(parse_parameter_list());
    co_await parse_token(lexeme::Arrow);
    auto ret = co_await parse_type_name();
    co_return uniq(std::move(paras), std::move(ret));
}

auto parse_any_expr() -> parsing<ast::any_expr::ptr>;

auto parse_block_expr() -> parsing<ast::block_expr::ptr>;

auto parse_else_expr() -> parsing<ast::else_expr::ptr>;

auto parse_if_expr() -> parsing<ast::if_expr::ptr>
{
    co_await parse_token(lexeme::If);
    auto cond = co_await parse_any_expr();
    auto tru  = co_await parse_block_expr();
    auto fls  = co_await parse_else_expr();
    co_return uniq(std::move(cond), std::move(tru), std::move(fls));
}

auto parse_else_expr() -> parsing<ast::else_expr::ptr>
{
    if (!co_await is_token_of(lexeme::Else)) co_return nullptr;
    co_await parse_token(lexeme::Else);
    if (co_await is_group_of(token_tree::delimiter::brace)) {
        co_return uniq(co_await parse_block_expr());
    } else if (co_await is_token_of(lexeme::If)) {
        co_return uniq(co_await parse_if_expr());
    } else {
        co_return fail(i18n::after_else());
    }
}

auto parse_while_expr() -> parsing<ast::while_expr::ptr>
{
    co_await parse_token(lexeme::While);
    auto cond = co_await parse_any_expr();
    auto body = co_await parse_block_expr();
    co_return uniq(std::move(cond), std::move(body));
}

auto is_bound_token() -> parsing<bool>
{
    if (co_await is_end()) co_return true;
    auto l = co_await peek_lexeme();
    if (l == lexeme::Semicolon) co_return true;
    if (is_close_paren(l)) co_return true;
    co_return false;
}

auto parse_return_expr() -> parsing<ast::return_expr::ptr>
{
    co_await parse_token(lexeme::Return);
    auto expr = ast::any_expr::ptr{};
    if (!co_await is_bound_token()) {
        expr = co_await parse_any_expr();
    }
    co_return uniq(std::move(expr));
}

auto parse_break_expr() -> parsing<ast::break_expr::ptr>
{
    co_await parse_token(lexeme::Break);
    co_return uniq();
}

auto parse_continue_expr() -> parsing<ast::continue_expr::ptr>
{
    co_await parse_token(lexeme::Continue);
    co_return uniq();
}

auto parse_paren_expr() -> parsing<ast::paren_expr::ptr>
{
    co_return uniq(co_await parse_paren(parse_any_expr()));
}

auto parse_expr_list() -> parsing<ast::any_expr::vec>
{
    auto exprs = ast::any_expr::vec{};
    co_await parse_token(lexeme::LeftParen);
    while (!co_await is_token_of(lexeme::RightParen)) {
        exprs.emplace_back(co_await parse_any_expr());
        if (!co_await is_token_of(lexeme::Comma)) break;
        co_await parse_token(lexeme::Comma);
    }
    co_await parse_token(lexeme::RightParen);
    co_return exprs;
}

auto parse_optional_lit_suffix() -> parsing<std::u32string>
{
    if (co_await is_token_of(lexeme::LitSuf)) {
        auto suffix = co_await parse_token(lexeme::LitSuf);
        co_return suffix->str();
    }
    co_return std::u32string{};
}

auto parse_lit_bool() -> parsing<ast::lit_bool::ptr>
{
    auto tok = co_await parse_token(lexeme::BoolLit);
    if (tok->text() == U"false"sv) {
        co_return uniq(false);
    }
    if (tok->text() == U"true"sv) {
        co_return uniq(true);
    }
    std::unreachable();
}

auto parse_lit_float() -> parsing<ast::lit_float::ptr>
{
    auto tok = co_await parse_token(lexeme::FloatLit);
    auto suf = co_await parse_optional_lit_suffix();
    co_return uniq(tok->str(), std::move(suf));
}

auto parse_lit_int(ast::lit_int::base base) -> parsing<ast::lit_int::ptr>
{
    static const auto map = std::flat_map<ast::lit_int::base, lexeme>{
        {ast::lit_int::base::bin, lexeme::BinLit},
        {ast::lit_int::base::oct, lexeme::OctLit},
        {ast::lit_int::base::dec, lexeme::DecLit},
        {ast::lit_int::base::hex, lexeme::HexLit},
    };
    auto tok  = co_await parse_token(map.at(base));
    auto u8s  = utf::utf32_to_utf8(tok->text());
    auto u8sv = std::string_view{u8s};
    do {
        if (tok->type() == lexeme::DecLit) {
            if (!u8s.starts_with("0d"sv)) break;
            if (!u8s.starts_with("0D"sv)) break;
        }
        u8sv.remove_prefix(2uz);
    } while (false);
    auto value  = std::uintmax_t{};
    auto result = std::from_chars(u8sv.begin(), u8sv.end(), value, std::to_underlying(base));
    if (result.ec == std::errc{}) {
        auto rem = std::string_view{result.ptr, u8sv.end()};
        if (rem.empty()) {
            auto suffix = co_await parse_optional_lit_suffix();
            co_return uniq(value, std::move(suffix), base);
        }
        co_return fail(i18n::bad_literal(rem));
    }
    co_return fail(i18n::bad_int_lit_bcs(std::make_error_condition(result.ec).message()));
}

auto parse_lit_str() -> parsing<ast::lit_str::ptr>
{
    auto tok    = co_await parse_token(lexeme::StrLit);
    auto suffix = co_await parse_optional_lit_suffix();
    co_return uniq(tok->str(), std::move(suffix));
}

auto parse_lit_char() -> parsing<ast::lit_char::ptr>
{
    auto tok   = co_await parse_token(lexeme::CharLit);
    auto value = char32_t{};
    auto text  = tok->text();
    text.remove_prefix(1uz);
    text.remove_suffix(1uz);
    if (text.starts_with(U'\\')) {
        text.remove_prefix(1uz);
        if (text.starts_with(U'{')) {
            text.remove_prefix(1uz);
            text.remove_suffix(1uz);

            auto codepoint = std::uint32_t{};
            for (auto ch : text) {
                auto digit = std::uint32_t{};
                if (is_digit(ch)) {
                    digit = ch - U'0';
                } else if (U'A' <= ch && ch <= U'F') {
                    digit = ch - U'A' + 10;
                } else if (U'a' <= ch && ch <= U'f') {
                    digit = ch - U'a' + 10;
                } else {
                    codepoint = std::uint32_t{};
                    break;
                }
                codepoint *= 16;
                codepoint += digit;
            }
            value = static_cast<char32_t>(codepoint);
        } else if (text == U"n"sv) {
            value = U'\n';
        } else if (text == U"'"sv) {
            value = U'\'';
        } else if (text == U"\""sv) {
            value = U'"';
        } else if (text == U"\\") {
            value = U'\\';
        }
    } else if (text.size() == 1uz) {
        value = text.front();
    }
    if (value == char32_t{}) {
        co_return fail(i18n::bad_char_lit(utf::utf32_to_utf8(tok->text())));
    }
    auto suffix = co_await parse_optional_lit_suffix();
    co_return uniq(value, std::move(suffix));
}

auto parse_lit_expr() -> parsing<ast::lit_expr::ptr>
{
    auto l = co_await peek_lexeme();
    if (l == lexeme::BoolLit) co_return uniq(co_await parse_lit_bool());
    if (l == lexeme::FloatLit) co_return uniq(co_await parse_lit_float());
    if (l == lexeme::BinLit) co_return uniq(co_await parse_lit_int(ast::lit_int::base::bin));
    if (l == lexeme::OctLit) co_return uniq(co_await parse_lit_int(ast::lit_int::base::oct));
    if (l == lexeme::DecLit) co_return uniq(co_await parse_lit_int(ast::lit_int::base::dec));
    if (l == lexeme::HexLit) co_return uniq(co_await parse_lit_int(ast::lit_int::base::hex));
    if (l == lexeme::StrLit) co_return uniq(co_await parse_lit_str());
    if (l == lexeme::CharLit) co_return uniq(co_await parse_lit_char());
    co_return fail(i18n::expected_literal());
}

auto parse_expr2() -> parsing<ast::any_expr::ptr>
{
    if (is_literal(co_await peek_lexeme())) {
        co_return uniq(co_await parse_lit_expr());
    }
    if (co_await is_token_of(lexeme::Ident)) {
        co_return uniq(co_await parse_name());
    }
    if (co_await is_group_of(token_tree::delimiter::paren)) {
        co_return uniq(co_await parse_paren_expr());
    }
    if (co_await is_group_of(token_tree::delimiter::brace)) {
        co_return uniq(co_await parse_block_expr());
    }
    co_return fail(i18n::expected_expr());
}

auto parse_expr1() -> parsing<ast::any_expr::ptr>
{
    auto lhs = co_await parse_expr2();
    while (co_await is_group_of(token_tree::delimiter::paren)) {
        auto args = co_await parse_paren(parse_expr_list());
        auto call = ast::call_expr::uniq(std::move(lhs), std::move(args));
        lhs       = ast::any_expr::uniq(std::move(call));
    }
    co_return lhs;
}

auto parse_any_expr() -> parsing<ast::any_expr::ptr>
{
    if (co_await is_token_of(lexeme::If)) {
        co_return uniq(co_await parse_if_expr());
    }
    if (co_await is_token_of(lexeme::While)) {
        co_return uniq(co_await parse_while_expr());
    }
    if (co_await is_token_of(lexeme::Return)) {
        co_return uniq(co_await parse_return_expr());
    }
    co_return co_await parse_expr1();
}

auto is_local_prefix() -> parsing<bool>
{
    if (!co_await is_pattern()) co_return false;
    if (co_await peek_lexeme(1uz) != lexeme::Colon) co_return false;
    co_return true;
}

auto parse_local() -> parsing<ast::local::ptr>
{
    auto pat = co_await parse_pattern();
    co_await parse_token(lexeme::Colon);
    auto type = ast::type_name::ptr{};
    if (co_await peek_lexeme() != lexeme::Assign) {
        type = co_await parse_type_name();
    }
    auto init = ast::any_expr::ptr{};
    if (co_await peek_lexeme() == lexeme::Assign) {
        co_await parse_token(lexeme::Assign);
        init = co_await parse_any_expr();
    }
    co_await parse_token(lexeme::Semicolon);
    co_return uniq(std::move(pat), std::move(type), std::move(init));
}

auto parse_any_stmt() -> parsing<ast::any_stmt::ptr>
{
    if (co_await is_local_prefix()) {
        co_return uniq(co_await parse_local());
    }

    auto expr = co_await parse_any_expr();
    auto need = expr->need_semi();
    auto semi = co_await is_token_of(lexeme::Semicolon);
    if (need && !semi) {
        co_await parse_bound();
        co_return uniq(std::move(expr));
    }
    if (semi) {
        co_await parse_token(lexeme::Semicolon);
        co_return uniq(ast::expr_stmt::uniq(std::move(expr)));
    }
    co_return uniq(std::move(expr));
}

auto parse_stmt_list() -> parsing<ast::any_stmt::vec>
{
    auto stmts = ast::any_stmt::vec{};
    co_await parse_token(lexeme::LeftBrace);
    while (!co_await is_token_of(lexeme::RightBrace)) {
        stmts.emplace_back(co_await parse_any_stmt());
    }
    co_await parse_token(lexeme::RightBrace);
    co_return stmts;
}

auto parse_block_expr() -> parsing<ast::block_expr::ptr>
{
    co_return uniq(co_await parse_brace(parse_stmt_list()));
}

auto parse_fn_decl() -> parsing<ast::fn_decl::ptr>
{
    auto name = co_await parse_name();
    co_await parse_token(lexeme::Colon);
    auto sign = co_await parse_fn_sign();
    co_await parse_token(lexeme::Assign);
    auto body = co_await parse_block_expr();
    co_return uniq(std::move(name), std::move(sign), std::move(body));
}

auto parse_trans_unit() -> parsing<ast::trans_unit::ptr>
{
    auto decls = ast::fn_decl::vec{};
    while (co_await has_next) {
        decls.push_back(co_await parse_fn_decl());
    }
    co_return uniq(std::move(decls));
}

auto parse(const token_tree& tree) -> std::expected<ast::trans_unit::ptr, error_entry> // -> parsing_result
{
    auto group = tree.get_group();
    assert_(group, i18n::expected_group_root());
    assert_(group->delim() == token_tree::delimiter::none, i18n::expected_delim_root());
    auto parsing = parse_trans_unit();
    parsing.resume(group);

    auto result = parsing.take_result();

    if (result) {
        return std::move(result).value();
    } else {
        return std::unexpected{std::move(result).error()};
    }
}

}
