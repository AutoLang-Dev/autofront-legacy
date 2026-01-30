export module autofront:common;

import std;
import :i18n;

namespace ranges = std::ranges;
namespace views  = std::views;
using namespace std::literals;

struct char_range
{
    char32_t start;
    char32_t end;
};

// 属于 Unicode XID_Start 的字符
constexpr auto xid_starts = std::array{
    char_range{.start = U'\u{41}', .end = U'\u{5A}'},
    char_range{.start = U'\u{61}', .end = U'\u{7A}'},
    // ...
};

// 属于 XID_Continue 但不属于 XID_Start 的字符
constexpr auto xid_continues_delta = std::array{
    char_range{.start = U'\u{30}', .end = U'\u{39}'},
    char_range{.start = U'\u{5F}', .end = U'\u{5F}'},
    // ...
};

template <typename T>
struct is_in_place_type_t
{
    static constexpr auto value = false;
};

template <typename T>
struct is_in_place_type_t<std::in_place_type_t<T>>
{
    static constexpr auto value = true;
};

template <typename T>
constexpr auto is_in_place_type_t_v = is_in_place_type_t<T>::value;

template <typename B>
concept boolean_testable = requires(B&& b) {
    requires std::convertible_to<B, bool>;
    { !std::forward<B>(b) } -> std::convertible_to<bool>;
};

template <typename T, typename U>
    requires requires(const T& t, const U& u) {
        { t < u } -> boolean_testable;
        { u < t } -> boolean_testable;
    }
constexpr auto synth_three_way(const T& t, const U& u)
{
    if constexpr (std::three_way_comparable_with<T, U>) {
        return t <=> u;
    } else {
        if (t < u) return std::weak_ordering::less;
        if (u < t) return std::weak_ordering::greater;
        return std::weak_ordering::equivalent;
    }
}

template <typename T, typename U = T>
using synth_three_way_result = decltype(synth_three_way(std::declval<T&>(), std::declval<U&>()));

struct delegate_t
{
} delegate;

template <typename T>
concept yes = true;

export namespace autofront
{

struct unit
{
};

struct source_position
{
    std::size_t lineno = 0uz;
    std::size_t colno  = 0uz;

    friend constexpr auto operator==(source_position, source_position) noexcept -> bool = default;
    friend constexpr auto operator<=>(source_position, source_position) noexcept        = default;

    constexpr auto next(std::size_t n) const -> source_position
    {
        return {
            .lineno = lineno,
            .colno  = colno + n,
        };
    }

    auto next_line_front() const -> source_position
    {
        return {
            .lineno = lineno + 1uz,
            .colno  = 1uz,
        };
    }
};

struct source_span
{
    source_position start;
    source_position end;

    static constexpr auto make(source_position pos, std::size_t n) -> source_span
    {
        return {
            .start = pos,
            .end   = pos.next(n),
        };
    }

    static constexpr auto make(source_span first, source_span last) -> source_span
    {
        return {
            .start = first.start,
            .end   = last.end,
        };
    }
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
    std::u32string_view text;
};

struct error_entry
{
    source_position where;
    std::string message;
    bool internal = false;
    bool fallback = false;
    std::source_location from;
};

// Unicode XID
enum struct xid : std::uint8_t
{
    None,
    Start,
    Continue,
};

auto get_xid(char32_t ch) -> xid
{
    auto lower_bound = [ch](std::span<const char_range> db) {
        auto it = ranges::lower_bound(db, ch, {}, &char_range::end);
        if (it != db.end()) {
            if (it->start <= ch) return true;
        }
        return false;
    };
    if (lower_bound(xid_starts)) return xid::Start;
    if (lower_bound(xid_continues_delta)) return xid::Continue;
    return xid::None;
}

auto is_xid_start(char32_t ch) -> bool
{
    return get_xid(ch) == xid::Start;
}

auto is_xid_continue(char32_t ch) -> bool
{
    // auto xid = get_xid(ch);
    // return xid == xid::Start || xid == xid::Continue;
    return get_xid(ch) != xid::None;
}

auto is_xid_none(char32_t ch) -> bool
{
    return get_xid(ch) == xid::None;
};

auto is_digit(char32_t ch) -> bool
{
    return U'0' <= ch && ch <= U'9';
};

auto is_xdigit(char32_t ch) -> bool
{
    return is_digit(ch) || (U'A' <= ch && ch <= U'F') || (U'a' <= ch && ch <= U'f');
};

auto is_space(char32_t ch) -> bool
{
    return U"\u{9}\u{A}\u{B}\u{C}\u{20}"sv.contains(ch);
}

template <class... Ts>
struct overloaded : Ts...
{
    using Ts::operator()...;
};

template <typename Promise = void>
struct unique_coro
{
public:
    using coroutine_handle = std::coroutine_handle<Promise>;

private:
    static constexpr auto is_type_erased   = std::same_as<Promise, void>;
    static constexpr auto noexcept_done    = noexcept(coroutine_handle{}.done());
    static constexpr auto noexcept_resume  = noexcept(coroutine_handle{}.resume());
    static constexpr auto noexcept_destroy = noexcept(coroutine_handle{}.destroy());
    static constexpr auto noexcept_promise = [] {
        if constexpr (is_type_erased) {
            return false;
        } else {
            return noexcept(coroutine_handle{}.resume());
        }
    }();
    using promise_reference = decltype([] {
        if constexpr (is_type_erased) {
            struct return_type
            {
                using type = std::monostate;
            };
            return return_type{};
        } else {
            struct return_type
            {
                using type = Promise&;
            };
            return return_type{};
        }
    }())::type;

public:
    constexpr unique_coro() = default;

    explicit constexpr unique_coro(coroutine_handle h) : h_{h} {}

    constexpr unique_coro(unique_coro const&) = delete;

    constexpr unique_coro(unique_coro&& rhs) noexcept
    {
        std::swap(h_, rhs.h_);
    }

    constexpr ~unique_coro() noexcept
    {
        reset();
    }

    constexpr auto operator=(unique_coro const&) = delete;

    constexpr auto operator=(unique_coro&& rhs) noexcept -> unique_coro&
    {
        if (this != &rhs) {
            reset(rhs.h_);
            rhs.release();
        }
        return *this;
    }

    constexpr auto reset(coroutine_handle coro = {}) noexcept(noexcept_destroy) -> void
    {
        if (h_) {
            h_.destroy();
        }
        h_ = coro;
    }

    constexpr auto release() noexcept -> coroutine_handle
    {
        return std::exchange(h_, coroutine_handle{});
    }

    constexpr auto get() const noexcept -> coroutine_handle
    {
        return h_;
    }

    constexpr operator unique_coro<>() && noexcept
    {
        auto h = std::exchange(h_, coroutine_handle{});
        return unique_coro<>{h};
    }

    constexpr auto done() noexcept(noexcept_done) -> bool
    {
        return h_.done();
    }

    constexpr explicit operator bool() const noexcept
    {
        return static_cast<bool>(h_);
    }

    constexpr auto operator()() const noexcept(noexcept_resume) -> void
    {
        resume();
    }

    constexpr auto resume() const noexcept(noexcept_resume) -> void
    {
        h_.resume();
    }

    constexpr auto destroy() noexcept(noexcept_destroy) -> void
    {
        h_.destroy();
    }

    auto promise() const noexcept(noexcept_promise) -> promise_reference
        requires(!is_type_erased)
    {
        return h_.promise();
    }

    static constexpr auto from_promise(promise_reference promise) -> unique_coro
        requires(!is_type_erased)
    {
        return unique_coro{coroutine_handle::from_promise(promise)};
    }

    constexpr auto address() const noexcept -> void*
    {
        return h_.address();
    }

    static constexpr auto from_address(void* addr) -> unique_coro
    {
        return unique_coro{coroutine_handle::from_address(addr)};
    }

    friend constexpr auto operator==(const unique_coro&, const unique_coro&) noexcept -> bool  = default;
    friend constexpr auto operator<=>(const unique_coro&, const unique_coro&) noexcept -> bool = default;

private:
    coroutine_handle h_;
};

struct this_promise_t
{
} this_promise;

template <typename T, typename E>
auto ignore_error(std::expected<T, E> r) -> std::optional<T>
{
    if (r.has_value()) {
        return std::optional{std::move(r).value()};
    } else {
        return std::nullopt;
    }
}

template <typename T>
struct remove_rval_cref
{
    using type = T;
};

template <typename T>
struct remove_rval_cref<T&&>
{
    using type = std::remove_const_t<T>;
};

template <typename T>
using remove_rval_cref_t = remove_rval_cref<T>::type;

template <typename T>
struct just_awaitable
{
private:
    using type = remove_rval_cref_t<T>;
    type r_;

public:
    just_awaitable(type r) : r_{std::forward<decltype(r)>(r)} {}

    constexpr auto await_ready() const noexcept -> bool
    {
        return true;
    }

    constexpr auto await_suspend(std::coroutine_handle<>) const noexcept -> void
    {
        std::unreachable();
    }

    constexpr auto await_resume() & noexcept -> T&
    {
        return r_;
    }

    constexpr auto await_resume() const& noexcept -> const T&
    {
        return r_;
    }

    constexpr auto await_resume() && noexcept -> T
    {
        return std::move(r_);
    }

    constexpr auto await_resume() const&& noexcept -> T
    {
        return r_;
    }
};

template <typename T>
just_awaitable(T x) -> just_awaitable<remove_rval_cref_t<T>>;

auto assert_(bool pred, std::string_view msg, std::source_location l = std::source_location::current())
{
    if (pred) return;
    std::println(std::cerr, "{}", i18n::assert_fail(l.file_name(), l.line(), l.column(), l.function_name()));
    std::println(std::cerr, "{}", msg);
    std::terminate();
}

struct indent_printer
{
protected:
    std::size_t indent_ = 0;
    bool new_line       = true;

    auto print_indent()
    {
        if (indent_) {
            std::print("{:{}}", "", indent_);
        }
    }

    auto indent(std::size_t n = 2uz)
    {
        struct guard
        {
        public:
            guard(std::size_t& ref, std::size_t n) : ref_{ref}, n_{n}
            {
                ref_ += n_;
            }
            ~guard()
            {
                ref_ -= n_;
            }

        private:
            std::size_t& ref_;
            std::size_t n_;
        };
        return guard{indent_, n};
    }

    template <typename... Args>
    auto print(std::format_string<Args...> fmt, Args&&... args)
    {
        if (new_line) {
            print_indent();
        }
        std::print(fmt, std::forward<Args>(args)...);
        new_line = false;
    }

    template <typename... Args>
    auto println(std::format_string<Args...> fmt, Args&&... args)
    {
        if (new_line) {
            print_indent();
        }
        std::println(fmt, std::forward<Args>(args)...);
        new_line = true;
    }

    auto println()
    {
        std::println();
        new_line = true;
    }

    auto indently_print(auto&& fn)
    {
        println("{{");
        {
            auto _ = indent();
            fn();
        }
        print("}}");
    }
};

template <typename T, typename Allocator>
concept indirectable = requires {
    requires std::is_object_v<T>;
    requires !std::is_array_v<T>;
    requires !std::same_as<T, std::in_place_t>;
    requires !is_in_place_type_t_v<T>;
    requires std::same_as<T, std::remove_cv_t<T>>;
    requires std::same_as<T, typename std::allocator_traits<Allocator>::value_type>;
};


template <typename T, typename Allocator = std::allocator<T>>
// requires indirectable<T, Allocator>
struct indirect;

template <typename T>
struct is_indirect_impl
{
    static constexpr auto value = false;
};

template <typename T>
struct is_indirect_impl<indirect<T>>
{
    static constexpr auto value = true;
};

template <typename T>
concept not_indirect = !is_indirect_impl<T>::value;

// std::indirect 的替代物
template <typename T, typename Allocator = std::allocator<T>>
// requires indirectable<T, Allocator>
struct indirect
{
    using value_type     = T;
    using allocator_type = Allocator;
    using pointer        = typename std::allocator_traits<Allocator>::pointer;
    using const_pointer  = typename std::allocator_traits<Allocator>::const_pointer;

private:
    template <typename U, typename A>
    // requires indirectable<U, A>
    friend struct indirect;

    using traits = std::allocator_traits<Allocator>;

    pointer p_;
    Allocator alloc_;

    template <typename... Args>
    auto construct_with_alloc(Allocator& alloc, Args&&... args) -> pointer
    {
        auto p = traits::allocate(alloc, sizeof(value_type));
        traits::construct(alloc, p, std::forward<Args>(args)...);
        return p;
    }

    template <typename... Args>
    auto construct(Args&&... args) -> pointer
    {
        return construct_with_alloc(alloc_, std::forward<Args>(args)...);
    }

    auto destroy_with_alloc(Allocator& alloc, pointer p) noexcept -> void
    {
        traits::destroy(alloc, p);
        traits::deallocate(alloc, p, sizeof(value_type));
    }

    auto destroy(pointer p) noexcept -> void
    {
        destroy_with_alloc(alloc_, p);
    }

    constexpr explicit indirect(delegate_t) : p_{}, alloc_{} {}
    constexpr explicit indirect(delegate_t, const Allocator& a)
        : p_{}, alloc_(a) // 按要求使用 direct-non-list-initialization
                          // 参考 https://zh.cppreference.com/w/cpp/memory/indirect/indirect.html
    {
    }

public:
    constexpr explicit indirect() noexcept(std::is_nothrow_default_constructible_v<value_type>)
        // requires std::is_default_constructible_v<T>
        : indirect{delegate}
    {
        p_ = construct();
    }

    constexpr explicit indirect(std::allocator_arg_t, const Allocator& a)
        // requires std::is_default_constructible_v<T>
        : indirect{delegate, a}
    {
        p_ = construct();
    }

    constexpr explicit indirect(T v) : indirect{delegate}
    {
        p_ = construct(std::move(v));
    }

    template <typename U = T>
    // requires std::constructible_from<T, U&&>
    constexpr explicit indirect(std::allocator_arg_t, const Allocator& a, U&& v) : indirect{delegate, a}
    {
        p_ = construct(std::forward<U>(v));
    }

    template <typename... Args>
    // requires std::constructible_from<T, Args...>
    constexpr explicit indirect(std::in_place_t, Args&&... args) : indirect{delegate}
    {
        p_ = construct(std::forward<Args>(args)...);
    }

    template <typename... Args>
    // requires std::constructible_from<T, Args...>
    constexpr explicit indirect(std::allocator_arg_t, const Allocator& a, std::in_place_t, Args&&... args)
        : indirect{delegate, a}
    {
        p_ = construct(std::forward<Args>(args)...);
    }

    template <typename I, typename... Args>
    // requires std::constructible_from<T, std::initializer_list<I>&, Args...>
    constexpr explicit indirect(std::in_place_t, std::initializer_list<I> ilist, Args&&... args) : indirect{delegate}
    {
        p_ = construct(ilist, std::forward<Args>(args)...);
    }

    template <typename I, typename... Args>
    // requires std::constructible_from<T, std::initializer_list<I>&, Args...>
    constexpr explicit indirect(
        std::allocator_arg_t, const Allocator& a, std::in_place_t, std::initializer_list<I> ilist, Args&&... args)
        : indirect{delegate, a}
    {
        p_ = construct(ilist, std::forward<Args>(args)...);
    }

    constexpr indirect(const indirect& other)
        // requires std::copy_constructible<T>
        : indirect{delegate, traits::select_on_container_copy_construction(other.alloc_)}
    {
        if (other.p_) p_ = construct(*other.p_);
    }

    constexpr indirect(std::allocator_arg_t, const Allocator& a, const indirect& other)
        // requires std::copy_constructible<T>
        : indirect{delegate, a}
    {
        if (other.p_) p_ = construct(*other.p_);
    }

    constexpr indirect(indirect&& other) noexcept
        // requires std::move_constructible<T>
        : indirect{delegate, std::move(other.alloc_)}
    {
        p_       = other.p_;
        other.p_ = nullptr;
    }

    constexpr indirect(std::allocator_arg_t, const Allocator& a, indirect&& other)
        noexcept(traits::is_always_equal::value)
        // requires std::move_constructible<T>
        : indirect{delegate, a}
    {
        if (alloc_ == other.alloc_) {
            p_       = other.p_;
            other.p_ = nullptr;
        } else if (other.p_) {
            p_ = construct(std::move(*other.p_));
        }
    }

    constexpr ~indirect() noexcept
    {
        if (p_) destroy(p_);
    }

    constexpr auto operator=(const indirect& other) -> indirect&
    // requires std::is_copy_assignable_v<T> && std::copy_constructible<T>
    {
        if (std::addressof(other) == this) return *this;
        constexpr auto update_alloc = traits::propagate_on_container_copy_assignment::value;
        if (!other.p_) {
            if (p_) {
                destroy(p_);
                p_ = nullptr;
            }
        } else if (alloc_ == other.alloc_ && p_) {
            *p_ = *other.p_;
        } else {
            auto& alloc = update_alloc ? other.alloc_ : alloc_;
            auto new_p  = construct_with_alloc(alloc, *other.p_);
            destroy(p_);
            p_ = new_p;
        }
        if constexpr (update_alloc) {
            alloc_ = other.alloc_;
        }
        return *this;
    }

    constexpr auto operator=(indirect&& other)
        noexcept(traits::propagate_on_container_move_assignment::value || traits::is_always_equal::value) -> indirect&
    // requires std::copy_constructible<T>
    {
        if (std::addressof(other) == this) return *this;
        constexpr auto update_alloc = traits::propagate_on_container_move_assignment::value;
        if (!other.p_) {
            if (p_) {
                destroy(p_);
                p_ = nullptr;
            }
        } else if (alloc_ == other.alloc_) {
            if (p_) {
                destroy(p_);
            }
            p_       = other.p_;
            other.p_ = nullptr;
        } else {
            auto& alloc = update_alloc ? other.alloc_ : alloc_;
            auto new_p  = construct_with_alloc(alloc, std::move(*other.p_));
            destroy(p_);
            p_ = new_p;
        }
        if constexpr (update_alloc) {
            alloc_ = other.alloc_;
        }
        return *this;
    }

    template <typename U = T>
    // requires requires {
    //     requires !std::same_as<std::remove_cvref_t<U>, indirect>;
    //     requires std::constructible_from<T, U>;
    //     requires std::assignable_from<T&, U>;
    // }
    constexpr auto operator=(U&& value) -> indirect&
    {
        if (!p_) {
            p_ = construct(std::forward<U>(value));
        } else {
            *p_ = std::forward<U>(value);
        }
    }

    constexpr auto operator->() const noexcept -> const_pointer
    {
        assert_(p_, i18n::valueless_indirect());
        return p_;
    }

    constexpr auto operator->() noexcept -> pointer
    {
        assert_(p_, i18n::valueless_indirect());
        return p_;
    }

    constexpr auto operator*() const& noexcept -> const T&
    {
        assert_(p_, i18n::valueless_indirect());
        return *p_;
    }

    constexpr auto operator*() & noexcept -> T&
    {
        assert_(p_, i18n::valueless_indirect());
        return *p_;
    }

    constexpr auto operator*() const&& noexcept -> const T&&
    {
        assert_(p_, i18n::valueless_indirect());
        return std::move(*p_);
    }

    constexpr auto operator*() && noexcept -> T&&
    {
        assert_(p_, i18n::valueless_indirect());
        return std::move(*p_);
    }

    constexpr auto valueless_after_move() const noexcept -> bool
    {
        return !p_;
    }

    constexpr auto get_allocator() const noexcept -> allocator_type
    {
        return alloc_;
    }

    constexpr auto swap(indirect& other)
        noexcept(traits::propagate_on_container_swap::value || traits::is_always_equal::value) -> void
    // requires std::swappable<Allocator>
    {
        std::swap(p_, other.p_);
        constexpr auto swap_allocators = traits::propagate_on_container_swap::value;
        if (swap_allocators) {
            using std::swap;
            swap(alloc_, other.alloc_);
        } else {
            assert_(alloc_ == other.alloc_, i18n::indirect_alloc_neq_swap());
        }
    }

    template <typename U, typename A>
    // requires std::equality_comparable_with<T, U>
    friend constexpr auto operator==(const indirect& lhs, const indirect<U, A>& rhs) noexcept(noexcept(*lhs == *rhs))
        -> bool
    {
        if (!lhs.p_ && !rhs.p_) return true;
        if (!lhs.p_ || !rhs.p_) return false;
        return *lhs.p_ == *rhs.p_;
    }

    template <typename U, typename A>
    friend constexpr auto operator<=>(const indirect& lhs, const indirect<U, A>& rhs)
        noexcept(noexcept(synth_three_way(*lhs, *rhs))) -> synth_three_way_result<T, U>
    {
        if (lhs.p_ && rhs.p_) return synth_three_way(*lhs.p_, *rhs.p_);
        return !lhs.valueless_after_move() <=> !rhs.valueless_after_move();
    }

    template <not_indirect U>
    // requires std::equality_comparable_with<T, U> // 不能写这行，否则会导致约束形成循环依赖
    friend constexpr auto operator==(const indirect& ind, const U& value) noexcept(noexcept(*ind == value)) -> bool
    {
        static_assert(std::equality_comparable_with<T, U>);
        if (!ind.p_) return false;
        return *ind.p_ == value;
    }

    template <typename U>
    friend constexpr auto operator<=>(const indirect& ind, const U& value)
        noexcept(noexcept(synth_three_way(*ind, value))) -> synth_three_way_result<T, U>
    {
        if (!ind.p_) return std::strong_ordering::less;
        return synth_three_way(*ind.p_, value);
    }

    friend constexpr auto swap(indirect& lhs, indirect& rhs) noexcept(noexcept(lhs.swap(rhs))) -> void
    {
        lhs.swap(rhs);
    }
};

template <typename Value>
indirect(Value) -> indirect<Value>;

template <typename Alloc, typename Value>
indirect(std::allocator_arg_t, Alloc, Value)
    -> indirect<Value, typename std::allocator_traits<Alloc>::template rebind_alloc<Value>>;

template <typename T, typename... Args>
auto make_indirect(Args&&... args) -> indirect<T>
{
    return indirect<T>(std::in_place, std::forward<Args>(args)...);
}

template <typename... Ts>
using sum = std::variant<indirect<Ts>...>;

template <typename... Ts>
using optsum = std::variant<std::monostate, indirect<Ts>...>;

}

template <>
struct std::formatter<autofront::source_position, char> : public std::formatter<std::string_view, char>
{
    auto format(autofront::source_position loc, auto&& ctx) const
    {
        return std::format_to(ctx.out(), "{}:{}", loc.lineno, loc.colno);
    }
};

// template <>
// struct std::formatter<char32_t, char> : public std::formatter<std::string_view, char>
// {
//     auto format(char32_t ch, auto&& ctx) const
//     {
//         return std::format_to(ctx.out(), "U+{:X}", static_cast<std::uint32_t>(ch));
//         // auto u32sv = std::u32string_view{&ch, 1uz};
//         // auto u8s = autofront::utf::utf32_to_utf8(u32sv);
//         // return std::format_to(ctx.out(), "{:?}", u8s);
//     }
// };


template <typename Promise>
struct std::hash<autofront::unique_coro<Promise>> : std::hash<std::coroutine_handle<Promise>>
{
};

template <typename T, typename Allocator>
    requires requires(const T& x) {
        { std::hash<T>{}(x) } -> std::convertible_to<std::size_t>;
    }
struct std::hash<autofront::indirect<T, Allocator>>
{
    static auto operator()(const autofront::indirect<T, Allocator>& ind) noexcept -> std::size_t
    {
        if (ind.valueless_after_move()) return std::hash<T*>{}(nullptr);
        return std::hash<T>{}(*ind);
    }
};
