export module autofront:common;

import std;

namespace ranges = std::ranges;
namespace views  = std::views;

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

export namespace autofront
{

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
    return get_xid(ch) != xid::None;
}

auto is_xid_continue(char32_t ch) -> bool
{
    return get_xid(ch) == xid::Continue;
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

}

template <>
struct std::formatter<autofront::source_position, char> : public std::formatter<std::string_view, char>
{
    auto format(autofront::source_position loc, auto&& ctx) const
    {
        return std::format_to(ctx.out(), "{}:{}", loc.lineno, loc.colno);
    }
};

template <typename Promise>
struct std::hash<autofront::unique_coro<Promise>> : std::hash<std::coroutine_handle<Promise>>
{
};
