export module autofront:common;

import std;

namespace ranges = std::ranges;
namespace views  = std::views;

export namespace autofront
{

struct source_position
{
    std::size_t lineno = 0uz;
    std::size_t colno  = 0uz;
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

auto is_start(char ch)
{
    return std::isalpha(ch) || ch == '_';
}

auto is_continue(char ch)
{
    return is_start(ch) || std::isdigit(ch);
}

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

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

template <typename T, typename E>
auto ignore_error(std::expected<T, E> r) -> std::optional<T>
{
    if (r.has_value()) {
        return std::optional{std::move(r).value()};
    } else {
        return std::nullopt;
    }
}

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
