export module autofront:i18n;

import std;

template <typename It>
using diff = std::iter_difference_t<It>;

template <typename>
struct get_type
{
};
template <typename T>
struct get_type<T* (*)()>
{
    using type = T;
};

template <typename... Args>
auto tr_dispatch(auto key, Args&&... args) -> std::string;

export namespace autofront
{

namespace i18n
{

namespace lang
{

#define TR(key, fmt)                                                                                                   \
    struct key                                                                                                         \
    {                                                                                                                  \
        template <typename... Args>                                                                                    \
        static auto format(Args&&... args) -> std::string                                                              \
        {                                                                                                              \
            return std::format(fmt, std::forward<Args>(args)...);                                                      \
        }                                                                                                              \
        template <typename Out, typename... Args>                                                                      \
        static auto format_to(Out out, Args&&... args) -> Out                                                          \
        {                                                                                                              \
            return std::format_to(out, fmt, std::forward<Args>(args)...);                                              \
        }                                                                                                              \
        template <typename Out, typename... Args>                                                                      \
        static auto format_to_n(Out out, diff<Out> n, Args&&... args) -> std::format_to_n_result<Out>                  \
        {                                                                                                              \
            return std::format_to_n(out, n, fmt, std::forward<Args>(args)...);                                         \
        }                                                                                                              \
        template <typename... Args>                                                                                    \
        static auto formatted_size(Args&&... args) -> std::size_t                                                      \
        {                                                                                                              \
            return std::formatted_size(fmt, std::forward<Args>(args)...);                                              \
        }                                                                                                              \
    };

struct zh
{
#include "i18n/zh"
};

struct en : zh
{
#include "i18n/en"
};

using default_lang = en;

using dispatcher = std::variant<zh, en>;

}

constexpr auto zh           = lang::zh{};
constexpr auto en           = lang::en{};
constexpr auto default_lang = lang::default_lang{};

template <typename Key, typename Lang>
struct i18n_traits;

// 请原谅，我知道它应该是 internationalizable，但显然我不可能写 i17e
template <typename Key, typename Lang, typename Out, typename... Args>
concept i18n_able = requires(i18n_traits<Key, Lang> trait, Out out, Args&&... args) {
    { trait.format(std::forward<Args>(args)...) } -> std::same_as<std::string>;
    { trait.format_to(out, std::forward<Args>(args)...) } -> std::same_as<Out>;
    { trait.format_to_n(out, diff<Out>{}, std::forward<Args>(args)...) } -> std::same_as<std::format_to_n_result<Out>>;
    { trait.formatted_size(std::forward<Args>(args)...) } -> std::same_as<std::size_t>;
};

template <typename Key, typename Out, typename... Args>
concept i18n_able_for_all = requires(lang::dispatcher lang) {
    lang.visit([]<typename Lang>
                   requires i18n_able<Key, Lang, Out, Args...>
               (Lang) {});
};

#define IDX(key)                                                                                                       \
    struct                                                                                                             \
    {                                                                                                                  \
    private:                                                                                                           \
        auto (*hack)() -> decltype(this);                                                                              \
        using this_type = get_type<decltype(hack)>::type;                                                              \
                                                                                                                       \
    public:                                                                                                            \
        template <typename... Args>                                                                                    \
        static auto operator()(Args&&... args) -> std::string                                                          \
        {                                                                                                              \
            return tr_dispatch(this_type{}, std::forward<Args>(args)...);                                        \
        }                                                                                                              \
    } key;                                                                                                             \
    template <typename Lang>                                                                                           \
    struct i18n_traits<decltype(key), Lang> : Lang::key                                                                \
    {                                                                                                                  \
    };

#include "i18n/index"

struct translator
{
    lang::dispatcher lang;

    static constexpr auto make(auto lang) -> translator
    {
        return {.lang = lang};
    }

    constexpr auto set(auto lang) -> void
    {
        this->lang = lang;
    }

#define LANG(L)                                                                                                        \
    static constexpr auto L() -> translator                                                                            \
    {                                                                                                                  \
        return make(lang::L{});                                                                                        \
    }                                                                                                                  \
    constexpr auto set_##L() -> void                                                                                   \
    {                                                                                                                  \
        set(lang::L{});                                                                                                \
    }

    LANG(default_lang)
    LANG(zh)
    LANG(en)

    template <typename Key, typename... Args>
    // requires i18n_able_for_all<Key, char*, Args...> // 并非我不想约束，只是一开 llvm 就崩溃
    auto format(Key, Args&&... args) -> std::string
    {
        return lang.visit([&]<typename Lang>(Lang) {
            return i18n_traits<Key, Lang>::format(std::forward<Args>(args)...);
        });
    }

    template <typename Out, typename Key, typename... Args>
    // requires i18n_able_for_all<Key, Out, Args...>
    auto format_to(Out out, Key, Args&&... args) -> Out
    {
        return lang.visit([&]<typename Lang>(Lang) {
            return i18n_traits<Key, Lang>::format_to(out, std::forward<Args>(args)...);
        });
    }

    template <typename Out, typename Key, typename... Args>
    // requires i18n_able_for_all<Key, Out, Args...>
    auto format_to_n(Out out, std::size_t n, Key, Args&&... args) -> std::format_to_n_result<Out>
    {
        return lang.visit([&]<typename Lang>(Lang) {
            return i18n_traits<Key, Lang>::format_to_n(out, n, std::forward<Args>(args)...);
        });
    }

    template <typename Key, typename... Args>
    // requires i18n_able_for_all<Key, char*, Args...>
    auto formatted_size(Key, Args&&... args) -> std::size_t
    {
        return lang.visit([&]<typename Lang>(Lang) {
            return i18n_traits<Key, Lang>::formatted_size(std::forward<Args>(args)...);
        });
    }
};

}

constinit auto tr = i18n::translator::default_lang();

}

template <typename... Args>
auto tr_dispatch(auto key, Args&&... args) -> std::string
{
    return autofront::tr.format(key, std::forward<Args>(args)...);
}
