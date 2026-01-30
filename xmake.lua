add_rules("mode.release", "mode.debug")
add_rules("plugin.compile_commands.autoupdate")

set_languages("c++26")
set_toolchains("llvm")
set_runtimes("c++_static")

add_repositories("kqm-repo https://github.com/KeqingMoe/xmake-repo.git")
add_requires("hardening-stl")

-- add_requires("argparse fix-xmake-modules", {
--     configs = {
--         enable_module = true,
--         enable_std_import = true
--     }
-- })

add_cxxflags("-Wall", "-Werror")
add_rules("@hardening-stl/debug")

target("autofront")
    set_kind("binary")
    -- add_packages("argparse")
    add_files("autofront/**.cppm")
    add_headerfiles("autofront/i18n/*", {public = true})
    add_files("autofront/autofront.cpp")

target("test-lex-repl")
    set_kind("binary")
    add_files("autofront/**.cppm")
    add_files("test/test_lex_repl.cpp")

target("test-token-tree")
    set_kind("binary")
    add_files("autofront/**.cppm")
    add_files("test/test_token_tree.cpp")
