project(src-fth NONE)

set(FORTH gforth)

enable_testing()
add_test(NAME test_1_fth COMMAND ${FORTH} test_1.fth)
