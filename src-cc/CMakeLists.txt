project(src-cc CXX)
set(CMAKE_CXX_FLAGS "-g -Wall -O3")

add_executable(test_1_cc test_1.cc lin.cc)
add_executable(test_ublas_1_cc test_ublas_1.cc)

enable_testing()
add_test(NAME test_1_cc COMMAND test_1_cc)
add_test(NAME test_ublas_1_cc COMMAND test_ublas_1_cc)
