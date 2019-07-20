cmake_minimum_required(VERSION 2.8)
project(1000x1000 C)

set(BLA_VENDOR "All")
find_package(LAPACK)
find_package(BLAS)

add_subdirectory(lib)
add_subdirectory(src-f)
add_subdirectory(src-c)
add_subdirectory(src-cc)
add_subdirectory(src-cl)
add_subdirectory(src-d)
add_subdirectory(src-fth)
add_subdirectory(src-java)
add_subdirectory(src-ml)
add_subdirectory(src-scm)

enable_testing()
