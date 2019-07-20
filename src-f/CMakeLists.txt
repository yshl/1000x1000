project(src-f Fortran)
set(CMAKE_Fortran_FLAGS "-Wall -O3")

include_directories(../lib)

add_executable(test_1_f test_1.f90 lin.f90)
add_executable(test_lapack_1_f test_lapack_1.f90)
target_link_libraries(test_lapack_1_f ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
add_executable(test_2_f test_2.f90 lin_complete.f90)
add_executable(test_lapack_2_f test_lapack_2.f90)
target_link_libraries(test_lapack_2_f dgetc2 ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})

enable_testing()
add_test(NAME test_1_f COMMAND test_1_f)
add_test(NAME test_lapack_1_f COMMAND test_lapack_1_f)
add_test(NAME test_2_f COMMAND test_2_f)
add_test(NAME test_lapack_2_f COMMAND test_lapack_2_f)
