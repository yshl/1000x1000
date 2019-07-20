project(src-ml NONE)

set(BIGLOO bigloo)
set(BIGLOOFLAGS -O6 -copt -O3)
set(CSC csc)
set(CSCFLAGS -O5 -C -O3)

function(add_executable_bigloo TARGET)
  set(SOURCES ${ARGN})
  add_custom_target(${TARGET} ALL
    COMMAND ${BIGLOO} ${BIGLOOFLAGS} -o ${TARGET} ${SOURCES}
    DEPENDS ${SOURCES}
  )
endfunction()

function(add_object_bigloo TARGET SOURCE)
  add_custom_target(${TARGET}
    COMMAND ${BIGLOO} ${BIGLOOFLAGS} -c -o ${TARGET} ${SOURCE}
    DEPENDS ${SOURCE}
  )
endfunction()

function(add_executable_chicken TARGET)
  set(SOURCES ${ARGN})
  add_custom_target(${TARGET} ALL
    COMMAND ${CSC} ${CSCFLAGS} -o ${TARGET} ${SOURCES}
    DEPENDS ${SOURCES}
  )
endfunction()

function(add_object_chicken TARGET SOURCE)
  add_custom_target(${TARGET}
    COMMAND ${CSC} ${CSCFLAGS} -c -o ${TARGET} ${SOURCE}
    DEPENDS ${SOURCE}
  )
endfunction()

add_executable_bigloo(test_bigloo_1_scm test_bigloo_1.o lin_bigloo.o)
add_executable_chicken(test_chicken_1_scm test_chicken_1.o lin_chicken.o)

add_object_bigloo(lin_bigloo.o lin_bigloo.scm)
add_object_bigloo(test_bigloo_1.o test_bigloo_1.scm)
add_object_chicken(lin_chicken.o lin_chicken.scm)
add_object_chicken(test_chicken_1.o test_chicken_1.scm)

enable_testing()
add_test(NAME test_bigloo_1_scm COMMAND test_bigloo_1_scm)
add_test(NAME test_chicken_1_scm COMMAND test_chicken_1_scm)
