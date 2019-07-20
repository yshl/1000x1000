project(src-ml NONE)

set(OCAMLC ocamlopt)
set(OCAMLFLAGS -O3)

function(add_executable_ocaml TARGET)
  set(SOURCES ${ARGN})
  add_custom_target(${TARGET} ALL
    COMMAND ${OCAMLC} ${OCAMLFLAGS} -o ${TARGET} ${SOURCES}
    DEPENDS ${SOURCES}
  )
endfunction()

add_executable_ocaml(test_1_ml lin.ml test_1.ml)
add_executable_ocaml(test_list_1_ml lin_list.ml test_list_1.ml)

enable_testing()
add_test(NAME test_1_ml COMMAND test_1_ml)
add_test(NAME test_list_1_ml COMMAND test_list_1_ml)
