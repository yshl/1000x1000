project(src-cl NONE)

set(SOURCES test_1.lisp lin.lisp)
add_custom_target(test_1_sbcl ALL
  COMMAND sbcl --noinform --script build_sbcl.lisp
  DEPENDS ${SOURCES}
)
add_custom_target(test_1_ecl ALL
  COMMAND ecl -shell build_ecl.lisp
  DEPENDS ${SOURCES}
)

enable_testing()
add_test(NAME test_1_sbcl COMMAND test_1_sbcl)
add_test(NAME test_1_ecl COMMAND test_1_ecl)
