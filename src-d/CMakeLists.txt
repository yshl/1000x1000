project(src-d NONE)

set(DC gdc)
set(DFLAGS -g -O3 -Wall)
set(OBJOPT -o)
#set(DC ldc2)
#set(DFLAGS -g -O3)
#set(OBJOPT "-of=")

function(add_executable_d TARGET)
  set(SOURCES ${ARGN})
  add_custom_target(${TARGET} ALL
    COMMAND ${DC} ${DFLAGS} ${OBJOPT}${TARGET} ${SOURCES}
    DEPENDS ${SOURCES}
  )
endfunction()

function(add_object_d TARGET SOURCE)
  add_custom_target(${TARGET}
    COMMAND ${DC} ${DFLAGS} -c ${OBJOPT}${TARGET} ${SOURCE}
    DEPENDS ${SOURCE}
  )
endfunction()

add_executable_d(test_static_1_d test_static_1.d lin_static.o)
add_executable_d(test_dynamic_1_d test_dynamic_1.d lin_dynamic.o)
add_object_d(lin_static.o lin_static.d)
add_object_d(lin_dynamic.o lin_dynamic.d)

enable_testing()
add_test(NAME test_static_1_d COMMAND test_static_1_d)
add_test(NAME test_dynamic_1_d COMMAND test_dynamic_1_d)
