project(src-java NONE)

set(JAVA java)
set(JAVAC javac)
set(JAR jar)
set(JAVAFLAGS)

function(add_executable_java TARGET)
  set(SOURCES ${ARGN})
  add_custom_target(${TARGET} ALL
    COMMAND ${JAR} cvfm ${TARGET} ${SOURCES}
    DEPENDS ${SOURCES}
  )
endfunction()

function(add_class_java TARGET SOURCE)
  add_custom_target(${TARGET}
    COMMAND ${JAVAC} ${JAVAFLAGS} ${SOURCE}
    DEPENDS ${SOURCE}
  )
endfunction()

add_executable_java(Test_1.jar Manifest.txt Test_1.class Lin.class)
add_class_java(Test_1.class test_1.java)
add_class_java(Lin.class lin.java)

enable_testing()
add_test(NAME test_1_java COMMAND java -jar Test_1.jar)
