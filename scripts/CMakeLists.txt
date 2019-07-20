project(scripts NONE)

set(RCOMMAND Rscript)
set(OCTAVE octave)
set(SCILAB scilab)

enable_testing()
add_test(NAME test_r1 COMMAND ${RCOMMAND} lin.r)
add_test(NAME test_r2 COMMAND ${RCOMMAND} lin.r)
add_test(NAME test_octave1 COMMAND ${OCTAVE} -q lin.m)
add_test(NAME test_octave2 COMMAND ${OCTAVE} -q lin.m)
add_test(NAME test_scilab1 COMMAND ${SCILAB} -nwni -nb -f lin.sce)
add_test(NAME test_scilab2 COMMAND ${SCILAB} -nwni -nb -f lin.sce)
