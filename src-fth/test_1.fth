s" lin.fth" included
1000 value n
n dup * floats allocate throw value a
n floats allocate throw value b

: f-fill! { v n f: r -- }
    n floats 0 +do
        v i + r f!
    float +loop
;
: set-a { a n -- }
    a n dup * 1.0e0 f-fill!
    n 0 +do
        a i n * i + floats +
        1001.0e0 f!
    loop
;
: check-x { b n -- }
    0.0e0
    n floats 0 +do
        b i + f@ 0.5e0 f-
        fover fabs fover fabs f> 0= if
            fswap
        then
        fdrop
    float +loop
    fdup fs. cr
    1.0e-8 f< 0= if 1 throw then
;
a n set-a
b n 1000.0e0 f-fill!
a b n solve

b n check-x

b free
a free
bye
