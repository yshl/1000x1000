s" lin.fth" included
1000 value n
n dup * floats allocate throw value a
n floats allocate throw value b

: f-fill! ( v n f:r -- )
    floats over + swap ?do
        fdup i f!
    float +loop
    fdrop
;
: set-a { a n -- }
    a n dup * 1.0e0 f-fill!
    n 0 +do
        a n 1+ i * floats +
        1001.0e0 f!
    loop
;
: fabsmax ( f1 f2 -- f3 )
    fover fabs fover fabs
    f> 0= if fswap then
    fdrop
;
: check-x ( b n -- )
    0.0e0
    floats over + swap ?do
        i f@ 0.5e0 f- fabsmax
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
