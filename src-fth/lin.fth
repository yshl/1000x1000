: f*! ( f: r addr -- )
    dup f@ f* f!
;
: fa* ( f:r arr n -- )
    floats over + swap u+do
        fdup i dup f@ f* f!
    float +loop
    fdrop
;

: rowmax ( arow n -- fmax )
    0.0e0
    floats over + swap +do
        i f@ fabs fmax
    float +loop
;
: scale-array { a b n -- }
    n 0 +do
        a n i * floats +
        dup n rowmax 1.0e0 fswap f/
        fdup n fa*
        b i floats + f*!
    loop
;

: maxji { ai n len -- piv }
    0.0e0 0
    len 0 +do
        ai i n * floats + f@ fabs
        fover fover f< if
            drop i
        then
        fmax
    loop
    fdrop
;

: swap-array ( ai1 ai2 -- )
    2dup f@ f@ f! f!
;
: swap-row ( arow1 arow2 len -- )
    floats over + swap +do
        dup i swap-array
        float+
    float +loop
    drop
;
: pivot { ai bi n len -- }
    ai n len maxji
    dup if
        dup n * floats ai + ai len swap-row
        floats bi + bi swap-array
    else
        drop
    then
;

: scale-pivot { ai bi len -- }
    ai 1.0e0 dup f@ f/
    fdup len fa*
    bi f*!
;

: fa*- ( fr arr1 arr2 n -- )
    floats over + swap float+ +do
        float+
        dup dup f@
        fover
        i f@
        f* f- f!
    float +loop
    drop fdrop
;
: f*-! ( f: r addr1 addr2 -- )
    f@ f* dup f@ fover f- f! fdrop
;
: elim-col { ai bi n len -- }
    len 1 +do
        ai i n * floats +
        dup f@
        fdup ai len fa*-
        bi i floats + bi f*-!
    loop
;

: forward-elimination { a b n -- }
    n 0 +do
        a n 1+ i * floats +
        b i floats +
        2dup n n i - pivot
        2dup n i - scale-pivot
        n n i - elim-col
    loop
;

: dot ( arr1 arr2 len )
    0.0e0
    floats over + swap +do
        dup f@ i f@ f* f+
        float+
    float +loop
    drop
;
: back-substitution { a b n -- }
    -1 n 1- -do
        b i floats +
        dup f@
        dup float+
        a n 1+ i * 1+ floats + n i 1+ - dot f-
        f!
    1 -loop
;

: solve { a b n -- }
    a b n scale-array
    a b n forward-elimination
    a b n back-substitution
;
