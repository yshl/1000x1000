: f*! ( f: r addr -- )
    dup f@ f* f!
;

: rowmax { arow n col -- }
    0.0e0
    n floats col floats +do
        arow i + f@ fabs fmax
    float +loop
;

: absmax { a n row col -- }
    a n row * floats + n col rowmax
;

: maxji { a n col -- piv }
    0.0e0 col
    n col +do
        a i n * col + floats + f@ fabs
        fover fover f< if
            drop i
            fswap
        then
        fdrop
    loop
    fdrop
;

: swap-array { a ind1 ind2 -- }
    a ind1 floats +
    a ind2 floats +
    2dup f@ f@ f! f!
;

: swap-row { a row1 row2 n -- }
    n row1 +do
        a
        n row1 * i +
        n row2 * i + swap-array
    loop
;

: pivot { a b n row -- }
    a n row maxji
    dup row <> if
        dup a row rot swap-row
        b row rot swap-array
    else
        drop
    then
;

: fa* { f: r arr n -- }
    n floats 0 +do
        arr i + dup f@ r f* f!
    float +loop
;
: fa*- { f: r arr1 arr2 n col -- }
    n floats col floats +do
        arr1 i + dup f@
        arr2 i + f@ r f* f- f!
    float +loop
;

: scale-pivot { a b n row -- }
    a n row * row + floats + f@
    1.0e0 fswap f/
    n row +do
        fdup
        a n row * i + floats + f*!
    loop
    b row floats + f*!
;

: scale-array { a b n -- }
    n 0 +do
        a n i 0 absmax
        1.0e0 fswap f/ fdup
        a n i * floats + n fa*
        b i floats + f*!
    loop
;

: elim-col { a b n row -- }
    n row 1+ +do
        a i n * row + floats + f@ fdup

        a i n * floats +
        a row n * floats + n row 1+ fa*-
        b i floats +
        b row floats + 1 0 fa*-
    loop
;

: forward-elimination { a b n -- }
    n 0 +do
        a b n i pivot
        a b n i scale-pivot
        a b n i elim-col
    loop
;

: back-substitution { a b n -- }
    -1 n 1- -do
        b i floats + f@
        n i 1+ +do
            a j n * i + floats + f@
            b i floats + f@
            f* f-
        loop
        b i floats + f!
    1 -loop
;

: solve { a b n -- }
    a b n scale-array
    a b n forward-elimination
    a b n back-substitution
;
