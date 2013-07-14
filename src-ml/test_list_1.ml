open Lin_list;;
let n=1000;;
let make_list n func=
    let rec make_row1 j lis=
        if j<0 then lis
        else make_row1 (j-1) ((func j)::lis)
    in make_row1 (n-1) [];;
let a=make_list n (fun i->make_list n (fun j->if j=i then 1001.0 else 1.0));;
let b=make_list n (fun i->1000.0);;
let b=Lin_list.solve a b;;
let maxerr=List.fold_left (
    fun errmax bi->
        if not((abs_float (bi -. 0.5)) <= (abs_float errmax))
        then bi -. 0.5
        else errmax
        ) 0.0 b;;
Printf.printf "%g\n" maxerr;;
exception Large_error;;
if not((abs_float maxerr) <= 1.0e-8) then
    raise Large_error
