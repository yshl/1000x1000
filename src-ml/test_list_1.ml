open Lin_list;;
let n=1000;;
let a=Array.to_list (
    Array.init n (fun i->
        let ai=Array.make n 1.0 in
        ai.(i)<- 1001.0;
        Array.to_list ai));;
let b=Array.to_list(Array.make n 1000.0);;
let b=Lin_list.solve a b;;
let maxerr=List.fold_left (fun errmax bi->
    if not((abs_float (bi -. 0.5)) <= (abs_float errmax)) then
        bi -. 0.5
    else
        errmax
) 0.0 b;;
Printf.printf "%g\n" maxerr;;
exception Large_error;;
if not((abs_float maxerr) <= 1.0e-8) then
    raise Large_error
