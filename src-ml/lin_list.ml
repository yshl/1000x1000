module Lin_list = struct
    let listmul a fac =
        List.map (fun ai -> fac *. ai) a;;

    let listsub a b fac =
        List.map2 (fun ai bi -> ai -. fac *. bi) a b;;

    let prod a b =
        List.fold_left2 (fun x ai bi -> x +. ai *. bi) 0.0 a b;;

    let scale_array ab=
        let absmax ai =
            List.fold_left (fun x aij -> max x (abs_float aij)) 0.0 ai in
        let scale_row abi =
            let ai, bi=abi in
            let factor = 1.0 /. (absmax ai) in
            let newai = listmul ai factor in
            let newbi = factor *. bi in
            newai, newbi in
        List.map scale_row ab;;

    let pivot ab=
        let getaji abi = abs_float (List.hd (fst abi)) in
        let rec pivot_rec pivot ab newab=
            match ab with
            |[] -> pivot::newab
            |abi::abtail ->
                    if (getaji abi) > (getaji pivot) then
                        pivot_rec abi abtail (pivot::newab)
                    else
                        pivot_rec pivot abtail (abi::newab) in
        match ab with
        |[] -> []
        |abhd::abtl -> pivot_rec abhd abtl [];;

    let scale_pivot abi=
        match abi with
        | [], _ -> failwith "invalid arg" 
        | aii::aij, bi ->
                let factor = 1.0 /. aii in
                let newaijs = listmul aij factor in
                let newbi = factor *. bi in
                newaijs, newbi;;

    let elim_col abjs aij bi =
        List.rev_map (fun abj ->
            match abj with
            | [], _ -> failwith "invalid arg"
            | aji::ajj, bj -> (listsub ajj aij aji),(bj -. aji *. bi)
            ) abjs;;

    let forward_elimination ab =
        let rec forward_rec ab acc=
            let ab = pivot ab in
            match ab with
            | [] -> acc
            | abi::abtl ->
                    let aijs, bi=scale_pivot abi in
                    let newab = elim_col abtl aijs bi in
                    forward_rec newab ((aijs, bi)::acc)  in
        forward_rec ab [];;

    let back_substitution ab =
        let rec back_rec ab bj=
            match ab with
            | [] -> bj
            | (aij, bi)::abjj ->
                    let newb = bi -. (prod aij bj) in
                    back_rec abjj (newb::bj) in
        back_rec ab [];;

    let solve a b=
        let ab=List.map2 (fun ai bi->(ai,bi)) a b in
        let ab=scale_array ab in
        let ab=forward_elimination ab in
        back_substitution ab;;

end
module type Lin_list=
    sig
        val solve : 'float list list -> 'float list -> 'float list
end
