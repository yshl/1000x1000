module Lin = struct
    let swap a i j =
        let tmp = a.(i) in
        a.(i)<-a.(j);
        a.(j)<-tmp;;

    let arrsub a b fac ibegin =
        for i=ibegin to (Array.length a)-1 do
            a.(i)<-a.(i) -. fac *. b.(i)
        done;;

    let prod a b ibegin =
        let r = ref 0.0 in
        for i = ibegin to (Array.length a)-1 do
            r := !r +. a.(i) *. b.(i)
        done;
        !r;;

    let scale_row factor ai jbegin b i =
        for j = jbegin to (Array.length ai)-1 do
            ai.(j) <- factor *. ai.(j)
        done;
        b.(i) <- factor *. b.(i);;

    let scale_array a b=
        let absmax ai =
            Array.fold_left (fun x aij->max x (abs_float aij)) 0.0 ai in
        for i=0 to (Array.length a)-1 do
            let factor = 1.0 /. (absmax a.(i)) in
            scale_row factor a.(i) 0 b i;
        done;;

    let maxji a i =
        let ajmax=ref (abs_float a.(i).(i)) in
        let maxi=ref i in
        for j=i+1 to (Array.length a)-1 do
            let aji = abs_float a.(j).(i) in
            if aji > !ajmax then (
                ajmax := aji;
                maxi := j
            )
        done;
        !maxi;;

    let pivot a b i=
        let maxi =maxji a i in
        if i<>maxi then (
            swap a i maxi;
            swap b i maxi
            );;

    let scale_pivot a b i=
        let factor = 1.0 /. a.(i).(i) in
        scale_row factor a.(i) (i+1) b i;;

    let elim_col a b i=
        let ai=a.(i) in
        for j = i+1 to (Array.length a) -1 do
            let aj = a.(j) in
            let factor = aj.(i) in
            arrsub aj ai factor (i+1);
            b.(j) <- b.(j) -. factor *. b.(i)
        done;;

    let forward_elimination a b=
        for i = 0 to (Array.length a)-1 do
            pivot a b i;
            scale_pivot a b i;
            elim_col a b i
        done;;

    let back_substitution a b=
        for i = (Array.length b)-1 downto 0 do
            b.(i) <- b.(i) -. (prod a.(i) b (i+1))
        done;
        b;;

    let solve a b=
        scale_array a b;
        forward_elimination a b;
        back_substitution a b;;

end
module type Lin=
    sig
        val solve : 'float array array -> 'float array -> 'float array
end
