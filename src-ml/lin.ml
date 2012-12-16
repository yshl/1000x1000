module Lin = struct
    let swap a i j =
        let tmp = a.(i) in
        begin
            a.(i) <- a.(j);
            a.(j) <- tmp;
            a
        end;;

    let absmax ai j=
        let n=Array.length ai in
        let rec absmax_rec aijmax j=
            if j>=n then
                aijmax
            else
                let aij=abs_float (ai.(j))in
                if aij>aijmax then
                    absmax_rec aij (j+1)
            else
                absmax_rec aijmax (j+1) in
        absmax_rec ai.(j) (j+1);;

    let scale a b=
        let n=Array.length a in
        for i=0 to n-1 do
            let factor=1.0 /. absmax a.(i) 0 in
            for j=0 to n-1 do
                a.(i).(j) <- factor *. a.(i).(j)
            done;
            b.(i) <- factor *. b.(i)
            done;
            a,b;;

    let pivot a b i=
        let rec maxji j ajimax maxi=
            if j >= Array.length a
            then maxi
            else
                (let aji=abs_float (a.(j).(i)) in
                if (aji>ajimax)
                then maxji (j+1) aji j
                else maxji (j+1) ajimax maxi) in
        let maxi=maxji (i+1) (a.(i).(i)) i in
        if i==maxi then (a, b)
        else ((swap a i maxi), (swap b i maxi));;

    let scale_row a b i=
        let n = Array.length a.(i) in
        let factor = 1.0 /. a.(i).(i) in
        begin
            for j=i+1 to n-1 do
                a.(i).(j) <- factor *. a.(i).(j) 
        done;
        b.(i) <- factor *. b.(i);
        a, b
        end;;

    let elim_col a b i=
        let n = Array.length a in
        begin
            for j=i+1 to n-1 do
                let factor = a.(j).(i) in
                begin
                    for k=i+1 to n-1 do
                        a.(j).(k) <- a.(j).(k) -. factor *. a.(i).(k)
            done;
            b.(j) <- b.(j) -. factor *. b.(i);
            ()
        end
                    done;
                    a,b
                end;;

    let forward_elimination a b=
        let n = Array.length a in
        let rec forward_elim_rec a b i =
            if i >= n then (a,b)
            else
                let a, b=pivot a b i in
                let a, b=scale_row a b i in
                let a, b=elim_col a b i in
                forward_elim_rec a b (i+1) in
        forward_elim_rec a b 0;;

    let back_substitution a b=
        let n = Array.length b in
        let rec back_subst_rec a b i=
            if i<0 then b
            else
                begin
                    for j=i+1 to n-1 do
                        b.(i) <- b.(i) -. a.(i).(j) *. b.(j)
            done;
            back_subst_rec a b (i-1)
        end in
        back_subst_rec a b (n-1)

    let solve a b=
        let a,b=scale a b in
        let a,b=forward_elimination a b in
        back_substitution a b
                end
module type Lin=
    sig
        val solve : 'float array array -> 'float array -> 'float array
    end
