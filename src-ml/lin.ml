module Lin = struct
    let swap a i j =
        let tmp = a.(i) in
        a.(i)<-a.(j);
        a.(j)<-tmp;;

    let arrsub a b fac ibegin iend=
        for i=ibegin to iend-1 do
            a.(i)<-a.(i) -. fac *. b.(i)
        done;;

    let prod a b ibegin =
        let r = ref 0.0 in
        for i = ibegin to (Array.length a)-1 do
            r := !r +. a.(i) *. b.(i)
        done;
        !r;;

    let scale_row factor ai jbegin jend b i =
        for j = jbegin to jend-1 do
            ai.(j) <- factor *. ai.(j)
        done;
        b.(i) <- factor *. b.(i);;

    let scale_array a b=
        let absmax ai =
            Array.fold_left (fun x aij->max x (abs_float aij)) 0.0 ai in
        for i=0 to (Array.length a)-1 do
            let factor = 1.0 /. (absmax a.(i)) in
            scale_row factor a.(i) 0 (Array.length a) b i;
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

    let update_lower_col a b i blockend=
        let n=(Array.length a) in
        for i1=i to blockend-1 do
            pivot a b i1;
            let ai=a.(i1) in
            scale_row (1.0 /. ai.(i1)) ai (i1+1) blockend b i1;
            for j=i1+1 to n-1 do
                let aj=a.(j) in
                let factor=aj.(i1) in
                arrsub aj a.(i1) factor (i1+1) blockend;
                b.(j) <- b.(j) -. factor *. b.(i1)
            done
        done;;

    let update_upper_row a i blockend=
        let n=(Array.length a) in
        for i1=i to blockend-1 do
            let ai=a.(i1) in
            let factor=1.0 /. ai.(i1)in
            for j=blockend to n-1 do
                ai.(j) <- ai.(j) *. factor
            done;
            for j=i1+1 to blockend-1 do
                let aj=a.(j) in
                arrsub aj ai aj.(i1) blockend n
            done
        done;;

    let update_a a i blockend=
        let n=(Array.length a) in
        let blocksize=4 in
        for j=blockend to n-1 do
            let aj=a.(j) in
            let mend=(blockend-i)/blocksize in
            for m=0 to mend-1 do
                let l=i+m*blocksize in
                let al0=a.(l+0) in
                let al1=a.(l+1) in
                let al2=a.(l+2) in
                let al3=a.(l+3) in
                let ajl0=aj.(l+0) in
                let ajl1=aj.(l+1) in
                let ajl2=aj.(l+2) in
                let ajl3=aj.(l+3) in
                for k=blockend to n-1 do
                    aj.(k)<- aj.(k) -. (ajl0 *. al0.(k) +. ajl1 *. al1.(k)
                        +. ajl2 *. al2.(k) +. ajl3 *. al3.(k))
                done
            done;
            for l=i+blocksize*mend to blockend-1 do
                let al=a.(l) in
                let ajl=aj.(l) in
                for k=blockend to n-1 do
                    aj.(k)<- aj.(k) -. ajl *. al.(k)
                done
            done
        done;;

    let forward_elimination a b=
        let n=Array.length a in
        let blocksize=32 in
        let rec forward_rec i=
            if i<n then
                begin
                    let blockend=min (i+blocksize) n in
                    update_lower_col a b i blockend;
                    update_upper_row a i blockend;
                    update_a a i blockend;
                    forward_rec (i+blocksize)
                end
            in
            forward_rec 0;;

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
