(* Kamil Herbetko *)

(* zadanie 2 *)
let rec fib n =
    if n <= 0 then 0
    else if n = 1 then 1
    else fib (n - 1) + fib (n - 2);;

let fibTail n =
    let rec fibTail_iter (n, accum1, accum2) =
        if n < 0 then accum1
        else if n = 0 then accum2
        else fibTail_iter (n - 1, accum2, accum1 + accum2)
    in fibTail_iter (n - 1, 0, 1)
;;

fib 5 = 5;;
fib 0 = 0;;
fib(-3) = 0;;
fibTail 5 = 5;;
fibTail 0 = 0;;
fibTail(-3) = 0;;
fib 42 = 267914296;;
fibTail 42 = 267914296;;

(* zadanie 3 *)
let root3 a =
    let rec root3_iter (a, accum) =
        if abs_float(accum ** 3.0 -. a) <= abs_float a *. 10.0 ** (-15.0) then accum
        else root3_iter (a, accum +. (a /. accum ** 2.0 -. accum) /. 3.0)
    in if a > 1.0 then root3_iter(a, a /. 3.0) else root3_iter(a, a);;

root3 0.0 -. 0.0 <= 10.0 ** (-15.0);;
root3 10.0 -. 2.1544346900318837 <= 10.0 ** (-15.0);;
root3 (-10.0) +. 2.1544346900318837 <= 10.0 ** (-15.0);;

(* zadanie 4 *)
(* a) *)
let [_; _; xa; _; _] = [-2; -1; 0; 1; 2];;

(* b) *)
let [(_, _); (xb, _)] = [(1, 2); (0, 1)];;

(* zadanie 5 *)
let rec initSegment (xs, ys) =
    match (xs, ys) with
    | ([], ys) -> true
    | (h1::t1, h2::t2) when h1 = h2 -> initSegment (t1, t2)
    | _ -> false;;

initSegment ([1; 2; 3; 4], [1; 2; 3; 4; 5; 6]) = true;;
initSegment ([], [1; 2; 3]) = true;;
initSegment ([1; 2; 3; 4; 5], [1; 2; 6; 7; 8; 9]) = false;;
initSegment (['a'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd']) = true;;

(* zadanie 6 *)
(* a) *)
let rec replaceNth (xs, n, x) =
    match (xs, n) with
    | ([], n) -> []
    | (h::t, n) when n <= 0 -> x::t
    | (h::[], n) -> [x]
    | (h::t, n) -> h::replaceNth (t, n-1, x);;

replaceNth (['o';'l';'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'], 1, 's') = ['o';'s';'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'];;
replaceNth ([1; 2; 3; 4; 5], 3, 7) = [1; 2; 3; 7; 5];;
replaceNth ([], 3, 7) = [];;
replaceNth ([1; 2; 3; 4; 5], 0, 6) = [6; 2; 3; 4; 5];;
replaceNth ([1; 2; 3], -5, 6) = [6; 2; 3];;
replaceNth ([1; 2; 3; 4; 5], 10, 8) = [1; 2; 3; 4; 8];;
