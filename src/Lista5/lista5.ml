(* Kamil Herbetko *)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k + 1)));;

let rec toLazyList = function
    | [] -> LNil
    | x :: xs -> LCons (x, lazy (toLazyList xs));;

let rec ltake = function
    | (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons (x, lazy xs)) -> x :: ltake (n-1, xs);;

(* Zadanie 1 *)
let lrepeat k xs =
    let rec lrepeat_iter n ys =
    match n, ys with
    | _, LNil -> LNil
    | 1, LCons (x, lazy lxs) -> LCons (x, lazy (lrepeat_iter k lxs))
    | _, LCons (x, _) -> LCons(x, lazy (lrepeat_iter (n - 1) ys))
    in lrepeat_iter k xs;;

ltake(12, (lrepeat 3 (toLazyList ['a'; 'b'; 'c'; 'd']))) = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c'; 'd'; 'd'; 'd'];;
ltake(15, (lrepeat 3 (lfrom 1))) = [1; 1; 1; 2; 2; 2; 3; 3; 3; 4; 4; 4; 5; 5; 5];;
ltake(15, (lrepeat 3 LNil)) = [];;

(* Zadanie 2 *)
let lfib =
    let rec lfib_iter l0 l1 = LCons(l0, lazy (lfib_iter l1 (l0 + l1)))
    in lfib_iter 0 1;;

ltake(12, lfib) = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89];;
ltake(2, lfib) = [0; 1];;
ltake(0, lfib) = [0];;
ltake(0, lfib) = [];;

(* Zadanie 3 *)
type 'a lBT = LEmpty | LNode of 'a * (unit -> 'a lBT) * (unit -> 'a lBT);;

(* a *)
let lBreadth lbt =
    let rec lBreadth_iter = function
        | LEmpty :: t -> LNil
        | LNode (x, lLT, lRT) :: t -> LCons(x, lazy (lBreadth_iter (t @ [lLT(); lRT()])))
    in lBreadth_iter [lbt];;

(* b *)
let rec lTree n = LNode (n, (function () -> lTree (2 * n)), (function () -> lTree (2 * n + 1)));;


ltake (20, lBreadth (lTree 1)) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];;
ltake (20, lBreadth LEmpty) = [];;


