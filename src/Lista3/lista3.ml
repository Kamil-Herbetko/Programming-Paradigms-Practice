(* Kamil Herbetko *)

(* Zadanie 2 *)
let curry3 f a b c = f (a, b, c);;
let curry3 = function f -> function a -> function b -> function c -> f (a, b, c);;

let uncurry3 f (a, b, c) = f a b c;;
let uncurry3 = function f -> function (a, b, c) -> f a b c;;

(* Zadanie 3 *)
let sumProd xs = List.fold_left (fun (sum, prod) -> fun h -> (sum + h, prod * h)) (0, 1) xs;;

sumProd [1; 2; 3; 4] = (10, 24);;

(* Zadanie 5 *)
(* a *)
let insertionsort pred xs =
    let rec insert pred x xs =
        match xs with
        | [] -> [x]
        | h :: t as ys -> if pred x h then x :: ys
                          else h :: (insert pred x t)
    in
    let rec insertionsort_iter xs ys =
        match xs with
        | [] -> ys
        | h :: t -> insertionsort_iter t (insert pred h ys)
    in insertionsort_iter xs [];;


insertionsort (<) [5; 4; 4; 2; 5; 3] = [2; 3; 4; 4; 5; 5];;
insertionsort (<) ["zs"; "ab1"; "dc"; "cd"] =  ["ab1"; "cd"; "dc"; "zs"];;
insertionsort (fun (x1, y1) (x2, y2) -> x1 < x2) [(6, 2); (6, 3); (4, 2); (1, 3); (1, 2)] = [(1, 3); (1, 2); (4, 2); (6, 2); (6, 3)];;
insertionsort (fun (x1, y1) (x2, y2) -> x1 < x2) [(6, 3); (6, 2); (1, 2); (4, 2); (1, 3)] = [(1, 2); (1, 3); (4, 2); (6, 3); (6, 2)];;
(* b *)
let rec mergesort pred xs =
    match xs with
        | [] -> []
        | [x] -> [x]
        | h :: t as l1 -> let (h1, h2) = halfsplit l1 in merge pred (mergesort h1, mergesort h2)

and halfsplit xs =
    match xs with
    | [] -> ([], [])
    | h :: t -> let (h1, h2) = halfsplit t in (h :: h2, h1)

and merge pred (xs, ys) =
    match (xs, ys) with
    | (xs, []) -> xs
    | ([], ys) -> ys
    | (h1 :: t1 as l1, h2 :: t2 as l2) -> if pred h1 h2 then h1 :: merge (t1, l2)
                                          else h2 :: merge (l1, t2);;





