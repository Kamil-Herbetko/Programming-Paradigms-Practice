(* Kamil Herbetko *)

(* Zadanie 2 *)
let curry3 f a b c = f (a, b, c);;
let curry3 = function f -> function a -> function b -> function c -> f (a, b, c);;
(* curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd = <fun> *)

let plus (x1, x2, x3) = x1 + x2 + x3;;
let concat3 (x1, x2, x3) = x1 ^ x2 ^ x3;;

let curriedPlus = curry3(plus);;
let curriedConcat3 = curry3(concat3);;

curriedPlus 1 2 3 = 6;;
curriedConcat3 "a" "b" "c" = "abc";;

let uncurry3 f (a, b, c) = f a b c;;
let uncurry3 = function f -> function (a, b, c) -> f a b c;;
(* uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd = <fun> *)

let plus x1 x2 x3 = x1 + x2 + x3;;
let concat3 x1 x2 x3 = x1 ^ x2 ^ x3;;

let uncurriedPlus = uncurry3 plus;;
let uncurriedConcat3 = uncurry3 concat3;;

uncurriedPlus(1, 2, 3) = 6;;
uncurriedConcat3("a", "b", "c") = "abc";;

(* Zadanie 3 *)
let sumProd xs = List.fold_left (fun (sum, prod) -> fun h -> (sum + h, prod * h)) (0, 1) xs;;

sumProd [1; 2; 3; 4] = (10, 24);;
sumProd [] = (0, 1);;
sumProd [4; 3; 2; 1] = (10, 24);;

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


let rec halfsplit n xs =
    match (n, xs) with
    | (0, xs) -> ([], xs)
    | n, h :: t -> let (l1, l2) = halfsplit (n - 1) t in (h :: l1, l2);;

let rec merge pred (xs, ys) =
    match (xs, ys) with
    | (xs, []) -> xs
    | ([], ys) -> ys
    | (h1 :: t1, h2 :: t2) -> if pred h1 h2 then h1 :: merge pred (t1, ys)
                              else h2 :: merge pred (xs, t2);;
let rec mergesort pred xs =
    match xs with
        | [] -> []
        | [x] -> [x]
        | h :: t as l1 -> let (h1, h2) = halfsplit (List.length l1 / 2) l1 in merge pred (mergesort pred h1, mergesort pred h2);;


mergesort (<=) [5; 4; 4; 2; 5; 3] = [2; 3; 4; 4; 5; 5];;
mergesort (<=) ["zs"; "ab1"; "dc"; "cd"] =  ["ab1"; "cd"; "dc"; "zs"];;
mergesort (fun (x1, y1) (x2, y2) -> x1 <= x2) [(6, 2); (6, 3); (4, 2); (1, 3); (1, 2)] = [(1, 3); (1, 2); (4, 2); (6, 2); (6, 3)];;
mergesort (fun (x1, y1) (x2, y2) -> x1 <= x2) [(6, 3); (6, 2); (1, 2); (4, 2); (1, 3)] = [(1, 2); (1, 3); (4, 2); (6, 3); (6, 2)];;




