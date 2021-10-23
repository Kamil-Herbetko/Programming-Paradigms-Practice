(* Kamil Herbetko *)

(* zadanie 1 *)
let rec flatten1 xss =
    if List.length xss = 0 then []
    else List.hd xss @ flatten1( List.tl xss);;

flatten1 [[5; 6];[1; 2; 3]] = [5; 6; 1; 2; 3];;
flatten1 [[]] = [];;
flatten1 [["Ma"; "ły"];["Ko"; "tek"]] = ["Ma"; "ły"; "Ko"; "tek"]

(* zadanie 2 *)
let rec count (x, xs) =
    if List.length xs = 0 then 0
    else if List.hd xs = x then 1 + count(x, List.tl xs)
    else count(x, List.tl xs);;

count ('a', ['a'; 'l'; 'a']) = 2;;
count (5, []) = 0;;
count(1, [1; 2; 3; 4; 5; 1; 1; 3]) = 3;;
count([], []) = 0;;
count([], [[]]) = 1;;

(* zadanie 3 *)
let rec replicate (x, n) =
    if n <= 0 then []
    else x :: replicate(x, n - 1);;

replicate("la", 3) = ["la"; "la"; "la"];;
replicate("komar", 0) = [];;
replicate(2, 5) = [2; 2; 2; 2; 2];;
replicate([], 2) = [[];[]];;
replicate("la", -3) = [];;

(* zadanie 4 *)
let rec sqrList xs =
    if List.length xs = 0 then []
    else List.hd xs * List.hd xs :: sqrList(List.tl xs);;

sqrList [1; 2; 3; -4] = [1; 4; 9; 16];;
sqrList [0; -9; -3; 3; 9; 0] = [0; 81; 9; 9; 81; 0];;
sqrList [] = [];;

(* zadanie 5 *)
let palindrome xs =
    if xs = List.rev xs then true
    else false;;

palindrome ['a'; 'l'; 'a'] = true;;
palindrome [1; 2; 3; 2; 1] = true;;
palindrome ['n'; 'i'; 'e'] = false;;
palindrome [] = true;;

(* zadanie 6 *)
let rec listLength xs =
    if xs = [] then 0
    else 1 + listLength(List.tl xs);;

listLength ['a'; 'b'; 'c'] = 3;;
listLength [] = 0;;
listLength [1; 2; 3; 4] = 4;;
listLength [[]; []] = 2;;