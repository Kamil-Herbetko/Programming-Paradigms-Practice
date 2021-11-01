(* Kamil Herbetko *)

(* Zadanie 2 *)
let rec f x = f x;;

(* Zadanie 3 *)
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let tt = Node(1,
               Node(2,
                     Node(4,
                           Empty,
                           Empty
                         ),
                     Empty
                    ),
               Node(3,
                     Node(5,
                          Empty,
                          Node(6,
                               Empty,
                               Empty
                              )
                          ),
                     Empty
                     )
              );;
let sBT = Node("a",
               Node("b",
                     Node("d",
                           Node("g",
                                Empty,
                                Empty
                               ),
                           Empty
                         ),
                     Node("e",
                          Empty,
                          Empty
                         )
                    ),
               Node("c",
                     Node("f",
                          Empty,
                          Empty
                          ),
                     Empty
                     )
              );;


let breadthBT bt =
    let rec breadthBT_iter = function
    | [] -> []
    | Empty :: t -> breadthBT_iter(t)
    | Node(v, t1, t2) :: t -> v :: breadthBT_iter (t @ [t1; t2])
    in breadthBT_iter [bt];;

breadthBT tt = [1; 2; 3; 4; 5; 6];;
breadthBT sBT = ["a"; "b"; "c"; "d"; "e"; "f"; "g"];;
breadthBT Empty = [];;

(* Zadanie 4 *)
(* a *)
let intBT bt =
    let rec intBT_iter bt depth =
    match bt with
    | Empty -> 0
    | Node(v, t1, t2) -> depth + intBT_iter t1 (depth + 1) + intBT_iter t2 (depth + 1)
    in intBT_iter bt 0;;

intBT tt = 9;;
intBT sBT = 11;;
intBT Empty = 0;;

(* b *)
let extBT bt =
    let rec extBT_iter bt depth =
    match bt with
    | Empty -> depth
    | Node(v, t1, t2) -> extBT_iter t1 (depth + 1) + extBT_iter t2 (depth + 1)
    in extBT_iter bt 0;;

extBT tt = 21;;
extBT sBT = 25;;
extBT Empty = 0;;

(* Zadanie 5 *)
type 'a graph = Graph of ('a -> 'a list);;

let g = Graph
(function
    | 0 -> [3]
    | 1 -> [0; 2; 4]
    | 2 -> [1]
    | 3 -> []
    | 4 -> [0; 2]
    | n -> failwith ("Graph g: node " ^ string_of_int n ^ " doesn't exist")
);;

let sg = Graph
(function
    | "a" -> ["a"; "b"; "d"]
    | "b" -> ["g"]
    | "c" -> []
    | "d" -> ["c"; "e"]
    | "e" -> []
    | "f" -> ["e"]
    | "g" -> ["d"; "f"]
    | s -> failwith ("Graph g: node " ^ s ^ " doesn't exist")
);;


let depthSearch (Graph graph) startNode =
    let rec search visited = function
    | [] -> []
    | h :: t -> if List.mem h visited then search visited t
                else h :: search (h :: visited) (graph h @ t)
    in search [] [startNode];;

depthSearch g 4 = [4; 0; 3; 2; 1];;
depthSearch sg "a" = ["a"; "b"; "g"; "d"; "c"; "e"; "f"];;
depthSearch g 2 = [2; 1; 0; 3; 4];;