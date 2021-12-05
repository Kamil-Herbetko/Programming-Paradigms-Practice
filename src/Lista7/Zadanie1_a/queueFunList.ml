(* Kamil Herbetko *)
(* Zadanie 1 *)
(* a) *)
type 'a t = EmptyQueue | Queue of 'a list
exception Empty of string

let empty () = EmptyQueue

let enqueue (e, q) =
    match q with
        | EmptyQueue -> Queue([e])
        | Queue(l) -> Queue( l @ [e])
let dequeue = function
    | Queue(h :: []) -> EmptyQueue
    | Queue(h :: t) -> Queue(t)
    | EmptyQueue -> EmptyQueue

let first = function
    | Queue(h :: t) -> h
    | EmptyQueue -> raise (Empty "module QueueFunList: operation first applied to an empty queue")

let isEmpty = function
    | Queue(_) -> false
    | EmptyQueue -> true
