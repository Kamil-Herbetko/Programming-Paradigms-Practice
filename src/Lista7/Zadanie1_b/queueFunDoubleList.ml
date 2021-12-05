(* Kamil Herbetko *)
(* Zadanie 1 *)
(* b) *)

type 'a t = EmptyQueue | Queue of 'a list * 'a list
exception Empty of string

let empty () = EmptyQueue

let enqueue (e, q) =
    match q with
        | EmptyQueue -> Queue([e], [])
        | Queue(l1, l2) -> Queue(l1, e :: l2)

let dequeue = function
    | Queue(_ :: t, l2) ->
        if t = [] && l2 = []  then EmptyQueue
        else if t = [] then Queue(List.rev l2, [])
        else Queue(t, l2)
    | EmptyQueue -> EmptyQueue

let first = function
    | Queue(h :: _, _) -> h
    | EmptyQueue -> raise (Empty "module QueueFunDoubleList: operation first applied to an empty queue")

let isEmpty = function
    | Queue(_, _) -> false
    | EmptyQueue -> true