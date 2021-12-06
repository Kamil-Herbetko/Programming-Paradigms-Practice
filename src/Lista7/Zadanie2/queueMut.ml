(* Kamil Herbetko *)
(* Zadanie 2 *)
type 'a t = {mutable f: int; mutable r: int; mutable a: 'a option array}
exception Empty of string
exception Full of string

let empty n = {f = 0; r = 0; a = Array.make (n + 1) None}

let enqueue(e, q) = begin if ((q.r + 1) mod (Array.length q.a)) = q.f then
                            raise (Full "module QueueMut: operation enqueue applied to a full queue")
	                    else
	                        begin
	                        	q.a.(q.r) <- Some e;
                                q.r <- (q.r + 1) mod (Array.length q.a)
	                        end;
                    end
let dequeue q = begin if q.f = q.r then ()
                    else q.f <- q.f + 1
                end

let first q = begin if q.f = q.r then raise (Empty "module QueueMut: operation first applied to an empty queue")
                  else match q.a.(q.f) with
                        | Some e -> e
                        | None -> failwith "module QueueMut: top (implementation error!)"
              end

let isEmpty q = q.f = q.r

let isFull q = ((q.r + 1) mod (Array.length q.a)) = q.f