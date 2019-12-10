(* Cyclic mutable array queue by module implementation with own signature *)
module type QUEUE_MUT =
sig
  type 'a t
  exception Empty of string
  exception Full of string
  val empty: int -> 'a t
  val enqueue: 'a * 'a t -> unit
  val dequeue: 'a t -> unit
  val first: 'a t -> 'a
  val isEmpty: 'a t -> bool
  val isFull: 'a t -> bool
end;;

module CyclicArrayQueue : QUEUE_MUT =
struct
  type 'a t = { arr : 'a option array; mutable f: int; mutable r: int }
  exception Empty of string
  exception Full of string

  let empty n = { arr = Array.make (n + 1) None; f = 0; r = 0 }

  let isEmpty queue = queue.r = queue.f

  let isFull queue =
    (* index r < index f *)
    queue.r - queue.f = -1 
    (* index r > index f *) 
    || queue.r - queue.f = Array.length queue.arr - 1

  let enqueue (element, queue) =
    if (isFull queue) then raise (Full "module CyclicArrayQueue: enqueue")
    else queue.arr.(queue.r) <- Some element;

    (* if last index *)
    if queue.r = Array.length queue.arr - 1 then queue.r <- 0
    else queue.r <- succ(queue.r)

  let dequeue queue =
    if (isEmpty queue) then () else
      (* if last index *)
    if queue.f = Array.length queue.arr - 1 then queue.f <- 0
    else queue.f <- succ(queue.f)

  let first queue =
    if (isEmpty queue) then raise (Empty "module CyclicArrayQueue: first")
    else match (queue.arr.(queue.f)) with
      | Some value -> value
      | None -> failwith "module CyclicArrayQueue: first (implementation error!!!)"
end;;
