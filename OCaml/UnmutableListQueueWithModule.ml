module type QUEUE_FUN =
sig
  type 'a t
  exception Empty of string
  val empty: unit -> 'a t
  val enqueue: 'a * 'a t -> 'a t
  val dequeue: 'a t -> 'a t
  val first: 'a t -> 'a
  val isEmpty: 'a t -> bool
end;;

module ListQueue : QUEUE_FUN =
struct
  type 'a t = 'a list

  exception Empty of string

  let empty () = []

  let enqueue (element, queue) = queue @ [element]

  let dequeue = function
    | [] -> []
    | head :: tail -> tail

  let first = function
    | [] -> raise (Empty "module ListQueue: first")
    | head :: tail -> head

  let isEmpty queue = queue = []
end;;


module PairOfListsQueue : QUEUE_FUN =
struct
  type 'a t = 'a list * 'a list

  exception Empty of string

  let empty () = ([], [])

  let normalize = function
    | ([], endOfQueue) -> (List.rev endOfQueue, [])
    | normalQueue -> normalQueue

  let enqueue (element, queue) =
    normalize(fst queue, element :: snd queue)

  let dequeue = function
    | ([], _) -> ([], [])
    | (head :: tail, endOfQueue) -> normalize(tail, endOfQueue)

  let first queue =
    match fst queue with
      | [] ->raise (Empty "module PairOfListsQueue: first")
      | (head :: tail) -> head

  let isEmpty queue =
    fst queue = []
end;;
