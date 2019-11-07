type 'a graph = Graph of ('a -> 'a list)

(*Breadth-first search (BFS) algorithm for traversing graph*)
let breadthSearch (Graph succ) startNode =
  let rec search visited = function
      [] -> []
    | h::t -> if List.mem h visited then search visited t
        else h::search (h :: visited) (t @ succ h)
  in search [] [startNode]
;;

(*Depth-first search (DFS) algorithm for traversing graph*)
let depthSearch (Graph succ) startNode =
  let rec search visited = function
      [] -> []
    | h::t -> if List.mem h visited then search visited t
        else h::search (h :: visited) (succ h @ t)
  in search [] [startNode]
;;

