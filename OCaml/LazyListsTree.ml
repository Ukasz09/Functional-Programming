(********************************************************************)
(*                        ADT tree and lists                        *)
(********************************************************************)

type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

type 'a llistByCloser = LNil | LCons of 'a * (unit -> 'a llistByCloser);;

(********************************************************************)
(*                             Functions                            *)
(********************************************************************)
(*Breadth-first search (BFS) algorithm for traversing binary tree return normal list of elements
  WARNING: stack when lazy list is infinitive *)
let breadthSearchByNormalList tree =
  let rec search = function
      [] -> []
    |LEmpty::t ->search t 
    | LNode(h,lbt,rbt)::t -> h::search (t@[lbt();rbt()])
  in search[tree]
;;

(*Breadth-first search (BFS) algorithm for traversing binary tree return lazy list of elements*)
let  breadthSearchByLazyList tree =
  let rec search = function
      [] -> LNil
    |LEmpty::t ->search t 
    | LNode(h,lbt,rbt)::t -> LCons(h,function()->search (t@[lbt();rbt()]))
  in search[tree]

(* Print tree (rotated 90 degrees) *)
let printBT tree =
  let rec printHelper(t, height) = match t with
      LNode(value, left, right) ->
        begin
          printHelper(right(), height+1);

          for i=0 to height-1 do print_string "---" done;
          print_int value; print_newline();

          printHelper(left(), height+1);
        end
    | LEmpty -> for i=0 to height-1 do print_string "---" done; print_endline "||"
  in printHelper(tree, 0)
;;

