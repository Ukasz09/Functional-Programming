(********************************************************************)
(*                              Functions                           *)
(********************************************************************)

(* Insert new element to sorted list with keeping order (list is still sorted) *)
let rec insertInSortedList list ?predicate:(predicate=fun a b->a>b) value =
  match list with
      []->[value]
    | hd::tl -> if(predicate hd value) then value::hd::tl
        else hd::insertInSortedList tl value ~predicate:predicate
;;
