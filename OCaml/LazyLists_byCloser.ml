(* ADT for lazy list using function *)
type 'a lazyList = LNil | LCons of 'a * (unit -> 'a lazyList);;

(* Return head of lazy list *)
let lhd = function
    LNil -> failwith "lhd"
  | LCons (x, _) -> x
;;

(* Return tail of lazy list *)
let ltl = function
    LNil -> failwith "ltl"
  | LCons (_, xf) -> xf()
;;

(* Generate consecuive element of list started from k *)
let rec lfrom k = LCons (k, function () -> lfrom (k+1));;

(* Return first n elements (as list) from lazy list *)
let rec ltake = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,xf)) -> x::ltake(n-1, xf())
;;

(* Return tuplet with first n elements (as list) from lazy list and tail of that list *)
let rec ltakeWithTail = function
    (0, lxs) -> ([], lxs)
  | (_, LNil) -> ([], LNil)
  | (n, LCons(x,xf)) ->
      let (l,tail) = 
        ltakeWithTail(n-1, xf()) 
      in (x::l, tail)
;;


(* Convert normal implementation of list to lazy list *) 
let rec toLazyList = function
    [] -> LNil
  | h::t -> LCons(h, function () -> toLazyList t)
;;

(* Concatenate lazy lists *)
let rec (@$) lazyList1 lazyList2 =
  match lazyList1 with
      LNil -> lazyList2
    | LCons(x, xf) -> LCons(x, function () -> (xf()) @$ lazyList2)
;;

(* Map function for lazy list *)
let rec lmap f = function
    LNil -> LNil
  | LCons(x,xf) -> LCons(f x, function () -> lmap f (xf()))
;;

(* Filter function for lazy lists *)
let rec lfilter pred = function
    LNil -> LNil
  | LCons(x,xf) -> if pred x then LCons(x, function () -> lfilter pred (xf()) )
      else lfilter pred (xf())
;;


