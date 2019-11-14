(* ADT for lazy list using lezy patterns*)
type 'a lazyList = LNil | LCons of 'a * 'a lazyList Lazy.t;;

(* Return head of lazy list *)
let lhd = function
    LNil -> failwith "lhd"
  | LCons (x, _) -> x
;;

(* Return tail of lazy list *)
let ltl = function
    LNil -> failwith "ltl"
  | LCons (_, lazy xf) -> xf
;;

(* Generate consecuive element of list started from k *)
let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

(* Return first n elements (as list) from lazy list *)
let rec ltake = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,lazy xf)) -> x::ltake(n-1, xf)
;;

(* Convert normal implementation of list to lazy list *) 
let rec toLazyList = function
    [] -> LNil
  | h::t -> LCons(h, lazy (toLazyList t))
;;

(* Concatenate lazy lists *)
let rec (@$) lazyList1 lazyList2 =
  match lazyList1 with
      LNil -> lazyList2
    | LCons(x, lazy xf) -> LCons(x, lazy(xf @$ lazyList2))
;;


(* Map function for lazy list *)
let rec lmap f = function
    LNil -> LNil
  | LCons(x, lazy xf) -> LCons(f x, lazy (lmap f xf))
;;


(* Filter function for lazy lists *)
let rec lfilter pred = function
    LNil -> LNil
  | LCons(x,lazy xf) -> if pred x then LCons(x, lazy(lfilter pred xf))
      else lfilter pred xf
;;