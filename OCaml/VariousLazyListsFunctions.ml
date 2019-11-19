(********************************************************************)
(*                           Auxiliary tools                        *)
(********************************************************************)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t

(* Return tail of lazy list *)
let ltl = function
    LNil -> failwith "ltl"
  | LCons (_, lazy xf) -> xf
;;

(* Return head of lazy list *)
let lhd = function
    LNil -> failwith "lhd"
  | LCons (x, _) -> x
;;


(********************************************************************)
(*                              Functions                           *)
(********************************************************************)

(* Repeat every element in lazy list for certain amount of times *)
let lrepeat k lazyList =
  let rec helper (n,restOfList) = match (n,restOfList) with
      (_,LNil) -> LNil
    | (0,LCons(_,lazy tl)) -> helper (k,tl)
    | (_,LCons(h,lazy tl)) -> LCons(h,lazy(helper(n-1,restOfList)))
  in if(k>0) then helper(k,lazyList) else LNil
;;

(* Repeat every element type int in lazy list for amount equal value of actual element *)
let lrepeat lazyList =
  let rec helper (n,restOfList) =
    if(n<0) then
      LNil
    else match (n,restOfList) with
        (_,LNil) -> LNil
      | (0,LCons(h,lazy tl)) -> helper ((lhd tl),tl)
      | (_,LCons(h,lazy tl)) -> LCons(h,lazy(helper(n-1,restOfList)))
  in if(lazyList<>LNil) then helper(lhd lazyList,lazyList) else LNil
;;

let ldivide list = 
  let rec listByIndex list indexPredicate index=
    match list with
        LNil-> LNil
      | LCons(value,lazy tail)->
          if(indexPredicate index) then
            LCons(value,lazy (listByIndex (ltl list) indexPredicate (index+1)))
          else listByIndex (ltl list) indexPredicate (index+1)
  in ((listByIndex list (fun n -> n mod 2 = 0) 0),(listByIndex list (fun n -> n mod 2 <> 0) 0))
;;
