type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t


(* Return first n elements (as list) from lazy list *)
let rec ltake = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,lazy xf)) -> x::ltake(n-1, xf)

;;

(* Return first n elements (as list) from lazy list *)
let rec apply = function
    (_, LNil) -> failwith ("Negative arg index")
  |(0, LCons(x,_)) -> x 
  | (n, LCons(x,lazy xf)) -> apply(n-1, xf)
;;

(* Return lazy evaluated list with consecutives Fibbonacci numbers *)
let lazyListFib= 
  let rec fibHelper first next=
    LCons(first, lazy (fibHelper next (first+next)))
  in fibHelper 0 1
;;

(* Return list with chosen amount of first Fibbonacci numbers by using lazy evaluation *)
let lazyFibNumberList amount=
  if(amount<0) then failwith ("Neagtive argument amount")
  else ltake (amount,lazyListFib)
;;

(* Return chosen index of Fibbonacci numbers by using lazy evaluation *)
let lazyFibNumberIndex index=
  if(index<0) then failwith ("Neagtive argument amount")
  else apply (index,lazyListFib)
;;
