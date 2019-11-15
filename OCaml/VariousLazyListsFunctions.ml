type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t


(* Repeat every element in lazy list for certain amount of times *)
let lrepeat k lazyList =
  let rec helper (n,restOfList) = match (n,restOfList) with
      (_,LNil) -> LNil
    | (0,LCons(_,lazy tl)) -> helper (k,tl)
    | (_,LCons(h,lazy tl)) -> LCons(h,lazy(helper(n-1,restOfList)))
  in helper(k,lazyList)
;;
