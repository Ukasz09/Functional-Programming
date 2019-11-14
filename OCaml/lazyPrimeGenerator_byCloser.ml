(* ADT for lazy list using function *)
type 'a lazyList = LNil | LCons of 'a * (unit -> 'a lazyList);;

(* Filter function for lazy lists *)
let rec lfilter pred = function
    LNil -> LNil
  | LCons(x,xf) -> if pred x then LCons(x, function () -> lfilter pred (xf()) )
      else lfilter pred (xf())
;;

(* Generate consecuive element of list started from k *)
let rec lfrom k = LCons (k, function () -> lfrom (k+1));;

(* Finding prime number by Erosthaenese method. Return infinitive primes list *)
let eratosthenesPrimeGenerator =
  let rec sieve = function
      LCons(hd,tf) -> LCons(hd,function () -> sieve(lfilter(function n -> n mod hd <> 0)(tf())))
    | LNil -> failwith "Internal error. Cannot be nil"
  in sieve (lfrom 2)
;;
