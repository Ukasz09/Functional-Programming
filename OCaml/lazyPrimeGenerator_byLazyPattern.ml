(* ADT for lazy list using lezy patterns*)
type 'a lazyList = LNil | LCons of 'a * 'a lazyList Lazy.t;;

(* Filter function for lazy lists *)
let rec lfilter pred = function
    LNil -> LNil
  | LCons(x,lazy xf) -> if pred x then LCons(x, lazy(lfilter pred xf))
      else lfilter pred xf
;;

(* Generate consecuive element of list started from k *)
let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

(* Finding prime number by Erosthaenese method. Return infinitive primes list *)
let eratosthenesPrimeGenerator =
  let rec sieve = function
      LCons(hd,lazy tf) -> LCons(hd,lazy(sieve(lfilter (function n -> n mod hd <> 0) tf)))
    | LNil -> failwith "Internal error. Cannot be nil"
  in sieve (lfrom 2)
;;
