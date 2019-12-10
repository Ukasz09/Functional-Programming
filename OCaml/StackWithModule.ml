(* signature - prevent encapsulation *)

(* signature - for unmutable Stack *)
module type STACK_UNMUTABLE =
sig
  type 'a t
  exception Empty of string
  val create: unit -> 'a t
  val push: 'a * 'a t -> 'a t
  val top: 'a t -> 'a
  val pop: 'a t -> 'a t
  val isEmpty: 'a t -> bool
end;;


(* Unmutable stack ADT by module and own signature *)
module StackUnmut : STACK_UNMUTABLE = (* if without own signature then only module Stack *)
struct
  type 'a t = EmptyStack | Push of 'a * 'a t
  exception Empty of string
  let create() = EmptyStack
  let push(e,s) = Push(e,s)
  let top = function
      Push(e,_) -> e
    | EmptyStack -> raise (Empty "module Stack: top")
  let pop = function
      Push(_,s) -> s
    | EmptyStack -> EmptyStack
  let isEmpty s = s = EmptyStack
end;;


(************************************************************************************************************)
(* Signature - for mutable stack - different with unmutable is only with push and pop return value *) 
module type STACK_MUTABLE =
sig
  type 'a t
  exception Empty of string
  val create: unit -> 'a t
  val push: 'a * 'a t -> unit
  val top: 'a t -> 'a
  val pop: 'a t -> unit
  val isEmpty: 'a t -> bool
end;;


(* Mutable stack ADT by module  - on list implementation*)
module StackMutList =
struct
  type 'a t = { mutable l : 'a list }
  exception Empty of string
  let create() = { l = [] }
  let push(e,s) = s.l <- e :: s.l (* <- variable assignment or declaration *)
  let top s =
    match s.l with
        hd::_ -> hd
      | [] -> raise (Empty "module StackMutList: top")
  let pop s =
    match s.l with
        hd::tl -> s.l <- tl
      | [] -> ()
  let isEmpty s = s.l = []
end;;

(* Mutable stack ADT  - on array implementation*)
module StackMutAr =
struct
  type 'a t = { mutable actualSize : int; mutable a : 'a option array }
  exception Empty of string
  let defaultSize = 5
  let create() = { actualSize=0 ; a = Array.make defaultSize None }
  let increase s = s.a <- Array.append s.a (Array.make defaultSize None)
  let push(e,s) = 
    begin if s.actualSize = Array.length s.a then increase s;
      s.a.(s.actualSize) <- Some e ;
      s.actualSize <- succ s.actualSize (* succ s.actualSize mean (s.actualSize)=(s.actualSize)+1 *)
    end
  let top s = if s.actualSize=0 then raise (Empty "module StackMutAr: top")
    else match s.a.(s.actualSize-1) with
        Some e -> e
      | None -> failwith
                  "module StackMutAr: top (implementation error!!!)"
  let pop s = if s.actualSize=0 then () else s.actualSize <- pred s.actualSize 
  (* pred s.actualSize mean (s.actualSize)=(s.actualSize)-1 *) 
  let isEmpty s = s.actualSize=0
end;;

(* encapsulation for ADT *)
module SML : STACK_MUTABLE = StackMutList;;
module SMA : STACK_MUTABLE = StackMutAr;;

(************************************************************************************************************)

(* Various way of using modules - syntactic sugar *)
let s1 = let open StackUnmut in push(2,push(1,create()));;
let s2 = StackUnmut.(push(2,push(1,create())));; 
let s3 = StackUnmut.push(2,StackUnmut.push(1,StackUnmut.create()));;

(* and with mutable *)
let s4=StackMutList.create();; StackMutList.push(1,s4);; StackMutList.push(2,s4);;
let s5=let open StackMutList in create();; let open StackMutList in push(1,s5);;






