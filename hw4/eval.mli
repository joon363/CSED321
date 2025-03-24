exception NotImplemented 
exception Stuck

(* one-step reduction in the call-by-value reduction strategy,
   raises Stuck if impossible *)
val stepv : Uml.exp -> Uml.exp

val freeVariable : Uml.exp -> Uml.var list

val swap_variables: Uml.var -> Uml.var -> Uml.exp -> Uml.exp

val substitute : Uml.exp -> Uml.var -> Uml.exp -> Uml.exp

(* ... returns NONE if impossible *)
val stepOpt : (Uml.exp -> Uml.exp) -> Uml.exp -> Uml.exp option

(* repeats step as many times as possible *)
val multiStep : (Uml.exp -> Uml.exp) -> Uml.exp -> Uml.exp

(* a stream of all steps *)
val stepStream : (Uml.exp -> Uml.exp) -> Uml.exp -> Uml.exp Stream.t
