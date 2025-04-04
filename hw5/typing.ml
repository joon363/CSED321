open Tml

exception TypeError

(***************************************************** 
 * replace unit by your own type for typing contexts *
 *****************************************************)
type context = Tml.var -> Tml.tp

(*
 * For each function you introduce, 
 * write its type, specification, and invariant. 
 *)

(* val createEmptyContext : unit -> context *)
let createEmptyContext () = fun x -> raise TypeError

(* val typing : context -> Tml.exp -> Tml.tp *)
(* takes a typing context Γ of type Typing.context and an 
expression e of type Tml.exp, and returns a type A such that Γ ⊢ e:A; 
It raises an exception TypeError if e does not type check, 
i.e., there is no A such that Γ ⊢ e : A. *)
let typing cxt e = 
  match e with
  (*
  *)(*
  *)
  | Tml.Var x -> cxt x
  (*
  | Tml.Lam (x, t, e) -> 
  | Tml.App (e1, e2) ->
  | Tml.Pair (e1, e2) -> 
  | Tml.Fst (e) -> 
  | Tml.Snd (e) -> 
  | Tml.Eunit ->
  | Tml.Inl (e, t) -> 
  | Tml.Inr (e, t) -> 
  | Tml.Case (e, x1, e1, x2, e2) ->
  | Tml.Fix (x, t, e) -> *)
  | Tml.True | Tml.False -> Tml.Bool
  (*
  | Tml.Ifthenelse (e, etrue, efalse) -> 
  *)
  | Num (n) -> Int
    (*
  | Plus -> 
  | Minus -> 
  | Eq -> 
  *)

(* val typeOf : Tml.exp -> Tml.tp *)
let typeOf e = typing (createEmptyContext ()) e 

(* val typeOpt : Tml.exp -> Tml.tp option *)
let typeOpt e = try Some (typeOf e) 
                with TypeError -> None



