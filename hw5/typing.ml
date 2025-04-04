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
let rec typing cxt e = 
  (* Γ, x:A *)
  let extendedCxt x t= fun var -> if var=x then t else cxt x in
  match e with
  | Tml.Var x -> cxt x

  | Tml.Lam (x, t, e) -> 
      Tml.Fun (t, typing (extendedCxt x t) e)

  | Tml.App (e1, e2) -> (
      let e1Type = typing cxt e1 in
      let e2Type = typing cxt e2 in
      match e1Type with
      | Tml.Fun (t1, t2) ->   (*Γ ⊢ e : A->B *)
        if t1=e2Type then t2  (*Γ ⊢ e' : A, then Γ ⊢ e e' = B*)
        else raise TypeError 
      | _ -> raise TypeError)

  | Tml.Pair (e1, e2) -> 
      let e1Type = typing cxt e1 in
      let e2Type = typing cxt e2 in
      Prod (e1Type, e2Type)

  | Tml.Fst (e) -> (
      let eType = typing cxt e in
      match eType with
      | Tml.Prod (t1, t2) -> t1  (*Γ ⊢ e : A1 x A2 *)
      | _ -> raise TypeError)

  | Tml.Snd (e) -> (
      let eType = typing cxt e in
      match eType with
      | Tml.Prod (t1, t2) -> t2  (*Γ ⊢ e : A1 x A2 *)
      | _ -> raise TypeError)

  | Tml.Eunit -> Tml.Unit

  | Tml.Inl (e, t) ->
      let eType = typing cxt e in
      Tml.Sum(eType,t)

  | Tml.Inr (e, t) -> 
      let eType = typing cxt e in
      Tml.Sum(t,eType)
      
  | Tml.Case (e, x1, e1, x2, e2) -> (
      let eType = typing cxt e in
      match eType with
      | Tml.Sum (t1, t2) ->
          let e1Type = typing (extendedCxt x1 t1) e1 in
          let e2Type = typing (extendedCxt x2 t2) e2 in
          if (e1Type==e2Type) then e1Type
          else raise TypeError
      | _ -> raise TypeError)
  
  | Tml.Fix (x, t, e) -> 
      typing (extendedCxt x t) e

  | Tml.True | Tml.False -> Tml.Bool

  | Tml.Ifthenelse (e, etrue, efalse) -> (
      let eType = typing cxt e in
      match eType with
      | Tml.Bool ->
          let etType = typing cxt etrue in
          let efType = typing cxt efalse in
          if (etType==efType) then efType
          else raise TypeError
      | _ -> raise TypeError)
  | Num (n) -> Int
  | Plus -> 
      Fun(Prod(Int, Int), Int)
  | Minus -> 
      Fun(Prod(Int, Int), Int)
  | Eq -> 
      Fun(Prod(Int, Int), Bool)

(* val typeOf : Tml.exp -> Tml.tp *)
let typeOf e = typing (createEmptyContext ()) e 

(* val typeOpt : Tml.exp -> Tml.tp option *)
let typeOpt e = try Some (typeOf e) 
                with TypeError -> None



