(*
 * Call-by-value reduction   
 *)

exception NotImplemented 
exception Stuck

let freshVarCounter = ref 0
open Uml
                          
(*   getFreshVariable : string -> string 
 *   use this function if you need to generate a fresh variable from s. 
 *)
let getFreshVariable s = 
  let _ = freshVarCounter := !freshVarCounter + 1
  in
  s ^ "__" ^ (string_of_int (!freshVarCounter))
               
(*
 * implement a single step with reduction using the call-by-value strategy.
 *)

(* implementation of [x<->y]e *)
let rec swap_variables x y e =
  match e with
  | Var v -> 
      if v = x then Var y
      else if v = y then Var x
      else Var v
  | Lam (v, e1) -> 
      if v = x then Lam (y, swap_variables x y e1)
      else if v = y then Lam (x, swap_variables x y e1)
      else Lam (v, swap_variables x y e1)
  | App (e1, e2) -> 
      App (swap_variables x y e1, swap_variables x y e2)

(* implementation of FV(e) *)
let rec freeVariable e = match e with
  | Var x -> [x]
  | Lam (x , e) -> 
    let lst1 = freeVariable(e) in
    let lst2 = [x] in
    List.filter (fun a -> not (List.mem a lst2)) lst1
  | App (e1, e2) -> 
    let lst1 = freeVariable(e1) in
    let lst2 = freeVariable(e2) in
    lst1 @ lst2

(* implementation of [e'/x]e *)
let rec substitute e' x e = match e with
  | Var a -> if (x=a) then e' else Var a
  | Lam (y , e1) -> 
      if(x = y) then Lam(y,e1)
      (* When y is not free variable of e' *)
      else if (not (List.mem y (freeVariable e'))) 
        then Lam (y , (substitute e' x e1))
      
      (* When y is free variable of e' *)
      else 
        let z = (getFreshVariable y) in
        Lam (z , (substitute e' x (swap_variables z y e1)))

  | App (e1, e2) ->
    let sube1 = substitute e' x e1 in
    let sube2 = substitute e' x e2 in
    App (sube1, sube2)

(* implementation of a reduction *)
let rec stepv e = match e with
| Var _ | Lam (_,_) -> raise Stuck 
| App (e1, e2) -> 
  try 
    (* e1|->e1' then e1 e2 |-> e1' e2*)
    let e1' = stepv e1 in
    App (e1', e2) 
  with Stuck -> 
    try
      (* e2 |-> e2' then lam x.e e2 |-> lam x.e e2' *)
      let e2' = stepv e2 in
      App (e1, e2')
    with Stuck -> match e1 with
    
      (* axiom (lam x.e) v |-> [v/x] e *)
      | Lam(x,e') -> substitute e2 x e'

let stepOpt stepf e = try Some (stepf e) with Stuck -> None

let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e = 
    match (stepOpt stepf e) with 
      None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in 
  Stream.icons e (steps e)

