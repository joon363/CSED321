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
let rec stepv e = raise NotImplemented

let rec swap_variables x y e =
  match e with
  | Var v -> 
      if v = x then Var y
      else if v = y then Var x
      else Var v
  | Lam (v, e) -> 
      if v = x then Lam (y, swap_variables x y e)
      else if v = y then Lam (x, swap_variables x y e)
      else Lam (v, swap_variables x y e)
  | App (e1, e2) -> 
      App (swap_variables x y e1, swap_variables x y e2)

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

let rec substitute eprime x e = match e with
  | Var a -> if (x=a) then eprime else Var a
  | Lam (y , e1) -> 
      if(x = y) then Lam(y,e1)
      else if (not (List.mem y (freeVariable eprime))) then Lam (y , (substitute eprime x e1))
        else let z = (getFreshVariable y) in
          Lam (z , (substitute eprime x (swap_variables z y e1)))
  | App (e1, e2) ->
    let sube1 = substitute eprime x e1 in
    let sube2 = substitute eprime x e2 in
    App (sube1, sube2)



let stepOpt stepf e = try Some (stepf e) with Stuck -> None

let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e = 
    match (stepOpt stepf e) with 
      None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in 
  Stream.icons e (steps e)

