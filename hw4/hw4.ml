open Loop
open Eval
open Uml

(*type exp =
    Var of var
  | Lam of var * exp
  | App of exp * exp*)
  
let rec string_of_exp e =
  match e with
  | Var v -> v
  | Lam (v, e) -> "λ" ^ v ^ "." ^ (string_of_exp e)
  | App (e1, e2) -> "(" ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ ")"

let print_test_list text res ans = 
  if (res=ans) then Printf.printf "%s: %s\n" text ("pass") else Printf.printf "%s: supposed to %s but %s\n" text (String.concat "; " ans) (String.concat "; " res)

let print_test_exp text ans res = 
    if (res=ans) then Printf.printf "%s: %s\n" text ("pass") else Printf.printf "%s: supposed to %s but %s\n" text (string_of_exp ans) (string_of_exp res)
  
let test_fv () =
  print_test_list "λx. x" (freeVariable (Lam ("x",Var "x"))) [];
  print_test_list "x y" (freeVariable (App (Var "x", Var"y")))  ["x";"y"];
  print_test_list "λx. x y" (freeVariable (Lam ("x",App (Var "x", Var "y")))) ["y"];
  print_test_list "λy. λx. x y" (freeVariable (Lam ("y",Lam ("x",App (Var "x", Var"y"))))) [];
  print_test_list "(λx. x y) (λx. x z)" (freeVariable (App (Lam ("x",App (Var "x", Var"y")), Lam ("x",App (Var "x", Var"z"))))) ["y";"z"]


let test_sub () = 
  (* case 1: 변수 x를 y로 교체 *)
  let expr1 = Var "x" in
  let expected1 = Var "y" in
  let result1 = substitute (Var "y") "x" expr1 in
  print_test_exp "substitute [y/x] x = y" expected1 result1;

  (* case 2: 변수 x를 y로 교체 *)
  let expr1 = Var "z" in
  let expected1 = Var "z" in
  let result1 = substitute (Var "y") "x" expr1 in
  print_test_exp "substitute [y/x] z = z" expected1 result1;

  (* case 3: 함수 호출 안에서 x를 y로 교체 *)
  let expr3 = App (Var "x", Var "z") in
  let expected3 = App (Var "y", Var "z") in
  let result3 = substitute (Var "y") "x" expr3 in
  print_test_exp "substitute [y/x] (x z) = (y z)" expected3 result3;


  (* case 4: 람다 표현식에서 변수 x를 y로 교체 *)
  let expr2 = Lam ("x", Var "x") in
  let expected2 = Lam ("x", Var "x") in
  let result2 = substitute (Var "y") "x" expr2 in
  print_test_exp "substitute [y/x] λx.x = λx.x" expected2 result2;

  (* case 5: free 아닌경우 *)
  let expr3 = Lam ("y", Var "x") in
  let expected3 = Lam ("y", Var "z") in
  let result3 = substitute (Var "z") "x" expr3 in
  print_test_exp "substitute [z/x] λy.x  = λy.[z/x]x = λy.z" expected3 result3;

  (* case 6: free인 경우 *)
  let expr4 = Lam ("y", App (Var "x", Var "y")) in
  let expected4 = Lam ("y__1", App (Var "y", Var "y__1")) in
  let result4 = substitute (Var "y") "x" expr4 in
  print_test_exp "substitute [y/x] λy. x y  = λz.[y/x][y<->z] x y = λz.[y/x]x z = λz.y z" expected4 result4

  
(* Activate this to test*)
(* let _ = 
  Printf.printf "============= Starting FreeVariable Test =============\n"; 
  test_fv()
  Printf.printf "============= Starting Substiture Test =============\n"; 
  test_sub() *)

(* Activate this to start Uml loop*)
let _ = loop (step Eval.stepv (wait show)) 