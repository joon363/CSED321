open Loop
open Eval
open Uml
(*let _ = loop (step Eval.stepv (wait show)) *)

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
  print_test_list "lambda x. x" (freeVariable (Lam ("x",Var "x"))) [];
  print_test_list "x y" (freeVariable (App (Var "x", Var"y")))  ["x";"y"];
  print_test_list "lambda x. x y" (freeVariable (Lam ("x",App (Var "x", Var "y")))) ["y"];
  print_test_list "lambda y. lambda x. x y" (freeVariable (Lam ("y",Lam ("x",App (Var "x", Var"y"))))) [];
  print_test_list "(lambda x. x y) (lambda x. x z)" (freeVariable (App (Lam ("x",App (Var "x", Var"y")), Lam ("x",App (Var "x", Var"z"))))) ["y";"z"]

let _ = 
  Printf.printf "============= Starting FreeVariable Test =============\n"; 
  test_fv()

let test_sub () = 
  (* 테스트 1: 변수 x를 y로 교체 *)
  let expr1 = Var "x" in
  let expected1 = Var "y" in
  let result1 = substitute (Var "y") "x" expr1 in
  print_test_exp "substitute [y/x] x" expected1 result1;

  (* 테스트 1.1: 변수 x를 y로 교체 *)
  let expr1 = Var "z" in
  let expected1 = Var "z" in
  let result1 = substitute (Var "y") "x" expr1 in
  print_test_exp "substitute [y/x] z" expected1 result1;

  (* 테스트 2: 람다 표현식에서 변수 x를 y로 교체 *)
  let expr2 = Lam ("x", Var "x") in
  let expected2 = Lam ("x", Var "x") in
  let result2 = substitute (Var "y") "x" expr2 in
  print_test_exp "substitute [y/x] λx.x" expected2 result2;

  (* 테스트 3: 함수 호출 안에서 x를 y로 교체 *)
  let expr3 = App (Var "x", Var "z") in
  let expected3 = App (Var "y", Var "z") in
  let result3 = substitute (Var "y") "x" expr3 in
  print_test_exp "substitute [y/x] (x z)" expected3 result3;

  (* 테스트 4: 람다 추상화 내부에서 x를 y로 교체 (교체하지 않아야 함) *)
  let expr4 = Lam ("x", Var "x") in
  let expected4 = Lam ("x", Var "y") in
  let result4 = substitute (Var "y") "y" expr4 in
  print_test_exp "substitute [y/y] λx.x" expected4 result4;

  (* 테스트 5: 함수 호출과 람다 추상화 내부에서 x를 y로 교체 *)
  let expr5 = App (Lam ("x", Var "x"), Var "x") in
  let expected5 = App (Lam ("x", Var "y"), Var "y") in
  let result5 = substitute (Var "y") "x" expr5 in
  print_test_exp "substitute [y/x] (λx.x x)" expected5 result5;

  (* 테스트 6: 복잡한 표현식에서 x를 y로 교체 *)
  let expr6 = App (Lam ("x", App (Var "x", Var "y")), Var "z") in
  let expected6 = App (Lam ("x", App (Var "y", Var "y")), Var "z") in
  let result6 = substitute (Var "y") "x" expr6 in
  print_test_exp "substitute [y/x] (λx. (x y) z)" expected6 result6;

  (* 테스트 7: 복잡한 표현식에서 x를 y로 교체 (여러 번 교체) *)
  let expr7 = App (Lam ("x", App (Var "x", Lam ("x", Var "x"))), Var "x") in
  let expected7 = App (Lam ("x", App (Var "y", Lam ("x", Var "y"))), Var "y") in
  let result7 = substitute (Var "y") "x" expr7 in
  print_test_exp "substitute [y/x] (λx. (x (λx.x)) x)" expected7 result7;

  (* 테스트 8: 변수 x를 y로 교체할 때 다른 변수가 동일한 이름일 때 *)
  let expr8 = Lam ("x", App (Var "x", Var "x")) in
  let expected8 = Lam ("x", App (Var "y", Var "y")) in
  let result8 = substitute (Var "y") "x" expr8 in
  print_test_exp "substitute [y/x] λx.(x x)" expected8 result8

let _ = 
  Printf.printf "============= Starting Substiture Test =============\n"; 
  test_sub()