open Loop
open Typing
open Tml

let print_test_tp text res ans = 
  if ((tp2string res)=ans) then Printf.printf "%s: %s\n" text ("pass") 
  else Printf.printf "%s: supposed to %s but %s\n" text (ans) (tp2string res)

let test_type () =
  let basicContext :Typing.context= (fun x -> match x with
    | "x" -> Tml.Unit
    | "n" -> Tml.Int 
    | "true" -> Tml.Bool) in
  print_test_tp "variable" (typing (basicContext) (Tml.Var "x")) "unit";
  print_test_tp "lambda" (typing (basicContext) (Tml.Lam("x", Tml.Int, Tml.Var "x"))) "int -> int";
  print_test_tp "app" (typing (basicContext) (Tml.App(Tml.Lam("x", Tml.Unit, Tml.True), Tml.Var "x"))) "bool";
  print_test_tp "pair" (typing (basicContext) (Tml.Pair(Tml.Var "x",Tml.Num 3))) "unit * int";
  print_test_tp "fst" (typing (basicContext) (Tml.Fst (Tml.Pair(Tml.Var "x",Tml.Num 3)))) "unit";
  print_test_tp "snd" (typing (basicContext) (Tml.Snd (Tml.Pair(Tml.Var "x",Tml.Num 3)))) "int";
  print_test_tp "Eunit" (typing (basicContext) (Tml.Eunit)) "unit";
  print_test_tp "Inl" (typing (basicContext) (Tml.Inl(Tml.Var "x", Tml.Int))) "unit + int";
  print_test_tp "Inr" (typing (basicContext) (Tml.Inr(Tml.Var "x", Tml.Int))) "int + unit";
  print_test_tp "Case" (typing (basicContext) (Tml.Case(Tml.Inl(Tml.Var "x", Tml.Int),"x",Tml.Num 3,"y",Tml.Num 5))) "int";
  print_test_tp "Fix" (typing (basicContext) (Tml.Fix("x", Tml.Int, Tml.Var "x"))) "int";
  print_test_tp "true" (typeOf Tml.True) "bool";
  print_test_tp "false" (typeOf Tml.False) "bool";
  print_test_tp "IfThenElse" (typing (basicContext) (Tml.Ifthenelse(Tml.True, Tml.True, Tml.False))) "bool";
  print_test_tp "Num" (typing (basicContext) (Tml.Num 3)) "int";
  print_test_tp "Plus" (typing (basicContext) (Tml.Plus)) "int * int -> int";
  print_test_tp "Minus" (typing (basicContext) (Tml.Minus)) "int * int -> int";
  print_test_tp "Eq" (typing (basicContext) (Tml.Eq)) "int * int -> bool"
  
(* Activate this to test
let _ = 
  Printf.printf "============= Starting Type Test =============\n"; 
  test_type()

*)
let _ = loop (step show);;

