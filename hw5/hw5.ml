open Loop
open Typing
open Tml

let print_test_tp text res ans = 
  if ((tp2string res)=ans) then Printf.printf "%s: %s\n" text ("pass") 
  else Printf.printf "%s: supposed to %s but %s\n" text (ans) (tp2string res)

let test_type () =
  let basicContext :Typing.context= fun x -> Tml.Int in
  print_test_tp "variable" (typing (basicContext) (Tml.Num 3)) "int";
  print_test_tp "true" (typeOf Tml.True) "bool";
  print_test_tp "false" (typeOf Tml.False) "bool"
  
(* Activate this to test*)
let _ = 
  Printf.printf "============= Starting Type Test =============\n"; 
  test_type()

(*
let _ = loop (step show);;
*)
