open Hw1

(* sum 테스트 *)
let test_sum () =
  try
    let result = sum 10 in
    if result = 55 then
      Printf.printf "sum: pass\n"
    else
      Printf.printf "sum: fail (expected 55, got %d)\n" result
  with Not_implemented -> Printf.printf "sum: Not implemented\n";;

(* power 테스트 *)
let test_power () =
  try
    let result = power 2 3 in
    if result = 8 then
      Printf.printf "power: pass\n"
    else
      Printf.printf "power: fail (expected 8, got %d)\n" result
  with Not_implemented -> Printf.printf "power: Not implemented\n";;

(* gcd 테스트 *)
let test_gcd () =
  try
    let result = gcd 15 20 in
    if result = 5 then
      Printf.printf "gcd: pass\n"
    else
      Printf.printf "gcd: fail (expected 5, got %d)\n" result
  with Not_implemented -> Printf.printf "gcd: Not implemented\n";;

(* combi 테스트 *)
let test_combi () =
  try
    let result = combi 5 2 in
    if result = 10 then
      Printf.printf "combi: pass\n"
    else
      Printf.printf "combi: fail (expected 10, got %d)\n" result
  with Not_implemented -> Printf.printf "combi: Not implemented\n";;

(* sum_tree 테스트 *)
let test_sum_tree () =
  try
    let result = sum_tree (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)) in
    if result = 17 then
      Printf.printf "sum_tree: pass\n"
    else
      Printf.printf "sum_tree: fail (expected 17, got %d)\n" result
  with Not_implemented -> Printf.printf "sum_tree: Not implemented\n";;

(* depth 테스트 *)
let test_depth () =
  try
    let result = depth (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)) in
    if result = 2 then
      Printf.printf "depth: pass\n"
    else
      Printf.printf "depth: fail (expected 2, got %d)\n" result
  with Not_implemented -> Printf.printf "depth: Not implemented\n";;

(* bin_search 테스트 *)
let test_bin_search () =
  try
    let result_true = bin_search (Node (Node (Leaf 1, 2, Leaf 3), 4, Leaf 7)) 2 in
    let result_false = bin_search (Node (Node (Leaf 1, 2, Leaf 3), 4, Leaf 7)) 5 in
    if result_true && not result_false then
      Printf.printf "bin_search: pass\n"
    else
      Printf.printf "bin_search: fail\n"
  with Not_implemented -> Printf.printf "bin_search: Not implemented\n";;

(* postorder 테스트 *)
let test_postorder () =
  try
    let result = postorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)) in
    if result = [1; 2; 3; 4; 7] then
      Printf.printf "postorder: pass\n"
    else
      Printf.printf "postorder: fail (expected [1; 2; 3; 4; 7], got %s)\n" (String.concat "; " (List.map string_of_int result))
  with Not_implemented -> Printf.printf "postorder: Not implemented\n";;

(* max 테스트 *)
let test_max () =
  try
    let result = max [5; 3; 6; 7; 4] in
    if result = 7 then
      Printf.printf "max: pass\n"
    else
      Printf.printf "max: fail (expected 7, got %d)\n" result
  with Not_implemented -> Printf.printf "max: Not implemented\n";;

(* list_add 테스트 *)
let test_list_add () =
  try
    let result = list_add [1; 2] [3; 4; 5] in
    if result = [4; 6; 5] then
      Printf.printf "list_add: pass\n"
    else
      Printf.printf "list_add: fail (expected [4; 6; 5], got %s)\n" (String.concat "; " (List.map string_of_int result))
  with Not_implemented -> Printf.printf "list_add: Not implemented\n";;

(* insert 테스트 *)
let test_insert () =
  try
    let result = insert 3 [1; 2; 4; 5] in
    if result = [1; 2; 3; 4; 5] then
      Printf.printf "insert: pass\n"
    else
      Printf.printf "insert: fail (expected [1; 2; 3; 4; 5], got %s)\n" (String.concat "; " (List.map string_of_int result))
  with Not_implemented -> Printf.printf "insert: Not implemented\n";;

(* insort 테스트 *)
let test_insort () =
  try
    let result = insort [3; 7; 5; 1; 2] in
    if result = [1; 2; 3; 5; 7] then
      Printf.printf "insort: pass\n"
    else
      Printf.printf "insort: fail (expected [1; 2; 3; 5; 7], got %s)\n" (String.concat "; " (List.map string_of_int result))
  with Not_implemented -> Printf.printf "insort: Not implemented\n";;

(* compose 테스트 *)
let test_compose () =
  try
    let f x = x + 1 in
    let g x = x * 2 in
    let result = compose f g 3 in
    if result =8 then
      Printf.printf "compose: pass\n"
    else
      Printf.printf "compose: fail (expected 8, got %d)\n" result
  with Not_implemented -> Printf.printf "compose: Not implemented\n";;

(* curry 테스트 *)
let test_curry () =
  try
    let f (x, y) = x + y in
    let curried = curry f in
    let result = curried 3 4 in
    if result = 7 then
      Printf.printf "curry: pass\n"
    else
      Printf.printf "curry: fail (expected 7, got %d)\n" result
  with Not_implemented -> Printf.printf "curry: Not implemented\n";;

(* uncurry 테스트 *)
let test_uncurry () =
  try
    let f x y = x + y in
    let uncurried = uncurry f in
    let result = uncurried (3, 4) in
    if result = 7 then
      Printf.printf "uncurry: pass\n"
    else
      Printf.printf "uncurry: fail (expected 7, got %d)\n" result
  with Not_implemented -> Printf.printf "uncurry: Not implemented\n";;

(* multifun 테스트 *)
let test_multifun () =
  try
    let f x = x * x  in
    let result = multifun f 3 2 in
    if result = 256 then
      Printf.printf "multifun: pass\n"
    else
      Printf.printf "multifun: fail (expected 8, got %d)\n" (result)
  with Not_implemented -> Printf.printf "multifun: Not implemented\n";;

(* ltake 테스트 *)
let test_ltake () =
  try
    let result = ltake [3; 7; 5; 1; 2] 3 in
    if result = [3; 7; 5] then
      Printf.printf "ltake: pass\n"
    else
      Printf.printf "ltake: fail (expected [3; 7; 5], got %s)\n" (String.concat "; " (List.map string_of_int result))
  with Not_implemented -> Printf.printf "ltake: Not implemented\n";;

(* lall 테스트 *)
let test_lall () =
  try
    let result_true = lall (fun x -> x > 0) [1; 2; 3] in
    let result_false = lall (fun x -> x > 0) [-1; -2; 3] in
    if result_true && not result_false then
      Printf.printf "lall: pass\n"
    else
      Printf.printf "lall: fail\n"
  with Not_implemented -> Printf.printf "lall: Not implemented\n";;

(* lmap 테스트 *)
let test_lmap () =
  try
    let result = lmap (fun x -> x + 1) [1; 2; 3] in
    if result = [2; 3; 4] then
      Printf.printf "lmap: pass\n"
    else
      Printf.printf "lmap: fail (expected [2; 3; 4], got %s)\n" (String.concat "; " (List.map string_of_int result))
  with Not_implemented -> Printf.printf "lmap: Not implemented\n";;

(* lrev 테스트 *)
let test_lrev () =
  try
    let result = lrev [1; 2; 3; 4] in
    if result = [4; 3; 2; 1] then
      Printf.printf "lrev: pass\n"
    else
      Printf.printf "lrev: fail (expected [4; 3; 2; 1], got %s)\n" (String.concat "; " (List.map string_of_int result))
  with Not_implemented -> Printf.printf "lrev: Not implemented\n";;

(* lflat 테스트 *)
let test_lflat () =
  try
    let result = lflat [[1; 2]; [3; 4; 5]; [6]] in
    if result = [1; 2; 3; 4; 5; 6] then
      Printf.printf "lflat: pass\n"
    else
      Printf.printf "lflat: fail (expected [1; 2; 3; 4; 5; 6], got %s)\n" (String.concat "; " (List.map string_of_int result))
  with Not_implemented -> Printf.printf "lflat: Not implemented\n";;

(* lzip 테스트 *)
let test_lzip () =
  try
    let result = lzip ["Rooney"; "Park"; "Scholes"] [8; 13; 18] in
    if result = [("Rooney", 8); ("Park", 13); ("Scholes", 18)] then
      Printf.printf "lzip: pass\n"
    else
      Printf.printf "lzip: fail (expected [('Rooney', 8); ('Park', 13); ('Scholes', 18)], got %s)\n" (String.concat "; " (List.map (fun (a, b) -> "(" ^ a ^ ", " ^ string_of_int b ^ ")") result))
  with Not_implemented -> Printf.printf "lzip: Not implemented\n";;

(* split 테스트 *)
let test_split () =
  try
    let result = split [1; 3; 5; 7; 9; 11] in
    if result = ([1; 5; 9], [3; 7; 11]) then
      Printf.printf "split: pass\n"
    else
      let (list1, list2) = result in
        Printf.printf "split: fail (expected ([1; 5; 9], [3; 7; 11]), got ([%s], [%s]))\n"
        (String.concat "; " (List.map string_of_int list1))
        (String.concat "; " (List.map string_of_int list2))
  with Not_implemented -> Printf.printf "split: Not implemented\n";;

(* cartprod 테스트 *)
let test_cartprod () =
  try
    let result = cartprod [1; 2] [3; 4; 5] in
    if result = [(1, 3); (1, 4); (1, 5); (2, 3); (2, 4); (2, 5)] then
      Printf.printf "cartprod: pass\n"
    else
      Printf.printf "cartprod: fail (expected [(1, 3); (1, 4); (1, 5); (2, 3); (2, 4); (2, 5)], got %s)\n" (String.concat "; " (List.map (fun (a, b) -> "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")") result))
  with Not_implemented -> Printf.printf "cartprod: Not implemented\n";;

(* powerset 테스트 *)
let test_powerset () =
  try
    let result = List.sort compare (powerset [1; 2]) in
    let expected = List.sort compare [[]; [1]; [2]; [1; 2]] in
    if result = expected then
      Printf.printf "powerset: pass\n"
    else
      Printf.printf "powerset: fail (expected [[]; [1]; [2]; [1; 2]], got %s)\n" (String.concat "; " (List.map (fun l -> "[" ^ (String.concat "; " (List.map string_of_int l)) ^ "]") result))
  with Not_implemented -> Printf.printf "powerset: Not implemented\n";;

(* 테스트 실행 *)
let () =
  test_sum ();
  test_power ();
  test_gcd ();
  test_combi ();
  test_sum_tree ();
  test_depth ();
  test_bin_search ();
  test_postorder ();
  test_max ();
  test_list_add ();
  test_insert ();
  test_insort ();
  test_compose ();
  test_curry ();
  test_uncurry ();
  test_multifun ();
  test_ltake ();
  test_lall ();
  test_lmap ();
  test_lrev ();
  test_lflat ();
  test_lzip ();
  test_split ();
  test_cartprod ();
  test_powerset ();