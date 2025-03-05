open Hw2
(* lrevrev 테스트 *)
let test_lrevrev () =
  try
    let result = lrevrev [[1; 2; 3]; [4; 5; 6]; [7]] in
    if result = [[7]; [6; 5; 4]; [3; 2; 1]] then
      Printf.printf "lrevrev: pass\n"
    else
      Printf.printf "lrevrev: fail (expected [[7]; [6; 5; 4]; [3; 2; 1]], got %s)\n" (String.concat "," (List.map string_of_int (List.flatten result)))
  with NotImplemented -> Printf.printf "lrevrev: Not implemented\n";;

(* lfoldl 테스트 *)
let test_lfoldl () =
  try
    let result = lfoldl (fun (x, y) -> x - y) 0 [1; 2; 3; 4] in
    if result = 4-(3-(2-(1-0))) then
      Printf.printf "lfoldl: pass\n"
    else
      Printf.printf "lfoldl: fail (expected 2, got %d)\n" result
  with NotImplemented -> Printf.printf "lfoldl: Not implemented\n";;

(* fact 테스트 *)
let test_fact () =
  try
    let result = fact 5 in
    if result = 120 then
      Printf.printf "fact: pass\n"
    else
      Printf.printf "fact: fail (expected 120, got %d)\n" result
  with NotImplemented -> Printf.printf "fact: Not implemented\n";;

(* fib 테스트 *)
let test_fib () =
  try
    let result = fib 6 in
    if result = 13 then
      Printf.printf "fib: pass\n"
    else
      Printf.printf "fib: fail (expected 13, got %d)\n" result
  with NotImplemented -> Printf.printf "fib: Not implemented\n";;

(* alterSum 테스트 *)
let test_alterSum () =
  try
    let result = alterSum [3; 2; 7; 3] in
    if result = 5 then
      Printf.printf "alterSum: pass\n"
    else
      Printf.printf "alterSum: fail (expected 5, got %d)\n" result
  with NotImplemented -> Printf.printf "alterSum: Not implemented\n";;

(* ltabulate 테스트 *)
let test_ltabulate () =
  try
    let result = ltabulate 4 (fun x -> x * x) in
    if result = [0; 1; 4; 9] then
      Printf.printf "ltabulate: pass\n"
    else
      Printf.printf "ltabulate: fail (expected [0; 1; 4; 9], got %s)\n" (String.concat "," (List.map string_of_int result))
  with NotImplemented -> Printf.printf "ltabulate: Not implemented\n";;

(* lfilter 테스트 *)
let test_lfilter () =
  try
    let result = lfilter (fun x -> x > 2) [0; 1; 2; 3; 4; 5] in
    if result = [3; 4; 5] then
      Printf.printf "lfilter: pass\n"
    else
      Printf.printf "lfilter: fail (expected [3; 4; 5], got %s)\n" (String.concat "," (List.map string_of_int result))
  with NotImplemented -> Printf.printf "lfilter: Not implemented\n";;

(* union 테스트 *)
let test_union () =
  try
    let result = union [1; 2; 3] [2; 4; 6] in
    if List.sort compare result = [1; 2; 3; 4; 6] then
      Printf.printf "union: pass\n"
    else
      Printf.printf "union: fail (expected [1; 2; 3; 4; 6], got %s)\n" (String.concat "," (List.map string_of_int result))
  with NotImplemented -> Printf.printf "union: Not implemented\n";;

(* inorder 테스트 *)
let test_inorder () =
  try
    let result = inorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)) in
    if result = [1; 3; 2; 7; 4] then
      Printf.printf "inorder: pass\n"
    else
      Printf.printf "inorder: fail (expected [1; 3; 2; 7; 4], got %s)\n" (String.concat "," (List.map string_of_int result))
  with NotImplemented -> Printf.printf "inorder: Not implemented\n";;

(* postorder 테스트 *)
let test_postorder () =
  try
    let result = postorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)) in
    if result = [1; 2; 3; 4; 7] then
      Printf.printf "postorder: pass\n"
    else
      Printf.printf "postorder: fail (expected [1; 2; 3; 4; 7], got %s)\n" (String.concat "," (List.map string_of_int result))
  with NotImplemented -> Printf.printf "postorder: Not implemented\n";;

(* preorder 테스트 *)
let test_preorder () =
  try
    let result = preorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)) in
    if result = [7; 3; 1; 2; 4] then
      Printf.printf "preorder: pass\n"
    else
      Printf.printf "preorder: fail (expected [7; 3; 1; 2; 4], got %s)\n" (String.concat "," (List.map string_of_int result))
  with NotImplemented -> Printf.printf "preorder: Not implemented\n";;

  (* quicksort 테스트 *)
let test_quicksort () =
  try
    let result = quicksort [3; 7; 5; 1; 2] in
    if result = [1; 2; 3; 5; 7] then
      Printf.printf "quicksort: pass\n"
    else
      Printf.printf "quicksort: fail (expected [1; 2; 3; 5; 7], got %s)\n" (String.concat "," (List.map string_of_int result))
  with NotImplemented -> Printf.printf "quicksort: Not implemented\n";;

(* mergesort 테스트 *)
let test_mergesort () =
  try
    let result = mergesort [3; 7; 5; 1; 2] in
    if result = [1; 2; 3; 5; 7] then
      Printf.printf "mergesort: pass\n"
    else
      Printf.printf "mergesort: fail (expected [1; 2; 3; 5; 7], got %s)\n" (String.concat "," (List.map string_of_int result))
  with NotImplemented -> Printf.printf "mergesort: Not implemented\n";;

(* Heap 테스트 *)
let test_heap () =
  try
    let open Heap in
    let h1 = empty () in
    let (h2, loc1) = allocate h1 10 in
    let (h3, loc2) = allocate h2 20 in
    let v1 = dereference h3 loc1 in
    let v2 = dereference h3 loc2 in
    if v1 = 10 && v2 = 20 then
      Printf.printf "Heap test: pass\n"
    else
      Printf.printf "Heap test: fail (expected 10 and 20, got %d and %d)\n" v1 v2
  with NotImplemented -> Printf.printf "Heap: Not implemented\n";;

(* DictList 테스트 *)
let test_dict_list () =
  try
    let open DictList in
    (* 빈 딕셔너리 생성 *)
    let d1 = empty () in
    
    (* key-value 삽입 *)
    let d2 = insert d1 ("a", 10) in
    let d3 = insert d2 ("b", 20) in
    
    (* key 조회 *)
    let v1 = lookup d3 "a" in
    let v2 = lookup d3 "b" in
    let v3 = lookup d3 "c" in

    (* 값 확인 *)
    match v1, v2, v3 with
    | Some 10, Some 20, None ->
        Printf.printf "DictList test: pass\n"
    | _ ->
        Printf.printf "DictList test: fail (expected Some 10, Some 20, None, got %s, %s, %s)\n"
          (match v1 with Some x -> string_of_int x | None -> "None")
          (match v2 with Some x -> string_of_int x | None -> "None")
          (match v3 with Some x -> string_of_int x | None -> "None")
  with NotImplemented -> Printf.printf "DictList test: Not implemented\n";;


  (* DictFun 테스트 *)
let test_dict_fun () =
  try
    let open DictFun in
    (* 빈 딕셔너리 생성 *)
    let d1 = empty () in
    
    (* key-value 삽입 *)
    let d2 = insert d1 ("a", 10) in
    let d3 = insert d2 ("b", 20) in
    
    (* key 조회 *)
    let v1 = lookup d3 "a" in
    let v2 = lookup d3 "b" in
    let v3 = lookup d3 "c" in

    (* 값 확인 *)
    match v1, v2, v3 with
    | Some 10, Some 20, None ->
        Printf.printf "DictFun test: pass\n"
    | _ ->
        Printf.printf "DictFun test: fail (expected Some 10, Some 20, None, got %s, %s, %s)\n"
          (match v1 with Some x -> string_of_int x | None -> "None")
          (match v2 with Some x -> string_of_int x | None -> "None")
          (match v3 with Some x -> string_of_int x | None -> "None")
  with NotImplemented -> Printf.printf "DictFun test: Not implemented\n";;

let () = 
  test_lrevrev();
  test_lfoldl();
  test_fact();
  test_fib();
  test_alterSum();
  test_ltabulate();
  test_lfilter();
  test_union();
  test_inorder();
  test_postorder();
  test_preorder();
  test_quicksort();
  test_mergesort();
  test_heap();
  test_dict_list();
  test_dict_fun();