open Hw3
open Common

let print_test text condition = 
  Printf.printf "%s: %s\n" text (if condition then "pass" else "fail")

let test_integer () =
  try
    let rec build_int n =
      if n = 0 then Integer.zero
      else if n = 1 then Integer.one
      else if n mod 2 = 0 then
        let half = build_int (n / 2) in
        Integer.(++) half half
      else
        Integer.(++) Integer.one (build_int (n - 1))
    in
    let ten = build_int 10 in
    let six = build_int 6 in
    let sixteen = build_int 16 in
    print_test "Addition (10 ++ 6)" (Integer.(++) ten six = sixteen)

  with
  | Integer.ScalarIllegal -> Printf.printf "Test failed with ScalarIllegal exception\n"
  | _ -> Printf.printf "Test failed with unexpected exception\n"

let _ = Printf.printf "=============Test Integer=============\n"; test_integer()

let test_boolean () =
  try
    (* Test for OR (++) *)
    let true_val = Boolean.one in
    let false_val = Boolean.zero in
    print_test "Addition (true ++ false)" (Boolean.(++) true_val false_val = true_val);
    print_test "Addition (false ++ false)" (Boolean.(++) false_val false_val = false_val);
    print_test "Addition (true ++ true)" (Boolean.(++) true_val true_val = true_val);

    (* Test for AND (**) *)
    print_test "Multiplication (true ** false)" (Boolean.( ** ) true_val false_val = false_val);
    print_test "Multiplication (false ** false)" (Boolean.( ** ) false_val false_val = false_val);
    print_test "Multiplication (true ** true)" (Boolean.( ** ) true_val true_val = true_val);

    (* Test for equality (==) *)
    print_test "Equality (true == true)" (Boolean.( == ) true_val true_val = true);
    print_test "Equality (false == true)" (Boolean.( == ) false_val true_val = false);
    print_test "Equality (false == false)" (Boolean.( == ) false_val false_val = true);
  with
  | Boolean.ScalarIllegal -> Printf.printf "Test failed with ScalarIllegal exception\n"
  | _ -> Printf.printf "Test failed with unexpected exception\n"


  let _ = Printf.printf "=============Test Boolean=============\n"; test_boolean()

  
  
  (* Create the VectorFn module for Boolean *)
  module BooleanVector = VectorFn(Boolean)
  
  let test_vector () =
    try
      (* Define some vectors using the created module *)
      let v1 = BooleanVector.create [Boolean.one; Boolean.zero; Boolean.one] in
      let v2 = BooleanVector.create [Boolean.zero; Boolean.one; Boolean.one] in
      let v3 = BooleanVector.create [Boolean.one; Boolean.one; Boolean.one] in
      let addv = BooleanVector.create [Boolean.one; Boolean.one; Boolean.one] in
  
      (* Test for vector addition (++) *)
      let sum_v1_v2 = BooleanVector.(++) v1 v2 in
      print_test "Addition (v1 ++ v2)" (BooleanVector.(==) sum_v1_v2 addv);
  
      (* Test for vector equality (==) *)
      print_test "Equality (v1 == v2)" (BooleanVector.(==) v1 v2 = false);
      print_test "Equality (v2 == v2)" (BooleanVector.(==) v2 v2 = true);
  
      (* Test for inner product (innerp) *)
      let inner_v1_v2 = BooleanVector.innerp v1 v2 in
      print_test "Inner product (v1 . v2)" (inner_v1_v2 = Boolean.one);
  
      let inner_v1_v3 = BooleanVector.innerp v1 v3 in
      print_test "Inner product (v1 . v3)" (inner_v1_v3 = Boolean.one);
  
    with
    | BooleanVector.VectorIllegal -> Printf.printf "Test failed with VectorIllegal exception\n"
    | _ -> Printf.printf "Test failed with unexpected exception\n"

    
let _ = Printf.printf "=============Test Vector=============\n"; test_vector()






module TestMatrix (Scal : SCALAR) = struct
  module Matrix = MatrixFn(Scal)

  let print_test text condition = 
    Printf.printf "%s: %s\n" text (if condition then "pass" else "fail")

  let test_matrix () =
    try
      (* 1. Matrix creation and identity matrix *)
      let m1 = Matrix.create [[Scal.one; Scal.zero]; [Scal.zero; Scal.one]] in
      let m2 = Matrix.create [[Scal.zero; Scal.one]; [Scal.one; Scal.zero]] in
      let identity_matrix = Matrix.identity 2 in

      (* Test identity matrix *)
      print_test "Identity matrix (2x2)" (Matrix.(==) identity_matrix (Matrix.identity 2));

      (* 2. Matrix addition (++) *)
      let sum = Matrix.(++) m1 m2 in
      let expected_sum = Matrix.create [[Scal.one; Scal.one]; [Scal.one; Scal.one]] in
      print_test "Matrix addition (m1 ++ m2)" (Matrix.(==) sum expected_sum);

      (* 3. Matrix multiplication (**) *)
      let m3 = Matrix.create [[Scal.one; Scal.zero]; [Scal.zero; Scal.one]] in
      let m4 = Matrix.create [[Scal.one; Scal.zero]; [Scal.zero; Scal.one]] in
      let mul_result = Matrix.( ** ) m3 m4 in
      let expected_mul = Matrix.create [[Scal.one; Scal.zero]; [Scal.zero; Scal.one]] in
      print_test "Matrix multiplication (m3 ** m4)" (Matrix.(==) mul_result expected_mul);

      (* 4. Matrix transpose *)
      let m5 = Matrix.create [[Scal.one; Scal.zero]; [Scal.zero; Scal.one]] in
      let transposed = Matrix.transpose m5 in
      print_test "Matrix transpose (m5)" (Matrix.(==) transposed m5);

      (* 5. Matrix equality (==) *)
      let equal = Matrix.(==) m1 m2 in
      print_test "Matrix equality (m1 == m2)" (equal = false);

    with
    | Matrix.MatrixIllegal -> Printf.printf "Test failed with MatrixIllegal exception\n"
    | _ -> Printf.printf "Test failed with unexpected exception\n"

  let test_matrix_exception1() = 
    try
      let invalid_m = Matrix.create [[Scal.one; Scal.zero]; [Scal.zero]] in
      print_test "Invalid matrix creation" false
    with
    | Matrix.MatrixIllegal -> print_test "Invalid matrix creation" true
    | _ -> Printf.printf "Test failed with unexpected exception\n"
    
  let test_matrix_exception2() =
    try
      let invalid_m1 = Matrix.create [[Scal.one]; [Scal.zero]] in
      let invalid_m2 = Matrix.create [[Scal.zero; Scal.one]; [Scal.one; Scal.zero]] in
      let invalid_res = Matrix.(++) invalid_m1 invalid_m2 in
      print_test "Invalid matrix addition" false
    with
    | Matrix.MatrixIllegal -> print_test "Invalid matrix addition" true
    | _ -> Printf.printf "Test failed with unexpected exception\n"
    
  let test_matrix_exception3() =
    try
      let invalid_m1 = Matrix.create [[Scal.one]; [Scal.zero]] in
      let invalid_m2 = Matrix.create [[Scal.zero; Scal.one]; [Scal.one; Scal.zero]] in
      let invalid_res = Matrix.( ** ) invalid_m1 invalid_m2 in
      print_test "Invalid matrix multiplication" false
    with
    | Matrix.MatrixIllegal -> print_test "Invalid matrix multiplication" true
    | _ -> Printf.printf "Test failed with unexpected exception\n"
    

  let test_all() = 
    test_matrix(); 
    test_matrix_exception1();
    test_matrix_exception2();
    test_matrix_exception3();
end

module BooleanMatrixTest = TestMatrix(Boolean)
module IntegerMatrixTest = TestMatrix(Integer)

let _ = Printf.printf "=============Test Boolean Matrix=============\n"; BooleanMatrixTest.test_all()
let _ = Printf.printf "=============Test Integer Matrix=============\n"; IntegerMatrixTest.test_all()
