open Common

exception NotImplemented

exception IllegalFormat

module Integer : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 0
  let one = 1

  let (++) x y = x + y
  let ( ** ) x y = x * y
  let (==) x y = x = y 
end


  
(* Problem 1-1 *)
(* Scalars *)

module Boolean : SCALAR with type t = bool 
=
struct
  type t = bool

  exception ScalarIllegal

  let zero = false
  let one = true

  let (++) x y = (x || y)
  let ( ** ) x y = (x && y)
  let (==) x y = (x = y)
end

(* Problem 1-2 *)
(* Vectors *)

module VectorFn (Scal : SCALAR) : VECTOR with type elem = Scal.t
=
struct
  type elem = Scal.t
  type t = elem list

  exception VectorIllegal

  let create lst = match lst with
    | [] -> raise VectorIllegal
    | _ -> lst
  let to_list v = v
  let dim v = List.length(v)
  let rec nth v n = 
    try List.nth v n
    with _ -> raise VectorIllegal

  let (++) v1 v2 = 
    if dim v1 != dim v2 then raise VectorIllegal
    else List.map2 (fun x y -> Scal.(++) x y) v1 v2
  let (==) v1 v2 = 
    if dim v1 != dim v2 then raise VectorIllegal
    else List.equal (fun x y -> Scal.(==) x y) v1 v2 
  let innerp v1 v2= 
    if dim v1 != dim v2 then raise VectorIllegal
    else List.fold_left 
      (fun a b -> Scal.(++) a b) (Scal.zero) (List.map2 (fun x y -> Scal.( ** ) x y) v1 v2)
end

(* Problem 1-3 *)
(* Matrices *)

module MatrixFn (Scal : SCALAR) : MATRIX with type elem = Scal.t
=
struct
  
  type elem = Scal.t
  type t = elem list list

  exception MatrixIllegal

  let create lst = match lst with
    | [] -> raise MatrixIllegal
    | _ -> List.map (fun row -> if (List.length row = List.length lst) then row else raise MatrixIllegal) lst
  let identity n = 
    if n <=0 then raise MatrixIllegal 
    else List.init n (fun i -> List.init n (fun j -> if i = j then Scal.one else Scal.zero))
  let dim mat = List.length mat
  let transpose mat = 
    List.init (dim mat) 
      (fun newRowIndex -> (List.init (dim mat) (fun newColumnIndex -> List.nth(List.nth mat newColumnIndex) newRowIndex)))
      (*newRowIndex = column index of original mat,
        newColumnIndex = row index of original mat
        therefore List.nth(List.nth mat newColumnIndex) newRowIndex is actually mat[row][col]*)
  let to_list mat = mat
  let get m r c =
    try List.nth (List.nth m r) c
    with _ -> raise MatrixIllegal 

  let (++) m1 m2 =
  if dim m1 != dim m2 then raise MatrixIllegal
  else 
    List.map2 
      (fun row1 row2 -> (List.map2 (fun x y -> Scal.(++) x y) row1 row2)) 
      m1 m2

  let ( ** ) m1 m2 =
    if dim m1 != dim m2 then raise MatrixIllegal
    else 
    List.map (fun row1 ->
      List.map (fun row2 ->
        List.fold_left (fun acc (x, y) -> Scal.(acc ++ (x ** y))) Scal.zero (List.combine row1 row2)
      ) (transpose m2)
    ) m1

  let (==) m1 m2 =
    if dim m1 != dim m2 then raise MatrixIllegal
    else List.for_all2 (fun row1 row2 -> List.equal (fun x y -> Scal.(==) x y) row1 row2) m1 m2
end

(* Problem 2-1 *)
(* Closure *)

module ClosureFn (Mat : MATRIX) :
sig
  val closure : Mat.t -> Mat.t
end
=
struct
  let closure _ = raise NotImplemented
end

(* Problem 2-2 *)
(* Applications to Graph Problems *)

module BoolMat = MatrixFn (Boolean)
module BoolMatClosure = ClosureFn (BoolMat)

let reach _ = raise NotImplemented

let al = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  false; false];
   [false; true;  true;  false; true;  false];
   [false; true;  false; true;  true;  true];
   [false; false; true;  true;  true;  false];
   [false; false; false; true;  false; true]]

let solution_al' = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true]]

module Distance : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 999999              (* Dummy value : Rewrite it! *)
  let one = 999999               (* Dummy value : Rewrite it! *)

  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* .. Write some code here .. *)

let distance _ = raise NotImplemented

let dl =
  [[  0;  -1;  -1;  -1;  -1;  -1 ];
   [ -1; 0  ; 35 ; 200; -1 ; -1  ];
   [ -1; 50 ; 0  ; -1 ; 150; -1  ];
   [ -1; 75;  -1 ; 0  ; 100; 25  ];
   [ -1; -1 ; 50 ; 65 ; 0  ; -1  ];
   [ -1; -1 ; -1 ; -1 ; -1 ; 0   ]]

let solution_dl' =
  [[0;  -1;  -1;  -1;  -1;  -1  ];
   [-1; 0;   35;  200; 185; 225 ];
   [-1; 50;  0;   215; 150; 240 ];
   [-1; 75;  110; 0;   100; 25  ];
   [-1; 100; 50;  65;  0;   90  ];
   [-1; -1;  -1;  -1;  -1;  0   ]]

module Weight : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 999999              (* Dummy value : Rewrite it! *)
  let one = 999999               (* Dummy value : Rewrite it! *)
 
  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* .. Write some code here .. *)

let weight _ = raise NotImplemented

let ml =
  [[-1; 0  ; 0  ; 0  ; 0  ; 0   ];
   [0 ; -1 ; 10 ; 100; 0  ; 0   ];
   [0 ; 50 ; -1 ; 0  ; 150; 0   ];
   [0 ; 75 ; 0  ; -1 ; 125; 40 ];
   [0 ; 0  ; 25 ; -1 ; -1 ; 0   ];
   [0 ; 0  ; 0  ; 0  ; 0  ; -1  ]]

let solution_ml' =
  [[-1; 0;  0;   0;   0;   0  ];
   [0;  -1; 25;  100; 100; 40 ];
   [0;  75; -1;  150; 150; 40 ];
   [0;  75; 25;  -1;  125; 40 ];
   [0;  75; 25;  -1;  -1;  40 ];
   [0;  0;  0;   0;   0;   -1 ]]

let _ =
  try 
  if reach al = solution_al' && distance dl = solution_dl' && weight ml = solution_ml' then
    print_endline "\nYour program seems fine (but no guarantee)!"
  else
    print_endline "\nYour program might have bugs!"
  with _ -> print_endline "\nYour program is not complete yet!" 
      





let print_test text condition = 
  Printf.printf "%s: %s\n" text (if condition then "pass" else "fail")


let test_integer () =
  try
    print_test "Zero" (Integer.zero = 0);
    print_test "One" (Integer.one = 1);
    print_test "Addition (10 ++ 6)" ((Integer.(++) 10 16) = 26);
    print_test "Multiplication (10 ++ 6)" ((Integer.( ** ) 10 16) = 160);
    print_test "Equality (3 == 4)" ((Integer.(==) 3 4) = false);
    print_test "Equality (1 == 1)" ((Integer.(==) 1 1) = true);

  with
  | Integer.ScalarIllegal -> Printf.printf "Test failed with ScalarIllegal exception\n"
  | _ -> Printf.printf "Test failed with unexpected exception\n"

let _ = Printf.printf "=============Test Integer=============\n"; test_integer()

let test_boolean () =
  try
    (* Test for OR (++) *)
    print_test "Addition (true ++ false)" (Boolean.(++) true false = true);
    print_test "Addition (false ++ false)" (Boolean.(++) false false = false);
    print_test "Addition (true ++ true)" (Boolean.(++) true true = true);

    (* Test for AND (**) *)
    print_test "Multiplication (true ** false)" (Boolean.( ** ) true false = false);
    print_test "Multiplication (false ** false)" (Boolean.( ** ) false false = false);
    print_test "Multiplication (true ** true)" (Boolean.( ** ) true true = true);

    (* Test for equality (==) *)
    print_test "Equality (true == true)" (Boolean.( == ) true true = true);
    print_test "Equality (false == true)" (Boolean.( == ) false true = false);
    print_test "Equality (false == false)" (Boolean.( == ) false false = true);
  with
  | Boolean.ScalarIllegal -> Printf.printf "Test failed with ScalarIllegal exception\n"
  | _ -> Printf.printf "Test failed with unexpected exception\n"


  let _ = Printf.printf "=============Test Boolean=============\n"; test_boolean()

  
  
  (* Create the VectorFn module for Boolean *)
  module BooleanVector = VectorFn(Boolean)
  
  let test_vector () =
    try
      (* Define some vectors using the created module *)
      let v1 = BooleanVector.create [true; false; true] in
      let v2 = BooleanVector.create [false; true; true] in
      let v3 = BooleanVector.create [true; true; true] in
      let addv = BooleanVector.create [true; true; true] in
  
      (* Test for vector addition (++) *)
      let sum_v1_v2 = BooleanVector.(++) v1 v2 in
      print_test "Addition (v1 ++ v2)" (BooleanVector.(==) sum_v1_v2 addv);
  
      (* Test for vector equality (==) *)
      print_test "Equality (v1 == v2)" (BooleanVector.(==) v1 v2 = false);
      print_test "Equality (v2 == v2)" (BooleanVector.(==) v2 v2 = true);
  
      (* Test for inner product (innerp) *)
      let inner_v1_v2 = BooleanVector.innerp v1 v2 in
      print_test "Inner product (v1 . v2)" (inner_v1_v2 = true);
  
      let inner_v1_v3 = BooleanVector.innerp v1 v3 in
      print_test "Inner product (v1 . v3)" (inner_v1_v3 = true);
  
    with
    | BooleanVector.VectorIllegal -> Printf.printf "Test failed with VectorIllegal exception\n"
    | _ -> Printf.printf "Test failed with unexpected exception\n"

    
let _ = Printf.printf "=============Test Vector=============\n"; test_vector()




module Matrix = MatrixFn(Integer)

let test_matrix () =
  try
    (* 1. Matrix creation and identity matrix *)
    let m1 = Matrix.create [[1; 0]; [0; 1]] in
    let m2 = Matrix.create [[0; 1]; [1; 0]] in
    let identity_matrix = Matrix.identity 2 in

    (* Test identity matrix *)
    print_test "Identity matrix (2x2)" (Matrix.(==) identity_matrix (Matrix.identity 2));

    (* 2. Matrix addition (++) *)
    let sum = Matrix.(++) m1 m2 in
    let expected_sum = Matrix.create [[1; 1]; [1; 1]] in
    print_test "Matrix addition (m1 ++ m2)" (Matrix.(==) sum expected_sum);

    (* 3. Matrix multiplication (**) *)
    let m3 = Matrix.create [[1; 0]; [0; 1]] in
    let m4 = Matrix.create [[1; 0]; [0; 1]] in
    let mul_result = Matrix.( ** ) m3 m4 in
    let expected_mul = Matrix.create [[1; 0]; [0; 1]] in
    print_test "Matrix multiplication (m3 ** m4)" (Matrix.(==) mul_result expected_mul);

    (* 4. Matrix transpose *)
    let m5 = Matrix.create [[1; 0]; [0; 1]] in
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
    let invalid_m = Matrix.create [[1; 0]; [0]] in
    print_test "Invalid matrix creation" false
  with
  | Matrix.MatrixIllegal -> print_test "Invalid matrix creation" true
  | _ -> Printf.printf "Test failed with unexpected exception\n"
  
let test_matrix_exception2() =
  try
    let invalid_m1 = Matrix.create [[1]; [0]] in
    let invalid_m2 = Matrix.create [[0; 1]; [1; 0]] in
    let invalid_res = Matrix.(++) invalid_m1 invalid_m2 in
    print_test "Invalid matrix addition" false
  with
  | Matrix.MatrixIllegal -> print_test "Invalid matrix addition" true
  | _ -> Printf.printf "Test failed with unexpected exception\n"
  
let test_matrix_exception3() =
  try
    let invalid_m1 = Matrix.create [[1]; [0]] in
    let invalid_m2 = Matrix.create [[0; 1]; [1; 0]] in
    let invalid_res = Matrix.( ** ) invalid_m1 invalid_m2 in
    print_test "Invalid matrix multiplication" false
  with
  | Matrix.MatrixIllegal -> print_test "Invalid matrix multiplication" true
  | _ -> Printf.printf "Test failed with unexpected exception\n"
  

let matrix_test_all() = 
  test_matrix(); 
  test_matrix_exception1();
  test_matrix_exception2();
  test_matrix_exception3()

let _ = Printf.printf "=============Test Integer Matrix=============\n"; matrix_test_all()
