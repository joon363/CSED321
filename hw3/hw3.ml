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
      