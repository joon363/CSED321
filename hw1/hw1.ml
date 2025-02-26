exception Not_implemented
exception EmptyList

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let sum n =
  let rec aux acc i =
    if i > n then acc
    else aux (acc + i) (i + 1)
  in aux 0 1

let rec power x n =
  if n = 0 then 1
  else x * power x (n - 1)

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let rec combi n k =
  if k = 0 || k = n then 1
  else combi (n - 1) (k - 1) + combi (n - 1) k

  let rec sum_tree t = match t with
  | Leaf x -> x
  | Node (l, x, r) -> sum_tree l + x + sum_tree r

let rec depth t = match t with
  | Leaf _ -> 0
  | Node (l, _, r) -> 1 + max (depth l) (depth r)

let rec bin_search t x = match t with
  | Leaf v -> v = x
  | Node (l, v, r) ->
      if x = v then true
      else if x < v then bin_search l x
      else bin_search r x

let rec postorder t =
  let rec aux t acc = match t with
    | Leaf x -> x :: acc
    | Node (l, x, r) -> aux l (aux r (x :: acc))
  in aux t []

  let rec max lst = match lst with
  | [] -> raise EmptyList
  | [x] -> x
  | h :: t -> let m = max t in if h > m then h else m

let rec list_add lst1 lst2 = match (lst1, lst2) with
  | [], [] -> []
  | h1::t1, h2::t2 -> (h1 + h2) :: list_add t1 t2
  | [], h::t -> h::t
  | h::t, [] -> h::t

let rec insert x lst = match lst with
  | [] -> [x]
  | h :: t -> if x <= h then x :: lst else h :: insert x t

let rec insort lst = match lst with
  | [] -> []
  | h :: t -> insert h (insort t)

let compose f g x = g (f x)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let rec multifun f n =
  if n = 0 then fun x -> x
  else fun x -> f (multifun f (n - 1) x)

let rec ltake lst n = match (n, lst) with
  | 0, _ | _, [] -> []
  | _, h :: t -> h :: ltake t (n - 1)

let rec lall f lst = match lst with
  | [] -> true
  | h :: t -> f h && lall f t

let rec lmap f lst = match lst with
  | [] -> []
  | h :: t -> f h :: lmap f t

let rec lrev lst =
  let rec aux acc l = match l with
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in aux [] lst

let rec lflat lst = match lst with
  | [] -> []
  | h :: t -> h @ lflat t

let rec lzip lst1 lst2 = match (lst1, lst2) with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: lzip t1 t2
  | _, _ -> failwith "lists must have same length"

let rec split lst = match lst with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | h1 :: h2 :: t -> let (l1, l2) = split t in (h1 :: l1, h2 :: l2)

let rec cartprod lst1 lst2 =
  List.concat (List.map (fun x -> List.map (fun y -> (x, y)) lst2) lst1)

let rec powerset lst = match lst with
  | [] -> [[]]
  | h :: t ->
      let ps = powerset t in
      ps @ List.map (fun subset -> h :: subset) ps
