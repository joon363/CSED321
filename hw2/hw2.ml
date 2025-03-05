exception NotImplemented
	    
type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree
						      
(** Recursive functions **)

let rec lrevrev lst = 
  let rec aux acc l = match l with
    | [] -> acc
    | h :: t -> aux ((aux2 [] h) :: acc) t

  and aux2 acc2 l = match l with
    | [] -> acc2
    | h :: t -> aux2 (h :: acc2) t
  in aux [] lst 

let rec lfoldl f e lst = match lst with
  | [] -> e
  | h::t -> lfoldl f (f(h,e)) t

			 
(** Tail recursive functions  **)

let fact n = 
  let rec aux acc i = 
    if(i = 1) then acc
    else aux (acc*i) (i-1)
  in aux 1 n

let fib n = 
  let rec aux i =
    if(i=0 || i=1) then 1
    else (aux(i-1))+(aux(i-2))
  in aux n

let alterSum lst = 
  let rec aux plus acc l = match l with
    | [] -> 0
    | [x] -> if(plus=true) then acc+x else acc-x
    | h::t -> if(plus=true) then (aux (false) (acc+h) (t))
              else (aux (true) (acc-h) (t))
  in aux true 0 lst
            

let ltabulate n f = 
  let rec aux i = 
    if(i=n-1) then [f(i)]
    else (f i)::(aux (i+1))
  in aux 0

let lfilter p l = 
  let rec aux lst = match lst with
    | []->[]
    | h::t -> if(p(h)==true) then h::(aux t) else (aux t)
  in aux l 

let rec union s t = 
  let rec contains lst x = 
    match lst with
    | [] -> false
    | head :: tail -> if head = x then true else contains tail x
  in
  match (s, t) with
  | ([], x) | (x, []) -> x
  | (h::t, x) -> 
      if contains x h then 
        union t x
      else 
        union t (h :: x)

let inorder t = 
  let rec aux t acc = match t with
    | Leaf x -> x :: acc
    | Node (l, x, r) -> aux l (x::(aux r acc))
  in aux t []

let postorder t= 
  let rec aux t acc = match t with
    | Leaf x -> x :: acc
    | Node (l, x, r) -> aux l (aux r (x::acc))
  in aux t []

let preorder t= 
  let rec aux t acc = match t with
    | Leaf x -> x :: acc
    | Node (l, x, r) -> x::(aux l (aux r acc))
  in aux t []
		       
(** Sorting in the ascending order **)

let rec quicksort lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | pivot :: rest ->
      let rec partition left right l = match l with
        | [] -> (left, right)
        | y :: ys ->
            if y < pivot then partition (y :: left) right ys
            else partition left (y :: right) ys
      in
      let (left, right) = partition [] [] rest in
      (quicksort left) @ [pivot] @ (quicksort right);;



let rec mergesort lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | _ -> 
    let rec split lst left right = 
      match lst with
      | [] -> (left, right)
      | [x] -> (x :: left, right)
      | x :: y :: tl -> split tl (x :: left) (y :: right) in
    let left, right = split lst [] [] in
    let rec merge l1 l2 =
      match (l1, l2) with
      | [], l | l, [] -> l
      | h1 :: t1, h2 :: t2 ->
          if h1 <= h2 then h1 :: merge t1 l2
          else h2 :: merge l1 t2 in
    merge (mergesort left) (mergesort right)
			
(** Structures **)

module type HEAP = 
  sig
    exception InvalidLocation
    type loc
    type 'a heap
    val empty : unit -> 'a heap
    val allocate : 'a heap -> 'a -> 'a heap * loc
    val dereference : 'a heap -> loc -> 'a 
    val update : 'a heap -> loc -> 'a -> 'a heap
  end
    
module type DICT =
  sig
    type key
    type 'a dict
    val empty : unit -> 'a dict
    val lookup : 'a dict -> key -> 'a option
    val delete : 'a dict -> key -> 'a dict
    val insert : 'a dict -> key * 'a -> 'a dict 
  end

module Heap : HEAP =
  struct
    exception InvalidLocation 
		
    type loc = int 
    type 'a heap = { next : int; store : (loc * 'a) list }
    (*list of (loc, value)*)

    let empty () = { next = 0; store = [] }
    let allocate h v = 
      let l = h.next in
      ({next = l+1; store = List.rev(((l,v)::List.rev(h.store)))}, l)
    let dereference h l = 
      try
        let (_, value) = List.nth h.store l in value
      with _  -> raise InvalidLocation

    let update h l v = 
      let rec update_store store idx =
        match store with
        | [] -> raise InvalidLocation
        | (loc', _) :: rest ->
          if (loc' = l) then
            let updated_store = (l, v) :: rest in
            (* Rebuild the store from the modified list *)
            { h with store = updated_store }
          else update_store rest (idx + 1)
      in
      update_store h.store 0
  end
    
module DictList : DICT with type key = string =
  struct
    type key = string
    type 'a dict = (key * 'a) list
			      
    let empty () = []
    let lookup d k = 
      List.find_map (fun (key,value) -> if k=key then Some value else None) d
    let delete d k = 
      List.filter (fun (key, _) -> k!=key) d
    let insert d (k,v) = 
      (k, v) :: (delete d k)


  end
    
module DictFun : DICT with type key = string =
  struct
    type key = string
    type 'a dict = key -> 'a option
			     
    let empty () = fun _ -> None
    let lookup d k = d k
    let delete d k = 
      fun key -> if((d key )!= None) then None else (d key)
    let insert d (k,v) = 
      fun key -> if key = k then Some v else d key
  end
