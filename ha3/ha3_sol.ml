(* shortcuts *)
let (%) g f x = g (f x)
let id x = x
let flip f x y = f y x
let neg f x = not (f x)
module Tuple2 = struct
  let fst (x,y) = x
  let snd (x,y) = y
  let map1 f (x,y) = f x, y
  let map2 f (x,y) = x, f y
end

module MyList = struct
  let cons x xs = x :: xs
  let rec length = function [] -> 0 | _::xs -> 1 + length xs
  let rec map f = function [] -> [] | x::xs -> f x :: map f xs
  let rec filter p = function [] -> [] | x::xs -> if p x then x :: filter p xs else filter p xs
  let rec fold_left f a = function [] -> a | x::xs -> fold_left f (f a x) xs
  let rec fold_right f = function [] -> (fun x -> x) | x::xs -> fun a -> f x (fold_right f xs a) (* not tail recursive! *)

  (* return Some element at index i or None if out of bounds. e.g. at 1 [1;2;3] = Some 2, at 1 [] = None *)
  let at i =
    let rec f j = function
      | [] -> None
      | x::xs -> if j=i then Some x else f (j+1) xs
    in f 0
  (* concatenate a list of lists. e.g. flatten [[1;2];[2;3]] = [1;2;2;3] *)
  let rec flatten = function
    | [] -> []
    | x::xs -> x @ flatten xs
  (* a list containing n elements x. e.g. make 3 1 = [1;1;1], make 2 'x' = ['x';'x'] *)
  let rec make n x = if n <= 0 then [] else x :: make (n-1) x
  (* range 1 3 = [1;2;3], range 1 (-1) = [1;0;-1] *)
  let rec range a b =
    let s x = if a<=b then x+1 else x-1 in
    if a=b then [a]
    else a :: range (s a) b
  (* [f 0; f 1; ...; f (n-1)]. e.g. init 3 (fun x -> Char.chr (x+65)) = ['A'; 'B'; 'C'], [] for n<1 *)
  let init n f = if n<1 then [] else map f (range 0 (n-1))
  (* reduce f [] = None, reduce f [1] = Some 1, reduce f [1;2;3] = Some (f (f 1 2) 3) *)
  let reduce f = function [] -> None | x::xs -> Some (fold_left f x xs)
  (* maximum element of a list. e.g. max_el [2;1;3] = Some 3, max_el [] = None *)
  let max_el xs = reduce max xs
  (* let rec max_el = function [] -> None | x::xs -> let m = max_el xs in if Some x>m then Some x else m *)
  (* min_max [] = None, min_max [2;4;1;2] = (1,4) *)
  let min_max xs = fold_right (fun x -> function Some (l,h) -> Some (min l x, max h x) | _ -> Some (x,x)) xs None
  (* mem 2 [1;2;3] = true; mem 2 [1;3] = false *)
  let rec mem e = function
    | [] -> false
    | x::xs -> x=e || mem e xs (* short-circuits :) *)
  (* find even [1;2;3;4] = Some 2, find even [1;3] = None *)
  let rec find p = function
    | [] -> None
    | x::xs -> if p x then Some x else find p xs
  (* filter_map (fun x -> if even x then Some (x*2) else None) [1;2;3;4] = [4;8], filter_map (const None) [1;2;3] = [] *)
  let rec filter_map f = function
    | [] -> []
    | x::xs -> (match f x with Some x -> cons x | None -> id) (filter_map f xs)
  (* partition even [1;2;3;4] = ([2;4],[1;3]) *)
  let rec partition p xs = fold_right (fun x (yay,nay) -> if p x then x::yay,nay else yay,x::nay) xs ([],[])
  (* index_of 2 [1;2;3] = Some 1, index_of 2 [] = None *)
  let index_of e =
    let rec f i = function
      | [] -> None
      | x::xs -> if x=e then Some i else f (i+1) xs
    in f 0
  (* split_at 1 [1;2;3] = ([1],[2;3]), split_at 2 [1] = ([1],[]) *)
  let split_at i =
    let rec f j = function
      | [] -> [],[]
      | x::xs -> Tuple2.(if j<i then map1 else map2) (cons x) (f (j+1) xs)
    in f 0
  (* remove_all even [1;2;3;4] = [1;3] *)
  let remove_all p = filter (neg p)
  (* remove_first even [1;2;3;4] = [1;3;4] *)
  let rec remove_first p = function
    | [] -> []
    | x::xs -> if p x then xs else x :: remove_first p xs
  (* take 2 [1;2;3;4] = [1;2], take 2 [] = [] *)
  let take n =
    let rec f i = function
      | x::xs when i<n -> x :: f (i+1) xs
      | _ -> []
    in f 0
  (* drop 2 [1;2;3;4] = [3;4], drop 2 [] = [] *)
  let drop n =
    let rec f i = function
      | x::xs when i<n -> f (i+1) xs
      | xs -> xs
    in f 0
  (* interleave 0 [1;2;3] = [1;0;2;0;3], interleave 0 [] = [], interleave 0 [1] = [1] *)
  let interleave e xs = match fold_right (fun x a -> e::x::a) xs [] with [] -> [] | x::xs -> xs
  (* split [1,2;2,3;3,4] = ([1;2;3],[2;3;4]), split [] = ([],[]) *)
  let rec split = function
    | [] -> [],[]
    | (x,y)::xs -> let a,b = split xs in x::a,y::b
  (* combine [1;2] [3;4] = Some [1,3;2,4], combine [1] [2;3] = None *)
  let rec combine a b = match a,b with
    | [],[] -> Some []
    | x::xs,y::ys -> (match combine xs ys with Some xs -> Some ((x,y) :: xs) | None -> None)
    | _ -> None
end

module MySet = struct
  type 'a t = 'a list (* this should be abstract in the interface so that one has to use {from,to}_list *)

  (* from_list [1;2;1] = [1;2] *)
  let rec from_list : 'a list -> 'a t = function
    | [] -> []
    | x::xs -> x :: from_list (MyList.filter ((<>) x) xs)
  let to_list x = x
  (* union [1;2] [2;3] = [1;2;3] *)
  let union a b = from_list (a@b)
  (* inter [1;2] [2;3] = [2] *)
  let inter a b = MyList.(union (filter (flip mem a) b) (filter (flip mem b) a))
  (* diff [1;2;3] [1;3] = [2] *)
  let diff a b = MyList.(filter (neg (flip mem b)) a)
end
