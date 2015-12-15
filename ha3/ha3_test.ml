let (%) g f x = g (f x)
let neg f x = not (f x)
let uncurry f (x,y) = f x y
open Kaputt.Abbreviations

module A = Ha3
module B = Ha3_sol
module type S = sig
  module MyList :
    sig
      val cons : 'a -> 'a list -> 'a list
      val length : 'a list -> int
      val map : ('a -> 'b) -> 'a list -> 'b list
      val filter : ('a -> bool) -> 'a list -> 'a list
      val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
      val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
      val at : int -> 'a list -> 'a option
      val flatten : 'a list list -> 'a list
      val make : int -> 'a -> 'a list
      val range : int -> int -> int list
      val init : int -> (int -> 'a) -> 'a list
      val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a option
      val max_el : 'a list -> 'a option
      val min_max : 'a list -> ('a * 'a) option
      val mem : 'a -> 'a list -> bool
      val find : ('a -> bool) -> 'a list -> 'a option
      val filter_map : ('a -> 'b option) -> 'a list -> 'b list
      val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
      val index_of : 'a -> 'a list -> int option
      val split_at : int -> 'a list -> 'a list * 'a list
      val remove_all : ('a -> bool) -> 'a list -> 'a list
      val remove_first : ('a -> bool) -> 'a list -> 'a list
      val take : int -> 'a list -> 'a list
      val drop : int -> 'a list -> 'a list
      val interleave : 'a -> 'a list -> 'a list
      val split : ('a * 'b) list -> 'a list * 'b list
      val combine : 'a list -> 'b list -> ('a * 'b) list option
    end
  module MySet :
    sig
      type 'a t
      val from_list : 'a list -> 'a t
      val to_list : 'a t -> 'a list
      val union : 'a t -> 'a t -> 'a t
      val inter : 'a t -> 'a t -> 'a t
      val diff : 'a t -> 'a t -> 'a t
    end
end

let tests = ref []
let add test = tests := !tests @ [test]

let add_enum title gen ft fs = add @@ Test.make_enum_test ~title:title
    gen ft [Spec.always => (fun (a,b) -> fs a = b)]
(* let add_random title gen ft fs = add @@ Test.make_random_test ~title:title *)
(*     ~nb_runs:10 gen ft [Spec.always => (fun (a,b) -> fs a = b)] *)
let rand title gen f = add @@ Test.make_random_test ~title:title
    ~nb_runs:10 gen (f (module A:S)) [Spec.always => (fun (i,r) -> f (module B:S) i = r)]
let () =
  let glist ?(min=0) ?(max=10) g = Gen.list (Gen.make_int min max) g in
  let gint = Gen.make_int (-10) 10 in
  let gpint = Gen.make_int 0 10 in
  let gints = glist gint in
  let gpints = glist gpint in
  (* let elist e = Enum.sequence [Enum.lift [] "[]"; Enum.for_each 1 10 (Enum.list Enum.unit)] in *)
  let even x = x mod 2 = 0 in
  let odd = neg even in

  rand "MyList.init n (fun x -> Char.chr (x+65))" gpint (fun (module X) n -> X.MyList.init n (fun x -> Char.chr (x+65)));
  rand "MyList.reduce" gints (fun (module X) -> X.MyList.reduce (-));
  rand "MyList.max_el" gints (fun (module X) -> X.MyList.max_el);
  rand "MyList.min_max" gints (fun (module X) -> X.MyList.min_max);
  rand "MyList.mem" (Gen.zip2 gint gints) (fun (module X) -> uncurry X.MyList.mem);
  rand "MyList.find even" gints (fun (module X) -> X.MyList.find even);
  rand "MyList.filter_map (fun x -> if even x then None else Some (x*3))" gints (fun (module X) -> X.MyList.filter_map (fun x -> if even x then None else Some (x*3)));
  rand "MyList.partition odd " gints (fun (module X) -> X.MyList.partition odd);
  rand "MyList.index_of" (Gen.zip2 gint gints) (fun (module X) -> uncurry X.MyList.index_of);
  rand "MyList.split_at" (Gen.zip2 gint gints) (fun (module X) -> uncurry X.MyList.split_at);
  rand "MyList.remove_all odd" gints (fun (module X) -> X.MyList.remove_all odd);
  rand "MyList.remove_first odd" gints (fun (module X) -> X.MyList.remove_first odd);
  rand "MyList.take" (Gen.zip2 gint gints) (fun (module X) -> uncurry X.MyList.take);
  rand "MyList.drop" (Gen.zip2 gint gints) (fun (module X) -> uncurry X.MyList.drop);
  rand "MyList.interleave" (Gen.zip2 gint gints) (fun (module X) -> uncurry X.MyList.interleave);
  rand "MyList.split" (glist (Gen.zip2 gpint gpint)) (fun (module X) -> X.MyList.split);
  rand "MyList.combine" (Gen.zip2 (glist ~min:7 gpint) (glist ~min:7 gpint)) (fun (module X) -> uncurry X.MyList.combine);
  let sort = List.sort compare in
  rand "MySet.from_list" gpints (fun (module X) -> sort % X.MySet.to_list % X.MySet.from_list);
  rand "MySet.union" (Gen.zip2 gpints gpints) (fun (module X) (x,y) -> sort @@ X.MySet.(to_list @@ union (from_list x) (from_list y)));
  rand "MySet.inter" (Gen.zip2 gpints gpints) (fun (module X) (x,y) -> sort @@ X.MySet.(to_list @@ inter (from_list x) (from_list y)));
  rand "MySet.diff" (Gen.zip2 gpints gpints) (fun (module X) (x,y) -> sort @@ X.MySet.(to_list @@ diff (from_list x) (from_list y)))


(* launch *)
let () =
  let point = Test.(function
    | Passed -> 1
    | Report (p,n,e,c,m) when p=n -> 1
    | _ -> 0)
  in
  let points = List.map point (Test.exec_tests !tests) in
  Test.run_tests ~output:(Test.Html_output (open_out "result.html")) !tests;
  Test.run_tests !tests;
  prerr_endline @@ "### GRADES: " ^ String.concat " " @@ List.map string_of_int points
