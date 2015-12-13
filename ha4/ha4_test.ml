open Batteries
open Kaputt.Abbreviations

module A = Ha4 (* ha *)
module B = Ha4_sol (* sol *)
module type S = module type of Ha4_sol

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
  (* let gint = Gen.make_int (-10) 10 in *)
  let gpint = Gen.make_int 0 10 in
  (* let gints = glist gint in *)
  (* let gpints = glist gpint in *)
  let even x = x mod 2 = 0 in
  rand "MyMap.mem" (Gen.zip2 gpint (glist (Gen.zip2 gpint gpint))) (fun (module X) (x,xs) -> X.MyMap.(mem x (from_list xs)));
  rand "MyMap.min" (glist (Gen.zip2 gpint gpint)) (fun (module X) xs -> X.MyMap.(min (from_list xs)));
  rand "MyMap.max" (glist (Gen.zip2 gpint gpint)) (fun (module X) xs -> X.MyMap.(max (from_list xs)));
  rand "MyMap.remove" (Gen.zip2 gpint (glist (Gen.zip2 gpint gpint))) (fun (module X) (x,xs) -> X.MyMap.(List.sort @@ to_list @@ remove x (from_list xs)));
  rand "MyMap.map" (glist (Gen.zip2 gpint gpint)) (fun (module X) xs -> X.MyMap.(to_list @@ map succ (from_list xs)));
  rand "MyMap.filter" (glist (Gen.zip2 gpint gpint)) (fun (module X) xs -> X.MyMap.(to_list @@ filter (even%snd) (from_list xs)));
  (* rand "MyMap.merge (* keep left *)" (Gen.zip2 (glist (Gen.zip2 gpint gpint)) (glist (Gen.zip2 gpint gpint))) (fun (module X) (xs,ys) -> X.MyMap.(to_list @@ merge (fun k v1 v2 -> v1) (from_list xs) (from_list ys))); *)
  (* rand "MyMap.merge (* keep right *)" (Gen.zip2 (glist (Gen.zip2 gpint gpint)) (glist (Gen.zip2 gpint gpint))) (fun (module X) (xs,ys) -> X.MyMap.(to_list @@ merge (fun k v1 v2 -> v2) (from_list xs) (from_list ys))); *)
  rand "MyMap.merge (* left/right/both *)" (Gen.zip2 (glist (Gen.zip2 gpint gpint)) (glist (Gen.zip2 gpint gpint))) (fun (module X) (xs,ys) -> X.MyMap.(List.sort @@ to_list @@ merge (fun k v1 v2 -> match v1,v2 with Some _, Some _ -> Some "both" | Some _,None -> Some "left" | None,Some _ -> Some "right" | _ -> None) (from_list xs) (from_list ys)));
  let open Assert in
  let open A.Json in
  add @@ Test.make_simple_test ~title:"Json.show nonrec (Null, Bool, Number, String)" (fun () ->
    List.iter (fun x -> equal_string (B.Json.show x) (show x)) [Null; Bool true; Bool false; Number 1.; Number 3.14; String "Hallo"]
  );
  let o1 = Object (A.MyMap.from_list ["a", Null; "b", Bool true; "c", Array [Null; String "huhu"]; "d", Object (A.MyMap.from_list ["xx", Array []; "yy", Null])]) in
  let p1 = [Field "no", o1; Field "a", o1; Index 1, Array [Null; Null]; Index 1, Array [Null]] in
  let p2 = [ [], o1; [Field "no"], o1; [Field "c"; Index 1], o1; [Field "d"; Field "xx"], o1; [Field "d"; Field "xx"; Index 0], o1 ] in
  let v = String "write" in
  let fm = function Bool x -> Bool (not x) | Null -> Bool false | Object x -> Object (A.MyMap.add "oh" Null x) | _ -> Null in
  add @@ Test.make_simple_test ~title:"Json.show rec (Object, Array)" (fun () ->
    List.iter (fun x -> equal_string (B.Json.show x) (show x)) [Object A.MyMap.empty; Array []; o1]
  );
  add @@ Test.make_simple_test ~title:"Json.map" (fun () ->
    List.iter (fun x -> equal_string B.Json.(show (map fm x)) (B.Json.show (map fm x))) [Null; Object A.MyMap.empty; Array []; o1]
  );
  let eq_string_opt a b = let p = Option.show B.Json.show in equal_string (p a) (p b) in
  add @@ Test.make_simple_test ~title:"Json.get_offset" (fun () ->
    List.iter (fun (p,o) -> eq_string_opt B.Json.(get_offset p o) (get_offset p o)) p1);
  add @@ Test.make_simple_test ~title:"Json.get" (fun () ->
    List.iter (fun (p,o) -> eq_string_opt B.Json.(get p o) (get p o)) p2);
  add @@ Test.make_simple_test ~title:"Json.set_offset" (fun () ->
    List.iter (fun (p,o) -> eq_string_opt B.Json.(set_offset p v o) (set_offset p v o)) p1);
  add @@ Test.make_simple_test ~title:"Json.set" (fun () ->
    List.iter (fun (p,o) -> eq_string_opt B.Json.(set p v o) (set p v o)) p2)


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
