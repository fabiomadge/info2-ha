open Batteries
open Kaputt.Abbreviations

module X = Ha1m
module Sol = Ha1_sol

let tests = ref []
let add test = tests := !tests @ [test]

(* probably don't need enumerators, just assert base case + random *)
let () =
  add @@ Test.make_simple_test ~title:"hello \"you\" = \"Hello you!\"" (fun () -> Assert.equal "Hello you!" @@ X.hello "you");
  add @@ Test.make_simple_test ~title:"fac 1 = 1" (fun () -> Assert.equal 1 @@ X.fac 1);
  add @@ Test.make_simple_test ~title:"fib 1 = 1" (fun () -> Assert.equal 1 @@ X.fib 1);
  add @@ Test.make_simple_test ~title:"even_sum 0 = 0" (fun () -> Assert.equal 0 @@ X.even_sum 0);
  add @@ Test.make_simple_test ~title:"is_empty" (fun () -> Assert.is_true (X.is_empty []); Assert.is_false (X.is_empty [1;2;3]));
  add @@ Test.make_simple_test ~title:"length [] = 0" (fun () -> Assert.equal 0 @@ X.length []);
  add @@ Test.make_simple_test ~title:"sum [] = 0" (fun () -> Assert.equal 0 @@ X.sum [])
let add_random title gen ft fs = add @@ Test.make_random_test ~title:title
    ~nb_runs:10 gen ft [Spec.always => (fun (a,b) -> fs a = b)]
let () =
  let small_pos_int = Gen.make_int 1 10 in
  let name = Gen.string small_pos_int Gen.letter in
  add_random "hello" name X.hello Sol.hello;
  add_random "fac" small_pos_int X.fac Sol.fac;
  add_random "fib" small_pos_int X.fib Sol.fib;
  add_random "even_sum" small_pos_int X.even_sum Sol.even_sum;
  let gen_list g = Gen.list small_pos_int g in
  let letters = gen_list Gen.letter in
  let ints = gen_list small_pos_int in
  add_random "List.length" letters X.length List.length;
  add_random "List.sum" ints X.sum (fun a -> try List.sum a with _ -> 0)


(* launch *)
let () =
  let point = Test.(function
    | Passed -> 1
    | Report (p,n,e,c,m) when p=n -> 1
    | _ -> 0)
  in
  let points = List.map point (Test.exec_tests !tests) in
  Test.run_tests ~output:(Test.Html_output (Legacy.open_out "result.html")) !tests;
  Test.run_tests !tests;
  prerr_endline @@ "### GRADES: " ^ String.concat " " @@ List.map string_of_int points
