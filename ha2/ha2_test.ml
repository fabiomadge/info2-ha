open Batteries
open Kaputt.Abbreviations

module Ha = Ha2m
module Sol = Ha2_sol

let tests = ref []
let add test = tests := !tests @ [test]

(* probably don't need enumerators, just assert base case + random *)
let () =
  let open Assert in
  let open Ha.MyList in
  add @@ Test.make_simple_test ~title:"avg [] = avgf [] = None, sumf [] = 0." (fun () -> equal None (avg []) ;equal None (avgf []) ;equal 0. (sumf []));
  add @@ Test.make_simple_test ~title:"head [] = tail [] = last [] = None" (fun () -> equal None (head []); equal None (tail []); equal None (last []));
  add @@ Test.make_simple_test ~title:"append [] [] = palindrome [] = reverse [] = map _ [] = double [] = filter _ [] = only_even [] = []" (fun () ->
    equal [] (append [] []);
    equal [] (palindrome []);
    equal [] (reverse []);
    equal [] (map identity []);
    equal [] (double []);
    equal [] (filter not []);
    equal [] (only_even [])
  );
  add @@ Test.make_simple_test ~title:"fold_left (+) 0 [] = fold_right (+) [] 0 = 0, fold_left const 9 [1;2;3] = 9, fold_right const [1;2;3] 9 = 1" (fun () ->
    equal 0 (fold_left (+) 0 []);
    equal 0 (fold_right (+) [] 0);
    equal 9 (fold_left const 9 [1;2;3]);
    equal 1 (fold_right const [1;2;3] 9)
  );
  add @@ Test.make_simple_test ~title:"exists identity [] = false, exists identity [false; true] = true, for_all identity [] = true, for_all identity [false, false] = false" (fun () ->
    equal false (exists identity []);
    equal true (exists identity [false; true]);
    equal true (for_all identity []);
    equal false (for_all identity [false; false])
  );
  let open Ha.NonEmptyList in
  add @@ Test.make_simple_test ~title:"NonEmptyList.from_list [] = None" (fun () -> equal None (from_list []));
  add @@ Test.make_simple_test ~title:"NonEmptyList.from_list [1;2;3]" (fun () -> equal (Some (Cons (1, Cons (2, Nil 3))))  (from_list [1;2;3]));
  add @@ Test.make_simple_test ~title:"NonEmptyList.to_list (Cons (1, Nil 2)) = [1;2]" (fun () -> equal [1;2] (to_list (Cons (1, Nil 2))));
  let open Ha.Db in
  add @@ Test.make_simple_test ~title:"Db.find_student \"No\" = None" (fun () -> equal None (find_student "No"));
  add @@ Test.make_simple_test ~title:"Db.avg_age 20 = None" (fun () -> equal None (avg_age 20));
  add @@ Test.make_simple_test ~title:"Db.avg_grade_student \"No\" = Db.avg_grade course \"No\" = Db.avg_grade_course_passed \"No\" = Db.avg_grade_course_semester \"No\" 20 = None" (fun () ->
    equal None (avg_grade_student "No");
    equal None (avg_grade_course "No");
    equal None (avg_grade_course_passed "No");
    equal None (avg_grade_course_semester "No" 20)
  )

let add_random title gen ft fs = add @@ Test.make_random_test ~title:title
    ~nb_runs:10 gen ft [Spec.always => (fun (a,b) -> fs a = b)]
let () =
  let module X = Ha.MyList in
  let module S = Sol.MyList in
  let gen_list ?(min=0) ?(max=10) g = Gen.list (Gen.make_int min max) g in
  let letters = gen_list Gen.letter in
  let neletters = gen_list ~min:1 Gen.letter in
  let small_int = Gen.make_int (-100) 100 in
  let ints = gen_list small_int in
  add_random "MyList.append" (Gen.zip2 letters letters) (uncurry X.append) (uncurry List.append);
  add_random "MyList.palindrome" letters X.palindrome Sol.MyList.palindrome;
  add_random "MyList.head" letters X.head Sol.MyList.head;
  add_random "MyList.tail" letters X.tail Sol.MyList.tail;
  add_random "MyList.last" letters X.last Sol.MyList.last;
  add_random "MyList.reverse" letters X.reverse Sol.MyList.reverse;
  add_random "MyList.is_palindrome" letters X.is_palindrome Sol.MyList.is_palindrome;
  let succ = (+) 1 in
  add_random "MyList.map ((+) 1)" ints (X.map succ) (List.map succ);
  add @@ Test.make_simple_test ~title:"MyList.map: check if f isn't called too often"
    (fun () ->
      (* let eq_int_list = Assert.make_equal_list (=) string_of_int in *)
      let f = Mock.from_function succ in
      let i = [0; 1; 2; 0] in
      let _ = X.map (Mock.func f) i in
      (* checking the order is tricky: let-in forces evaluation, w/o it's from right to left *)
      (* eq_int_list i (Mock.calls f); *)
      Assert.equal_int (List.length i) (Mock.total f));
  add_random "MyList.double" ints X.double (Sol.MyList.map (fun x -> 2*x));
  let p = (>=) 0 in add_random "MyList.filter ((>=) 0)" ints (X.filter p) (Sol.MyList.filter p);
  add_random "MyList.even and odd" small_int (fun x -> X.(even x, odd x)) (fun x -> Sol.MyList.(even x, odd x));
  add_random "MyList.only_even" ints X.only_even (Sol.MyList.only_even);
  add_random "MyList.fold_left (-) 9999 xs" ints (X.fold_left (-) 9999) (Sol.MyList.fold_left (-) 9999);
  add_random "MyList.fold_right (-) xs 1" ints (fun xs -> X.fold_right (-) xs 1) (fun xs -> Sol.MyList.fold_right (-) xs 1);
  add_random "MyList.exists ((>) 80)" ints (X.exists ((>) 80)) (Sol.MyList.exists ((>) 80));
  add_random "MyList.for_all ((>) 80)" ints (X.for_all ((>) 0)) (Sol.MyList.for_all ((>) 0));
  let module X =  Ha.NonEmptyList in
  let module S = Sol.NonEmptyList in
  let rec from_list = function
    | [] -> None
    | x::xs ->
      Some (match from_list xs with
      | None -> X.Nil x
      | Some z -> X.Cons (x, z))
  in
  let from_list = Option.get % from_list in
  let rec to_list = function
    | X.Cons (x,xs) -> x :: to_list xs
    | X.Nil x -> [x]
  in
  (* add_random "NonEmptyList.from_list" letters X.from_list S.from_list; *)
  add_random "NonEmptyList.to_list" neletters (X.to_list%from_list) (S.to_list%Option.get%S.from_list);
  add_random "NonEmptyList.head" neletters (X.head%from_list) (S.head%Option.get%S.from_list);
  add_random "NonEmptyList.tail" neletters (Option.map(to_list)%X.tail%from_list) (Option.map(S.to_list)%S.tail%Option.get%S.from_list);
  add_random "NonEmptyList.last" neletters (X.last%from_list) (S.last%Option.get%S.from_list);
  let module X =  Ha.Db in
  let module S = Sol.Db in
  let snames = Gen.select_list (List.map (fun x -> x.S.sname) S.students) identity in
  let cnames = Gen.select_list (List.map (fun x -> x.S.cname) S.courses) identity in
  let semesters = Gen.select_list (List.map (fun x -> x.S.semester) S.students) string_of_int in
  add_random "Db.find_student" snames (Option.map (fun s -> s.X.sname) % X.find_student) (Option.map (fun s -> s.S.sname) % S.find_student);
  add_random "Db.avg_age" semesters X.avg_age S.avg_age;
  add_random "Db.avg_grade_student" snames X.avg_grade_student S.avg_grade_student;
  add_random "Db.avg_grade_course" cnames X.avg_grade_course S.avg_grade_course;
  add_random "Db.avg_grade_course_passed" cnames X.avg_grade_course_passed S.avg_grade_course_passed;
  add_random "Db.avg_grade_course_semester" (Gen.zip2 cnames semesters) (uncurry X.avg_grade_course_semester) (uncurry S.avg_grade_course_semester)


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
