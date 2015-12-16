open Batteries

module Json = struct
  (* our json datatype using maps and lists *)
  type t =
    | Null
    | Bool of bool
    | Number of int
    | String of string
    | Object of (string,t) Map.t
    | Array  of t list
  (* t is recursive! offset is used for accesssing inner json values *)
  type offset = Field of string | Index of int (* Field for Object, Index for Array *)
  (* we write foo.bar[1].baz for [Field "foo"; Field "bar"; Index 1; Field "baz"] *)
  type path = offset list

  (* example:
    { "a": null, "b": true, "c": 2, "d": "hello \"you\"!", "e": {  }, "f": [0, false, "no"] }
  *)
  let rec show = function
    | Null -> "null"
    | Bool x -> string_of_bool x
    | Number x -> string_of_int x
    | String x -> "\"" ^ String.escaped x ^ "\""
    | Object x -> "{ " ^ String.concat ", " (List.map (fun (k,v) -> show (String k) ^ ": " ^ show v) (Map.to_list x)) ^ " }"
    | Array x -> "[" ^ String.concat ", " (List.map show x) ^ "]"

  (* map f over all values of type t *)
  let rec map f = function
    | Object x -> f (Object (Map.map (map f) x))
    | Array x -> f (Array (List.map (map f) x))
    | x -> f x

  (* return the content of a Field or Index of a json value or None *)
  (* e.g. get (Index 1) (Array [Null, Bool true, Bool false]) = Some (Bool true)
   *      get_offset (Index 1) (Array []) = None
   *      get_offset (Index 1) Null = None
   *)
  let get_offset offset json = match offset, json with
    | Field f, Object o -> Map.find f o
    | Index i, Array a -> List.at i a
    | _ -> None

  (* same as get_offset, but now on a whole path. if any part of the path fails we return None, otherwise the value. *)
  (* e.g. get [Field "a"; Index 0] (Object (Map.from_list ["a", Array [Null]])) = Some Null
   *      get [Field "a"; Index 1] (Object (Map.from_list ["a", Array [Null]])) = None
   *)
  let rec get path json =
    match path with
    | [] -> Some json
    | x::xs -> Option.(get_offset x json >>= get xs)

  (* same as get_offset, but now we want to set some value *)
  (* e.g. set_offset (Field "a") Null (Object Map.empty) = Some (Object (Map.from_list ["a", Null]))
          set_offset (Index 1) (Bool true) (Array [Null; Null; Null] = Some (Array [Null; Bool true; Null]
          if the index is out of range we just return None:
          set_offset (Index 1) (Bool true) (Array [Null]) = None

   *)
  let set_offset offset v json = match offset, json with
    | Field f, Object o -> Some (Object (Map.add f v o))
    | Index i, Array a when i < List.length a -> Some (Array (List.mapi (fun j x -> if j=i then v else x) a))
    | _ -> None

  (* same as set_offset, but for setting a json value at some path *)
  (* e.g. set [Field "a"; Index 1] (Bool true) (Object (Map.from_list ["a", Array [Null; Null]])) = Some (Object (Map.from_list ["a", Array [Null; Bool true]]))
   *      set [Field "a"; Index 2] (Bool true) (Object (Map.from_list ["a", Array [Null; Null]])) = None
   *      set [Field "a"] (Bool true) (Object (Map.empty)) = Some (Object Map.(add "a" (Bool true) empty))
   *      set [] v json = Some v
   *)
  let rec set path value json =
    let open Option in
    match path with
    | [] -> Some value
    | [x] -> set_offset x value json
    | x::xs -> get_offset x json >>= set xs value >>= fun v -> set_offset x v json


  (* homework *)
  (* shows the path: we write .foo.bar[1].baz for [Field "foo"; Field "bar"; Index 1; Field "baz"] *)
  let rec show_path path = todo ()

  (* gets the children, i.e., the values of Object and Array *)
  let get_children json = todo ()

  (* gets all subtrees that match the path (path does not have to start at the root).
   * e.g. get_all [Field "foo"] (Array [Null; Object (Map.from_list ["foo", String "one"; "bar", Null]); Object (Map.from_list ["baz", Null; "foo", String "two"])]) = [String "one"; String "two"] *)
  let rec get_all path json = todo ()

  (* parse a string and return some json *)
  (* simplifications:
   * - only the same format as produced by show must be supported
   * - numbers are now integers instead of floats
   * - no escaping in strings, i.e., a string contains no quotes
   *)
  let from_string s = todo ()
end
