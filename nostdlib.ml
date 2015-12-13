(* ye shall not use stdlib! *)
module Nope = struct end
(* from Batteries.Legacy: *)
module Arg = Nope module Array = Nope module ArrayLabels = Nope module Buffer = Nope module Bytes = Nope module Callback = Nope module Char = Nope module Complex = Nope module Digest = Nope module Filename = Nope module Format = Nope module Gc = Nope module Genlex = Nope module Hashtbl = Nope module Int32 = Nope module Int64 = Nope module Lazy = Nope module Lexing = Nope module List = Nope module ListLabels = Nope module Map = Nope module Marshal = Nope module MoreLabels = Nope module Nativeint = Nope module Oo = Nope module Parsing = Nope module Printexc = Nope module Printf = Nope module Queue = Nope module Random = Nope module Scanf = Nope module Set = Nope module Sort = Nope module Stack = Nope module StdLabels = Nope module Stream = Nope module String = Nope module StringLabels = Nope module Sys = Nope module Weak = Nope module Unix = Nope module Num = Nope module Big_int = Nope module Bigarray = Nope module Str = Nope

(* still need to take care of Pervasives, which is included by default *)
module Pervasives = Nope
let open_out x = failwith "No IO allowed!"
let open_out_bin x = failwith "No IO allowed!"
let open_out_gen xs i x = failwith "No IO allowed!"
let open_in x = failwith "No IO allowed!"
let open_in_bin x = failwith "No IO allowed!"
let open_in_gen xs i x = failwith "No IO allowed!"
