module Json :
  sig
    type t =
        Null
      | Bool of bool
      | Number of float
      | String of string
      | Object of (string, t) Batteries.Map.t
      | Array of t list
    type offset = Field of string | Index of int
    type path = offset list
    val show : t -> string
    val map : (t -> t) -> t -> t
    val get_offset : offset -> t -> t option
    val get : offset list -> t -> t option
    val set_offset : offset -> t -> t -> t option
    val set : offset list -> t -> t -> t option
    val show_path : offset list -> string
    val get_children : t -> t list
    val get_all : offset list -> t -> t list
    val from_string : string -> t option
  end
