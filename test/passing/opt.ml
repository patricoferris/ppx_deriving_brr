module Shape : sig
  type t
  type opt

  val opt :
    ?width:int -> ?height:int -> ?two_d:bool -> urls:string array -> unit -> opt

  val create : ?opt:opt -> string -> t
end = struct
  type t = Jv.t

  type opt_internal = {
    width : int option;
    height : int option;
    two_d : bool option;
    urls : string array;
  }
  [@@deriving brr_opt]

  type opt = Jv.t

  let opt = opt_internal_to_jv

  let create ?(opt = Jv.undefined) s =
    Jv.new' (Jv.obj [||]) [| Jv.of_string s; opt |]
end
