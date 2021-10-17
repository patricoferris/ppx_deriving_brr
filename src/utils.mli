val snake_to_camel : string -> string
(** [snake_to_camel] takes a string in snake-case and converts it to camel-case *)

val conv_name : ?to_:bool -> string -> string
(** [conv_name] converts the type name using the standard ppx way of doing things *)
