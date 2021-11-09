ppx_deriving_brr
----------------

*Status: WIP and just an experiment*

A ppx for generating the frequent idioms of brr. Quick thank you to the folks maintaining [mirage/repr](https://github.com/mirage/repr) from whom the ppx test framework was stolen.

Brr already has great [documentation](https://erratique.ch/software/brr/doc/index.html) and an [FFI guide](https://erratique.ch/software/brr/doc/ffi_manual.html). This ppx was just meant to make some of that writing a little easier whilst sticking to the principles as best I could.

## Getters & Setters

When working with `js_of_ocaml` you often have to ask yourself is it worth turning this Javascript value into an OCaml value? There's an associated cost with doing so which is particularly apparent if you convert lots of JSON into OCaml in the browser. An approach (in the brr) world that tends to be better is to use getters and setters for the different fields of the value and keep it in the form `type t = Jv.t`.

For an OCaml programmer (or at least for me) it can be quick and easier to reason about these when thinking of them as OCaml record. The `[@@deriving brr]` annotation produces these getters and setters for some `internal` type. 

```ocaml
type t = Jv.t

type internal = {
  name : string;
  nick_name : string option;
  age : int option;
  books : Jstr.t list;
  favourite_numbers : int option array;
}
[@@deriving brr]
```

Which you can then use. 

```ocaml
let how_many_books t = print_int (List.length @@ books t)
```

*Aside: I'm not 100% sure how useful this really is, for large APIs maybe it's easier to write an OCaml record.*

## Optional Arguments

Another common Brr idiom is to have some optional arguments: 

```ocaml
type opt = Jv.t 
let opt ?width ?height ?two_d ~urls () =
  let jv = Jv.obj [||] in
  Jv.set jv "urls" (Jv.of_array Jv.of_string urls);
  Jv.Bool.set_if_some jv "twoD" two_d;
  Jv.Int.set_if_some jv "height" height;
  Jv.Int.set_if_some jv "width" width;
  jv
let create ?(opt = Jv.undefined) s =
  Jv.new' (Jv.obj [||]) [| Jv.of_string s; opt |]
```

With `[@@deriving brr_opt]` you can generate the (sometimes quite plentiful) optional arguments

```ocaml
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
```

Again, I'm not sure how much better this is, perhaps if the `ppx` rewrote the type so there wasn't an indirection it would better (if I can work out how to do that, I will I think).

## Conversion Functions

Sometimes you really do just want to take a `Jv.t` and make a `t`... 


```ocaml
type t = { name : string; age : int; height : int option; books : string list }
[@@deriving brr_jv]
```

This will produce `of_jv` and `to_jv` functions for the type `t`.
