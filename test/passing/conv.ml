type t = { name : string; age : int; height : int option; books : string list }
[@@deriving brr_jv]

let pr { name; age; height; books } =
  Format.printf "name: %s; age: %i; height: %a; books: %a" name age
    Format.(pp_print_option pp_print_int)
    height
    Format.(pp_print_list pp_print_string)
    books

let () =
  let jv =
    Jv.obj
      [|
        ("name", Jv.of_string "Bactrian");
        ("age", Jv.of_int 100);
        ("height", Jv.of_int 200);
        ("books", Jv.of_jstr_list [ Jstr.v "Alice in Wonderland" ]);
      |]
  in
  let p = of_jv jv in
  let jv' = to_jv p in
  let p' = of_jv jv' in
  assert (p = p');
  pr p
