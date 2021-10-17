type t = Jv.t

type internal = {
  name : string;
  nick_name : string option;
  age : int option;
  books : Jstr.t list;
  favourite_numbers : int option array;
}
[@@deriving brr]

let () =
  let o =
    Jv.obj
      [|
        ("name", Jv.of_string "Bactrian");
        ("nickName", Jv.of_string "B");
        ("age", Jv.of_int 100);
        ("books", Jv.of_list Jv.of_jstr [ Jstr.v "Alice in Wonderland" ]);
        ( "favouriteNumbers",
          Jv.of_jv_array
            [| Jv.of_int 42; Jv.of_int 1347; Jv.undefined; Jv.of_int 17 |] );
      |]
  in
  Format.printf "name: %s, nick_name: %a; age: %a; books: %a, numbers: %a"
    (name o)
    Format.(pp_print_option pp_print_string)
    (nick_name o)
    Format.(pp_print_option pp_print_int)
    (age o)
    Format.(pp_print_list pp_print_string)
    (books o |> List.map Jstr.to_string)
    Format.(pp_print_list (pp_print_option pp_print_int))
    (favourite_numbers o |> Array.to_list)
