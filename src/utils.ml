let snake_to_camel s =
  String.split_on_char '_' s
  |> List.mapi (fun i s -> if i > 0 then String.capitalize_ascii s else s)
  |> String.concat ""

let conv_name ?(to_ = false) s =
  let v = if to_ then "to_jv" else "of_jv" in
  if String.equal s "t" then v else s ^ "_" ^ v
