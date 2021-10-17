let snake_to_camel s =
  String.split_on_char '_' s
  |> List.mapi (fun i s -> if i > 0 then String.capitalize_ascii s else s)
  |> String.concat ""
