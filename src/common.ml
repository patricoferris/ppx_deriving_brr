open Ppxlib
open Utils
open Ast_builder.Default

let rec type_to_conv ?(to_ = false) typ =
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: int] -> if to_ then [%expr Jv.to_int] else [%expr Jv.of_int]
  | [%type: float] -> if to_ then [%expr Jv.to_float] else [%expr Jv.of_float]
  | [%type: Jstr.t] -> if to_ then [%expr Jv.to_jstr] else [%expr Jv.of_jstr]
  | [%type: Jv.t] -> if to_ then [%expr Jv.Id.to_jv] else [%expr Jv.Id.of_jv]
  | [%type: string] ->
      if to_ then [%expr Jv.to_string] else [%expr Jv.of_string]
  | [%type: bool] -> if to_ then [%expr Jv.to_bool] else [%expr Jv.of_bool]
  | [%type: [%t? typ'] option] ->
      let conv = type_to_conv ~to_ typ' in
      [%expr fun jv -> if Jv.is_none jv then None else Some ([%e conv] jv)]
  | [%type: [%t? typ'] list] ->
      let conv = type_to_conv ~to_ typ' in
      if to_ then [%expr Jv.to_list [%e conv]] else [%expr Jv.of_list [%e conv]]
  | [%type: [%t? typ'] array] ->
      let conv = type_to_conv ~to_ typ' in
      if to_ then [%expr Jv.to_array [%e conv]]
      else [%expr Jv.of_array [%e conv]]
  | _ -> assert false

let rec type_to_getter key typ =
  let loc = typ.ptyp_loc in
  let key' = estring ~loc @@ snake_to_camel key in
  match typ with
  | [%type: int] -> [%expr Jv.Int.get jv [%e key']]
  | [%type: float] -> [%expr Jv.Float.get jv [%e key']]
  | [%type: Jstr.t] -> [%expr Jv.Jstr.get jv [%e key']]
  | [%type: Jv.t] -> [%expr Jv.get jv [%e key']]
  | [%type: string] -> [%expr Jv.Jstr.get jv [%e key'] |> Jstr.to_string]
  | [%type: bool] -> [%expr Jv.Bool.get jv [%e key']]
  | [%type: [%t? typ'] option] ->
      let expr = type_to_conv ~to_:true typ' in
      [%expr
        let jv = Jv.get jv [%e key'] in
        if Jv.is_none jv then None else Some ([%e expr] jv)]
  | [%type: [%t? typ'] list] ->
      let conv = type_to_conv ~to_:true typ' in
      [%expr
        let jv = Jv.get jv [%e key'] in
        Jv.to_list [%e conv] jv]
  | [%type: [%t? typ'] array] ->
      let conv = type_to_conv ~to_:true typ' in
      [%expr
        let jv = Jv.get jv [%e key'] in
        Jv.to_array [%e conv] jv]
  | _ -> assert false

let rec type_to_setter key typ =
  let loc = typ.ptyp_loc in
  let key' = estring ~loc @@ snake_to_camel key in
  let var = evar ~loc key in
  match typ with
  | [%type: int] -> [%expr Jv.Int.set jv [%e key'] [%e var]]
  | [%type: float] -> [%expr Jv.Float.set jv [%e key'] [%e var]]
  | [%type: Jstr.t] -> [%expr Jv.Jstr.set jv [%e key'] [%e var]]
  | [%type: Jv.t] -> [%expr Jv.set jv [%e key'] [%e var]]
  | [%type: string] -> [%expr Jv.Jstr.set jv [%e key'] (Jstr.v [%e var])]
  | [%type: bool] -> [%expr Jv.Bool.set jv [%e key'] [%e var]]
  | [%type: [%t? typ'] option] ->
      let expr = type_to_setter key typ' in
      let var' = pvar ~loc key in
      [%expr (function None -> () | Some [%p var'] -> [%e expr]) [%e var]]
  | [%type: [%t? typ'] list] ->
      let conv = type_to_conv ~to_:false typ' in
      [%expr Jv.set jv [%e key'] (Jv.of_list [%e conv] [%e var])]
  | [%type: [%t? typ'] array] ->
      let conv = type_to_conv ~to_:false typ' in
      [%expr Jv.set jv [%e key'] (Jv.of_array [%e conv] [%e var])]
  | _ -> assert false
