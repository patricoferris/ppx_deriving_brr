open Ppxlib
open Utils
open Ast_builder.Default

let rec type_to_conv ?(to_ = false) typ =
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: int] -> if to_ then [%expr Jv.to_int] else [%expr Jv.of_int]
  | [%type: float] -> if to_ then [%expr Jv.to_float] else [%expr Jv.of_float]
  | [%type: Jstr.t] -> if to_ then [%expr Jv.to_jstr] else [%expr Jv.of_jstr]
  | [%type: string] ->
      if to_ then [%expr Jv.to_string] else [%expr Jv.of_string]
  | [%type: bool] -> if to_ then [%expr Jv.to_bool] else [%expr Jv.of_bool]
  | _ -> assert false

let rec type_to_getter key typ =
  let loc = typ.ptyp_loc in
  let key' = estring ~loc @@ snake_to_camel key in
  match typ with
  | [%type: int] -> [%expr Jv.Int.get jv [%e key']]
  | [%type: float] -> [%expr Jv.Float.get jv [%e key']]
  | [%type: Jstr.t] -> [%expr Jv.Jstr.get jv [%e key']]
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
  | _ -> assert false

let impl (ld : label_declaration) =
  let loc = ld.pld_loc in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat = ppat_var ~loc ld.pld_name;
        pvb_expr =
          pexp_fun ~loc Nolabel None
            (ppat_var ~loc { loc; txt = "jv" })
            (type_to_getter ld.pld_name.txt ld.pld_type);
        pvb_attributes = [];
        pvb_loc = loc;
      };
    ]
