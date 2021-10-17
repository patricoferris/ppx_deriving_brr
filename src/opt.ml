open Ppxlib
open Utils
open Ast_builder.Default

let record_names_patt ~loc { pld_name; _ } =
  let var = pvar ~loc pld_name.txt in
  ({ txt = lident pld_name.txt; loc = pld_name.loc }, [%pat? [%p var]])

let fun_expr ~loc const (ld : label_declaration list) =
  let is_optional = function [%type: [%t? _] option] -> true | _ -> false in
  let required, optional =
    List.fold_left
      (fun (r, o) ld ->
        if is_optional ld.pld_type then (r, ld :: o) else (ld :: r, o))
      ([], []) ld
  in
  let fun_r =
    if required = [] then [%expr fun () -> [%e const]]
    else
      List.fold_left
        (fun acc { pld_name; pld_loc = loc; _ } ->
          pexp_fun ~loc (Labelled pld_name.txt) None (pvar ~loc pld_name.txt)
            [%expr [%e acc]])
        [%expr fun () -> [%e const]]
        required
  in
  List.fold_left
    (fun acc { pld_name; pld_loc = loc; _ } ->
      pexp_fun ~loc (Optional pld_name.txt) None (pvar ~loc pld_name.txt)
        [%expr [%e acc]])
    fun_r optional

let impl_to_jv ~loc ~name (ld : label_declaration list) =
  let label_impl acc ld =
    let setter = Common.type_to_setter ld.pld_name.txt ld.pld_type in
    [%expr
      [%e setter];
      [%e acc]]
  in
  let setters = List.fold_left label_impl [%expr jv] ld in
  let constructor =
    [%expr
      let jv = Jv.obj [||] in
      [%e setters]]
  in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat = ppat_var ~loc { name with txt = conv_name ~to_:true name.txt };
        pvb_expr = fun_expr ~loc constructor ld;
        pvb_attributes = [];
        pvb_loc = loc;
      };
    ]
