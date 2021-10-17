open Ppxlib
open Utils
open Ast_builder.Default

let record_names_expr ~loc { pld_name; _ } =
  let var = evar ~loc pld_name.txt in
  ({ txt = lident pld_name.txt; loc = pld_name.loc }, [%expr [%e var]])

let record_names_patt ~loc { pld_name; _ } =
  let var = pvar ~loc pld_name.txt in
  ({ txt = lident pld_name.txt; loc = pld_name.loc }, [%pat? [%p var]])

let impl_of_jv ~loc ~name (ld : label_declaration list) =
  let label_impl acc ld =
    let loc = ld.pld_loc in
    let name = ld.pld_name.txt |> pvar ~loc in
    let getter = Common.type_to_getter ld.pld_name.txt ld.pld_type in
    [%expr
      let [%p name] = [%e getter] in
      [%e acc]]
  in
  let record =
    let value = List.map (record_names_expr ~loc) ld in
    pexp_record ~loc value None
  in
  let constructor = List.fold_left label_impl record ld in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat =
          ppat_var ~loc { name with txt = conv_name ~to_:false name.txt };
        pvb_expr =
          pexp_fun ~loc Nolabel None
            (ppat_var ~loc { loc; txt = "jv" })
            constructor;
        pvb_attributes = [];
        pvb_loc = loc;
      };
    ]

let impl_to_jv ~loc ~name (ld : label_declaration list) =
  let label_impl acc ld =
    let setter = Common.type_to_setter ld.pld_name.txt ld.pld_type in
    [%expr
      [%e setter];
      [%e acc]]
  in
  let constructor = List.fold_left label_impl [%expr jv] ld in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat = ppat_var ~loc { name with txt = conv_name ~to_:true name.txt };
        pvb_expr =
          pexp_fun ~loc Nolabel None
            (ppat_record ~loc (List.map (record_names_patt ~loc) ld) Closed)
            [%expr
              let jv = Jv.obj [||] in
              [%e constructor]];
        pvb_attributes = [];
        pvb_loc = loc;
      };
    ]
