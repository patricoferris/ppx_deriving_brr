open Ppxlib
open Ast_builder.Default

let impl (ld : label_declaration) =
  let loc = ld.pld_loc in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat = ppat_var ~loc ld.pld_name;
        pvb_expr =
          pexp_fun ~loc Nolabel None
            (ppat_var ~loc { loc; txt = "jv" })
            (Common.type_to_getter ld.pld_name.txt ld.pld_type);
        pvb_attributes = [];
        pvb_loc = loc;
      };
    ]
