(*
 * Copyright (c) 2021 Patrick Ferris <patrick@sirref.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Ppxlib
open Ast_builder.Default

let accessor_intf ~ptype_name (ld : label_declaration) =
  let loc = ld.pld_loc in
  psig_value ~loc
    {
      pval_name = ld.pld_name;
      pval_type =
        ptyp_arrow ~loc Nolabel
          (ptyp_constr ~loc { loc; txt = lident ptype_name.txt } [])
          ld.pld_type;
      pval_attributes = [];
      pval_loc = loc;
      pval_prim = [];
    }

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
          Location.raise_errorf ~loc
            "Cannot derive accessors for non record types"
      | { ptype_kind = Ptype_record fields; _ } -> List.map Accessor.impl fields)
    type_declarations
  |> List.concat

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
          Location.raise_errorf ~loc
            "Cannot derive accessors for non record types"
      | { ptype_kind = Ptype_record fields; ptype_name; _ } ->
          List.map (accessor_intf ~ptype_name) fields)
    type_declarations
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let () =
  Deriving.add "brr" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
  |> Deriving.ignore
