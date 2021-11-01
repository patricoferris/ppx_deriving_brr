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

let generate_accessor_impl ~ctxt (_rec_flag, type_declarations) =
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

let generate_conv_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
          Location.raise_errorf ~loc
            "Cannot derive conversion functions for non record types"
      | {
       ptype_kind = Ptype_record fields;
       ptype_loc = loc;
       ptype_name = name;
       _;
      } ->
          Conv.impl_of_jv ~name ~loc fields)
    type_declarations
  @ List.map
      (fun (td : type_declaration) ->
        match td with
        | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
            Location.raise_errorf ~loc
              "Cannot derive conversion functions for non record types"
        | {
         ptype_kind = Ptype_record fields;
         ptype_loc = loc;
         ptype_name = name;
         _;
        } ->
            Conv.impl_to_jv ~name ~loc fields)
      type_declarations

let opt_impl_generator ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
          Location.raise_errorf ~loc
            "Cannot derive optional functions for non record types"
      | {
       ptype_kind = Ptype_record fields;
       ptype_loc = loc;
       ptype_name = name;
       _;
      } ->
          Opt.impl_to_jv ~name ~loc fields)
    type_declarations

let accessor_impl_generator =
  Deriving.Generator.V2.make_noarg generate_accessor_impl

let conv_impl_generator = Deriving.Generator.V2.make_noarg generate_conv_impl
let opt_impl_generator = Deriving.Generator.V2.make_noarg opt_impl_generator

let () =
  Deriving.add "brr" ~str_type_decl:accessor_impl_generator |> Deriving.ignore;
  Deriving.add "brr_jv" ~str_type_decl:conv_impl_generator |> Deriving.ignore;
  Deriving.add "brr_opt" ~str_type_decl:opt_impl_generator |> Deriving.ignore
