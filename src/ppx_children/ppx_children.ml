(* [@@deriving children] for a recursive group of syntax types.

   The group's type declarations split into two kinds:

   - nodes, marked [@children.node]: the boundaries of a traversal (in
     Grammar, the annotated sorts exp_t, pat_t, ... and any_t);
   - everything else, traversed through: the term types and their helpers.

   For each type T that is not a node alias (a node that is a variant, like
   any_t, still gets them) the deriver generates

     map_children_T  : 'a children_map -> 'a T -> 'a T
     fold_children_T : ('a, 'acc) children_fold -> 'a T -> 'acc -> 'acc

   which visit exactly the nodes directly under a value of T, in declaration
   order, by calling the record field named after the node's type. Neither
   function recurses into a node: deep traversals are built on top of them.

   An external type constructor whose arguments mention the group or a type
   parameter is an error unless it says what to do:

   - [@children.opaque]: never look inside;
   - [@children.container (map_f, fold_f)]: a one-parameter container, where
     map_f : ('x => 'x, c('x)) => c('x)
     fold_f : (('x, 'acc) => 'acc, c('x), 'acc) => 'acc.

   That error is the point: a new constructor or container can't silently
   become a leaf. *)

open Ppxlib
module B = Ast_builder.Default

let opaque =
  Attribute.declare "children.opaque" Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()

let container =
  Attribute.declare "children.container" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload (pexp_tuple (__ ^:: __ ^:: nil)))
    (fun map_f fold_f -> (map_f, fold_f))

let node =
  Attribute.declare "children.node" Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()

let map_record = "children_map"
let fold_record = "children_fold"
let acc_var = "children_acc"
let map_fn name = "map_children_" ^ name
let fold_fn name = "fold_children_" ^ name

type group = { members : string list; nodes : string list }

let is_member g name = List.mem name g.members
let is_node g name = List.mem name g.nodes

(* Does [te] mention the group or a type parameter anywhere? *)
let rec mentions g te =
  match te.ptyp_desc with
  | Ptyp_var _ -> true
  | Ptyp_constr ({ txt = Lident name; _ }, _) when is_member g name -> true
  | Ptyp_constr (_, args) | Ptyp_tuple args -> List.exists (mentions g) args
  | _ -> false

(* Whether a value of type [te] can contain a node. *)
let rec has_children g te =
  let loc = te.ptyp_loc in
  match (Attribute.get opaque te, Attribute.get container te) with
  | Some (), _ -> false
  | None, Some _ -> true
  | None, None -> (
      match te.ptyp_desc with
      | Ptyp_var _ -> false
      | Ptyp_tuple tes -> List.exists (has_children g) tes
      | Ptyp_constr ({ txt = Lident name; _ }, _) when is_member g name -> true
      | Ptyp_constr ({ txt = Lident ("list" | "option"); _ }, [ t ]) ->
          has_children g t
      | Ptyp_constr (_, args) when List.exists (mentions g) args ->
          Location.raise_errorf ~loc
            "ppx_children: say whether to traverse this type, with \
             [@children.opaque] or [@children.container (map_f, fold_f)]"
      | _ -> false)

let evar ~loc name = B.evar ~loc name
let field ~loc record name = B.pexp_field ~loc record { txt = Lident name; loc }

let fresh_vars ~loc prefix tes =
  List.mapi (fun i _ -> Printf.sprintf "%s%d" prefix i) tes
  |> List.map (fun v -> (v, B.pvar ~loc v, evar ~loc v))

let container_arg ~loc te =
  match te.ptyp_desc with
  | Ptyp_constr (_, [ arg ]) -> arg
  | _ ->
      Location.raise_errorf ~loc
        "ppx_children: [@children.container] needs a one-parameter type"

(* A function [te -> te] that maps every node under a [te]. *)
let rec map_expr g te =
  let loc = te.ptyp_loc in
  match Attribute.get container te with
  | Some (map_f, _) ->
      [%expr fun __x -> [%e map_f] [%e map_expr g (container_arg ~loc te)] __x]
  | None -> (
      match te.ptyp_desc with
      | Ptyp_constr ({ txt = Lident name; _ }, _) when is_node g name ->
          field ~loc [%expr __m] name
      | Ptyp_constr ({ txt = Lident name; _ }, _) when is_member g name ->
          [%expr [%e evar ~loc (map_fn name)] __m]
      | Ptyp_constr ({ txt = Lident "list"; _ }, [ t ]) ->
          [%expr Stdlib.List.map [%e map_expr g t]]
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ t ]) ->
          [%expr Stdlib.Option.map [%e map_expr g t]]
      | Ptyp_tuple tes ->
          let vars = fresh_vars ~loc "__t" tes in
          let body =
            B.pexp_tuple ~loc
              (List.map2 (fun te (_, _, e) -> map_arg g te e) tes vars)
          in
          B.pexp_fun ~loc Nolabel None
            (B.ppat_tuple ~loc (List.map (fun (_, p, _) -> p) vars))
            body
      | _ -> Location.raise_errorf ~loc "ppx_children: unsupported type")

and map_arg g te e =
  if has_children g te then B.eapply ~loc:e.pexp_loc (map_expr g te) [ e ]
  else e

(* A function [te -> 'acc -> 'acc] that folds over every node under a [te]. *)
let rec fold_expr g te =
  let loc = te.ptyp_loc in
  match Attribute.get container te with
  | Some (_, fold_f) ->
      [%expr
        fun __x __acc ->
          [%e fold_f] [%e fold_expr g (container_arg ~loc te)] __x __acc]
  | None -> (
      match te.ptyp_desc with
      | Ptyp_constr ({ txt = Lident name; _ }, _) when is_node g name ->
          field ~loc [%expr __f] name
      | Ptyp_constr ({ txt = Lident name; _ }, _) when is_member g name ->
          [%expr [%e evar ~loc (fold_fn name)] __f]
      | Ptyp_constr ({ txt = Lident "list"; _ }, [ t ]) ->
          [%expr
            fun __xs __acc ->
              Stdlib.List.fold_left
                (fun __acc __x -> [%e fold_expr g t] __x __acc)
                __acc __xs]
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ t ]) ->
          [%expr
            fun __o __acc ->
              match __o with
              | None -> __acc
              | Some __x -> [%e fold_expr g t] __x __acc]
      | Ptyp_tuple tes ->
          let vars = fresh_vars ~loc "__t" tes in
          [%expr
            fun [%p B.ppat_tuple ~loc (List.map (fun (_, p, _) -> p) vars)]
                __acc ->
              [%e
                fold_args g
                  (List.combine tes (List.map (fun (_, _, e) -> e) vars))]]
      | _ -> Location.raise_errorf ~loc "ppx_children: unsupported type")

(* Thread [__acc] through the arguments that have children, left to right. *)
and fold_args g args =
  List.fold_left
    (fun acc (te, e) ->
      if has_children g te then
        let loc = e.pexp_loc in
        B.eapply ~loc (fold_expr g te) [ e; acc ]
      else acc)
    (let loc = Location.none in
     [%expr __acc])
    args

(* Patterns binding only the arguments that have children. *)
let arg_patterns ~loc g tes =
  List.mapi
    (fun i te ->
      if has_children g te then
        let v = Printf.sprintf "__a%d" i in
        (B.pvar ~loc v, Some (evar ~loc v))
      else (B.ppat_any ~loc, None))
    tes

let construct_pattern ~loc name pats =
  let lid = { txt = Lident name; loc } in
  match pats with
  | [] -> B.ppat_construct ~loc lid None
  | [ p ] -> B.ppat_construct ~loc lid (Some p)
  | ps -> B.ppat_construct ~loc lid (Some (B.ppat_tuple ~loc ps))

let constructor_args ~loc cd =
  match cd.pcd_args with
  | Pcstr_tuple tes -> tes
  | Pcstr_record _ ->
      Location.raise_errorf ~loc "ppx_children: inline records are unsupported"

let map_body g td =
  let loc = td.ptype_loc in
  match (td.ptype_kind, td.ptype_manifest) with
  | Ptype_variant cds, _ ->
      let case cd =
        let loc = cd.pcd_loc in
        let tes = constructor_args ~loc cd in
        if not (List.exists (has_children g) tes) then
          B.case
            ~lhs:
              (construct_pattern ~loc cd.pcd_name.txt
                 (List.map (fun _ -> B.ppat_any ~loc) tes))
            ~guard:None ~rhs:[%expr __x]
        else
          let vars = fresh_vars ~loc "__a" tes in
          let lid = { txt = Lident cd.pcd_name.txt; loc } in
          let args = List.map2 (fun te (_, _, e) -> map_arg g te e) tes vars in
          B.case
            ~lhs:
              (construct_pattern ~loc cd.pcd_name.txt
                 (List.map (fun (_, p, _) -> p) vars))
            ~guard:None
            ~rhs:
              (B.pexp_construct ~loc lid
                 (match args with
                 | [ a ] -> Some a
                 | args -> Some (B.pexp_tuple ~loc args)))
      in
      B.pexp_match ~loc [%expr __x] (List.map case cds)
  | Ptype_record lds, _ ->
      B.pexp_record ~loc
        (List.map
           (fun ld ->
             let lid = { txt = Lident ld.pld_name.txt; loc } in
             (lid, map_arg g ld.pld_type (B.pexp_field ~loc [%expr __x] lid)))
           lds)
        None
  | Ptype_abstract, Some te -> map_arg g te [%expr __x]
  | _ -> Location.raise_errorf ~loc "ppx_children: unsupported declaration"

let fold_body g td =
  let loc = td.ptype_loc in
  match (td.ptype_kind, td.ptype_manifest) with
  | Ptype_variant cds, _ ->
      let case cd =
        let loc = cd.pcd_loc in
        let tes = constructor_args ~loc cd in
        let pats = arg_patterns ~loc g tes in
        let args =
          List.filter_map
            (fun (te, (_, e)) -> Option.map (fun e -> (te, e)) e)
            (List.combine tes pats)
        in
        B.case
          ~lhs:(construct_pattern ~loc cd.pcd_name.txt (List.map fst pats))
          ~guard:None ~rhs:(fold_args g args)
      in
      B.pexp_match ~loc [%expr __x] (List.map case cds)
  | Ptype_record lds, _ ->
      fold_args g
        (List.map
           (fun ld ->
             ( ld.pld_type,
               B.pexp_field ~loc [%expr __x]
                 { txt = Lident ld.pld_name.txt; loc } ))
           lds)
  | Ptype_abstract, Some te -> fold_args g [ (te, [%expr __x]) ]
  | _ -> Location.raise_errorf ~loc "ppx_children: unsupported declaration"

let params_of td =
  List.map
    (fun (te, _) ->
      match te.ptyp_desc with
      | Ptyp_var v -> v
      | _ ->
          Location.raise_errorf ~loc:te.ptyp_loc
            "ppx_children: type parameters must be variables")
    td.ptype_params

let generate ~ctxt (_rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let g =
    {
      members = List.map (fun td -> td.ptype_name.txt) tds;
      nodes =
        List.filter_map
          (fun td ->
            Option.map (fun () -> td.ptype_name.txt) (Attribute.get node td))
          tds;
    }
  in
  let params = params_of (List.hd tds) in
  List.iter
    (fun td ->
      if params_of td <> params then
        Location.raise_errorf ~loc:td.ptype_loc
          "ppx_children: every type in the group needs the same parameters")
    tds;
  let pvars = List.map (B.ptyp_var ~loc) params in
  let acc = B.ptyp_var ~loc acc_var in
  let ty name = B.ptyp_constr ~loc { txt = Lident name; loc } pvars in
  let node_tds = List.filter (fun td -> is_node g td.ptype_name.txt) tds in
  let walked =
    List.filter
      (fun td ->
        match td.ptype_kind with
        | Ptype_abstract -> not (is_node g td.ptype_name.txt)
        | Ptype_variant _ | Ptype_record _ | Ptype_open -> true)
      tds
  in
  let record name field_ty extra_params =
    B.pstr_type ~loc Recursive
      [
        B.type_declaration ~loc ~name:{ txt = name; loc }
          ~params:
            (List.map
               (fun v -> (v, (NoVariance, NoInjectivity)))
               (pvars @ extra_params))
          ~cstrs:[] ~private_:Public ~manifest:None
          ~kind:
            (Ptype_record
               (List.map
                  (fun td ->
                    let n = td.ptype_name.txt in
                    B.label_declaration ~loc ~name:{ txt = n; loc }
                      ~mutable_:Immutable
                      ~type_:(field_ty (ty n)))
                  node_tds));
      ]
  in
  let map_ty = B.ptyp_constr ~loc { txt = Lident map_record; loc } pvars in
  let fold_ty =
    B.ptyp_constr ~loc { txt = Lident fold_record; loc } (pvars @ [ acc ])
  in
  let binding name poly_vars fn_ty body =
    B.value_binding ~loc
      ~pat:
        (B.ppat_constraint ~loc (B.pvar ~loc name)
           (B.ptyp_poly ~loc
              (List.map (fun v -> { txt = v; loc }) poly_vars)
              fn_ty))
      ~expr:body
  in
  let map_binding td =
    let n = td.ptype_name.txt in
    binding (map_fn n) params [%type: [%t map_ty] -> [%t ty n] -> [%t ty n]]
      [%expr fun __m __x -> [%e map_body g td]]
  in
  let fold_binding td =
    let n = td.ptype_name.txt in
    binding (fold_fn n) (params @ [ acc_var ])
      [%type: [%t fold_ty] -> [%t ty n] -> [%t acc] -> [%t acc]]
      [%expr fun __f __x __acc -> [%e fold_body g td]]
  in
  [
    record map_record (fun t -> [%type: [%t t] -> [%t t]]) [];
    record fold_record
      (fun t -> [%type: [%t t] -> [%t acc] -> [%t acc]])
      [ acc ];
    [%stri [@@@ocaml.warning "-26-27-39"]];
    B.pstr_value ~loc Recursive (List.map map_binding walked);
    B.pstr_value ~loc Recursive (List.map fold_binding walked);
  ]
  |> fun items ->
  [ B.pstr_include ~loc (B.include_infos ~loc (B.pmod_structure ~loc items)) ]

let () =
  Deriving.add "children"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate)
  |> Deriving.ignore
