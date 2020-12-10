open Lang

let salvage_constructor : exp -> (string * typ list) option =
  let rec helper type_args exp =
    match exp with
    | EVar name ->
        if Char2.uppercase_char name.[0] then Some (name, type_args)
        else None
    | EApp (_, head, EAType type_arg) -> helper (type_arg :: type_args) head
    | _ -> None
  in
  helper []

let exp : exp -> exp =
 fun root ->
  Fresh.set_largest_hole (Exp.largest_hole root) ;
  let rec helper exp =
    match exp with
    (* Main cases *)

    (* Handle constructor applications *)
    | EApp (special, e1, EAExp e2) -> (
      match salvage_constructor e1 with
      | Some (ctor_name, type_args) -> ECtor (ctor_name, type_args, helper e2)
      | None -> EApp (special, helper e1, EAExp (helper e2)) )
    (* Handle syntactic sugar for unapplied constructors *)
    | EVar name ->
        if Char2.uppercase_char name.[0] then ECtor (name, [], ETuple [])
        else EVar name
    | EApp (special, head, EAType type_arg) -> (
      match salvage_constructor exp with
      | Some (ctor_name, type_args) -> ECtor (ctor_name, type_args, ETuple [])
      | None -> EApp (special, helper head, EAType type_arg) )
    (* Set proper hole names *)
    | EHole hole_name ->
        if Int.equal hole_name Fresh.unused then EHole (Fresh.gen_hole ())
        else EHole hole_name
    (* Other cases *)
    | EFix (f, x, body) -> EFix (f, x, helper body)
    | ETuple components -> ETuple (List.map helper components)
    | EProj (n, i, arg) -> EProj (n, i, helper arg)
    | ECtor (ctor_name, type_args, arg) ->
        ECtor (ctor_name, type_args, helper arg)
    | ECase (scrutinee, branches) ->
        ECase
          ( helper scrutinee
          , List.map (Pair2.map_snd (Pair2.map_snd helper)) branches )
    | EAssert (e1, e2) -> EAssert (helper e1, helper e2)
    | ETypeAnnotation (e, tau) -> ETypeAnnotation (helper e, tau)
  in
  helper root
