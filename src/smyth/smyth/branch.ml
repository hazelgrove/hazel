open Lang

module Ctor_map = struct
  include Map.Make (struct
    type t = string

    let compare = String.compare
  end)

  let from_assoc (xs : (string * 'a) list) : 'a t =
    List.fold_left (fun dict (ctor_name, x) -> add ctor_name x dict) empty xs

  let from_assoc_many (xs : (string * 'a) list) : 'a list t =
    List.fold_left
      (fun dict (ctor_name, x) ->
        union (fun _ k v -> Some (k @ v)) (singleton ctor_name [x]) dict)
      empty xs
end

let filter (ws : worlds) : worlds =
  List.filter (fun (_env, ex) -> ex <> ExTop) ws

let distribute (delta : hole_ctx) (sigma : datatype_ctx) (hf : hole_filling)
    (ctor_names : string list) (arg_name : string) (scrutinee : exp)
    ((env, ex) : world) : ((string * world) * constraints) Nondet.t =
  let open Nondet.Syntax in
  (* Type soundness ensures that scrutinee evaluates either to a constructor
   * or to an indeterminate result.
   *)
  match Eval.eval env scrutinee with
  | Ok (RCtor (ctor_name, arg), []) ->
      Nondet.pure
        ( (ctor_name, (Env.add_res (arg_name, arg) env, ex))
        , Constraints.empty )
  | Ok (r, []) ->
      let* ctor_name = Nondet.from_list ctor_names in
      let+ ks = Uneval.uneval delta sigma hf r (ExCtor (ctor_name, ExTop)) in
      ( ( ctor_name
        , (Env.add_res (arg_name, RCtorInverse (ctor_name, r)) env, ex) )
      , ks )
  | _ ->
      Log.warn
        ( "Scrutinee did not evaluate to a constructor or indeterminate "
        ^ "result" ) ;
      Nondet.none

let branch max_scrutinee_size delta sigma hf
    ({gamma; goal_type; goal_dec; term_kind}, worlds) =
  let open Nondet.Syntax in
  let* _ = Nondet.guard (Option.is_none goal_dec) in
  let filtered_worlds = filter worlds in
  let arg_name = Term_gen.fresh_ident gamma Term_gen.match_char in
  let* data_name, (datatype_params, data_ctors) = Nondet.from_list sigma in
  let ctor_names = List.map fst data_ctors in
  let ctor_info : (string * typ) Ctor_map.t =
    data_ctors
    |> List.map (Pair2.map_snd @@ fun typ -> (arg_name, typ))
    |> Ctor_map.from_assoc
  in
  let* scrutinee =
    Term_gen.up_to_e sigma max_scrutinee_size
      { gamma
      ; goal_type=
          TData (data_name, List.map (fun _ -> Type.wildcard) datatype_params)
      ; goal_dec= None
      ; term_kind }
    (* TODO: should this be E? *)
  in
  let* datatype_args =
    Type.infer sigma gamma scrutinee
    |> Result2.to_option |> Option.map fst
    |> Option2.and_then (function
         | TData (_, datatype_args) -> Some datatype_args
         | _ ->
             Log.warn
               ( "Non-datatype scrutinee from term generation: "
               ^ Pretty.exp scrutinee ) ;
             None)
    |> Nondet.lift_option
  in
  let top_worlds =
    data_ctors
    |> List.map (Pair2.map_snd @@ fun _ -> (Env.empty, ExTop))
    |> Ctor_map.from_assoc_many
  in
  let* distributed_worldss, ks =
    filtered_worlds
    |> List.map (distribute delta sigma hf ctor_names arg_name scrutinee)
    |> Nondet.one_of_each |> Nondet.map List.split
    |> Nondet.map (Pair2.map_fst Ctor_map.from_assoc_many)
    (* Informativeness Restriction (A) *)
    |> Nondet.filter (fun (ctor_map, _) ->
           List.length data_ctors = 1 || Ctor_map.cardinal ctor_map >= 2)
    |> Nondet.map
         (Pair2.map_fst @@ Ctor_map.union (fun _ _ w -> Some w) top_worlds)
    |> Nondet.and_then (fun (dw, kss) ->
           kss |> Constraints.merge
           |> Option2.map (fun ks -> (dw, ks))
           |> Nondet.lift_option)
  in
  let+ branches_goals =
    Nondet.lift_option
    @@ Ctor_map.fold
         (fun ctor_name distributed_worlds acc_opt ->
           let open! Option2.Syntax in
           let* acc = acc_opt in
           let+ arg_name, raw_arg_type =
             Ctor_map.find_opt ctor_name ctor_info
           in
           let arg_type =
             Type.substitute_many
               ~bindings:(List.combine datatype_params datatype_args)
               raw_arg_type
           in
           let arg_bind_spec =
             scrutinee |> Type.bind_spec gamma |> Type.sub_bind_spec
           in
           let hole_name = Fresh.gen_hole () in
           let goal =
             ( hole_name
             , ( { gamma=
                     Type_ctx.add_type
                       (arg_name, (arg_type, arg_bind_spec))
                       gamma
                 ; goal_type
                 ; goal_dec= None
                 ; term_kind }
               , distributed_worlds ) )
           in
           let branch = (ctor_name, (PVar arg_name, EHole hole_name)) in
           (branch, goal) :: acc)
         distributed_worldss (Some [])
  in
  ( branches_goals |> List.split
    |> Pair2.map_fst (fun branches -> ECase (scrutinee, branches))
  , ks )
