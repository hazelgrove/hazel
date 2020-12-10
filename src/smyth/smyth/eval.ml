open Lang

type eval_result = (res * resumption_assertions, string) result

type eval_env_result = (env * resumption_assertions, string) result

(* Note: fuel gets applied at every application. *)
module FuelLimited = struct
  let rec eval fuel env exp =
    let open Result2.Syntax in
    let* _ =
      Result2.guard "Evaluation time exceeded"
      @@ Timer.Single.check Timer.Single.Eval
    in
    let* _ = Result2.guard "Ran out of fuel" (fuel > 0) in
    match exp with
    | EFix (f, x, body) -> Ok (RFix (env, f, x, body), [])
    | EVar x -> (
      match List.assoc_opt x (Env.all_res env) with
      | None -> Error ("Variable not found: " ^ x)
      | Some r -> Ok (r, []) )
    | EHole name -> Ok (RHole (env, name), [])
    | ETuple comps ->
        comps
        |> List.map (eval fuel env)
        |> Result2.sequence
        |> Result2.map (fun evals ->
               evals |> List.split
               |> Pair2.map_fst (fun rs -> RTuple rs)
               |> Pair2.map_snd List.concat)
    (* Ctor types are erased at runtime *)
    | ECtor (name, _, arg) ->
        let+ r, ks = eval fuel env arg in
        (RCtor (name, r), ks)
    | EApp (_, e1, EAExp e2) -> (
        let* r1, ks1 = eval fuel env e1 in
        let* r2, ks2 = eval fuel env e2 in
        match r1 with
        | RFix (f_env, f, PatParam x, body) ->
            let f_env_extension = Pat.bind_rec_name_res f r1 in
            let* x_env_extension =
              Pat.bind_res x r2
              |> Option.to_result ~none:"Pattern match failed"
            in
            let new_env =
              Env.concat [x_env_extension; f_env_extension; f_env]
            in
            Result2.map
              (Pair2.map_snd @@ fun ks3 -> ks1 @ ks2 @ ks3)
              (eval (fuel - 1) new_env body)
        | _ -> Ok (RApp (r1, RARes r2), ks1 @ ks2) )
    | EApp (_, head, EAType type_arg) -> (
        let* r_head, ks1 = eval fuel env head in
        match r_head with
        | RFix (f_env, f, TypeParam type_param, body) ->
            let f_env_extension = Pat.bind_rec_name_res f r_head in
            let param_env_extension =
              Env.add_type (type_param, type_arg) Env.empty
            in
            let new_env =
              Env.concat [param_env_extension; f_env_extension; f_env]
            in
            Result2.map
              (Pair2.map_snd @@ fun ks2 -> ks1 @ ks2)
              (eval fuel new_env body)
        | _ -> Ok (RApp (r_head, RAType type_arg), ks1) )
    | EProj (n, i, arg) -> (
        if i <= 0 then Error "Non-positive projection index"
        else if i > n then
          Error "Projection index greater than projection length"
        else
          let* r_arg, ks_arg = eval fuel env arg in
          match r_arg with
          | RTuple comps ->
              if n <> List.length comps then
                Error "Projection length does not match tuple size"
              else
                (* 0 < i <= n = |comps| *)
                Ok (List.nth comps (i - 1), ks_arg)
          | _ -> Ok (RProj (n, i, r_arg), ks_arg) )
    | ECase (scrutinee, branches) -> (
        let* r0, ks0 = eval fuel env scrutinee in
        match r0 with
        | RCtor (ctor_name, r_arg) -> (
          match List.assoc_opt ctor_name branches with
          | Some (arg_pattern, body) ->
              let* arg_env_extension =
                Pat.bind_res arg_pattern r_arg
                |> Option.to_result ~none:"Pattern match failed"
              in
              Result2.map
                (Pair2.map_snd @@ fun ks_body -> ks0 @ ks_body)
                (eval fuel (Env.concat [arg_env_extension; env]) body)
          | None ->
              Error
                ( "Non-exhaustive pattern match, "
                ^ "could not find constructor '" ^ ctor_name ^ "'" ) )
        | _ -> Ok (RCase (env, r0, branches), ks0) )
    | EAssert (e1, e2) -> (
        let* r1, ks1 = eval fuel env e1 in
        let* r2, ks2 = eval fuel env e2 in
        match Res.consistent r1 r2 with
        | Some ks3 -> Ok (RTuple [], ks1 @ ks2 @ ks3)
        | None -> Error "Result consistency failure" )
    | ETypeAnnotation (e, _) -> eval fuel env e

  let rec resume fuel hf res =
    let open Result2.Syntax in
    match res with
    | RHole (env, name) -> (
      match Hole_map.find_opt name hf with
      | Some binding ->
          if binding = EHole name then Ok (res, [])
          else
            let* r, ks = eval fuel env binding in
            let+ r', ks' = resume fuel hf r in
            (r', ks @ ks')
      | None ->
          let+ env', ks = resume_env fuel hf env in
          (RHole (env', name), ks) )
    | RFix (env, f, x, body) ->
        let+ env', ks = resume_env fuel hf env in
        (RFix (env', f, x, body), ks)
    | RTuple comps ->
        comps
        |> List.map (resume fuel hf)
        |> Result2.sequence
        |> Result2.map (fun rs ->
               rs |> List.split
               |> Pair2.map_fst (fun rs -> RTuple rs)
               |> Pair2.map_snd List.concat)
    | RCtor (name, arg) ->
        let+ arg', ks = resume fuel hf arg in
        (RCtor (name, arg'), ks)
    | RApp (r1, RARes r2) -> (
        let* r1', ks1 = resume fuel hf r1 in
        let* r2', ks2 = resume fuel hf r2 in
        match r1' with
        | RFix (f_env, f, PatParam x, body) ->
            let f_env_extension = Pat.bind_rec_name_res f r1' in
            let* x_env_extension =
              Pat.bind_res x r2'
              |> Option.to_result ~none:"Pattern match failed"
            in
            let new_env =
              Env.concat [x_env_extension; f_env_extension; f_env]
            in
            let* r, ks = eval (fuel - 1) new_env body in
            let+ r', ks' = resume (fuel - 1) hf r in
            (r', ks1 @ ks2 @ ks @ ks')
        | _ -> Ok (RApp (r1', RARes r2'), ks1 @ ks2) )
    | RApp (r_head, RAType type_arg) -> (
        let* r_head', ks_head = resume fuel hf r_head in
        match r_head' with
        | RFix (f_env, f, TypeParam type_param, body) ->
            let f_env_extension = Pat.bind_rec_name_res f r_head' in
            let param_env_extension =
              Env.add_type (type_param, type_arg) Env.empty
            in
            let new_env =
              Env.concat [param_env_extension; f_env_extension; f_env]
            in
            let* r, ks = eval (fuel - 1) new_env body in
            let+ r', ks' = resume (fuel - 1) hf r in
            (r', ks_head @ ks @ ks')
        | _ -> Ok (RApp (r_head', RAType type_arg), ks_head) )
    | RProj (n, i, arg) -> (
        if i <= 0 then Error "Non-positive projection index"
        else if i > n then
          Error "Projection index greater than projection length"
        else
          let* arg', ks_arg = resume fuel hf arg in
          match arg' with
          | RTuple comps ->
              if n <> List.length comps then
                Error "Projection length does not match tuple size"
              else
                (* 0 < i <= n = |comps| *)
                Ok (List.nth comps (i - 1), ks_arg)
          | _ -> Ok (RProj (n, i, arg'), ks_arg) )
    | RCase (env, scrutinee, branches) -> (
        let* r0, ks0 = resume fuel hf scrutinee in
        match r0 with
        | RCtor (ctor_name, r_arg) -> (
          match List.assoc_opt ctor_name branches with
          | Some (arg_pattern, body) ->
              Result2.map
                (Pair2.map_snd @@ fun ks_body -> ks0 @ ks_body)
                ( resume fuel hf
                @@ RApp
                     ( RFix (env, None, PatParam arg_pattern, body)
                     , RARes r_arg ) )
          | None ->
              Error
                ( "Non-exhaustive pattern match, "
                ^ "could not find constructor '" ^ ctor_name ^ "'" ) )
        | _ ->
            let+ env', ks_env = resume_env fuel hf env in
            (RCase (env', r0, branches), ks0 @ ks_env) )
    | RCtorInverse (name, arg) ->
        let+ arg', ks = resume fuel hf arg in
        (RCtorInverse (name, arg'), ks)

  and resume_env fuel hf env : eval_env_result =
    let open Result2.Syntax in
    let+ new_res_env, ks =
      env |> Env.all_res
      |> List.map (fun binding ->
             binding
             |> Pair2.map_snd (resume fuel hf)
             |> Pair2.lift_snd_result)
      |> Result2.sequence
      |> Result2.map (fun binding ->
             binding
             |> List.map (fun (x, (r, ks)) -> ((x, r), ks))
             |> List.split |> Pair2.map_snd List.concat)
    in
    ((new_res_env, Env.all_type env), ks)
end

let eval env exp =
  Timer.Single.start Timer.Single.Eval ;
  FuelLimited.eval !Params.initial_fuel env exp

let resume hf res =
  Timer.Single.start Timer.Single.Eval ;
  FuelLimited.resume !Params.initial_fuel hf res
