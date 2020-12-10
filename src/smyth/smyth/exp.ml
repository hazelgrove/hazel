open Lang

let rec syntactically_equal e1 e2 =
  match (e1, e2) with
  | EFix (mf1, x1, body1), EFix (mf2, x2, body2) ->
      let mf_equal =
        match (mf1, mf2) with
        | Some f1, Some f2 -> String.equal f1 f2
        | None, None -> true
        | _ -> false
      in
      let x_equal =
        match (x1, x2) with
        | PatParam p1, PatParam p2 -> Pat.syntactically_equal p1 p2
        | TypeParam t1, TypeParam t2 -> String.equal t1 t2
        | _ -> false
      in
      mf_equal && x_equal && syntactically_equal body1 body2
  | EApp (b1, head1, EAExp arg1), EApp (b2, head2, EAExp arg2) ->
      Bool.equal b1 b2
      && syntactically_equal head1 head2
      && syntactically_equal arg1 arg2
  | EApp (b1, e1, EAType t1), EApp (b2, e2, EAType t2) ->
      Bool.equal b1 b2 && syntactically_equal e1 e2 && Type.equal t1 t2
  | EVar x1, EVar x2 -> String.equal x1 x2
  | ETuple es1, ETuple es2 ->
      Int.equal (List.length es1) (List.length es2)
      && List.for_all2 syntactically_equal es1 es2
  | EProj (n1, i1, arg1), EProj (n2, i2, arg2) ->
      Int.equal n1 n2 && Int.equal i1 i2 && syntactically_equal arg1 arg2
  | ECtor (name1, taus1, arg1), ECtor (name2, taus2, arg2) ->
      String.equal name1 name2
      && Int.equal (List.length taus1) (List.length taus2)
      && List.for_all2 Type.equal taus1 taus2
      && syntactically_equal arg1 arg2
  | ECase (s1, branches1), ECase (s2, branches2) ->
      syntactically_equal s1 s2
      && Int.equal (List.length branches1) (List.length branches2)
      && List.for_all2
           (fun (ctor1, (arg1, body1)) (ctor2, (arg2, body2)) ->
             String.equal ctor1 ctor2
             && Pat.syntactically_equal arg1 arg2
             && syntactically_equal body1 body2)
           branches1 branches2
  | EHole name1, EHole name2 -> Int.equal name1 name2
  | EAssert (left1, right1), EAssert (left2, right2) ->
      syntactically_equal left1 left2 && syntactically_equal right1 right2
  | ETypeAnnotation (e1, tau1), ETypeAnnotation (e2, tau2) ->
      Type.equal tau1 tau2 && syntactically_equal e1 e2
  | _ -> false

let rec largest_hole : exp -> hole_name =
 fun exp ->
  match exp with
  (* Main case *)
  | EHole hole_name -> hole_name
  (* Other cases *)
  | EApp (_, e1, EAExp e2) | EAssert (e1, e2) ->
      max (largest_hole e1) (largest_hole e2)
  | EFix (_, _, e)
   |EApp (_, e, EAType _)
   |EProj (_, _, e)
   |ECtor (_, _, e)
   |ETypeAnnotation (e, _) ->
      largest_hole e
  | EVar _ -> Fresh.unused
  | ETuple components ->
      components |> List.map largest_hole |> List2.maximum
      |> Option2.with_default Fresh.unused
  | ECase (scrutinee, branches) ->
      let branch_max =
        branches
        |> List.map (snd >> snd >> largest_hole)
        |> List2.maximum
        |> Option2.with_default Fresh.unused
      in
      max (largest_hole scrutinee) branch_max

let rec has_special_recursion : exp -> bool = function
  | EFix (_, _, body) -> has_special_recursion body
  | EApp (special, e1, EAExp e2) ->
      special || has_special_recursion e1 || has_special_recursion e2
  | EApp (special, e1, EAType _) -> special || has_special_recursion e1
  | EVar _ -> false
  | ETuple components -> List.exists has_special_recursion components
  | EProj (_, _, arg) -> has_special_recursion arg
  | ECtor (_, _, arg) -> has_special_recursion arg
  | ECase (scrutinee, branches) ->
      has_special_recursion scrutinee
      || List.exists (fun (_, (_, e)) -> has_special_recursion e) branches
  | EHole _ -> false
  | EAssert (e1, e2) -> has_special_recursion e1 || has_special_recursion e2
  | ETypeAnnotation (e, _) -> has_special_recursion e

let fill_hole : hole_name * exp -> exp -> exp =
 fun (hole_name, hole_exp) ->
  let rec helper : exp -> exp = function
    (* Main case *)
    | EHole hole_name' ->
        if Int.equal hole_name hole_name' then hole_exp else EHole hole_name'
    (* Other cases *)
    | EFix (f, x, body) -> EFix (f, x, helper body)
    | EApp (special, e1, EAExp e2) ->
        EApp (special, helper e1, EAExp (helper e2))
    | EApp (special, e1, EAType type_arg) ->
        EApp (special, helper e1, EAType type_arg)
    | EVar x -> EVar x
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
  helper
