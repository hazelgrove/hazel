open DerivationType
open VerificationError

exception UnReachable

module PropVer = struct
  open Prop

  let expect (v : variant) p : (t bind, VerErr.t) result =
    if variant_match v p.value then Ok p else Error (MisMatch (Prop (v, p)))

  let bind_unit = function Ok _ -> Ok () | Error e -> Error e

  let bind_unzip (res : (t bind, VerErr.t) result) =
    match res with
    | Ok p ->
        Ok
          (match p.value with
          | And (a, b) | Or (a, b) | Implies (a, b) ->
              ( { location = p.location; value = a },
                { location = p.location; value = b } )
          | _ -> raise UnReachable)
    | Error e -> Error e

  let expect_Atom p = expect Atom p
  let expect_And p = expect And p |> bind_unzip
  let expect_Or p = expect Or p |> bind_unzip
  let expect_Implies p = expect Implies p |> bind_unzip
  let expect_Truth p = expect Truth p |> bind_unit
  let expect_Falsity p = expect Falsity p |> bind_unit

  let expect_eq (a : t bind) (b : t bind) : (unit, VerErr.t) result =
    if equal a.value b.value then Ok () else Error (NotEqual (Prop (a, b)))
end

module CtxVer = struct
  open Ctx

  let expect_in_ctx (p : Prop.t bind) (c : t bind) : (unit, VerErr.t) result =
    if List.mem p.value c.value then Ok () else Error (NotInContext (p, c))

  let expect_eq (a : t bind) (b : t bind) : (unit, VerErr.t) result =
    if equal a.value b.value then Ok () else Error (NotEqual (Ctx (a, b)))

  let expect_eq_after_extend (a : t bind) (b : t bind) (p : Prop.t bind) :
      (unit, VerErr.t) result =
    if equal (extend a.value p.value) b.value then Ok ()
    else Error (CtxNotEqualAfterExtend (a, b, p))
end

module JudgementVer = struct
  open Judgement

  let expect (v : variant) (j : t bind) : (t bind, VerErr.t) result =
    if variant_match v j.value then Ok j
    else Error (MisMatch (Judgement (v, j)))

  let bind_unzip (res : (t bind, VerErr.t) result) =
    match res with
    | Ok j ->
        Ok
          (match j.value with
          | Entail (c, p) ->
              ( { location = j.location; value = c },
                { location = j.location; value = p } ))
    | Error e -> Error e

  let expect_Entail j = expect Entail j |> bind_unzip

  let expect_eq (a : t bind) (b : t bind) : (unit, VerErr.t) result =
    if equal a.value b.value then Ok () else Error (NotEqual (Judgement (a, b)))
end

module PremiseVer = struct
  open Judgement

  let expect_len (premises : t list) (n : int) :
      (int -> t bind, VerErr.t) result =
    if List.length premises = n then
      Ok (fun i -> { location = Premise i; value = List.nth premises i })
    else Error (PremiseMismatch (n, List.length premises))
end

let ( let$ ) x f = match x with Ok x -> f x | Error e -> Error e

let verify (conclusion : Judgement.t) (rule : Rule.t)
    (premises : Judgement.t list) : (unit, VerErr.t) result =
  let conclusion = { location = Conclusion; value = conclusion } in
  match rule with
  | Assumption ->
      let$ _ = PremiseVer.expect_len premises 0 in
      let$ ctx, prop = JudgementVer.expect_Entail conclusion in
      let$ () = CtxVer.expect_in_ctx prop ctx in
      Ok ()
  | And_I ->
      let$ p = PremiseVer.expect_len premises 2 in
      let$ ctx, prop = JudgementVer.expect_Entail conclusion in
      let$ a, b = PropVer.expect_And prop in
      let$ ctx', a' = JudgementVer.expect_Entail (p 0) in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_eq a a' in
      let$ ctx', b' = JudgementVer.expect_Entail (p 1) in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_eq b b' in
      Ok ()
  | And_E_L ->
      let$ p = PremiseVer.expect_len premises 1 in
      let$ ctx, a = JudgementVer.expect_Entail conclusion in
      let$ ctx', prop' = JudgementVer.expect_Entail (p 0) in
      let$ a', _ = PropVer.expect_And prop' in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_eq a a' in
      Ok ()
  | And_E_R ->
      let$ p = PremiseVer.expect_len premises 1 in
      let$ ctx, b = JudgementVer.expect_Entail conclusion in
      let$ ctx', prop' = JudgementVer.expect_Entail (p 0) in
      let$ _, b' = PropVer.expect_And prop' in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_eq b b' in
      Ok ()
  | Or_I_L ->
      let$ p = PremiseVer.expect_len premises 1 in
      let$ ctx, prop = JudgementVer.expect_Entail conclusion in
      let$ a, _ = PropVer.expect_Or prop in
      let$ ctx', a' = JudgementVer.expect_Entail (p 0) in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_eq a a' in
      Ok ()
  | Or_I_R ->
      let$ p = PremiseVer.expect_len premises 1 in
      let$ ctx, prop = JudgementVer.expect_Entail conclusion in
      let$ _, b = PropVer.expect_Or prop in
      let$ ctx', b' = JudgementVer.expect_Entail (p 0) in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_eq b b' in
      Ok ()
  | Or_E ->
      let$ p = PremiseVer.expect_len premises 3 in
      let$ ctx, c = JudgementVer.expect_Entail conclusion in
      let$ ctx', prop = JudgementVer.expect_Entail (p 0) in
      let$ a, b = PropVer.expect_Or prop in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ ctx_a', c' = JudgementVer.expect_Entail (p 1) in
      let$ () = CtxVer.expect_eq_after_extend ctx ctx_a' a in
      let$ () = PropVer.expect_eq c c' in
      let$ ctx_b', c' = JudgementVer.expect_Entail (p 2) in
      let$ () = CtxVer.expect_eq_after_extend ctx ctx_b' b in
      let$ () = PropVer.expect_eq c c' in
      Ok ()
  | Implies_I ->
      let$ p = PremiseVer.expect_len premises 1 in
      let$ ctx, prop = JudgementVer.expect_Entail conclusion in
      let$ a, b = PropVer.expect_Implies prop in
      let$ ctx_a', b' = JudgementVer.expect_Entail (p 0) in
      let$ () = CtxVer.expect_eq_after_extend ctx ctx_a' a in
      let$ () = PropVer.expect_eq b b' in
      Ok ()
  | Implies_E ->
      let$ p = PremiseVer.expect_len premises 2 in
      let$ ctx, b = JudgementVer.expect_Entail conclusion in
      let$ ctx', prop = JudgementVer.expect_Entail (p 0) in
      let$ a, b' = PropVer.expect_Implies prop in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_eq b b' in
      let$ ctx', a' = JudgementVer.expect_Entail (p 1) in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_eq a a' in
      Ok ()
  | Truth_I ->
      let$ _ = PremiseVer.expect_len premises 0 in
      let$ _, prop = JudgementVer.expect_Entail conclusion in
      let$ () = PropVer.expect_Truth prop in
      Ok ()
  | Falsity_E ->
      let$ p = PremiseVer.expect_len premises 1 in
      let$ ctx, _ = JudgementVer.expect_Entail conclusion in
      let$ ctx', prop = JudgementVer.expect_Entail (p 0) in
      let$ () = CtxVer.expect_eq ctx ctx' in
      let$ () = PropVer.expect_Falsity prop in
      Ok ()

let rec mark (d : Derivation.t) : MarkedDerivation.t =
  match d with
  | D (conclusion, rule, derivations) -> (
      let marked = (conclusion, rule, List.map mark derivations) in
      let premises = Derivation.premises d in
      match verify conclusion rule premises with
      | Ok () -> Correct marked
      | Error e -> Incorrect (marked, VerErr.repr e))
