exception UnReachable

module Prop = struct
  type t =
    | Atom of string
    | And of t * t
    | Or of t * t
    | Implies of t * t
    | Truth
    | Falsity

  and variant = Atom | And | Or | Implies | Truth | Falsity

  let rec repr = function
    | Atom s -> s
    | And (a, b) -> Printf.sprintf "(%s ∧ %s)" (repr a) (repr b)
    | Or (a, b) -> Printf.sprintf "(%s ∨ %s)" (repr a) (repr b)
    | Implies (a, b) ->
        if b = Falsity then Printf.sprintf "¬%s" (repr a)
        else Printf.sprintf "%s ⊃ %s" (repr a) (repr b)
    | Truth -> "⊤"
    | Falsity -> "⊥"

  let variant_repr (v : variant) =
    match v with
    | Atom -> "Atom"
    | And -> "And(∧)"
    | Or -> "Or(∨)"
    | Implies -> "Implies(⊃)"
    | Truth -> "Truth(⊤)"
    | Falsity -> "Falsity(⊥)"

  let variant_match (v : variant) = function
    | Atom _ -> v = Atom
    | And _ -> v = And
    | Or _ -> v = Or
    | Implies _ -> v = Implies
    | Truth -> v = Truth
    | Falsity -> v = Falsity

  let equal (a : t) (b : t) = a = b
end

module Ctx = struct
  type t = Prop.t list

  let repr ctx = String.concat ", " (List.map Prop.repr ctx)
  let equal (a : t) (b : t) = a = b
  let extend ctx prop = if List.mem prop ctx then ctx else ctx @ [ prop ]
end

module Judgement = struct
  type t = Entail of Ctx.t * Prop.t
  and variant = Entail

  let repr = function
    | Entail (ctx, prop) ->
        Printf.sprintf "%s ⊢ %s" (Ctx.repr ctx) (Prop.repr prop)

  let variant_repr (v : variant) = match v with Entail -> "Entail(⊢)"
  let variant_match (v : variant) = function Entail _ -> v = Entail
  let equal (a : t) (b : t) = a = b
end

module Rule = struct
  type t =
    | Assumption
    | And_I
    | And_E_L
    | And_E_R
    | Or_I_L
    | Or_I_R
    | Or_E
    | Implies_I
    | Implies_E
    | Truth_I
    | Falsity_E

  let repr = function
    | Assumption -> "assumption"
    | And_I -> "∧-I"
    | And_E_L -> "∧-E-L"
    | And_E_R -> "∧-E-R"
    | Or_I_L -> "∨-I-L"
    | Or_I_R -> "∨-I-R"
    | Or_E -> "∨-E"
    | Implies_I -> "⊃-I"
    | Implies_E -> "⊃-E"
    | Truth_I -> "⊤-I"
    | Falsity_E -> "⊥-E"
end

module Derivation = struct
  type t = D of d
  and d = Judgement.t * Rule.t * t list

  let rec repr_helper (indent : int) (d : t) : string =
    let indent_str = String.make indent ' ' in
    match d with
    | D (judgement, rule, derivations) ->
        Printf.sprintf "%s\n%s<%s>\n%s%s"
          (repr_list (indent + 2) derivations)
          indent_str (Rule.repr rule) indent_str (Judgement.repr judgement)

  and repr_list (indent : int) (ds : t list) : string =
    String.concat "" (List.map (fun d -> repr_helper indent d) ds)

  let repr = repr_helper 0
  let premises (D (_, _, ds)) = List.map (fun (D (j, _, _)) -> j) ds
end

module MarkedDerivation = struct
  type t = Correct of d | Incorrect of d * string
  and d = Judgement.t * Rule.t * t list

  let rec repr_helper (indent : int) (d : t) : string =
    let indent_str = String.make indent ' ' in
    match d with
    | Correct (judgement, rule, derivations) ->
        Printf.sprintf "%s\n%s<%s>✅\n%s%s"
          (repr_list (indent + 2) derivations)
          indent_str (Rule.repr rule) indent_str (Judgement.repr judgement)
    | Incorrect ((judgement, rule, derivations), msg) ->
        Printf.sprintf "%s\n%s<%s>❌: %s\n%s%s"
          (repr_list (indent + 2) derivations)
          indent_str (Rule.repr rule) msg indent_str (Judgement.repr judgement)

  and repr_list (indent : int) (ds : t list) : string =
    String.concat "" (List.map (fun d -> repr_helper indent d) ds)

  let repr = repr_helper 0

  let rec fold f acc d =
    match d with
    | Correct (_, _, ds) -> List.fold_left (fold f) (f acc d) ds
    | Incorrect _ -> f acc d

  let correct = function Correct _ -> true | _ -> false
  let all_correct = fold (fun acc d -> acc && correct d) true
end

module Location = struct
  type t = Conclusion | Premise of int

  let repr = function
    | Conclusion -> "Conclusion"
    | Premise i -> Printf.sprintf "Premise %d" i
end

type 'a bind = { location : Location.t; value : 'a }

module MisMatchCommon = struct
  type t =
    | Prop of Prop.variant * Prop.t bind
    | Judgement of Judgement.variant * Judgement.t bind

  let msg = Printf.sprintf "Expected %s %s, got %s [%s]"

  let repr = function
    | Prop (v, p) ->
        msg "Proposition" (Prop.variant_repr v) (Prop.repr p.value)
          (Location.repr p.location)
    | Judgement (v, j) ->
        msg "Judgement" (Judgement.variant_repr v) (Judgement.repr j.value)
          (Location.repr j.location)
end

module NotEqualCommon = struct
  type t =
    | Prop of Prop.t bind * Prop.t bind (* expected, actual *)
    | Ctx of Ctx.t bind * Ctx.t bind
    | Judgement of Judgement.t bind * Judgement.t bind

  let msg = Printf.sprintf "%s %s [%s] not equal to %s [%s]"

  let repr = function
    | Prop (a, b) ->
        msg "Proposition" (Prop.repr a.value) (Location.repr a.location)
          (Prop.repr b.value) (Location.repr b.location)
    | Ctx (a, b) ->
        msg "Context" (Ctx.repr a.value) (Location.repr a.location)
          (Ctx.repr b.value) (Location.repr b.location)
    | Judgement (a, b) ->
        msg "Judgement" (Judgement.repr a.value) (Location.repr a.location)
          (Judgement.repr b.value) (Location.repr b.location)
end

module VerErr = struct
  type t =
    | PremiseMismatch of int * int (* expected, actual *)
    | NotInContext of Prop.t bind * Ctx.t bind
    | MisMatch of MisMatchCommon.t
    | NotEqual of NotEqualCommon.t
    | CtxNotEqualAfterExtend of
        Ctx.t bind * Ctx.t bind * Prop.t bind (* expected, actual, prop *)

  let repr = function
    | PremiseMismatch (e, a) ->
        Printf.sprintf "Expected %d premises, got %d" e a
    | NotInContext (p, c) ->
        Printf.sprintf "Proposition %s [%s] not in context %s [%s]"
          (Prop.repr p.value) (Location.repr p.location) (Ctx.repr c.value)
          (Location.repr c.location)
    | MisMatch m -> MisMatchCommon.repr m
    | NotEqual n -> NotEqualCommon.repr n
    | CtxNotEqualAfterExtend (a, b, p) ->
        Printf.sprintf
          "Context %s [%s] not equal to %s [%s] after extending with %s[%s]"
          (Ctx.repr a.value) (Location.repr a.location) (Ctx.repr b.value)
          (Location.repr b.location) (Prop.repr p.value)
          (Location.repr p.location)
end

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
