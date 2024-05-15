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
    | Implies (a, b) -> Printf.sprintf "(%s ⊃ %s)" (repr a) (repr b)
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
