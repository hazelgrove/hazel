module Prop = struct
  type t =
    | Atom of string
    | And of t * t
    | Or of t * t
    | Imply of t * t
    | Truth
    | Falsity

  let rec repr = function
    | Atom s -> s
    | And (a, b) -> Printf.sprintf "(%s ∧ %s)" (repr a) (repr b)
    | Or (a, b) -> Printf.sprintf "(%s ∨ %s)" (repr a) (repr b)
    | Imply (a, b) -> Printf.sprintf "(%s ⊃ %s)" (repr a) (repr b)
    | Truth -> "⊤"
    | Falsity -> "⊥"

  let error_mismatch = Printf.sprintf "Expected %s, got %s"

  let expect_Atom = function
    | Atom s -> Ok s
    | p -> Error (error_mismatch "Atom" (repr p))

  let expect_And = function
    | And (a, b) -> Ok (a, b)
    | p -> Error (error_mismatch "And(∧)" (repr p))

  let expect_Or = function
    | Or (a, b) -> Ok (a, b)
    | p -> Error (error_mismatch "Or(∨)" (repr p))

  let expect_Imply = function
    | Imply (a, b) -> Ok (a, b)
    | p -> Error (error_mismatch "Imply(⊃)" (repr p))

  let expect_Truth = function
    | Truth -> Ok ()
    | p -> Error (error_mismatch "Truth(⊤)" (repr p))

  let expect_Falsity = function
    | Falsity -> Ok ()
    | p -> Error (error_mismatch "Falsity(⊥)" (repr p))
end

module Ctx = struct
  type t = Prop.t list

  let repr ctx = String.concat ", " (List.map Prop.repr ctx)
end

module Judgement = struct
  type t = Entail of Ctx.t * Prop.t

  let repr = function
    | Entail (ctx, prop) ->
        Printf.sprintf "%s ⊢ %s" (Ctx.repr ctx) (Prop.repr prop)

  let expect_Entail = function Entail (ctx, prop) -> Ok (ctx, prop)
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
    | Imply_I
    | Imply_E
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
    | Imply_I -> "⊃-I"
    | Imply_E -> "⊃-E"
    | Truth_I -> "⊤-I"
    | Falsity_E -> "⊥-E"

  let ( let$ ) x f =
    match x with Ok x, _ -> f x | Error e, where -> Some (e ^ where)

  let expect_True (b : bool) (e : string) = if b then Ok () else Error e

  let expect_premises_len (premises : 'a list) (len : int) =
    expect_True
      (List.length premises = len)
      (Printf.sprintf "Expected %d premises, got %d" len (List.length premises))

  let expect_match (a : 'a) (b : 'a) (where : string) (what : string) =
    expect_True (a = b) (Printf.sprintf "Cannot match %s of %s" what where)

  let verify (conclusion : Judgement.t) (rule : t) (premises : Judgement.t list)
      : string option =
    let where = " in conclusion" in
    match rule with
    | Assumption ->
        let$ () = (expect_premises_len premises 0, "") in
        let$ ctx, prop = (Judgement.expect_Entail conclusion, where) in
        let$ () =
          ( expect_True (List.mem prop ctx)
              (Printf.sprintf "Proposition %s not in context" (Prop.repr prop)),
            "" )
        in
        None
    | And_I ->
        let$ () = (expect_premises_len premises 2, "") in
        let p1 = List.nth premises 0 in
        let p2 = List.nth premises 1 in
        let$ ctx, prop = (Judgement.expect_Entail conclusion, where) in
        let$ a, b = (Prop.expect_And prop, where) in
        let where = " in premise 1" in
        let$ ctx', a' = (Judgement.expect_Entail p1, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let$ () = (expect_match a a' "conclusion" "prop", where) in
        let where = " in premise 2" in
        let$ ctx', b' = (Judgement.expect_Entail p2, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let$ () = (expect_match b b' "conclusion" "prop", where) in
        None
    | And_E_L ->
        let$ () = (expect_premises_len premises 1, "") in
        let p = List.nth premises 0 in
        let$ ctx, a = (Judgement.expect_Entail conclusion, where) in
        let where = " in premise 1" in
        let$ ctx', prop = (Judgement.expect_Entail p, where) in
        let$ a', _ = (Prop.expect_And prop, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let$ () = (expect_match a a' "conclusion" "prop", where) in
        None
    | And_E_R ->
        let$ () = (expect_premises_len premises 1, "") in
        let p = List.nth premises 0 in
        let$ ctx, b = (Judgement.expect_Entail conclusion, where) in
        let where = " in premise 1" in
        let$ ctx', prop = (Judgement.expect_Entail p, where) in
        let$ _, b' = (Prop.expect_And prop, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let$ () = (expect_match b b' "conclusion" "prop", where) in
        None
    | Or_I_L ->
        let$ () = (expect_premises_len premises 1, "") in
        let p = List.nth premises 0 in
        let$ ctx, prop = (Judgement.expect_Entail conclusion, where) in
        let$ a, _ = (Prop.expect_Or prop, where) in
        let where = " in premise 1" in
        let$ ctx', a' = (Judgement.expect_Entail p, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let$ () = (expect_match a a' "conclusion" "prop", where) in
        None
    | Or_I_R ->
        let$ () = (expect_premises_len premises 1, "") in
        let p = List.nth premises 0 in
        let$ ctx, prop = (Judgement.expect_Entail conclusion, where) in
        let$ _, b = (Prop.expect_Or prop, where) in
        let where = " in premise 1" in
        let$ ctx', b' = (Judgement.expect_Entail p, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let$ () = (expect_match b b' "conclusion" "prop", where) in
        None
    | Or_E ->
        let$ () = (expect_premises_len premises 3, "") in
        let p1 = List.nth premises 0 in
        let p2 = List.nth premises 1 in
        let p3 = List.nth premises 2 in
        let$ ctx, c = (Judgement.expect_Entail conclusion, where) in
        let where = " in premise 1" in
        let$ ctx', prop = (Judgement.expect_Entail p1, where) in
        let$ a, b = (Prop.expect_Or prop, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let where = " in premise 2" in
        let$ ctx_a', c' = (Judgement.expect_Entail p2, where) in
        let$ () =
          (expect_match (a :: ctx) ctx_a' "conclusion" "contexts", where)
        in
        let$ () = (expect_match c c' "conclusion" "prop", where) in
        let where = " in premise 3" in
        let$ ctx_b', c' = (Judgement.expect_Entail p3, where) in
        let$ () =
          (expect_match (b :: ctx) ctx_b' "conclusion" "contexts", where)
        in
        let$ () = (expect_match c c' "conclusion" "prop", where) in
        None
    | Imply_I ->
        let$ () = (expect_premises_len premises 1, "") in
        let p = List.nth premises 0 in
        let$ ctx, prop = (Judgement.expect_Entail conclusion, where) in
        let$ a, b = (Prop.expect_Imply prop, where) in
        let where = " in premise 1" in
        let$ ctx_a', b' = (Judgement.expect_Entail p, where) in
        let$ () =
          (expect_match (a :: ctx) ctx_a' "conclusion" "contexts", where)
        in
        let$ () = (expect_match b b' "conclusion" "prop", where) in
        None
    | Imply_E ->
        let$ () = (expect_premises_len premises 2, "") in
        let p1 = List.nth premises 0 in
        let p2 = List.nth premises 1 in
        let$ ctx, b = (Judgement.expect_Entail conclusion, where) in
        let where = " in premise 1" in
        let$ ctx', prop = (Judgement.expect_Entail p1, where) in
        let$ a, b' = (Prop.expect_Imply prop, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let$ () = (expect_match b b' "conclusion" "prop", where) in
        let where = " in premise 2" in
        let$ ctx', a' = (Judgement.expect_Entail p2, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        let$ () = (expect_match a a' "conclusion" "prop", where) in
        None
    | Truth_I ->
        let$ () = (expect_premises_len premises 0, "") in
        let$ _, prop = (Judgement.expect_Entail conclusion, where) in
        let$ () = (Prop.expect_Truth prop, where) in
        None
    | Falsity_E ->
        let$ () = (expect_premises_len premises 1, "") in
        let p = List.nth premises 0 in
        let$ ctx, _ = (Judgement.expect_Entail conclusion, where) in
        let where = " in premise 1" in
        let$ ctx', prop = (Judgement.expect_Entail p, where) in
        let$ () = (Prop.expect_Falsity prop, where) in
        let$ () = (expect_match ctx ctx' "conclusion" "contexts", where) in
        None
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
        Printf.sprintf "%s\n%s<%s>❌[%s]\n%s%s"
          (repr_list (indent + 2) derivations)
          indent_str (Rule.repr rule) msg indent_str (Judgement.repr judgement)

  and repr_list (indent : int) (ds : t list) : string =
    String.concat "" (List.map (fun d -> repr_helper indent d) ds)

  let repr = repr_helper 0

  let rec all_correct : t -> bool = function
    | Correct (_, _, ds) -> List.for_all all_correct ds
    | Incorrect _ -> false

  let rec mark (d : Derivation.t) : t =
    match d with
    | D (conclusion, rule, derivations) -> (
        let premises =
          List.map (function Derivation.D (j, _, _) -> j) derivations
        in
        let marked = (conclusion, rule, List.map mark derivations) in
        match Rule.verify conclusion rule premises with
        | None -> Correct marked
        | Some msg -> Incorrect (marked, msg))
end
