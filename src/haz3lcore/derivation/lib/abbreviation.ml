open Derivation

let strip_fail () = failwith "Unexpected Abbr"

module Abbr = struct
  type 'a t = Abbr of string | Just of 'a

  let repr a_repr = function Abbr s -> s | Just a -> a_repr a
  
  let check a_check = function Abbr _ -> false | Just a -> a_check a

  let strip a_strip = function Abbr _ -> strip_fail () | Just a -> a_strip a
end

type 'a abbr = 'a Abbr.t

module PropAbbr = struct
  type t = t' abbr

  and t' =
    | Atom of string
    | And of t * t
    | Or of t * t
    | Imply of t * t
    | Truth
    | Falsity

  let rec repr (a : t) : string =
    match a with
    | Abbr.Abbr s -> s
    | Just a -> (
        match a with
        | Atom s -> s
        | And (a, b) -> Printf.sprintf "(%s ∧ %s)" (repr a) (repr b)
        | Or (a, b) -> Printf.sprintf "(%s ∨ %s)" (repr a) (repr b)
        | Imply (a, b) -> Printf.sprintf "(%s ⊃ %s)" (repr a) (repr b)
        | Truth -> "⊤"
        | Falsity -> "⊥")

  let rec check = 
    function
    | Abbr.Abbr _ -> false
    | Just a -> 
        match a with
        | Atom _ -> true
        | And (a, b) -> check a && check b
        | Or (a, b) -> check a && check b
        | Imply (a, b) -> check a && check b
        | Truth -> true
        | Falsity -> true

  let rec strip = function
    | Abbr.Abbr _ -> strip_fail ()
    | Just a -> 
      match a with
      | Atom s -> Prop.Atom s
      | And (a, b) -> And (strip a, strip b)
      | Or (a, b) -> Or (strip a, strip b)
      | Imply (a, b) -> Imply (strip a, strip b)
      | Truth -> Truth
      | Falsity -> Falsity
end

module CtxAbbr = struct
  type t = PropAbbr.t list abbr

  let repr =
    Abbr.repr (function ctx -> String.concat ", " (List.map PropAbbr.repr ctx))

  let check = Abbr.check (List.for_all PropAbbr.check)
  let strip = Abbr.strip (List.map PropAbbr.strip)
end

module JudgementAbbr = struct
  type t = t' abbr
  and t' = Entail of CtxAbbr.t * PropAbbr.t

  let repr =
    Abbr.repr (function Entail (ctx, prop) ->
        Printf.sprintf "%s ⊢ %s" (CtxAbbr.repr ctx) (PropAbbr.repr prop))

  let check = Abbr.check (function Entail (ctx, prop) -> CtxAbbr.check ctx && PropAbbr.check prop)

  let strip = Abbr.strip (function Entail (ctx, prop) -> Judgement.Entail (CtxAbbr.strip ctx, PropAbbr.strip prop))
end

module DerivationAbbr = struct
  type t = t' abbr
  and t' = D of d
  and d = JudgementAbbr.t * Rule.t * t list

  let rec repr_helper (indent : int) =
    function 
    | Abbr.Abbr s -> 
      let indent_str = String.make indent ' ' in
      "\n" ^ indent_str ^ s
    | Just d ->
        let indent_str = String.make indent ' ' in
        match d with
        | D (judgement, rule, derivations) ->
            Printf.sprintf "%s\n%s<%s>\n%s%s"
              (repr_list (indent + 2) derivations)
              indent_str (Rule.repr rule) indent_str
              (JudgementAbbr.repr judgement)

  and repr_list (indent : int) (ds : t list) : string =
    String.concat "" (List.map (fun d -> repr_helper indent d) ds)

  let repr = repr_helper 0
(* 
  let rec check = 
    function
    | Abbr.Abbr _ -> false
    | Just D (judgement, _, derivations) ->
        JudgementAbbr.check judgement && List.for_all check derivations

  let rec strip = function
    | Abbr.Abbr _ -> strip_fail ()
    | Just D (judgement, rule, derivations) ->
        Derivation.D (JudgementAbbr.strip judgement, rule, List.map strip derivations) *)
end

module MarkedDerivationAbbr = struct
  type t = t' abbr
  and t' = Correct of d | Incorrect of d * string
  and d = JudgementAbbr.t * Rule.t * t list

  let rec repr_helper (indent : int) =
    function 
    | Abbr.Abbr s -> 
      let indent_str = String.make indent ' ' in
      "\n" ^ indent_str ^ s
    | Just d ->
        let indent_str = String.make indent ' ' in
        match d with
        | Correct (judgement, rule, derivations) ->
            Printf.sprintf "%s\n%s<%s>✅\n%s%s"
              (repr_list (indent + 2) derivations)
              indent_str (Rule.repr rule) indent_str
              (JudgementAbbr.repr judgement)
        | Incorrect ((judgement, rule, derivations), msg) ->
            Printf.sprintf "%s\n%s<%s>❌[%s]\n%s%s"
              (repr_list (indent + 2) derivations)
              indent_str (Rule.repr rule) msg indent_str
              (JudgementAbbr.repr judgement)

  and repr_list (indent : int) (ds : t list) : string =
    String.concat "" (List.map (fun d -> repr_helper indent d) ds)

  let repr = repr_helper 0
end

module Bind = struct
  type ('a, 'b) t = Bind of string * 'b * ('a, 'b) t | Just of 'a

  let rec repr repr_a repr_b = function
    | Bind (s, b, t) ->
        Printf.sprintf "Let %s =\n%s\nin\n%s" s (repr_b b)
          (repr repr_a repr_b t)
    | Just a -> repr_a a
end

module DerivationBind = struct
  type t = (DerivationAbbr.t, bindable) Bind.t

  and bindable =
    | Prop of PropAbbr.t
    | Ctx of CtxAbbr.t
    | Judgement of JudgementAbbr.t
    | Derivation of DerivationAbbr.t

  let repr =
    Bind.repr DerivationAbbr.repr (function
      | Prop p -> PropAbbr.repr p
      | Ctx ctx -> CtxAbbr.repr ctx
      | Judgement j -> JudgementAbbr.repr j
      | Derivation d -> DerivationAbbr.repr d)
end

module MarkedDerivationBind = struct
  type t = (MarkedDerivationAbbr.t, bindable) Bind.t
  and bindable =
    | Prop of PropAbbr.t
    | Ctx of CtxAbbr.t
    | Judgement of JudgementAbbr.t
    | MarkedDerivation of MarkedDerivationAbbr.t
  
  let repr =
    Bind.repr MarkedDerivationAbbr.repr (function
      | Prop p -> PropAbbr.repr p
      | Ctx ctx -> CtxAbbr.repr ctx
      | Judgement j -> JudgementAbbr.repr j
      | MarkedDerivation d -> MarkedDerivationAbbr.repr d)

  type resolved_bindable =
    | Prop of Prop.t
    | Ctx of Prop.t list
    | Judgement of Judgement.t
    | Derivation of Derivation.t
  and pair = string * resolved_bindable

  let rec mark_helper (d : DerivationBind.t) (ctx: pair list) : t =
    match d with
    | Bind.Just d -> Bind.Just (MarkedDerivationAbbr.Abbr (MarkedDerivationAbbr.strip (DerivationAbbr.strip d)))
    | Bind.Bind (s, b, t) -> 
        match b with
        | Prop p -> mark_helper t ( (s, Prop (PropAbbr.strip p)) :: ctx)
        | Ctx c -> mark_helper t ctx
        | Judgement j -> mark_helper t ctx
        | Derivation d -> 
            let ctx = (s, DerivationAbbr.strip d) :: ctx in
            Bind.Bind (s, MarkedDerivationAbbr.Abbr (MarkedDerivationAbbr.strip (DerivationAbbr.strip d)), mark_helper t ctx)
end

