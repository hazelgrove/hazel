open DerivationType

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
