open Language;

/* Infer a type from a single sample value by running statics on it.
   Uses the provided context so user-defined types are visible, but with
   use_mode cleared: a sample is an already-elaborated value, and leaving the
   source's mode set re-runs Operators.replace_literal over it, so an Int
   sample inside `use Nat` would be reported as Nat.

   The value is closed first: a function value is a `Closure(env, body)`, and
   statics discards a closure's env, so the body's free variables would
   otherwise resolve against `ctx` and capture whatever same-named binder is
   in scope where the sample was taken. Substituting each closure's own env
   leaves nothing free to capture. Substitution mints fresh ids, so the lookup
   uses the substituted expression's rep_id, not the sample's. */
let type_of_sample = (~ctx: Ctx.t, sample: Sample.t): option(Typ.t) => {
  let ctx = Ctx.set_use_mode(ctx, None);
  let exp = Substitution.in_exp(Environment.empty, sample.value);
  let (info_map, _elab) = Statics.mk(CoreSettings.on, ctx, exp);
  IdTagged.rep_id(exp)
  |> Statics.Map.lookup(_, info_map)
  |> Option.bind(
       _,
       fun
       | Info.InfoExp(e) => Some(e.ty)
       | _ => None,
     );
};

let dynamic_typ_of_samples =
    (~ctx: Ctx.t, samples: list(Sample.t)): option(Typ.t) =>
  switch (samples) {
  /* Nothing was observed. Meeting no types would report Unknown, which reads
     as runtime having found the type to be `?`. */
  | [] => None
  | _ =>
    Option.bind(
      List.map(type_of_sample(~ctx), samples) |> Util.OptUtil.sequence,
      Typ.meet_all(~empty=Typ.fresh(Unknown(Internal)), ctx),
    )
  };

type typ_to_seg_with_diff_ids =
  (~ctx: Ctx.t, ~against: Typ.t, Typ.t) => (Base.segment, Id.Set.t);

let segment_and_dynamic_ids =
    (
      ~typ_to_seg_with_diff_ids: typ_to_seg_with_diff_ids,
      ~ctx: Ctx.t,
      ~static_typ: Typ.t,
      ~dynamic_typ: Typ.t,
    )
    : (Base.segment, Id.Set.t) =>
  /* Statics builds types with Typ.temp, so every node shares the Id.invalid
     sentinel. Distinct ids are a precondition of naming printed tokens, so
     they are minted here, where the type becomes something to print. */
  typ_to_seg_with_diff_ids(
    ~ctx,
    ~against=static_typ,
    Typ.replace_temp(dynamic_typ),
  );
