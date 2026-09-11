open Language;

/* Infer a type from a single sample value by running statics on it.
   Uses the provided context so user-defined types are visible, but with
   use_mode cleared: a sample is an already-elaborated value, and leaving the
   source's mode set re-runs Operators.replace_literal over it, so an Int
   sample inside `use Nat` would be reported as Nat. */
let type_of_sample = (~ctx: Ctx.t, sample: Sample.t): option(Typ.t) => {
  let ctx = Ctx.set_use_mode(ctx, None);
  let (info_map, _elab) = Statics.mk(CoreSettings.on, ctx, sample.value);
  IdTagged.rep_id(sample.value)
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

type render_with_diff_ids =
  (~ctx: Ctx.t, ~against: Typ.t, Typ.t) => (Base.segment, Id.Set.t);

let displayed_segment_and_dynamic_ids =
    (
      ~render_with_diff_ids: render_with_diff_ids,
      ~ctx: Ctx.t,
      ~static_typ: Typ.t,
      ~samples: list(Sample.t),
    )
    : (Base.segment, Id.Set.t) => {
  /* With nothing to infer from, the static type stands in: it diffs against
     itself, so nothing is marked. */
  let displayed =
    dynamic_typ_of_samples(~ctx, samples)
    |> Option.value(~default=static_typ);
  /* Statics builds types with Typ.temp, so every node shares the Id.invalid
     sentinel. Distinct ids are a precondition of naming rendered tokens, so
     they are minted here, where the type becomes something to render. */
  render_with_diff_ids(
    ~ctx,
    ~against=static_typ,
    Typ.replace_temp(displayed),
  );
};
