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
  |> Id.Map.find_opt(_, info_map)
  |> Option.bind(
       _,
       fun
       | InfoExp(e) => Some(e.ty)
       | _ => None,
     );
};

/* Compute the dynamic type from a list of samples by inferring each
   sample's type and meeting them all. Returns None if any sample
   fails to type-check or if the types are inconsistent. */
let dynamic_typ_of_samples =
    (~ctx: Ctx.t, samples: list(Sample.t)): option(Typ.t) => {
  let types =
    List.map(type_of_sample(~ctx), samples) |> Util.OptUtil.sequence;
  Option.bind(
    types,
    Typ.meet_all(~empty=Typ.fresh(Unknown(Internal)), ctx),
  );
};

/* Like dynamic_typ_of_samples, but defaults to Unknown(Internal) on failure. */
let dynamic_typ_of_samples_or_unknown =
    (~ctx: Ctx.t, samples: list(Sample.t)): Typ.t =>
  dynamic_typ_of_samples(~ctx, samples)
  |> Option.value(~default=Typ.fresh(Unknown(Internal)));

/* Rendering a type and naming the tokens of it that some other type does not
   account for: ProjectorBase.utility.typ_to_seg_with_diff_ids, injected rather
   than called directly because the renderer sits downstream of the
   projectors and this module cannot name it. */
type render_with_diff_ids =
  (~ctx: Ctx.t, ~against: Typ.t, Typ.t) => (Base.segment, Id.Set.t);

/* The segment to show in Dynamic mode, and the ids of its tokens that came
   from runtime rather than from statics. */
let displayed_segment_and_dynamic_ids =
    (
      ~render_with_diff_ids: render_with_diff_ids,
      ~ctx: Ctx.t,
      ~static_typ: Typ.t,
      ~samples: list(Sample.t),
    )
    : (Base.segment, Id.Set.t) => {
  let dynamic_typ = dynamic_typ_of_samples_or_unknown(~ctx, samples);
  /* Statics builds types with Typ.temp, so every node of one carries the
     Id.invalid sentinel rather than a distinct id. Left in, the sentinel
     collapses the dynamic ids: the renderer freshens duplicate tile ids, so
     every token but the first ends up in no type and so cannot be coloured,
     and `diff`'s wrapped_replaced test fires on any node sharing the sentinel
     with a replaced one. */
  render_with_diff_ids(
    ~ctx,
    ~against=static_typ,
    Typ.replace_temp(dynamic_typ),
  );
};
