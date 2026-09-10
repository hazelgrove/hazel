open Language;

/* Infer a type from a single sample value by running statics on it.
   Uses the provided context so user-defined types are visible.

   The value is closed first: a function value is a `Closure(env, body)`, and
   statics discards a closure's env, so the body's free variables would
   otherwise resolve against `ctx` and capture whatever same-named binder is
   in scope where the sample was taken. Substituting each closure's own env
   leaves nothing free to capture. Note this mints fresh ids, so the lookup
   below must use the substituted expression's rep_id, not the sample's. */
let type_of_sample = (~ctx: Ctx.t, sample: Sample.t): option(Typ.t) => {
  let exp = Substitution.in_exp(Environment.empty, sample.value);
  let (info_map, _elab) = Statics.mk(CoreSettings.on, ctx, exp);
  IdTagged.rep_id(exp)
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

/* The segment to show for a runtime-refined type, and the ids of its tokens
     that came from runtime rather than from statics.
   *
   * Both types are normalized first -- ExpToSegment.normalize_typ, the same
   * pass the renderer applies -- and the diff is taken between the normalized
   * forms. That is the whole trick. Defensive parenthesization inserts real
   * Parens NODES with fresh ids, and padding adds id slots; both only exist
   * after normalization, and both end up carrying tokens. Diffing the types as
   * the caller passed them cannot name those ids, which is why parentheses
   * used to render in the static colour inside a wholly runtime-derived type.
   *
   * The normalized dynamic type is rendered once and returned: normalize_typ is
   * not idempotent (it mints a fresh id per added Parens), so re-normalizing or
   * re-rendering would produce a segment the marks do not describe. */
let segment_and_marks =
    /* normalize/render_normalized are injected rather than called directly:
       ExpToSegment sits downstream of the projectors, so this module cannot
       name it. Pass utility.normalize_typ and utility.render_normalized_typ,
       which are built from one settings value so both halves agree. */
    (
      ~normalize: Typ.t => Typ.t,
      ~render_normalized: Typ.t => Base.segment,
      ~ctx: option(Ctx.t),
      ~static_typ: Typ.t,
      ~dynamic_typ: Typ.t,
    )
    : (Base.segment, Id.Set.t) => {
  let static_n = normalize(static_typ);
  /* Statics builds types with Typ.temp, so every node of one carries the
     Id.invalid sentinel rather than a distinct id -- true both of a type
     inferred from samples and of a live-typing elab_syn_ty. Left in, the
     sentinel collapses the marks: the renderer freshens duplicate tile ids,
     so every token but the first ends up in no type and unmarkable, and
     `diff`'s wrapped_replaced test fires on any node sharing the sentinel
     with a replaced one. Replaced here because this is where a type's ids
     become the ids of rendered tokens. */
  let dynamic_n = normalize(Typ.replace_temp(dynamic_typ));
  let marks = Typ.diff(~ctx?, static_n, dynamic_n) |> Id.Set.of_list;
  (render_normalized(dynamic_n), marks);
};

/* segment_and_marks for the type probe's Dynamic mode, where the dynamic type
   is inferred from the samples rather than supplied. */
let displayed_segment_and_marks =
    (
      ~normalize: Typ.t => Typ.t,
      ~render_normalized: Typ.t => Base.segment,
      ~ctx: Ctx.t,
      ~static_typ: Typ.t,
      ~samples: list(Sample.t),
    )
    : (Base.segment, Id.Set.t) =>
  segment_and_marks(
    ~normalize,
    ~render_normalized,
    ~ctx=Some(ctx),
    ~static_typ,
    ~dynamic_typ=dynamic_typ_of_samples_or_unknown(~ctx, samples),
  );
