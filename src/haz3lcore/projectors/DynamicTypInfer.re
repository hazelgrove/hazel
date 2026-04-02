open Language;

/* Infer a type from a single sample value by running statics on it.
   Uses the provided context so user-defined types are visible. */
let type_of_sample = (~ctx: Ctx.t, sample: Sample.t): option(Typ.t) => {
  let info_map = Statics.mk(CoreSettings.on, ctx, sample.value);
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
