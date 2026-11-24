open Language;

type t = {
  typ: Typ.t,
  settings: ExpToSegment.Settings.t,
};

let prepare = (~settings: ExpToSegment.Settings.t, typ: Typ.t) => {
  typ:
    typ
    |> Typ.desugar_sig(Ctx.empty)
    |> ExpToSegment.parenthesize_typ(
         ~parenthesization=settings.parenthesization,
         ~show_filters=settings.show_filters,
         ~show_ascriptions=settings.show_ascriptions,
       )
    |> PadIds.pad_typ_ids
    |> ExpToSegment.uniquify_typ_ids,
  settings,
};

let to_segment = ({typ, settings}) =>
  ExpToSegment.typ_to_pretty(~settings, typ)
  |> PrettySegment.select
  |> ExpToSegment.strip_if_incomplete(Typ(typ))
  |> ExpToSegment.uniquify_repeated_tiles;

let diff_ids = (~ctx=?, ~against: t, t: t) =>
  Typ.diff(~ctx?, against.typ, t.typ) |> Id.Set.of_list;

let ids_sufficient = (~settings: ExpToSegment.Settings.t, typ: Typ.t) => {
  let {typ, _} = prepare(~settings, typ);
  Any.subterms(Typ(typ))
  |> List.filter_map(~f=Any.is_typ)
  |> List.for_all(~f=(ty: Typ.t) =>
       List.length(ty.annotation.ids) >= PadIds.necessary_ids(ty)
     );
};

let typ_to_segment =
    (~settings: ExpToSegment.Settings.t, typ: Typ.t): Base.segment =>
  prepare(~settings, typ) |> to_segment;

let typ_to_segment_with_diff_ids =
    (
      ~settings: ExpToSegment.Settings.t,
      ~ctx: option(Ctx.t)=?,
      ~against: Typ.t,
      typ: Typ.t,
    )
    : (Base.segment, Id.Set.t) => {
  let prepared = prepare(~settings, typ);
  let against = prepare(~settings, against);
  (to_segment(prepared), diff_ids(~ctx?, ~against, prepared));
};
