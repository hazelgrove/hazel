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
  let ok = ref(true);
  let _ =
    Typ.map_term(
      ~f_typ=
        (cont, ty) => {
          if (List.length(ty.annotation.ids) < PadIds.necessary_ids(ty)) {
            ok := false;
          };
          cont(ty);
        },
      typ,
    );
  ok^;
};

let typ_to_segment =
    (~settings: ExpToSegment.Settings.t, typ: Typ.t): Base.segment =>
  prepare(~settings, typ) |> to_segment;

/* A `proof_of e end` is coloured as a unit. Typ.diff reports the ids of the
   whole quoted expression, including annotations the printer never shows,
   while the expression's printed tiles carry ids the type never had. So drop
   the quoted expression's unprinted ids, and colour every token printed
   inside a coloured `proof_of`. Any other id must still be printed, as the
   soundness property checks. */
let colour_proof_of = (typ: Typ.t, seg: Base.segment, ids: Id.Set.t): Id.Set.t => {
  let emitted = Segment.ids(seg) |> Id.Set.of_list;
  let quoted = ref(Id.Set.empty);
  let _ =
    Typ.map_term(
      ~f_typ=
        (cont, ty) => {
          switch (Typ.term_of(ty)) {
          | ProofOf(_) =>
            let own = IdTagged.ids(ty) |> Id.Set.of_list;
            quoted :=
              Id.Set.union(
                quoted^,
                Id.Set.diff(Typ.all_ids(ty) |> Id.Set.of_list, own),
              );
          | _ => ()
          };
          cont(ty);
        },
      typ,
    );
  let ids = Id.Set.diff(ids, Id.Set.diff(quoted^, emitted));
  let rec under = (acc, seg: Base.segment) =>
    List.fold_left(
      (acc, p: Base.piece) =>
        switch (p) {
        | Tile({form: Compound(ProofOf), id, children, _})
            when Id.Set.mem(id, ids) =>
          List.fold_left(
            (acc, kid) =>
              List.fold_left(
                (acc, id) => Id.Set.add(id, acc),
                acc,
                Segment.ids(kid),
              ),
            acc,
            children,
          )
        | Tile({children, _}) => List.fold_left(under, acc, children)
        | _ => acc
        },
      acc,
      seg,
    );
  under(ids, seg);
};

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
  let seg = to_segment(prepared);
  (
    seg,
    colour_proof_of(prepared.typ, seg, diff_ids(~ctx?, ~against, prepared)),
  );
};
