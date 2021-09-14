open Pretty;

let promote_annot =
    (selected: MetaVar.t => bool, annot: HTypAnnot.t): DHAnnot.t =>
  switch (annot) {
  | EmptyTagHole(u) => EmptyTagHole(selected(u), u)
  | HoleLabel => HoleLabel
  | Delim => Delim
  };

let promote = (selected: MetaVar.t => bool, d: HTypDoc.t): DHDoc.t =>
  d |> Doc.map_annot(promote_annot(selected));

let mk =
    (
      ~enforce_inline as _: bool,
      ~selected_tag_hole: option(MetaVar.t),
      tag: UHTag.t,
    )
    : DHDoc.t => {
  let selected = (u: MetaVar.t): bool =>
    selected_tag_hole
    |> Option.map(MetaVar.eq(u))
    |> Option.value(~default=false);
  tag |> HTypDoc_Tag.mk |> promote(selected);
};
