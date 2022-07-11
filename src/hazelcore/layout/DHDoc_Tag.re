open Pretty;

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
  switch (tag) {
  | Tag(NotInTagHole, name) => name |> Doc.text
  | Tag(InTagHole(reason, u), name) =>
    name |> Doc.text |> Doc.annot(DHAnnot.NonEmptyTagHole(reason, u))
  | EmptyTagHole(u) =>
    Int.to_string(u + 1)
    |> Doc.text
    |> Doc.annot(DHAnnot.EmptyTagHole(selected(u), u))
    |> Doc.annot(DHAnnot.HoleLabel)
    |> Doc.annot(DHAnnot.Delim)
  };
};
