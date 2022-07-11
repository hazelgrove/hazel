module Doc = Pretty.Doc;

type t = Doc.t(HTypAnnot.t);

let mk_EmptyTagHole = (u: MetaVar.t): t =>
  Int.to_string(u + 1)
  |> Doc.text
  |> Doc.annot(HTypAnnot.EmptyTagHole(u))
  |> Doc.annot(HTypAnnot.HoleLabel)
  |> Doc.annot(HTypAnnot.Delim);

let mk_Tag = (t: string): t => Doc.(text(t) |> annot(HTypAnnot.Delim));

let mk = (tag: UHTag.t): t => {
  switch (tag) {
  | EmptyTagHole(u) => mk_EmptyTagHole(u)
  | Tag(_, t) => mk_Tag(t)
  };
};
