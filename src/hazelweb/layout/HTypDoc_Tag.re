module Doc = Pretty.Doc;

type t = Doc.t(HTypAnnot.t);

let mk = (tag: UHTag.t): t => {
  Doc.(
    switch (tag) {
    | TagHole(u) =>
      Int.to_string(u + 1)
      |> Doc.text
      |> Doc.annot(HTypAnnot.HoleLabel)
      |> Doc.annot(HTypAnnot.Delim)

    | Tag(t) => text(t)
    }
  );
};
