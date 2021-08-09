module Doc = Pretty.Doc;

type t = Doc.t(HTypAnnot.t);

let mk = (tag: UHTag.t): t => {
  Doc.(
    switch (tag) {
    | TagHole(_) =>
      annot(HTypAnnot.Delim, annot(HTypAnnot.HoleLabel, text("?")))
    | Tag(t) => text(t)
    }
  );
};
