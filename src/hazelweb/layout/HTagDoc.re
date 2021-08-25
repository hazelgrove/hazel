module Doc = Pretty.Doc;

type t = Doc.t(HTypAnnot.t);

let mk = (tag: UHTag.t): t => {
  Doc.(
    switch (tag) {
    | TagHole(u) =>
      annot(
        HTypAnnot.Delim,
        annot(HTypAnnot.HoleLabel, DHDoc_common.mk_TagHole(u)),
      )

    | Tag(t) => text(t)
    }
  );
};
