module Doc = Pretty.Doc;

type t = Doc.t(KindAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let pad_child =
    (
      ~inline_padding as (l, r)=(Doc.empty(), Doc.empty()),
      ~enforce_inline: bool,
      child: formattable_child,
    )
    : t => {
  let inline_choice = Doc.hcats([l, child(~enforce_inline=true), r]);
  let para_choice =
    Doc.(
      hcats([
        linebreak(),
        indent_and_align(child(~enforce_inline=false)),
        linebreak(),
      ])
    );
  enforce_inline ? inline_choice : Doc.choice(inline_choice, para_choice);
};

let mk_delim = s => Doc.(annot(KindAnnot.Delim, text(s)));

let rec mk = (~parenthesize=false, ~enforce_inline: bool, k: Kind.t): t => {
  open Doc;
  let (doc, parenthesize) =
    switch (k) {
    | Hole => (
        annot(KindAnnot.Delim, annot(KindAnnot.HoleLabel, text("?"))),
        parenthesize,
      )
    | Type => (text("Type"), parenthesize)
    | S(_ty) => (
        hcats([
          mk_delim("S("),
          /* TODO: (eric) show singleton kind's type */
          (
            (~enforce_inline) =>
              annot(KindAnnot.Step(0), mk(~enforce_inline, Hole))
          )
          |> pad_child(~enforce_inline),
          mk_delim(")"),
        ]),
        parenthesize,
      )
    };
  let doc = annot(KindAnnot.Term, doc);
  parenthesize ? Doc.hcats([mk_delim("("), doc, mk_delim(")")]) : doc;
};
