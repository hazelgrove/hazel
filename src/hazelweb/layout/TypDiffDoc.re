module Doc = Pretty.Doc;

type t = Doc.t(TypDiffAnnot.t);

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

let mk_delim = s =>
  Doc.(annot(TypDiffAnnot.TypAnnot(HTypAnnot.Delim), text(s)));

let rec mk = (~parenthesize=false, ~enforce_inline: bool, diff: TypDiff.t): t => {
  open Doc;
  let mk' = mk(~enforce_inline);
  let mk_right_associative_operands = (precedence_op, diff1, diff2) => (
    mk'(~parenthesize=TypDiff.precedence(diff1) <= precedence_op, diff1),
    mk'(~parenthesize=TypDiff.precedence(diff2) < precedence_op, diff2),
  );
  let (doc, parenthesize) =
    switch (diff) {
    | Hole(highlight_diff) =>
      let a =
        annot(
          TypDiffAnnot.TypAnnot(HTypAnnot.Delim),
          annot(TypDiffAnnot.TypAnnot(HTypAnnot.HoleLabel), text("?")),
        );
      highlight_diff
        ? (annot(TypDiffAnnot.Diff, a), parenthesize) : (a, parenthesize);
    | Int(highlight_diff) =>
      let t = text("Int");
      highlight_diff
        ? (annot(TypDiffAnnot.Diff, t), parenthesize) : (t, parenthesize);
    | Float(highlight_diff) =>
      let t = text("Float");
      highlight_diff
        ? (annot(TypDiffAnnot.Diff, t), parenthesize) : (t, parenthesize);
    | Bool(highlight_diff) =>
      let t = text("Bool");
      highlight_diff
        ? (annot(TypDiffAnnot.Diff, t), parenthesize) : (t, parenthesize);
    | List(diff, highlight_diff) =>
      let h =
        hcats([
          mk_delim("["),
          mk(diff) |> pad_child(~enforce_inline),
          mk_delim("]"),
        ]);
      highlight_diff
        ? (annot(TypDiffAnnot.Diff, h), parenthesize) : (h, parenthesize);
    | Arrow(diff1, diff2, highlight_diff) =>
      let (d1, d2) =
        mk_right_associative_operands(HTyp.precedence_Arrow, diff1, diff2);
      let h =
        hcats([
          d1,
          hcats([
            choices([linebreak(), space()]),
            text(Unicode.typeArrowSym ++ " "),
          ]),
          d2,
        ]);
      highlight_diff
        ? (annot(TypDiffAnnot.Diff, h), parenthesize) : (h, parenthesize);
    | Prod([], highlight_diff) =>
      let t = text("()");
      highlight_diff
        ? (annot(TypDiffAnnot.Diff, t), parenthesize) : (t, parenthesize);
    | Prod([head, ...tail], highlight_diff) =>
      let h =
        [
          mk'(
            ~parenthesize=TypDiff.precedence(head) <= HTyp.precedence_Prod,
            head,
          ),
          ...List.map(
               diff =>
                 mk'(
                   ~parenthesize=
                     TypDiff.precedence(diff) <= HTyp.precedence_Prod,
                   diff,
                 ),
               tail,
             ),
        ]
        |> ListUtil.join(
             hcats([text(","), choices([linebreak(), space()])]),
           )
        |> hcats;
      highlight_diff ? (annot(TypDiffAnnot.Diff, h), true) : (h, true);
    | Sum(ty1, ty2, highlight_diff) =>
      let (d1, d2) =
        mk_right_associative_operands(HTyp.precedence_Sum, ty1, ty2);
      let h =
        hcats([
          d1,
          hcats([choices([linebreak(), space()]), text("| ")]),
          d2,
        ]);
      highlight_diff
        ? (annot(TypDiffAnnot.Diff, h), parenthesize) : (h, parenthesize);
    };
  parenthesize ? Doc.hcats([mk_delim("("), doc, mk_delim(")")]) : doc;
};
