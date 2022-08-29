module Doc = Pretty.Doc;

type t = Doc.t(HTypAnnot.t);

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

let mk_delim = s => Doc.(annot(HTypAnnot.Delim, text(s)));

let rec mk = (~parenthesize=false, ~enforce_inline: bool, ty: HTyp.t): t => {
  open Doc;
  let mk' = mk(~enforce_inline);
  let mk_right_associative_operands = (precedence_op, ty1, ty2) => (
    annot(
      HTypAnnot.Step(0),
      mk'(~parenthesize=HTyp.precedence(ty1) <= precedence_op, ty1),
    ),
    annot(
      HTypAnnot.Step(1),
      mk'(~parenthesize=HTyp.precedence(ty2) < precedence_op, ty2),
    ),
  );
  let (doc, parenthesize) =
    switch (HTyp.to_syntax(ty)) {
    | Hole => (
        annot(HTypAnnot.Delim, annot(HTypAnnot.HoleLabel, text("?"))),
        parenthesize,
      )
    | TyVar(_, t) => (text(t), parenthesize)
    | TyVarHole(_, _, t)
    | InvalidText(_, t) => (
        annot(HTypAnnot.TyVarHole, text(t)),
        parenthesize,
      )
    | Int => (text("Int"), parenthesize)
    | Float => (text("Float"), parenthesize)
    | Bool => (text("Bool"), parenthesize)
    | List(ty) => (
        hcats([
          mk_delim("["),
          (
            (~enforce_inline) =>
              annot(
                HTypAnnot.Step(0),
                mk(~enforce_inline, HTyp.of_syntax(ty)),
              )
          )
          |> pad_child(~enforce_inline),
          mk_delim("]"),
        ]),
        parenthesize,
      )
    | Arrow(ty1, ty2) =>
      let (d1, d2) =
        mk_right_associative_operands(
          HTyp.precedence_Arrow(),
          HTyp.of_syntax(ty1),
          HTyp.of_syntax(ty2),
        );
      (
        hcats([
          d1,
          hcats([
            choices([linebreak(), space()]),
            text(Unicode.typeArrowSym ++ " "),
          ]),
          d2,
        ]),
        parenthesize,
      );
    | Prod([]) => (text("()"), parenthesize)
    | Prod([head, ...tail]) =>
      let center =
        [
          annot(
            HTypAnnot.Step(0),
            mk'(
              ~parenthesize=
                HTyp.precedence(HTyp.of_syntax(head))
                <= HTyp.precedence_Prod(),
              HTyp.of_syntax(head),
            ),
          ),
          ...List.mapi(
               (i, ty) =>
                 annot(
                   HTypAnnot.Step(i + 1),
                   mk'(
                     ~parenthesize=
                       HTyp.precedence(HTyp.of_syntax(ty))
                       <= HTyp.precedence_Prod(),
                     HTyp.of_syntax(ty),
                   ),
                 ),
               tail,
             ),
        ]
        |> ListUtil.join(
             hcats([text(","), choices([linebreak(), space()])]),
           )
        |> hcats;
      (center, true);
    | Sum(ty1, ty2) =>
      let (d1, d2) =
        mk_right_associative_operands(
          HTyp.precedence_Sum(),
          HTyp.of_syntax(ty1),
          HTyp.of_syntax(ty2),
        );
      (
        hcats([
          d1,
          hcats([choices([linebreak(), space()]), text("| ")]),
          d2,
        ]),
        parenthesize,
      );
    | Forall(tp, ty_body) => (
        hcats([
          mk_delim("forall"),
          annot(
            HTypAnnot.Step(0),
            // TODO: (poly) copied from DHDoc_TPat
            // changed all DHAnnot to HTypAnnot
            switch (tp) {
            | TPat.EmptyHole =>
              annot(HTypAnnot.Delim, annot(HTypAnnot.HoleLabel, text("?")))
            | TPat.TyVar(NotInHole, name) => text(name)
            | TPat.TyVar(InHole(_reason, _u), name) =>
              annot(HTypAnnot.TyVarHole, text(name))
            },
          ),
          hcats([
            choices([linebreak(), space()]),
            text(Unicode.typeArrowSym ++ " "),
          ]),
          annot(HTypAnnot.Step(1), mk'(HTyp.of_syntax(ty_body))),
        ]),
        parenthesize,
      )
    };
  let doc = annot(HTypAnnot.Term, doc);
  parenthesize ? Doc.hcats([mk_delim("("), doc, mk_delim(")")]) : doc;
};
