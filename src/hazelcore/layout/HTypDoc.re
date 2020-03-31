open Pretty;

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
  let mk_right_associative_operands = (ty, ty1, ty2) => (
    mk'(~parenthesize=HTyp.precedence(ty1) >= HTyp.precedence(ty), ty1),
    mk'(~parenthesize=HTyp.precedence(ty2) > HTyp.precedence(ty), ty2),
  );
  let doc =
    switch (ty) {
    | Hole => annot(HTypAnnot.Delim, annot(HTypAnnot.HoleLabel, text("?")))
    | Unit => text("()")
    | Num => text("Num")
    | Bool => text("Bool")
    | List(ty) =>
      hcats([
        mk_delim("["),
        mk(ty) |> pad_child(~enforce_inline),
        mk_delim("]"),
      ])
    | Arrow(ty1, ty2) =>
      let (d1, d2) = mk_right_associative_operands(ty, ty1, ty2);
      hcats([
        d1,
        hcats([
          choices([linebreak(), space()]),
          text(UnicodeConstants.typeArrowSym ++ " "),
        ]),
        d2,
      ]);
    | Prod([]) =>
      raise(Invalid_argument("Encountered tuple type with 0 elements!"))
    | Prod([head, ...tail]) =>
      [
        mk'(
          ~parenthesize=HTyp.precedence(head) >= HTyp.precedence_Prod,
          head,
        ),
        ...List.map(
             ty =>
               mk'(
                 ~parenthesize=HTyp.precedence(ty) > HTyp.precedence_Prod,
                 ty,
               ),
             tail,
           ),
      ]
      |> ListUtil.join(
           hcats([text(","), choices([linebreak(), space()])]),
         )
      |> hcats
    | Sum(ty1, ty2) =>
      let (d1, d2) = mk_right_associative_operands(ty, ty1, ty2);
      hcats([
        d1,
        hcats([choices([linebreak(), space()]), text("| ")]),
        d2,
      ]);
    };
  parenthesize ? Doc.hcats([mk_delim("("), doc, mk_delim(")")]) : doc;
};
