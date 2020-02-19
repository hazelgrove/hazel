type t = Doc.t(HTypAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let precedence_const = 0;
let precedence_Prod = 1;
let precedence_Sum = 2;
let precedence_Arrow = 3;
let precedence_ty = (ty: HTyp.t): int =>
  switch (ty) {
  | Num
  | Bool
  | Hole
  | Unit
  | List(_) => precedence_const
  | Prod(_, _) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

let pad_child =
    (
      ~inline_padding as (l, r)=(Doc.empty, Doc.empty),
      ~enforce_inline: bool,
      child: formattable_child,
    )
    : t => {
  let inline_choice = Doc.hcats([l, child(~enforce_inline=true), r]);
  let para_choice =
    Doc.hcats([
      Linebreak,
      Doc.indent_and_align(child(~enforce_inline=false)),
      Linebreak,
    ]);
  enforce_inline ? inline_choice : Doc.Choice(inline_choice, para_choice);
};

let mk_delim = s => Doc.Annot(HTypAnnot.Delim, Text(s));

let rec mk = (~parenthesize=false, ~enforce_inline: bool, ty: HTyp.t): t => {
  open Doc;
  let doc =
    switch (ty) {
    | Hole => Annot(HTypAnnot.Delim, Annot(HoleLabel, Text("?")))
    | Unit => Text("()")
    | Num => Text("Num")
    | Bool => Text("Bool")
    | List(ty) =>
      hcats([
        mk_delim("["),
        mk(ty) |> pad_child(~enforce_inline),
        mk_delim("]"),
      ])
    | Arrow(ty1, ty2) =>
      let d1 =
        mk(
          ~parenthesize=precedence_ty(ty1) >= precedence_Arrow,
          ~enforce_inline,
          ty1,
        );
      let d2 =
        mk(
          ~parenthesize=precedence_ty(ty2) > precedence_Arrow,
          ~enforce_inline,
          ty2,
        );
      hcats([
        d1,
        hcats([
          choices([Linebreak, space]),
          Text(UnicodeConstants.typeArrowSym ++ " "),
        ]),
        d2,
      ]);
    | Prod(ty1, ty2) =>
      let d1 =
        mk(
          ~parenthesize=precedence_ty(ty1) >= precedence_Prod,
          ~enforce_inline,
          ty1,
        );
      let d2 =
        mk(
          ~parenthesize=precedence_ty(ty2) > precedence_Prod,
          ~enforce_inline,
          ty2,
        );
      hcats([d1, hcats([Text(","), choices([Linebreak, space])]), d2]);
    | Sum(ty1, ty2) =>
      let d1 =
        mk(
          ~parenthesize=precedence_ty(ty1) >= precedence_Sum,
          ~enforce_inline,
          ty1,
        );
      let d2 =
        mk(
          ~parenthesize=precedence_ty(ty2) > precedence_Sum,
          ~enforce_inline,
          ty2,
        );
      hcats([d1, hcats([choices([Linebreak, space]), Text("| ")]), d2]);
    };
  parenthesize ? Doc.hcats([mk_delim("("), doc, mk_delim(")")]) : doc;
};
