open Util;
open Haz3lcore;
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

let rec mk = (~parenthesize=false, ~enforce_inline: bool, ty: Typ.t): t => {
  open Doc;
  let mk' = mk(~enforce_inline);
  let mk_right_associative_operands = (precedence_op, ty1, ty2) => (
    annot(
      HTypAnnot.Step(0),
      mk'(~parenthesize=Typ.precedence(ty1) <= precedence_op, ty1),
    ),
    annot(
      HTypAnnot.Step(1),
      mk'(~parenthesize=Typ.precedence(ty2) < precedence_op, ty2),
    ),
  );
  let (doc, parenthesize) =
    switch (ty) {
    | Unknown(_) => (
        annot(HTypAnnot.Delim, annot(HTypAnnot.HoleLabel, text("?"))),
        parenthesize,
      )
    | Int => (text("Int"), parenthesize)
    | Float => (text("Float"), parenthesize)
    | Bool => (text("Bool"), parenthesize)
    | String => (text("String"), parenthesize)
    | Var(name) => (text(name), parenthesize)
    | List(ty) => (
        hcats([
          mk_delim("["),
          (
            (~enforce_inline) =>
              annot(HTypAnnot.Step(0), mk(~enforce_inline, ty))
          )
          |> pad_child(~enforce_inline),
          mk_delim("]"),
        ]),
        parenthesize,
      )
    | Arrow(ty1, ty2) =>
      let (d1, d2) =
        mk_right_associative_operands(Typ.precedence_Arrow, ty1, ty2);
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
              ~parenthesize=Typ.precedence(head) <= Typ.precedence_Prod,
              head,
            ),
          ),
          ...List.mapi(
               (i, ty) =>
                 annot(
                   HTypAnnot.Step(i + 1),
                   mk'(
                     ~parenthesize=Typ.precedence(ty) <= Typ.precedence_Prod,
                     ty,
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
        mk_right_associative_operands(Typ.precedence_Sum, ty1, ty2);
      (
        hcats([
          d1,
          hcats([choices([linebreak(), space()]), text("| ")]),
          d2,
        ]),
        parenthesize,
      );
    | LSum(_) =>
      //TODO(andrew)
      (text("LabelleSumTODO"), parenthesize)
    };
  let doc = annot(HTypAnnot.Term, doc);
  parenthesize ? Doc.hcats([mk_delim("("), doc, mk_delim(")")]) : doc;
};
