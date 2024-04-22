open Util;
open Haz3lcore;
module Doc = Pretty.Doc;

type t = Doc.t(HTypAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let precedence_Prod = 1;
let precedence_Arrow = 2;
let precedence_Sum = 3;
let precedence_Ap = 4;
let precedence_Const = 5;

let precedence = (ty: Typ.t): int =>
  switch (Typ.term_of(ty)) {
  | Int
  | Float
  | Bool
  | String
  | Unknown(_)
  | Var(_)
  | Forall(_)
  | Rec(_)
  | Sum(_) => precedence_Sum
  | List(_) => precedence_Const
  | Prod(_) => precedence_Prod
  | Arrow(_, _) => precedence_Arrow
  | Parens(_) => precedence_Const
  | Ap(_) => precedence_Ap
  };

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
        indent_and_align(child(~enforce_inline)),
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
      mk'(~parenthesize=precedence(ty1) <= precedence_op, ty1),
    ),
    annot(
      HTypAnnot.Step(1),
      mk'(~parenthesize=precedence(ty2) < precedence_op, ty2),
    ),
  );
  let (doc, parenthesize) =
    switch (Typ.term_of(ty)) {
    | Parens(ty) => (mk(~parenthesize=true, ~enforce_inline, ty), false)
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
        mk_right_associative_operands(precedence_Arrow, ty1, ty2);
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
            mk'(~parenthesize=precedence(head) <= precedence_Prod, head),
          ),
          ...List.mapi(
               (i, ty) =>
                 annot(
                   HTypAnnot.Step(i + 1),
                   mk'(~parenthesize=precedence(ty) <= precedence_Prod, ty),
                 ),
               tail,
             ),
        ]
        |> ListUtil.join(
             hcats([text(","), choices([linebreak(), space()])]),
           )
        |> hcats;
      (center, true);
    | Rec(name, ty) => (
        hcats([
          text("rec " ++ Type.tpat_view(name) ++ "->{"),
          (
            (~enforce_inline) =>
              annot(HTypAnnot.Step(0), mk(~enforce_inline, ty))
          )
          |> pad_child(~enforce_inline),
          mk_delim("}"),
        ]),
        parenthesize,
      )
    | Forall(name, ty) => (
        hcats([
          text("forall " ++ Type.tpat_view(name) ++ "->{"),
          (
            (~enforce_inline) =>
              annot(HTypAnnot.Step(0), mk(~enforce_inline, ty))
          )
          |> pad_child(~enforce_inline),
          mk_delim("}"),
        ]),
        parenthesize,
      )
    | Sum(sum_map) =>
      let center =
        List.mapi(
          (i, vr) => {
            ConstructorMap.(
              switch (vr) {
              | Variant(ctr, _, None) =>
                annot(HTypAnnot.Step(i + 1), text(ctr))
              | Variant(ctr, _, Some(ty)) =>
                annot(
                  HTypAnnot.Step(i + 1),
                  hcats([text(ctr ++ "("), mk'(ty), text(")")]),
                )
              | BadEntry(ty) =>
                annot(HTypAnnot.Step(i + 1), hcats([mk'(ty)]))
              }
            )
          },
          sum_map,
        )
        |> ListUtil.join(
             hcats([text(" +"), choices([linebreak(), space()])]),
           )
        |> hcats;
      (center, true);
    | Ap(t1, t2) => (
        hcats([mk'(t1), text("("), mk'(t2), text(")")]),
        parenthesize,
      )
    };
  let doc = annot(HTypAnnot.Term, doc);
  parenthesize ? Doc.hcats([mk_delim("("), doc, mk_delim(")")]) : doc;
};
