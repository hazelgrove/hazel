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
        mk_right_associative_operands(TypBase.precedence_Arrow, ty1, ty2);
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
              ~parenthesize=Typ.precedence(head) <= TypBase.precedence_Prod,
              head,
            ),
          ),
          ...List.mapi(
               (i, ty) =>
                 annot(
                   HTypAnnot.Step(i + 1),
                   mk'(
                     ~parenthesize=
                       Typ.precedence(ty) <= TypBase.precedence_Prod,
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
    | Rec(name, ty) => (
        hcats([
          text("rec " ++ name ++ "->{"),
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
          text("forall " ++ name ++ "->{"),
          (
            (~enforce_inline) =>
              annot(HTypAnnot.Step(0), mk(~enforce_inline, ty))
          )
          |> pad_child(~enforce_inline),
          mk_delim("}"),
        ]),
        parenthesize,
      )
    | Module({inner_ctx: [], incomplete: false}) => (
        text("Module{}"),
        parenthesize,
      )
    | Module({inner_ctx: [], incomplete: true}) => (
        text("Module{...}"),
        parenthesize,
      )
    | Module({inner_ctx, incomplete}) =>
      let decomp_entry =
          (m: Haz3lcore.TypBase.Ctx.entry): list((string, Typ.t)) => {
        switch (m) {
        | VarEntry({name: n0, typ: t0, _})
        | ConstructorEntry({name: n0, typ: t0, _}) => [(n0, t0)]
        | TVarEntry(_) => []
        };
      };
      let ntpairs = List.map(decomp_entry, inner_ctx) |> List.flatten;
      let center =
        List.mapi(
          (i, (name, ty)) =>
            hcats([
              text(name ++ ":"),
              annot(
                HTypAnnot.Step(i + 1),
                mk'(
                  ~parenthesize=Typ.precedence(ty) <= TypBase.precedence_Prod,
                  ty,
                ),
              ),
            ]),
          ntpairs,
        )
        |> ListUtil.join(
             hcats([text(","), choices([linebreak(), space()])]),
           )
        |> hcats;
      (
        hcats([
          text("Module{"),
          center,
          text((incomplete ? "..." : "") ++ "}"),
        ]),
        parenthesize,
      );
    | Member(name, _) => (text(name), parenthesize)
    | Sum(sum_map) =>
      let center =
        List.mapi(
          (i, (ctr, ty)) =>
            switch (ty) {
            | None => annot(HTypAnnot.Step(i + 1), text(ctr))
            | Some(ty) =>
              annot(
                HTypAnnot.Step(i + 1),
                hcats([text(ctr ++ "("), mk'(ty), text(")")]),
              )
            },
          sum_map,
        )
        |> ListUtil.join(
             hcats([text(" +"), choices([linebreak(), space()])]),
           )
        |> hcats;
      (center, true);
    };
  let doc = annot(HTypAnnot.Term, doc);
  parenthesize ? Doc.hcats([mk_delim("("), doc, mk_delim(")")]) : doc;
};
