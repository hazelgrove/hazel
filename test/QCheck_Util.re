open Haz3lcore;
open Language;
open QCheck;
open MenhirParser;

let with_ppx_minimal_idents =
    (minimal_idents: bool, gen: Gen.t('a)): Gen.t('a) =>
  st => {
    let prev = AST.ppx_minimal_idents^;
    AST.ppx_minimal_idents := minimal_idents;
    switch (gen(st)) {
    | x =>
      AST.ppx_minimal_idents := prev;
      x;
    | exception e =>
      AST.ppx_minimal_idents := prev;
      raise(e);
    };
  };

/**
 * Render a core expression as editable syntax, for QCheck counterexamples and
 * Alcotest failure output. Not `of_core(_, CoreSettings.off)`: that hides
 * ascriptions and folds function bodies, so expressions `Equality.semantic`
 * counts as different can print identically.
 */
let show_core_exp = exp =>
  exp
  |> ExpToSegment.exp_to_segment(
       ~settings=ExpToSegment.Settings.editable(~inline=true),
       _,
     )
  |> Printer.of_segment(~holes="?", _);

/**
 * Render a type as editable syntax, for QCheck counterexamples and Alcotest
 * failure output. The `Typ.show` deriver prints the whole IdTagged record,
 * which buries the type in annotation noise.
 */
let show_core_typ = typ =>
  typ
  |> TypToSegment.typ_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
       _,
     )
  |> Printer.of_segment(~holes="?", _);

/* Full-syntax variants: every Menhir AST constructor. `size` is depth fuel;
 * ~6 is the safe ceiling, beyond which construction OOMs. */
let arb_exp_full = (~minimal_idents=false, size: int) => {
  let menhir_arb = AST.arb_exp_full(size);
  let menhir_arb =
    set_gen(
      with_ppx_minimal_idents(minimal_idents, menhir_arb.gen),
      menhir_arb,
    );
  let mapped =
    map(
      ~rev=
        (core_exp: Exp.t) => {
          core_exp
          |> Grammar.map_exp_annotation(_ => false)
          |> Conversion.Exp.of_core
        },
      (menhir_exp: AST.exp) =>
        Conversion.Exp.of_menhir_ast(menhir_exp)
        |> Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh()),
      menhir_arb,
    );
  set_print(show_core_exp, mapped);
};

let arb_typ_full = (~minimal_idents=false, size: int) => {
  let menhir_arb = AST.arb_typ_full(size);
  let menhir_arb =
    set_gen(
      with_ppx_minimal_idents(minimal_idents, menhir_arb.gen),
      menhir_arb,
    );
  let mapped =
    map(
      ~rev=
        (core_typ: Typ.t) => {
          core_typ
          |> Grammar.map_typ_annotation(_ => false)
          |> Conversion.Typ.of_core
        },
      (menhir_typ: AST.typ) =>
        Conversion.Typ.of_menhir_ast(menhir_typ)
        |> Grammar.map_typ_annotation(_ => IdTagged.IdTag.fresh()),
      menhir_arb,
    );
  set_print(show_core_typ, mapped);
};
