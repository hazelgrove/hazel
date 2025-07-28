open Haz3lcore;
open Language;
open QCheck;
open MenhirParser;

/**
 * An arbitrary generator for expressions of type `Exp.t`.
 * This uses the generator from the menhirParser AST to produce random instances of `Exp.t`
 * for property-based testing.
 */
let arb_exp = (~minimal_idents: bool, size: int) => {
  let show_core_exp = exp =>
    exp
    |> ExpToSegment.exp_to_segment(
         ~settings=
           ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
         _,
       )
    |> Printer.of_segment(
         ~holes="?",
         ~projector_to_segment=Printer.default_projector_to_segment,
         _,
       );
  let arb_exp =
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
      AST.arb_exp(~minimal_idents, size),
    );
  set_print(show_core_exp, arb_exp);
};

let arb_typ = (~minimal_idents: bool, size: int) => {
  let show_core_typ = typ =>
    typ
    |> ExpToSegment.typ_to_segment(
         ~settings=
           ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
         _,
       )
    |> Printer.of_segment(
         ~holes="?",
         ~projector_to_segment=Printer.default_projector_to_segment,
         _,
       );
  let arb_typ =
    map(
      ~rev=
        (core_exp: Typ.t) => {
          core_exp
          |> Grammar.map_typ_annotation(_ => false)
          |> Conversion.Typ.of_core
        },
      (menhir_typ: AST.typ) =>
        Conversion.Typ.of_menhir_ast(menhir_typ)
        |> Grammar.map_typ_annotation(_ => IdTagged.IdTag.fresh()),
      AST.arb_typ(~minimal_idents, size),
    );
  set_print(show_core_typ, arb_typ);
};
