open Haz3lcore;
open QCheck;
open Haz3lmenhir;

/**
 * An arbitrary generator for expressions of type `Exp.t`.
 * This uses the generator from the Haz3lmenhir AST to produce random instances of `Exp.t`
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
    |> Printer.of_segment(~holes=Some("?"), _);
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
