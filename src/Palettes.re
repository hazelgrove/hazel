open Semantics.Core;

module BooleanPalette = {
  let name = "$checkbox";
  let bool_ty: HTyp.t = HTyp.Sum(HTyp.Num, HTyp.Num); /* TODO change to unit */
  let dummy_num: UHExp.t = UHExp.Tm(NotInHole, UHExp.NumLit(0));
  let true_exp: UHExp.t = UHExp.Tm(NotInHole, UHExp.Inj(UHExp.L, dummy_num));
  let false_exp: UHExp.t =
    UHExp.Tm(NotInHole, UHExp.Inj(UHExp.R, dummy_num));
  let palette_defn =
    UHExp.PaletteDefinition.{
      expansion_ty: bool_ty,
      initial_model: "F",
      to_exp: model => String.equal(model, "T") ? true_exp : false_exp,
    };
};

let initial_palette_ctx: PaletteCtx.t =
  PaletteCtx.extend(
    PaletteCtx.empty,
    (BooleanPalette.name, BooleanPalette.palette_defn),
  );
