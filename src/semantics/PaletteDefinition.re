type t = {
  expansion_ty: HTyp.t,
  initial_model: SpliceGenMonad.t(PaletteSerializedModel.t),
  to_exp: PaletteSerializedModel.t => UHExp.t,
};

