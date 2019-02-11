type t = {
  expansion_ty: HTyp.t,
  initial_model: SpliceGenMonad.t(SerializedModel.t),
  to_exp: SerializedModel.t => UHExp.t,
};

