[@deriving sexp]
type t = {
  expansion_ty: HTyp.t,
  init_model: SpliceGenMonad.t(SerializedModel.t),
  expand: SerializedModel.t => UHExp.t,
};
