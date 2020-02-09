[@deriving sexp]
type t = {
  expansion_ty: HTyp.t,
  init_model: SpliceGenCmd.t(SerializedModel.t),
  update:
    (SerializedAction.t, SerializedModel.t) =>
    SpliceGenCmd.t(SerializedModel.t),
  expand: SerializedModel.t => UHExp.t,
};
