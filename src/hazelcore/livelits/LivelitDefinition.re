open Sexplib.Std;

[@deriving sexp]
type livelit_error =
  | Error;

[@deriving sexp]
type expand_error =
  | NotStringlit
  | NotSexp
  | NotUHExp;

[@deriving sexp]
type livelit_expand_result =
  | Success(UHExp.t)
  | Failure(expand_error);

[@deriving sexp]
type t = {
  name: LivelitName.t,
  expansion_ty: HTyp.t,
  captures_ty: option(HTyp.t),
  param_tys: list((Var.t, HTyp.t)),
  init_model: SpliceGenCmd.t(SerializedModel.t),
  update:
    (SerializedAction.t, SerializedModel.t) =>
    SpliceGenCmd.t(SerializedModel.t),
  expand: SerializedModel.t => livelit_expand_result,
};
