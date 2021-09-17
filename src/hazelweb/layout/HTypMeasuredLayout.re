module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

[@deriving sexp]
type t = MeasuredLayout.t(HTypAnnot.t);
type with_offset = MeasuredLayout.with_offset(HTypAnnot.t);
include MeasuredLayout.Make(WeakMap);
