module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

[@deriving sexp]
type t = MeasuredLayout.t(DHAnnot.t);
type with_offset = MeasuredLayout.with_offset(DHAnnot.t);
include MeasuredLayout.Make(WeakMap);
