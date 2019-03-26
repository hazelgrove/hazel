open Sexplib;
open LangUtil;

let ensure_well_typed_before_serialization = uhexp =>
  switch (Statics.syn((VarMap.empty, PaletteCtx.empty), uhexp)) {
  | None => raise(IllFormed(uhexp))
  | _ => uhexp
  };

let string_of_uhexp = e => {
  let e = ensure_well_typed_before_serialization(e);
  Sexp.to_string(UHExp.sexp_of_t(e));
};
