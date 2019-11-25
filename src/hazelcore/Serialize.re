open Sexplib;
open LangUtil;

let ensure_well_typed_before_serialization = e =>
  switch (Statics.Exp.syn((VarMap.empty, PaletteCtx.empty), e)) {
  | None => raise(IllFormed(e))
  | _ => e
  };

let string_of_exp = e => {
  let e = ensure_well_typed_before_serialization(e);
  Sexp.to_string(UHExp.sexp_of_t(e));
};
