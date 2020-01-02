open Sexplib;
open LangUtil;

let ensure_well_typed_before_serialization = block =>
  switch (Statics.syn_block((VarMap.empty, PaletteCtx.empty), block)) {
  | None => raise(IllFormed(block))
  | _ => block
  };

let string_of_block = block => {
  let block = ensure_well_typed_before_serialization(block);
  Sexp.to_string(UHExp.sexp_of_block(block));
};
