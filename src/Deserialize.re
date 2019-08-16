open Sexplib;

let block_of_string = (s: string): UHExp.block => {
  let block = UHExp.block_of_sexp(Sexp.of_string(s));
  let (block, _, _) =
    Statics.fix_and_renumber_holes((VarCtx.empty, LivelitCtx.empty), block);
  block;
};
