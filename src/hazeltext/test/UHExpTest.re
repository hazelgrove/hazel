module Parsing = Hazeltext.Parsing;

let verbose = false;

let read = (ctx: Contexts.t, text: string): option(UHExp.t) => {
  switch (Parsing.ast_of_string_with_contexts(ctx, text)) {
  | Ok(e) =>
    if (verbose) {
      print_endline("PARSED:");
      print_endline(Sexplib.Sexp.to_string_hum(UHExp.sexp_of_t(e)));
    };
    Some(e);
  | Error(msg) =>
    print_endline("PARSER " ++ msg);
    None;
  };
};

// let test_read = (text: string, expected_ty: HTyp.t): bool =>
//   switch (read(text)) {
//   | None => false
//   | Some(got_ty) => HTyp.equivalent(TyVarCtx.empty, got_ty, expected_ty)
//   };
