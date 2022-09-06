module Parsing = Text.Parsing;

let verbose = false;

let read = (text: string): option(UHExp.t) => {
  switch (Parsing.ast_of_string(text)) {
  | Ok(e) =>
    if (verbose) {
      Format.printf(
        "PARSED:\n%s\n",
        Sexplib.Sexp.to_string_hum(UHExp.sexp_of_t(e)),
      );
    };
    Some(e);
  | Error(msg) =>
    Format.printf("PARSER %s\n", msg);
    None;
  };
};

// let test_read = (text: string, expected_ty: HTyp.t): bool =>
//   switch (read(text)) {
//   | None => false
//   | Some(got_ty) => HTyp.equivalent(TyVarCtx.empty, got_ty, expected_ty)
//   };
