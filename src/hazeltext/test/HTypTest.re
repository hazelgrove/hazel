module Parsing = Hazeltext.Parsing;

let verbose = false;

let read = (ty_text: string): option(HTyp.t) => {
  let e_text = Format.sprintf({|let ? : (%s) = ? in ?|}, ty_text);
  switch (Parsing.ast_of_string(e_text)) {
  | Ok([LetLine(OpSeq(_, S(TypeAnn(_, _, uty), E)), _), _]) =>
    if (verbose) {
      Format.printf(
        "EXTRACTED:\n%s\n",
        Sexplib.Sexp.to_string_hum(UHTyp.sexp_of_t(uty)),
      );
    };
    Some(UHTyp.expand(uty));
  | Ok(e) =>
    if (verbose) {
      Format.printf(
        "PARSED:\n%s\n",
        Sexplib.Sexp.to_string_hum(UHExp.sexp_of_t(e)),
      );
    };
    None;
  | Error(msg) =>
    Format.printf("PARSER %s\n", msg);
    None;
  };
};

let test_read = (text: string, expected_ty: HTyp.t): bool =>
  switch (read(text)) {
  | None => false
  | Some(got_ty) => HTyp.equivalent(Context.initial, got_ty, expected_ty)
  };
