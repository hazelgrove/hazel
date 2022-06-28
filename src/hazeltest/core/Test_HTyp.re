open Tezt;
open Tezt.Base;

module Parsing = Hazeltext.Parsing;

let verbose = false;

let read = (ty_text: string): option(HTyp.t) => {
  let e_text = Format.sprintf({|let ? : (%s) = ? in ?|}, ty_text);
  switch (Parsing.ast_of_string(e_text)) {
  | Ok([LetLine(OpSeq(_, S(TypeAnn(_, _, uty), E)), _), _]) =>
    open OptUtil.Syntax;
    if (verbose) {
      Format.printf(
        "EXTRACTED:\n%s\n",
        Sexplib.Sexp.to_string_hum(UHTyp.sexp_of_t(uty)),
      );
    };
    let+ (ty, _, _) =
      Elaborator_Typ.syn_elab(InitialContext.ctx, Delta.empty, uty);
    ty;
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
  | Some(got_ty) => HTyp.equivalent(InitialContext.ctx, got_ty, expected_ty)
  };

let register_test = (text, tags, expected_ty) =>
  Test.register(
    ~__FILE__,
    ~title="read HTyp " ++ text,
    ~tags=["hazelcore", "htyp"] @ tags,
    () =>
    test_read(text, expected_ty) ? unit : Test.fail("read HTyp failed!")
  );

let () = register_test("?", [], HTyp.hole());
let () = register_test("Int", [], HTyp.int());
let () = register_test("Float", [], HTyp.float());
let () = register_test("Bool", [], HTyp.bool());
let () = register_test("? -> ?", [], HTyp.arrow(HTyp.hole(), HTyp.hole()));
let () =
  register_test("Int -> Bool", [], HTyp.arrow(HTyp.int(), HTyp.bool()));
let () = register_test("? | ?", [], HTyp.sum(HTyp.hole(), HTyp.hole()));
let () =
  register_test("Float | Int", [], HTyp.sum(HTyp.float(), HTyp.int()));
let () = register_test("[?]", [], HTyp.list(HTyp.hole()));
let () = register_test("[Int]", [], HTyp.list(HTyp.int()));
let () =
  register_test("? , ?", [], HTyp.product([HTyp.hole(), HTyp.hole()]));
let () =
  register_test(
    "Bool , Float",
    [],
    HTyp.product([HTyp.bool(), HTyp.float()]),
  );
let () =
  register_test(
    "[Int] -> Bool",
    [],
    HTyp.arrow(HTyp.list(HTyp.int()), HTyp.bool()),
  );
