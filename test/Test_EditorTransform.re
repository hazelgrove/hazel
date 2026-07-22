open Alcotest;
open Language;
open Test_Evaluator_Prelude;

let parse_zipper = (s: string): Haz3lcore.Zipper.t =>
  switch (Haz3lcore.Parser.to_zipper(s, ~root=Exp)) {
  | Some(zipper) => zipper
  | None => Alcotest.fail("Failed to parse zipper: " ++ s)
  };

let zipper_term = (zipper: Haz3lcore.Zipper.t): Exp.t =>
  Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;

let serialize = (z: Haz3lcore.Zipper.t): string =>
  Haz3lcore.Printer.of_zipper(~holes="?", z);

let count_newlines = (s: string): int =>
  String.fold_left((n, c) => c == '\n' ? n + 1 : n, 0, s);

let tests = (
  "EditorTransform",
  [
    test_case(
      "root transform rewrites the root expression",
      `Quick,
      () => {
        let zipper = parse_zipper("1 + 2");
        let transformed =
          Haz3lcore.EditorTransform.apply_exp_transform(zipper, _ =>
            parse_exp("3 * 4")
          );
        check(
          dhexp_typ,
          "root rewrite should replace expression",
          parse_exp("3 * 4"),
          zipper_term(transformed),
        );
      },
    ),
    test_case(
      "targeted transform rewrites only matching id",
      `Quick,
      () => {
        let zipper = parse_zipper("(2 + 3) + 4");
        let root_exp = zipper_term(zipper);
        let target_id =
          switch (ProofHacks.nth_exp(parse_exp("2 + 3"), 0, root_exp)) {
          | Some(exp) => Exp.rep_id(exp)
          | None => Alcotest.fail("Could not find target exp id")
          };
        let transformed =
          Haz3lcore.EditorTransform.apply_exp_transform(~target_id, zipper, _ =>
            parse_exp("10")
          );
        check(
          dhexp_typ,
          "only selected sub-expression changes",
          parse_exp("10 + 4"),
          zipper_term(transformed),
        );
      },
    ),
    test_case(
      "missing target id is a no-op",
      `Quick,
      () => {
        let zipper = parse_zipper("1 + 2");
        let original = zipper_term(zipper);
        let transformed =
          Haz3lcore.EditorTransform.apply_exp_transform(
            ~target_id=Id.mk(), zipper, _ =>
            parse_exp("0")
          );
        check(
          dhexp_typ,
          "missing id leaves expression unchanged",
          original,
          zipper_term(transformed),
        );
      },
    ),
    test_case(
      "apply_patch keeps zipper semantically valid",
      `Quick,
      () => {
        let zipper = parse_zipper("1 + 2");
        let target_id = None;
        let transformed =
          Haz3lcore.EditorTransform.apply_patch(
            zipper,
            Haz3lcore.EditorTransform.mk_patch(
              ~target_id?,
              parse_exp("let x = 1 in x"),
            ),
          );
        let term = zipper_term(transformed);
        check(
          dhexp_typ,
          "patched zipper should still produce a term",
          parse_exp("let x = 1 in x"),
          term,
        );
      },
    ),
  ],
);
