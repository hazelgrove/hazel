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

/* Find the first Theorem's proof sub-term in an expression. */
let find_theorem_proof = (e: Exp.t): option(Proof.t) => {
  let found = ref(None);
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Theorem(_, _, proof, _) when found^ == None =>
      found := Some(proof);
      e;
    | _ => continue(e)
    };
  let _ = TermBase.Exp.map_term(~f_exp, e);
  found^;
};

let theorem_proof = (src: string): Proof.t =>
  switch (find_theorem_proof(parse_exp(src))) {
  | Some(p) => p
  | None => Alcotest.fail("no theorem proof in: " ++ src)
  };

let serialize = (z: Haz3lcore.Zipper.t): string =>
  Haz3lcore.Printer.of_zipper(~holes="?", z);

let count_newlines = (s: string): int =>
  String.fold_left((n, c) => c == '\n' ? n + 1 : n, 0, s);

/* Apply a proof patch that replaces the (hole) proof of `target_src` with the
   proof parsed from `replacement_src`, and return the serialized result. */
let patch_proof = (~reflow=true, target_src: string, replacement_src: string) => {
  let z = parse_zipper(target_src);
  /* target_id must come from the same zipper `z` (ids are per-parse). */
  let target_id =
    switch (find_theorem_proof(zipper_term(z))) {
    | Some(p) => Proof.rep_id(p)
    | None => Alcotest.fail("no theorem proof in target: " ++ target_src)
    };
  let replacement = theorem_proof(replacement_src);
  Haz3lcore.EditorTransform.apply_patch(
    z,
    Haz3lcore.EditorTransform.mk_proof_patch(
      ~target_id,
      ~reflow,
      replacement,
    ),
  )
  |> serialize;
};

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
    test_case(
      "proof patch reflows multi-step proof onto separate lines",
      `Quick,
      () => {
        let target = "theorem t = 1 + 1 + 1 == 2 proof ? in t";
        let two_step = "theorem u = 1 == 1 proof eval 1 at 0 end; eval 1 at 0 end in u";
        let reflowed = patch_proof(target, two_step);
        let flat = patch_proof(~reflow=false, target, two_step);
        check(
          bool,
          "reflow=true introduces more linebreaks than reflow=false",
          true,
          count_newlines(reflowed) > count_newlines(flat),
        );
      },
    ),
    test_case(
      "proof patch reflows induction cases onto separate lines",
      `Quick,
      () => {
        let target = "theorem t = 1 == 1 proof ? in t";
        let induction = "theorem u = 1 == 1 proof induction y | a => axiom y at y on y end | b => axiom z at z on z end end in u";
        let reflowed = patch_proof(target, induction);
        let flat = patch_proof(~reflow=false, target, induction);
        check(
          bool,
          "reflow=true introduces more linebreaks than reflow=false",
          true,
          count_newlines(reflowed) > count_newlines(flat),
        );
      },
    ),
  ],
);
