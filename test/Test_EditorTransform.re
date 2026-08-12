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

/* Find ALL theorem proofs, in traversal order (for multi-theorem programs). */
let find_theorem_proofs = (e: Exp.t): list(Proof.t) => {
  let found = ref([]);
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Theorem(_, _, proof, _) =>
      found := found^ @ [proof];
      continue(e);
    | _ => continue(e)
    };
  let _ = TermBase.Exp.map_term(~f_exp, e);
  found^;
};

/* Collect proof sub-nodes satisfying `pred`, in traversal order. */
let rec find_proofs = (pred: Proof.t => bool, p: Proof.t): list(Proof.t) => {
  let kids =
    switch (p.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_) => []
    | Seq(a, b) => find_proofs(pred, a) @ find_proofs(pred, b)
    | Induction(_, cases) =>
      List.concat_map(((_, body)) => find_proofs(pred, body), cases)
    | Forall(_, body) => find_proofs(pred, body)
    };
  (pred(p) ? [p] : []) @ kids;
};

let is_axiom_step = (p: Proof.t): bool =>
  switch (p.term) {
  | AxiomStep(_) => true
  | _ => false
  };

let is_eval_step = (p: Proof.t): bool =>
  switch (p.term) {
  | EvalStep(_) => true
  | _ => false
  };

let serialize = (z: Haz3lcore.Zipper.t): string =>
  Haz3lcore.Printer.of_zipper(~holes="?", z);

let count_newlines = (s: string): int =>
  String.fold_left((n, c) => c == '\n' ? n + 1 : n, 0, s);

let contains_substring = (haystack: string, needle: string): bool => {
  let hl = String.length(haystack);
  let nl = String.length(needle);
  let rec go = i =>
    i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

let check_contains = (~msg: string, haystack: string, needle: string) =>
  check(
    bool,
    msg ++ " — expected to find:\n" ++ needle ++ "\nin:\n" ++ haystack,
    true,
    contains_substring(haystack, needle),
  );

/* Apply a proof patch to the proof selected by `select` (out of all proof
   sub-nodes of all theorems), returning the patched zipper. */
let patch_proof_zipper =
    (
      ~reflow=true,
      ~select: Exp.t => option(Proof.t)=find_theorem_proof,
      z: Haz3lcore.Zipper.t,
      replacement: Proof.t,
    )
    : Haz3lcore.Zipper.t => {
  /* target_id must come from the same zipper `z` (ids are per-parse). */
  let target_id =
    switch (select(zipper_term(z))) {
    | Some(p) => Proof.rep_id(p)
    | None => Alcotest.fail("proof selector found no target")
    };
  Haz3lcore.EditorTransform.apply_patch(
    z,
    Haz3lcore.EditorTransform.mk_proof_patch(
      ~target_id,
      ~reflow,
      replacement,
    ),
  );
};

/* Apply a proof patch that replaces the (hole) proof of `target_src` with the
   proof parsed from `replacement_src`, and return the serialized result. */
let patch_proof = (~reflow=true, target_src: string, replacement_src: string) => {
  let z = parse_zipper(target_src);
  let replacement = theorem_proof(replacement_src);
  patch_proof_zipper(~reflow, z, replacement) |> serialize;
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
    /* ---- Locality: a patch must not rewrite text outside its extent ---- */
    test_case(
      "proof patch preserves theorem statement byte-for-byte",
      `Quick,
      () => {
        /* Extra parens and non-canonical spacing in the statement: the
           old whole-program writer normalized these on every patch. */
        let statement = "forall x:(([Int])) ->  x  ==  x";
        let target = "theorem t = " ++ statement ++ " proof ? in t";
        let out =
          patch_proof(
            target,
            "theorem u = 1 == 1 proof eval 1 at 0 end; eval 1 at 0 end in u",
          );
        check_contains(~msg="statement survives untouched", out, statement);
      },
    ),
    test_case(
      "proof patch preserves comments elsewhere in the program",
      `Quick,
      () => {
        let target = "#keep me exactly#\ntheorem t = 1 == 1 proof ? in t";
        let out =
          patch_proof(
            target,
            "theorem u = 1 == 1 proof eval 1 at 0 end in u",
          );
        check_contains(~msg="comment survives", out, "#keep me exactly#");
      },
    ),
    test_case(
      "proof patch preserves literal lexemes elsewhere",
      `Quick,
      () => {
        /* `007` canonicalizes to `7` under the whole-program writer
           (use_literal_lexemes=false), so its survival proves the
           patch spliced locally instead of re-serializing. */
        let target = "let k = 007 in theorem t = 1 == 1 proof ? in k";
        let out =
          patch_proof(
            target,
            "theorem u = 1 == 1 proof eval 1 at 0 end in u",
          );
        check_contains(
          ~msg="literal spelling survives",
          out,
          "let k = 007 in",
        );
      },
    ),
    test_case(
      "proof patch preserves multi-line formatting of earlier bindings",
      `Quick,
      () => {
        let binding = "let f = fun x ->\n  x + 1\nin";
        let target = binding ++ "\ntheorem t = 1 == 1 proof ? in f(1)";
        let out =
          patch_proof(
            target,
            "theorem u = 1 == 1 proof eval 1 at 0 end in u",
          );
        check_contains(~msg="binding layout survives", out, binding);
      },
    ),
    test_case(
      "patching one theorem leaves a sibling theorem untouched",
      `Quick,
      () => {
        let first = "theorem a = 1  ==  1 proof ? in";
        let target = first ++ "\ntheorem t = 2 == 2 proof ? in t";
        let z = parse_zipper(target);
        let select = (e: Exp.t) =>
          switch (find_theorem_proofs(e)) {
          | [_, second, ..._] => Some(second)
          | _ => None
          };
        let replacement =
          theorem_proof("theorem u = 1 == 1 proof eval 1 at 0 end in u");
        let out = patch_proof_zipper(~select, z, replacement) |> serialize;
        check_contains(~msg="first theorem survives", out, first);
        check_contains(~msg="patch landed", out, "eval 1 at 0 end");
      },
    ),
    /* ---- Stability: repeated patching must not accumulate anything ---- */
    test_case(
      "re-applying an equivalent proof patch is text-stable",
      `Quick,
      () => {
        let target = "theorem t = forall x:[Int] -> x == x proof ? in t";
        let replacement_src = "theorem u = 1 == 1 proof eval 1 at 0 end; eval 1 at 0 end in u";
        let z1 =
          patch_proof_zipper(
            parse_zipper(target),
            theorem_proof(replacement_src),
          );
        let once = serialize(z1);
        /* Patch the result again with a fresh parse of the same proof. */
        let z2 = patch_proof_zipper(z1, theorem_proof(replacement_src));
        check(string, "second patch is a fixed point", once, serialize(z2));
      },
    ),
    test_case(
      "adding then removing an induction case leaves the statement alone",
      `Quick,
      () => {
        /* The original bug: every case add/remove re-serialized the
           whole program and grew parens on the forall's ascription. */
        let statement = "forall x:[Int] -> x == x";
        let one_case =
          "theorem t = "
          ++ statement
          ++ " proof induction y | a => axiom y at y on y end end in t";
        let two_case =
          "theorem t = "
          ++ statement
          ++ " proof induction y | a => axiom y at y on y end | b => axiom z at z on z end end in t";
        let z = parse_zipper(one_case);
        let z = patch_proof_zipper(z, theorem_proof(two_case));
        check_contains(
          ~msg="statement intact after adding a case",
          serialize(z),
          statement,
        );
        let z = patch_proof_zipper(z, theorem_proof(one_case));
        check_contains(
          ~msg="statement intact after removing the case",
          serialize(z),
          statement,
        );
        /* And a second add/remove cycle lands on exactly the same text. */
        let text_after_one_cycle = serialize(z);
        let z = patch_proof_zipper(z, theorem_proof(two_case));
        let z = patch_proof_zipper(z, theorem_proof(one_case));
        check(
          string,
          "add/remove cycle is a fixed point",
          text_after_one_cycle,
          serialize(z),
        );
      },
    ),
    /* ---- Reparse safety: splices must preserve term structure ---- */
    test_case(
      "exp patch into operator context keeps precedence",
      `Quick,
      () => {
        let z = parse_zipper("2 * 3");
        let target_id =
          switch (ProofHacks.nth_exp(parse_exp("3"), 0, zipper_term(z))) {
          | Some(exp) => Exp.rep_id(exp)
          | None => Alcotest.fail("Could not find target exp id")
          };
        let transformed =
          Haz3lcore.EditorTransform.apply_patch(
            z,
            Haz3lcore.EditorTransform.mk_patch(
              ~target_id,
              parse_exp("5 + 6"),
            ),
          );
        check(
          dhexp_typ,
          "splice must not re-associate as (2 * 5) + 6",
          parse_exp("2 * (5 + 6)"),
          zipper_term(transformed),
        );
      },
    ),
    test_case(
      "atomic exp patch splices without disturbing surroundings",
      `Quick,
      () => {
        let z = parse_zipper("2 * 3");
        let target_id =
          switch (ProofHacks.nth_exp(parse_exp("3"), 0, zipper_term(z))) {
          | Some(exp) => Exp.rep_id(exp)
          | None => Alcotest.fail("Could not find target exp id")
          };
        let out =
          Haz3lcore.EditorTransform.apply_patch(
            z,
            Haz3lcore.EditorTransform.mk_patch(~target_id, parse_exp("7")),
          )
          |> serialize;
        check(string, "only the operand changes", "2 * 7", out);
      },
    ),
    test_case(
      "multi-step proof replacing one step of a seq keeps all steps",
      `Quick,
      () => {
        /* Replacing a step-with-siblings by a multi-piece proof can't be
           spliced safely (proofs have no parens form), so this exercises
           the whole-program fallback; the steps must still all be there. */
        let target = "theorem t = 1 == 1 proof axiom y at y on y end; axiom z at z on z end in t";
        let z = parse_zipper(target);
        let select = (e: Exp.t) =>
          switch (find_theorem_proof(e)) {
          | Some(p) =>
            switch (find_proofs(is_axiom_step, p)) {
            | [first, ..._] => Some(first)
            | [] => None
            }
          | None => None
          };
        let replacement =
          theorem_proof(
            "theorem u = 1 == 1 proof eval 1 at 0 end; eval 2 at 0 end in u",
          );
        let out = patch_proof_zipper(~select, z, replacement);
        let expected = "theorem t = 1 == 1 proof eval 1 at 0 end; eval 2 at 0 end; axiom z at z on z end in t";
        check(
          dhexp_typ,
          "all three steps present in order",
          parse_exp(expected),
          zipper_term(out),
        );
      },
    ),
    test_case(
      "patching a case body reflows only the induction tile",
      `Quick,
      () => {
        let statement = "1  ==  1";
        let target =
          "theorem t = "
          ++ statement
          ++ " proof induction y | a => axiom y at y on y end | b => axiom z at z on z end end in t";
        let z = parse_zipper(target);
        let select = (e: Exp.t) =>
          switch (find_theorem_proof(e)) {
          | Some(p) =>
            switch (find_proofs(is_axiom_step, p)) {
            | [first, ..._] => Some(first)
            | [] => None
            }
          | None => None
          };
        let replacement =
          theorem_proof("theorem u = 1 == 1 proof eval 9 at 0 end in u");
        let out = patch_proof_zipper(~select, z, replacement);
        let text = serialize(out);
        check_contains(~msg="statement intact", text, statement);
        check_contains(
          ~msg="patched case body landed",
          text,
          "eval 9 at 0 end",
        );
        /* The untouched case is still present with its tokens intact. */
        check_contains(
          ~msg="sibling case intact",
          text,
          "axiom z at z on z end",
        );
      },
    ),
    test_case(
      "patched output reparses to the patched term",
      `Quick,
      () => {
        let target = "theorem t = forall x:[Int] -> x == x proof ? in t";
        let replacement_src = "theorem u = 1 == 1 proof induction y | a => axiom y at y on y end | b => axiom z at z on z end end in u";
        let z =
          patch_proof_zipper(
            parse_zipper(target),
            theorem_proof(replacement_src),
          );
        /* Serialize and reparse: the text must parse and mean the same. */
        let reparsed = parse_zipper(serialize(z));
        check(
          dhexp_typ,
          "text roundtrips through reparse",
          zipper_term(z),
          zipper_term(reparsed),
        );
      },
    ),
    test_case(
      "missing target id leaves the program text untouched",
      `Quick,
      () => {
        /* Non-canonical formatting everywhere: a no-op patch must not
           reformat anything. */
        let target = "let k = 007 in\ntheorem t = 1  ==  1 proof ? in k";
        let z = parse_zipper(target);
        let before = serialize(z);
        let out =
          Haz3lcore.EditorTransform.apply_patch(
            z,
            Haz3lcore.EditorTransform.mk_proof_patch(
              ~target_id=Id.mk(),
              theorem_proof("theorem u = 1 == 1 proof eval 1 at 0 end in u"),
            ),
          );
        check(string, "no-op patch changes nothing", before, serialize(out));
      },
    ),
    test_case(
      "extending an eval-step chain splices the second step",
      `Quick,
      () => {
        /* Mirrors the stepper UI's ExtendProof patch: taking a second
           step replaces the previous step's leaf with Seq(leaf, new). */
        let target = "theorem thm = 1 + 4 == 5 proof \neval 1 + 4 at 0 end\n  in";
        let z = parse_zipper(target);
        let prev =
          switch (find_theorem_proof(zipper_term(z))) {
          | Some(p) =>
            switch (find_proofs(is_eval_step, p)) {
            | [step, ..._] => step
            | [] => Alcotest.fail("no eval step found")
            }
          | None => Alcotest.fail("no theorem proof")
          };
        let head =
          theorem_proof("theorem u = 1 == 1 proof eval 5 == 5 at 0 end in u");
        let replacement = Proof.fresh(Seq(prev, head));
        let out =
          Haz3lcore.EditorTransform.apply_patch(
            z,
            Haz3lcore.EditorTransform.mk_proof_patch(
              ~target_id=Proof.rep_id(prev),
              replacement,
            ),
          );
        check(
          string,
          "second step chains onto the first",
          "theorem thm = 1 + 4 == 5 proof \n"
          ++ "eval 1 + 4 at 0 end;\n"
          ++ "eval 5 == 5 at 0 end\n"
          ++ "  in?",
          serialize(out),
        );
      },
    ),
    test_case(
      "reflow=false proof patch still splices locally",
      `Quick,
      () => {
        let statement = "forall x:(([Int])) ->  x  ==  x";
        let target = "theorem t = " ++ statement ++ " proof ? in t";
        let out =
          patch_proof(
            ~reflow=false,
            target,
            "theorem u = 1 == 1 proof eval 1 at 0 end in u",
          );
        check_contains(~msg="statement intact", out, statement);
        check_contains(~msg="patch landed", out, "eval 1 at 0 end");
      },
    ),
    test_case(
      "remove patch splices a mid-chain step and its semicolon",
      `Quick,
      () => {
        let src = "theorem t = 1 + 4 == 5 proof eval 1 + 4 at 0 end; eval 1 + 3 at 0 end; eval 5 == 5 at 0 end in t";
        let z = parse_zipper(src);
        let target_id =
          switch (find_theorem_proof(zipper_term(z))) {
          | Some(p) =>
            switch (find_proofs(is_eval_step, p)) {
            | [_, second, _] => Proof.rep_id(second)
            | _ => Alcotest.fail("expected three eval steps")
            }
          | None => Alcotest.fail("no proof")
          };
        let out =
          Haz3lcore.EditorTransform.apply_patch(
            z,
            Haz3lcore.EditorTransform.mk_proof_remove_patch(~target_id),
          )
          |> serialize;
        check(
          bool,
          "removed step is gone — got:\n" ++ out,
          false,
          contains_substring(out, "1 + 3"),
        );
        check_contains(~msg="first step kept", out, "eval 1 + 4 at 0 end");
        check_contains(~msg="third step kept", out, "eval 5 == 5 at 0 end");
        /* The `;` went with the step: the result reparses to a two-step
         * chain with no hole where the step was. */
        let reparsed =
          switch (find_theorem_proof(parse_exp(out))) {
          | Some(p) => p
          | None => Alcotest.fail("no proof after removal: " ++ out)
          };
        check(
          int,
          "two steps remain",
          2,
          List.length(find_proofs(is_eval_step, reparsed)),
        );
        check(
          bool,
          "no hole left behind — got:\n" ++ out,
          false,
          Proof.has_hole(reparsed),
        );
      },
    ),
    test_case(
      "remove patch on the sole step leaves a hole proof",
      `Quick,
      () => {
        let src = "theorem t = 1 + 4 == 5 proof eval 1 + 4 at 0 end in t";
        let z = parse_zipper(src);
        let target_id =
          switch (find_theorem_proof(zipper_term(z))) {
          | Some(p) => Proof.rep_id(p)
          | None => Alcotest.fail("no proof")
          };
        let out =
          Haz3lcore.EditorTransform.apply_patch(
            z,
            Haz3lcore.EditorTransform.mk_proof_remove_patch(~target_id),
          )
          |> serialize;
        check(
          bool,
          "step is gone — got:\n" ++ out,
          false,
          contains_substring(out, "eval"),
        );
        let reparsed =
          switch (find_theorem_proof(parse_exp(out))) {
          | Some(p) => p
          | None => Alcotest.fail("no proof after removal: " ++ out)
          };
        check(
          bool,
          "an empty proof (hole) remains",
          true,
          Proof.has_hole(reparsed),
        );
      },
    ),
  ],
);
