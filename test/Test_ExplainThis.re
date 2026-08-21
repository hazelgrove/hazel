open Language;

/* Property-based test ensuring ExplainThis never raises while producing
   documentation for any sub-term of an expression. The documentation for
   each form substitutes term ids into its explanation string via a format,
   so a mismatch between the number of `%s` placeholders and the number of
   supplied arguments crashes at runtime. This test guards against that. */

let globals = Web.Globals.Model.init(~settings=Web.Settings.Model.init, ());
let docs = Web.ExplainThisModel.init;

let statics = term =>
  fst(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    ),
  );

let qcheck_explainthis_does_not_crash =
  QCheck.Test.make(
    ~name="ExplainThis.get_doc does not crash",
    ~count=1000,
    QCheck_Util.arb_exp(~minimal_idents=true, 12),
    exp => {
    /* Statics failures are out of scope; we only assert that ExplainThis
       itself does not raise for any sub-term it is asked to document. */
    switch (statics(exp)) {
    | exception _ => true
    | info_map =>
      Id.Map.iter(
        (_id, info: Info.t) => {
          let _ =
            Web.ExplainThis.get_doc(
              ~globals,
              ~docs,
              Some(info),
              Web.ExplainThis.Colorings,
            );
          ();
        },
        info_map,
      );
      true;
    }
  });

/* --- prover-form documentation completeness ------------------------------ */

/* Every form of the Proof sort must map to a REAL message, not the generic
   "Proof term" fallback the cursor inspector used to show. `ProofDoc.single`
   is total over `Proof.term`, so the compiler already guarantees an arm
   exists; what this test guarantees is that each arm carries prose, that
   the prose actually references every child it colors (a coloring whose id
   never appears in the message paints nothing), that no `%s` placeholder
   was left unfilled, and that each example's source really parses. */

let contains = (hay: string, needle: string): bool => {
  let (nh, nn) = (String.length(hay), String.length(needle));
  let rec go = (i: int) =>
    i + nn > nh ? false : String.sub(hay, i, nn) == needle || go(i + 1);
  go(0);
};

let hole_proof = () => Proof.fresh(EmptyHole);
let some_exp = (): Exp.t => Exp.fresh(EmptyHole);
let some_pat = (): Pat.t => Pat.fresh(EmptyHole);
let inst = () => Some((some_exp(), some_exp()));

/* Representative terms, one per constructor (and one per shape of the
   optional `with` clause, which selects a different message). Exhaustive
   by construction: a new proof form fails to compile here. */
let representatives = (cls: Proof.cls): list(Proof.t) =>
  switch (cls) {
  | Invalid => [Proof.fresh(Invalid("~"))]
  | EmptyHole => [Proof.fresh(EmptyHole)]
  | MultiHole => [Proof.fresh(MultiHole([]))]
  | Seq => [Proof.fresh(Seq(hole_proof(), hole_proof()))]
  | AxiomStep =>
    [Util.Direction.Right, Util.Direction.Left]
    |> List.concat_map(direction =>
         [None, inst()]
         |> List.map(instantiation =>
              Proof.fresh(
                AxiomStep({
                  at_idx: some_exp(),
                  at_exp: some_exp(),
                  direction,
                  equality: some_exp(),
                  instantiation,
                }),
              )
            )
       )
  | AlgebriteStep => [
      Proof.fresh(
        AlgebriteStep({
          at_idx: some_exp(),
          at_exp: some_exp(),
          with_exp: some_exp(),
        }),
      ),
    ]
  | EvalStep => [
      Proof.fresh(
        EvalStep({
          at_idx: some_exp(),
          at_exp: some_exp(),
        }),
      ),
    ]
  | Induction => [
      Proof.fresh(Induction(some_exp(), [(some_pat(), hole_proof())])),
    ]
  | Forall => [Proof.fresh(Forall(some_pat(), hole_proof()))]
  | Assume => [Proof.fresh(Assume(some_exp(), hole_proof()))]
  | Generalize => [Proof.fresh(Generalize(some_exp(), hole_proof()))]
  | Revert => [
      Proof.fresh(Revert(some_exp(), None, hole_proof())),
      Proof.fresh(Revert(some_exp(), inst(), hole_proof())),
    ]
  | Contradiction => [
      Proof.fresh(Contradiction(some_exp(), None)),
      Proof.fresh(Contradiction(some_exp(), inst())),
    ]
  | Have => [Proof.fresh(Have(some_exp(), hole_proof(), hole_proof()))]
  };

/* The degenerate forms carry a message but no worked example. */
let example_free = (cls: Proof.cls): bool =>
  switch (cls) {
  | Invalid
  | EmptyHole
  | MultiHole => true
  | Seq
  | AxiomStep
  | AlgebriteStep
  | EvalStep
  | Induction
  | Forall
  | Assume
  | Generalize
  | Revert
  | Contradiction
  | Have => false
  };

let check_doc =
    (~what: string, ~needs_example: bool, doc: Web.ExplainThisForm.Simple.t) => {
  let msg = doc.explanation;
  Alcotest.check(
    Alcotest.bool,
    what ++ ": message is real prose",
    true,
    String.length(msg) > 80,
  );
  Alcotest.check(
    Alcotest.bool,
    what ++ ": message is not the generic fallback",
    false,
    contains(msg, "Proof term"),
  );
  Alcotest.check(
    Alcotest.bool,
    what ++ ": no unfilled format placeholder",
    false,
    contains(msg, "%s"),
  );
  /* Every colored child must be referenced by the message. */
  let (_, colorings) = doc.abstract;
  List.iter(
    ((_, id)) =>
      Alcotest.check(
        Alcotest.bool,
        what ++ ": message references its colored child " ++ Id.to_string(id),
        true,
        contains(msg, Id.to_string(id)),
      ),
    colorings,
  );
  if (needs_example) {
    Alcotest.check(
      Alcotest.bool,
      what ++ ": has at least one example",
      true,
      doc.examples != [],
    );
  };
  /* An example whose source failed to parse comes back as an empty
     segment, so this checks the example programs are real syntax. */
  List.iter(
    (ex: Web.ExplainThisForm.example) =>
      Alcotest.check(
        Alcotest.bool,
        what ++ ": example parses",
        true,
        ex.term != [],
      ),
    doc.examples,
  );
};

let test_proof_forms_documented = () =>
  List.iter(
    (cls: Proof.cls) =>
      List.iter(
        p =>
          check_doc(
            ~what=Proof.show_cls(cls),
            ~needs_example=!example_free(cls),
            Web.ProofDoc.single(p),
          ),
        representatives(cls),
      ),
    Proof.all_of_cls,
  );

/* The two restricted binders are Exp forms, but they are prover syntax and
   were equally undocumented. */
let test_restricted_binders_documented = () => {
  let ids = () => (Id.mk(), Id.mk(), Id.mk());
  let (pat_id, guard_id, body_id) = ids();
  check_doc(
    ~what="forall where",
    ~needs_example=true,
    Web.ForallExp.where_single(~pat_id, ~guard_id, ~body_id),
  );
  let (pat_id, guard_id, body_id) = ids();
  check_doc(
    ~what="fun where",
    ~needs_example=true,
    Web.FunWhereExp.single(~pat_id, ~guard_id, ~body_id),
  );
};

/* End-to-end: the cursor inspector's own path. `get_doc` in Colorings mode
   returns an EMPTY color map for a fallback message (`simple("Proof term")`
   was one), and a populated one for a real form whose children are
   referenced — so a non-empty map on every InfoProof in these programs is
   the assertion that the dispatch reaches ProofDoc rather than a stub.
   Between them the sources cover every step form that takes children. */
let proof_sources = [
  "theorem t = 1 + 4 == 5 proof eval 1 + 4 at 0 end; eval 5 == 5 at 0 end in 0",
  "theorem t = 1 + 1 == 2 proof rewrite 1 + 1 with 2 at 0 end in 0",
  "theorem t = (true && false) == (false && true) proof axiom and_comm at 0 on true && false end in 0",
  "theorem t = (true && false) == (false && true) proof axiomrev and_comm at 0 on false && true end in 0",
  "theorem t = forall x -> x == x proof forall x => axiom refl_eq at 0 on x == x end in 0",
  "theorem t = forall n: Int -> n == 1 ==> n == 1 proof assume n == 1 => axiom assume at 0 on n == 1 end in 0",
  "theorem t = forall n: Int -> false ==> n == n proof assume false => contradiction false end in 0",
  "theorem t = forall n: Int -> n == 1 ==> n == 2 ==> false proof assume n == 1 => assume n == 2 => contradiction n == 1 with n = 2 end in 0",
  "theorem t = forall x -> x == 1 proof have 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end => ? in 0",
  "theorem inv = forall w where w != 0 -> w / w == 1 proof ? in theorem t = forall n where n != 0 -> n / n == 1 proof generalize n => forall n => axiom inv at 0 on n / n end in 0",
  "let f = fun x where x != 0 -> 100 / x in theorem t = forall y -> f(y) == f(y) proof ? in 0",
  "let f = fun x -> x + 1 in theorem lem = forall n: Int -> n == 1 ==> f(2) == 3 proof ? in theorem g = f(2) == 3 proof axiom lem with n = 1 at 0 on f(2) end in 0",
  "let f = fun x -> x + 1 in theorem p = forall n: Int -> n + 0 == n proof ? in theorem g = f(2) == 3 proof revert p with n = 5 => ? in 0",
  "type Nt = +Z+S(Nt) in let pos = fun e -> case e | Z => true | S(b) => true end in theorem t = forall e: Nt -> pos(e) proof induction e | Z => eval pos(Z) at 0 end | S(b) => revert pos(b) => axiom ih at 0 on pos(b) end end in 0",
];

let parse_exp = (s: string) =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse: " ++ s)
  };

let test_cursor_on_proof_step_explains = () => {
  let seen = ref(0);
  List.iter(
    src => {
      let info_map = statics(parse_exp(src));
      Id.Map.iter(
        (_id, info: Info.t) =>
          switch (info) {
          | InfoProof({user_term, _}) =>
            switch (user_term.term) {
            /* The degenerate forms have no children to color. */
            | EmptyHole
            | MultiHole(_)
            | Invalid(_) => ()
            | _ =>
              incr(seen);
              let (_, (_, (color_map, _)), _) =
                Web.ExplainThis.get_doc(
                  ~globals,
                  ~docs,
                  Some(info),
                  Web.ExplainThis.Colorings,
                );
              Alcotest.check(
                Alcotest.bool,
                Proof.show_cls(Proof.cls_of_term(user_term.term))
                ++ ": cursor gets a message referencing its children",
                false,
                Id.Map.is_empty(color_map),
              );
            }
          | _ => ()
          },
        info_map,
      );
    },
    proof_sources,
  );
  Alcotest.check(
    Alcotest.bool,
    "the sources really contain proof steps",
    true,
    seen^ >= 20,
  );
};

let tests = (
  "ExplainThis",
  [
    QCheck_alcotest.to_alcotest(qcheck_explainthis_does_not_crash),
    Alcotest.test_case(
      "every prover form has a real message",
      `Quick,
      test_proof_forms_documented,
    ),
    Alcotest.test_case(
      "restricted binders documented",
      `Quick,
      test_restricted_binders_documented,
    ),
    Alcotest.test_case(
      "cursor on a proof step gets a real message",
      `Quick,
      test_cursor_on_proof_step_explains,
    ),
  ],
);
