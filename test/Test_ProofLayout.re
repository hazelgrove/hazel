open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* How theorems and their proofs are laid out: by the pretty printer over a
   whole program, and after an edit to a proof, which lays out only that
   theorem's proof. */

let layout = Test_PrettyPrint.test_format_seg;

let form_tests = [
  layout(
    ~name="a short proof stays on the theorem's line",
    ~input="theorem t = 1 == 1 proof eval in t",
    ~expected={|theorem t = 1 == 1 proof eval in
t|},
    (),
  ),
  layout(
    ~name="an unwritten proof stays on the theorem's line",
    ~input="theorem t = 1 == 1 proof ? in t",
    ~expected={|theorem t = 1 == 1 proof ? in
t|},
    (),
  ),
  layout(
    ~name="several steps go one per line, between `proof` and `in`",
    ~input="theorem t = 1 == 1 proof from 1 do eval end; eval in t",
    ~expected={|theorem t = 1 == 1 proof
  from 1 do eval end;
  eval
in
t|},
    (),
  ),
  layout(
    ~name="one step too long for the theorem's line goes below it",
    ~width=40,
    ~input=
      "theorem t = f(1) + g(2) == 3 proof from f(1) + g(2) do eval end in t",
    ~expected=
      {|theorem t = f(1) + g(2) == 3 proof
  from f(1) + g(2) do eval end
in
t|},
    (),
  ),
  layout(
    ~name="a statement too long for its line breaks as a let's would",
    ~width=40,
    ~input=
      "theorem t = some_function(1) + another(2) == the_result(3) proof eval in t",
    ~expected=
      {|theorem t =
  some_function(1) + another(2)
  == the_result(3)
proof
  eval
in
t|},
    (),
  ),
  layout(
    ~name=
      "a quantified statement keeps its first binder on the theorem's line",
    ~width=50,
    ~input=
      "theorem t = forall x: Int -> forall y: Int -> x + y == y + x proof eval in t",
    ~expected=
      {|theorem t = forall x : Int ->
  forall y : Int -> x + y == y + x
proof
  eval
in
t|},
    (),
  ),
  layout(
    ~name="a focus with several steps is a block",
    ~input=
      "theorem t = 1 == 1 proof from (2 + 3) * 2 do from 2 + 3 do eval end; eval end; eval in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  from (2 + 3) * 2 do
    from 2 + 3 do eval end;
    eval
  end;
  eval
in
t|},
    (),
  ),
  layout(
    ~name="a focus on a metavariable selector",
    ~input=
      "theorem t = 1 == 1 proof from 2nd $e([]) do eval end; from $e(a, h) do eval; eval end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  from 2nd $e([]) do eval end;
  from $e(a, h) do
    eval;
    eval
  end
in
t|},
    (),
  ),
  layout(
    ~name="a focus with one step too long for its line is a block",
    ~width=50,
    ~input=
      "theorem t = 1 == 1 proof from a_long_function(x, y + 1) do axiom a_long_lemma with n = f(1, 2) end end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  from a_long_function(x, y + 1) do
    axiom a_long_lemma with n = f(1, 2) end
  end
in
t|},
    (),
  ),
  layout(
    ~name="a wrapping form with a short body stays on one line",
    ~input=
      "theorem t = forall n: Int -> n == n proof forall n => axiom refl_eq end in t",
    ~expected=
      {|theorem t = forall n : Int -> n == n proof forall n => axiom refl_eq end in
t|},
    (),
  ),
  layout(
    ~name="a wrapping form's steps are indented below it, as a fun's body is",
    ~input=
      "theorem t = 1 == 1 proof forall n => from n + 0 do rewrite n end end; axiom refl_eq end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  forall n =>
    from n + 0 do rewrite n end end;
    axiom refl_eq end
in
t|},
    (),
  ),
  layout(
    ~name="wrapping forms nest",
    ~input=
      "theorem t = 1 == 1 proof alias h = assume => revert x with n = 2 => from y do axiom h with n = 2 end end; contradiction y == 1 with n = 2 + 3 end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  alias h = assume =>
    revert x with n = 2 =>
      from y do axiom h with n = 2 end end;
      contradiction y == 1 with n = 2 + 3 end
in
t|},
    (),
  ),
  layout(
    ~name="a wrapping form after other steps takes the steps after it",
    ~input="theorem t = 1 == 1 proof eval; forall x => eval; eval in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  eval;
  forall x =>
    eval;
    eval
in
t|},
    (),
  ),
  layout(
    ~name="a wrapping form with an unwritten body leaves it a line",
    ~input="theorem t = 1 == 1 proof forall n => ? in t",
    ~expected={|theorem t = 1 == 1 proof
  forall n =>
    ?
in
t|},
    (),
  ),
  layout(
    ~name="a have's proof is a block, closed by `=>`",
    ~input=
      "theorem t = 1 == 1 proof assume p as h => have q proof from q do axiom h end end; eval => axiom refl_eq end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  assume p as h =>
    have q proof
      from q do axiom h end end;
      eval
    =>
      axiom refl_eq end
in
t|},
    (),
  ),
  layout(
    ~name="a have with a one-step proof keeps it on its line",
    ~input=
      "theorem t = 1 == 1 proof have q proof eval => eval; axiom refl_eq end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  have q proof eval =>
    eval;
    axiom refl_eq end
in
t|},
    (),
  ),
  layout(
    ~name="induction is laid out as case is",
    ~input=
      "theorem t = 1 == 1 proof induction xs | [] => eval | y :: t => from rev(y :: t) do axiom rev_cons end end; axiom refl_eq end end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  induction xs
  | [] => eval
  | y :: t =>
    from rev(y :: t) do axiom rev_cons end end;
    axiom refl_eq end
  end
in
t|},
    (),
  ),
  layout(
    ~name="induction with one short case still gives it a line",
    ~input="theorem t = 1 == 1 proof induction f(b) | true => eval end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  induction f(b)
  | true => eval
  end
in
t|},
    (),
  ),
  layout(
    ~name="induction-as keeps its scrutinee and name on one line",
    ~input=
      "theorem t = 1 == 1 proof induction is_value(x) as h | true => from b do axiom h end end | false => ? end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  induction is_value(x) as h
  | true => from b do axiom h end end
  | false => ?
  end
in
t|},
    (),
  ),
  layout(
    ~name="an induction inside a case of another",
    ~input=
      "theorem t = 1 == 1 proof induction xs | [] => induction ys | [] => eval | z :: zs => eval; eval end | y :: t => eval end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  induction xs
  | [] =>
    induction ys
    | [] => eval
    | z :: zs =>
      eval;
      eval
    end
  | y :: t => eval
  end
in
t|},
    (),
  ),
  layout(
    ~name="a wrapped induction, its cases wrapped in turn",
    ~input=
      "theorem t = 1 == 1 proof generalize acc => induction xs | [] => forall acc => eval; eval | y :: t => forall acc => eval end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  generalize acc =>
    induction xs
    | [] =>
      forall acc =>
        eval;
        eval
    | y :: t => forall acc => eval
    end
in
t|},
    (),
  ),
  layout(
    ~name="a citation too long for its line breaks before its keywords",
    ~width=30,
    ~input=
      "theorem t = 1 == 1 proof axiom a_long_lemma_name with n = some_function(1, 2) end in t",
    ~expected=
      {|theorem t = 1 == 1 proof
  axiom a_long_lemma_name
  with n
  = some_function(1, 2)
  end
in
t|},
    (),
  ),
  layout(
    ~name="a parenthesized proof keeps its `;`",
    ~input="theorem t = 1 == 1 proof (forall x => eval); eval in t",
    ~expected={|theorem t = 1 == 1 proof
  (forall x => eval);
  eval
in
t|},
    (),
  ),
  layout(
    ~name="a theorem in an indented position",
    ~input=
      "let f = fun x -> theorem t = 1 == 1 proof from 1 do eval end; eval in t in f",
    ~expected=
      {|let f = fun x ->
  theorem t = 1 == 1 proof
    from 1 do eval end;
    eval
  in
  t in
f|},
    (),
  ),
];

/* A hand-formatted program of theorems about lists. It has no `$e`
   selectors: the fast parser doesn't read them, and the typing parser
   takes seconds on a program this long. */
let examples = {|let rev = fun xs -> case xs | [] => [] | x :: t => rev(t) @ [x] end in
let total = fun prices -> case prices | [] => 0 | p :: rest => p + total(rest) end in
let add = fun (acc, p) -> acc + p in

# Warm-ups #
theorem ordinals = (1 + 1) * (1 + 1) == 4 proof
  from 2nd 1 + 1 do eval end;
  from 1 + 1 do eval end;
  from 2 * 2 do eval end;
  eval
in
theorem nested = (2 + 3) * 2 == 10 proof
  from (2 + 3) * 2 do
    from 2 + 3 do eval end;
    eval
  end;
  eval
in
theorem algebra = 4 / 2 == 2 proof
  from 4 / 2 do rewrite 2 end end;
  eval
in
# Under a binder x is a fixed, arbitrary Int. #
theorem under_fun = (fun x: Int -> x + 0) == (fun x: Int -> x) proof
  from x + 0 do rewrite x end end;
  axiom refl_eq end
in

# Lists #
theorem rev_nil = rev([]) == [] proof
  from rev([]) do eval end;
  axiom refl_eq end
in
theorem rev_cons = forall x: Int -> forall t: [Int] -> rev(x :: t) == rev(t) @ [x] proof
  from rev(x :: t) do eval end;
  axiom refl_eq end
in
theorem app_cons = forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m) proof
  from (y :: l) @ m do eval end;
  axiom refl_eq end
in
theorem snoc = forall xs: [Int] -> forall x: Int -> rev(xs @ [x]) == x :: rev(xs) proof
  induction xs
  | [] =>
    from rev([]) do axiom rev_nil end end;
    from rev([] @ [x]) do
      from [] @ [x] do eval end;
      eval;
      from rev([]) do eval end;
      eval
    end;
    from x :: [] do eval end;
    axiom refl_eq end
  | y :: t =>
    from (y :: t) @ [x] do axiom app_cons end end;
    from rev(y :: (t @ [x])) do axiom rev_cons end end;
    from rev(t @ [x]) do axiom ih end end;
    from (x :: rev(t)) @ [y] do axiom app_cons end end;
    from rev(y :: t) do axiom rev_cons end end;
    axiom refl_eq end
  end
in
theorem invol = forall xs: [Int] -> rev(rev(xs)) == xs proof
  induction xs
  | [] =>
    from rev([]) do axiom rev_nil end end;
    from rev([]) do axiom rev_nil end end;
    axiom refl_eq end
  | x :: t =>
    from rev(x :: t) do axiom rev_cons end end;
    from rev(rev(t) @ [x]) do axiom snoc end end;
    from rev(rev(t)) do axiom ih end end;
    axiom refl_eq end
  end
in

# A simple total and a fold-based fast total agree #
theorem total_nil = total([]) == 0 proof
  from total([]) do eval end;
  axiom refl_eq end
in
theorem total_cons = forall p: Int -> forall rest: [Int] -> total(p :: rest) == p + total(rest) proof
  from total(p :: rest) do eval end;
  axiom refl_eq end
in
theorem fold_nil = forall a: Int -> fold_left([], add, a) == a proof
  from fold_left([], add, a) do eval end;
  axiom refl_eq end
in
theorem fold_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, add, a) == fold_left(t, add, a + h) proof
  from fold_left(h :: t, add, a) do eval; from add(a, h) do eval end; eval end;
  from fold_left(t, add, a + h) do eval end;
  axiom refl_eq end
in
theorem shuffle = forall a: Int -> forall b: Int -> forall c: Int -> a + (b + c) == (a + b) + c proof
  from a + (b + c) do rewrite (a + b) + c end end;
  axiom refl_eq end
in
theorem total_acc = forall prices: [Int] -> forall acc: Int -> acc + total(prices) == fold_left(prices, add, acc) proof
  generalize acc => induction prices
  | [] => forall acc =>
    from total([]) do axiom total_nil end end;
    from fold_left([], add, acc) do axiom fold_nil end end;
    from acc + 0 do rewrite acc end end;
    axiom refl_eq end
  | p :: rest => forall acc =>
    from total(p :: rest) do axiom total_cons end end;
    from fold_left(p :: rest, add, acc) do axiom fold_cons end end;
    from acc + (p + total(rest)) do axiom shuffle end end;
    from fold_left(rest, add, acc + p) do axiomrev ih end end;
    axiom refl_eq end
  end
in
theorem totals_agree = forall prices: [Int] -> total(prices) == fold_left(prices, add, 0) proof
  from fold_left(prices, add, 0) do axiomrev total_acc end end;
  from 0 + total(prices) do rewrite total(prices) end end;
  axiom refl_eq end
in

# To try: #
theorem app_assoc = forall xs: [Int] -> forall ys: [Int] -> forall zs: [Int] -> (xs @ ys) @ zs == xs @ (ys @ zs) proof
  induction xs
  | [] => ?
  | x :: t => ?
  end
in
0|};

/* The theorems of `examples` whose proofs are written as the pretty printer
   lays them out. */
let canonical_theorems = [
  "ordinals",
  "nested",
  "algebra",
  "under_fun",
  "rev_nil",
  "rev_cons",
  "app_cons",
  "snoc",
  "invol",
  "total_nil",
  "total_cons",
  "fold_nil",
  "shuffle",
  "totals_agree",
  "app_assoc",
];

let fold_cons_proof = {|proof
  from fold_left(h :: t, add, a) do
    eval;
    from add(a, h) do eval end;
    eval
  end;
  from fold_left(t, add, a + h) do eval end;
  axiom refl_eq end
in|};

let total_acc_proof = {|proof
  generalize acc =>
    induction prices
    | [] =>
      forall acc =>
        from total([]) do axiom total_nil end end;
        from fold_left([], add, acc) do axiom fold_nil end end;
        from acc + 0 do rewrite acc end end;
        axiom refl_eq end
    | p :: rest =>
      forall acc =>
        from total(p :: rest) do axiom total_cons end end;
        from fold_left(p :: rest, add, acc) do axiom fold_cons end end;
        from acc + (p + total(rest)) do axiom shuffle end end;
        from fold_left(rest, add, acc + p) do axiomrev ih end end;
        axiom refl_eq end
    end
in|};

let check_contains = Test_EditorTransform.check_contains;

/* Parses as the editor does when it loads text, which takes a fraction of
   the time that typing the examples in does. */
let parse_zipper = (text: string): Haz3lcore.Zipper.t =>
  switch (
    Haz3lcore.PersistentZipper.parse_text(~source="test", ~root=Exp, text)
  ) {
  | Some(z) => z
  | None => Alcotest.fail("Failed to parse: " ++ text)
  };

let examples_zipper = lazy(parse_zipper(examples));

let format = (z: Haz3lcore.Zipper.t): string =>
  Haz3lcore.Zipper.unselect_and_zip(z)
  |> Haz3lcore.PrettySegment.prettify
  |> Haz3lcore.Printer.of_segment(~holes="?");

let formatted_examples = lazy(format(Lazy.force(examples_zipper)));

let examples_tests = [
  test_case(
    "formatting the examples twice changes nothing",
    `Quick,
    () => {
      let once = Lazy.force(formatted_examples);
      check(string, "idempotent", once, format(parse_zipper(once)));
    },
  ),
  test_case("the formatted examples parse to the same program", `Quick, () =>
    check(
      dhexp_typ,
      "same term",
      Test_EditorTransform.zipper_term(Lazy.force(examples_zipper)),
      Test_EditorTransform.zipper_term(
        parse_zipper(Lazy.force(formatted_examples)),
      ),
    )
  ),
  test_case(
    "the examples' proofs, formatted",
    `Quick,
    () => {
      let formatted = Lazy.force(formatted_examples);
      /* A statement that fits its line leaves the proof as it was written. */
      check_contains(
        ~msg="nested",
        formatted,
        {|theorem nested = (2 + 3) * 2 == 10 proof
  from (2 + 3) * 2 do
    from 2 + 3 do eval end;
    eval
  end;
  eval
in|},
      );
      check_contains(
        ~msg="snoc",
        formatted,
        {|theorem snoc = forall xs : [Int] ->
  forall x : Int -> rev(xs @ [x]) == x :: rev(xs)
proof
  induction xs
  | [] =>
    from rev([]) do axiom rev_nil end end;
    from rev([] @ [x]) do
      from [] @ [x] do eval end;
      eval;
      from rev([]) do eval end;
      eval
    end;
    from x :: [] do eval end;
    axiom refl_eq end
  | y :: t =>
    from (y :: t) @ [x] do axiom app_cons end end;
    from rev(y :: (t @ [x])) do axiom rev_cons end end;
    from rev(t @ [x]) do axiom ih end end;
    from (x :: rev(t)) @ [y] do axiom app_cons end end;
    from rev(y :: t) do axiom rev_cons end end;
    axiom refl_eq end
  end
in|},
      );
      check_contains(~msg="fold_cons", formatted, fold_cons_proof);
      check_contains(~msg="total_acc", formatted, total_acc_proof);
      check_contains(
        ~msg="app_assoc",
        formatted,
        {|proof
  induction xs
  | [] => ?
  | x :: t => ?
  end
in
0|},
      );
    },
  ),
];

// Edits

module Edit = {
  let zipper_term = Test_EditorTransform.zipper_term;
  let serialize = Test_EditorTransform.serialize;

  /* The proof of the theorem named `name`. */
  let proof_of = (name: string, e: Exp.t): Proof.t => {
    let found = ref(None);
    let f_exp = (continue, e: Exp.t): Exp.t => {
      switch (e.term) {
      | Theorem({term: Var(x), _}, _, proof, _) when x == name =>
        found := Some(proof)
      | _ => ()
      };
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, e);
    switch (found^) {
    | Some(proof) => proof
    | None => Alcotest.fail("no theorem named " ++ name)
    };
  };

  let rec last_step = (p: Proof.t): Proof.t =>
    switch (p.term) {
    | Seq(_, rest) => last_step(rest)
    | _ => p
    };

  /* The last step of the `i`th case of an induction. */
  let last_of_case = (i: int, p: Proof.t): Proof.t =>
    switch (p.term) {
    | Induction(_, _, cases) => last_step(snd(List.nth(cases, i)))
    | _ => Alcotest.fail("not an induction")
    };

  /* Replaces the node `target` picks from the proof of `name`. */
  let patch =
      (
        ~target: Proof.t => Proof.t=p => p,
        ~replacement: Proof.t => Proof.t,
        name: string,
        z: Haz3lcore.Zipper.t,
      )
      : Haz3lcore.Zipper.t => {
    let target = target(proof_of(name, zipper_term(z)));
    Haz3lcore.EditorTransform.apply_patch(
      z,
      Haz3lcore.EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(target),
        replacement(target),
      ),
    );
  };

  /* What the stepper does to take a step at the end of a proof. */
  let add_step = (step: Proof.t) =>
    patch(~target=last_step, ~replacement=last =>
      Proof.fresh(Seq(last, step))
    );

  let eval_step = () => Proof.fresh(EvalStep);

  /* `text` with the first `before` replaced by `after`. */
  let replace_first = (~before: string, ~after: string, text: string) =>
    Str.replace_first(Str.regexp_string(before), after, text);

  let tests = [
    test_case(
      "laying out a proof that is already laid out changes nothing",
      `Quick,
      () => {
        let z = Lazy.force(examples_zipper);
        List.iter(
          name =>
            check(
              string,
              name,
              serialize(z),
              serialize(patch(~replacement=p => p, name, z)),
            ),
          canonical_theorems,
        );
      },
    ),
    test_case(
      "an edit that changes nothing keeps the proof's hand layout",
      `Quick,
      () => {
        /* The examples write fold_cons's first focus on one line, where
           the printer would make it a block. */
        let z = Lazy.force(examples_zipper);
        check(
          string,
          "fold_cons",
          serialize(z),
          serialize(patch(~replacement=p => p, "fold_cons", z)),
        );
      },
    ),
    test_case(
      "a step added to a proof of several adds one line",
      `Quick,
      () => {
        let z = Lazy.force(examples_zipper);
        check(
          string,
          "ordinals",
          replace_first(
            ~before="  from 2 * 2 do eval end;\n  eval\nin",
            ~after="  from 2 * 2 do eval end;\n  eval;\n  eval\nin",
            serialize(z),
          ),
          serialize(add_step(eval_step(), "ordinals", z)),
        );
      },
    ),
    test_case(
      "a step added inside an induction's case adds one line",
      `Quick,
      () => {
        let z = Lazy.force(examples_zipper);
        check(
          string,
          "invol",
          replace_first(
            ~before="    axiom refl_eq end\n  | x :: t =>",
            ~after="    axiom refl_eq end;\n    eval\n  | x :: t =>",
            serialize(z),
          ),
          serialize(
            patch(
              ~target=last_of_case(0),
              ~replacement=last => Proof.fresh(Seq(last, eval_step())),
              "invol",
              z,
            ),
          ),
        );
      },
    ),
    test_case(
      "a removed step takes its line with it",
      `Quick,
      () => {
        let z = Lazy.force(examples_zipper);
        let step = last_of_case(1, proof_of("snoc", zipper_term(z)));
        check(
          string,
          "snoc",
          replace_first(
            ~before=
              "    from rev(y :: t) do axiom rev_cons end end;\n    axiom refl_eq end\n  end",
            ~after="    from rev(y :: t) do axiom rev_cons end end\n  end",
            serialize(z),
          ),
          serialize(
            Haz3lcore.EditorTransform.apply_patch(
              z,
              Haz3lcore.EditorTransform.mk_proof_remove_patch(
                ~target_id=Proof.rep_id(step),
              ),
            ),
          ),
        );
      },
    ),
    test_case(
      "a first step stays on the theorem's line, a second makes a block",
      `Quick,
      () => {
        let z = parse_zipper("theorem t = 1 == 1 proof ? in t");
        let z = patch(~replacement=_ => eval_step(), "t", z);
        check(
          string,
          "one step",
          "theorem t = 1 == 1 proof eval in t",
          serialize(z),
        );
        let z = add_step(eval_step(), "t", z);
        check(
          string,
          "two steps",
          "theorem t = 1 == 1 proof\n  eval;\n  eval\nin t",
          serialize(z),
        );
      },
    ),
    test_case(
      "a proof is indented from where its theorem sits",
      `Quick,
      () => {
        let z =
          parse_zipper(
            "let f = fun x ->\n  theorem t = 1 == 1 proof eval in\n  t\nin f",
          );
        check(
          string,
          "nested theorem",
          "let f = fun x ->\n  theorem t = 1 == 1 proof\n    eval;\n    eval\n  in\n  t\nin f",
          serialize(add_step(eval_step(), "t", z)),
        );
      },
    ),
  ];
};

/* What each stepper write leaves of the text the user wrote: comments
   and hand layout around and inside the edited steps stay, and only the
   new text is laid out. Each test checks the whole text after the write,
   that every comment survives, and that the text parses to the program
   the write makes. */
module KeepText = {
  module Patches = Web.StepperPicker.Patches;
  module Terms = Web.StepperPicker.Terms;
  module Exits = Web.Obligations.Exits;
  module Rewrite = Haz3lcore.EditorTransform.Rewrite;
  let serialize = Edit.serialize;
  let zipper_term = Edit.zipper_term;

  let program = {|# about t #
theorem t = forall xs: [Int] -> rev(rev(xs)) == xs proof
  # first #
  induction xs  # scrut #
  | [] =>
    from rev([]) do axiom rev_nil end end;  # base one #
    from rev([]) do  axiom rev_nil  end end;   axiom refl_eq end
  | x :: t =>
    # step #
    from rev(x :: t) do axiom rev_cons end end;
    from rev(rev(t)) do
      axiom ih end;   # in focus #
      eval
    end;
    axiom refl_eq end  # last #
  end
in t|};

  let rec steps = (p: Proof.t): list(Proof.t) =>
    switch (p.term) {
    | Seq(a, b) => steps(a) @ steps(b)
    | _ => [p]
    };

  let induction = (z): Proof.t => Edit.proof_of("t", zipper_term(z));

  /* The steps of the induction's `i`th case. */
  let case = (i: int, z): list(Proof.t) =>
    switch (induction(z).term) {
    | Induction(_, _, cases) => steps(snd(List.nth(cases, i)))
    | _ => Alcotest.fail("not an induction")
    };

  let row = (kind: Web.StepperRows.kind, step: Proof.t): Web.StepperRows.t => {
    key: (Proof.rep_id(step), kind == End ? Outgoing : Incoming),
    step,
    kind,
    entry: None,
    goal: None,
    acts_on: None,
    continues: true,
  };

  let comments = (text: string): list(string) => {
    let rec from = pos =>
      switch (String.index_from_opt(text, pos, '#')) {
      | None => []
      | Some(start) =>
        switch (String.index_from_opt(text, start + 1, '#')) {
        | None => []
        | Some(stop) => [
            String.sub(text, start, stop - start + 1),
            ...from(stop + 1),
          ]
        }
      };
    from(0) |> List.sort(compare);
  };

  /* The program the patch makes, printed fresh and parsed back. */
  let patched = (z, patch: Haz3lcore.EditorTransform.patch): Exp.t => {
    let root = zipper_term(z);
    let proof = (target_id, replacement) =>
      Rewrite.proof_in_exp(~target_id, replacement, root);
    let new_root =
      switch (patch) {
      | ExpPatch({target_id, replacement, _}) =>
        Rewrite.exp(~target_id, replacement, root) |> Option.map(snd)
      | ProofPatch({target_id, replacement, _}) =>
        proof(target_id, replacement) |> Option.map(snd)
      | ProofRemovePatch({target_id}) =>
        let (target_id, replacement) =
          Rewrite.step_removal(
            ~target_id,
            Rewrite.seq_parent_in_exp(~target_id, root),
          );
        proof(target_id, replacement) |> Option.map(snd);
      };
    switch (new_root) {
    | Some(e) =>
      e
      |> Haz3lcore.ExpToSegment.exp_to_segment(
           ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
         )
      |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[])
      |> parse_exp
    | None => Alcotest.fail("the patch's target isn't in the program")
    };
  };

  let check_write =
      (
        ~src=program,
        ~lost: list(string)=[],
        ~expected: string,
        patch: Haz3lcore.Zipper.t => Haz3lcore.EditorTransform.patch,
      ) => {
    let z = parse_zipper(src);
    let patch = patch(z);
    let out = serialize(Haz3lcore.EditorTransform.apply_patch(z, patch));
    check(string, "text", expected, out);
    check(
      list(string),
      "comments",
      List.filter(c => !List.mem(c, lost), comments(src)),
      comments(out),
    );
    check(
      bool,
      "parses to the patched program",
      true,
      Exp.fast_equal(patched(z, patch), parse_exp(out)),
    );
  };

  let eval = () => Proof.fresh(EvalStep);

  let tests = [
    test_case("Enter after a case's last step adds one line", `Quick, () =>
      check_write(
        ~expected=
          Edit.replace_first(
            ~before="   axiom refl_eq end\n",
            ~after="   axiom refl_eq end;\n    eval\n",
            program,
          ),
        z =>
        Patches.write(row(End, Util.ListUtil.last(case(0, z))), eval())
      )
    ),
    test_case(
      "Enter after a line ending in a comment leaves the comment there",
      `Quick,
      () =>
      check_write(
        ~expected=
          Edit.replace_first(
            ~before="axiom refl_eq end  # last #\n",
            ~after="axiom refl_eq end;  # last #\n    eval\n",
            program,
          ),
        z =>
        Patches.write(row(End, Util.ListUtil.last(case(1, z))), eval())
      )
    ),
    test_case("Tab before a step adds one line above it", `Quick, () =>
      check_write(
        ~expected=
          Edit.replace_first(
            ~before="    from rev(rev(t)) do\n",
            ~after="    eval;\n    from rev(rev(t)) do\n",
            program,
          ),
        z =>
        Patches.write(row(Step(Leaf), List.nth(case(1, z), 1)), eval())
      )
    ),
    test_case("a step joined to a focus's steps adds one line", `Quick, () =>
      check_write(
        ~expected=
          Edit.replace_first(
            ~before="      eval\n    end;",
            ~after="      eval;\n      eval\n    end;",
            program,
          ),
        z =>
        switch (List.nth(case(1, z), 1).term) {
        | Focus({body, _}) => Patches.join(body, eval())
        | _ => Alcotest.fail("not a focus")
        }
      )
    ),
    test_case(
      "a strategy's wrapping form goes in above the step and its comment",
      `Quick,
      () =>
      check_write(
        ~expected=
          Edit.replace_first(
            ~before="  | x :: t =>\n",
            ~after=
              "  | x :: t =>\n    (\n      generalize t =>\n        ?\n    );\n",
            program,
          ),
        z =>
        Patches.write(
          row(Step(Leaf), List.hd(case(1, z))),
          Proof.fresh(Terms.generalize(~exp=Exp.fresh(Var("t")))),
        )
      )
    ),
    test_case("removing a step removes it and its `;` only", `Quick, () =>
      check_write(
        ~expected=
          Edit.replace_first(
            ~before="\n    from rev([]) do  axiom rev_nil  end end;   axiom",
            ~after="\n    axiom",
            program,
          ),
        z =>
        Patches.remove(List.nth(case(0, z), 1))
      )
    ),
    test_case("removing a chain's last step removes its `;`", `Quick, () =>
      check_write(
        ~expected=
          Edit.replace_first(
            ~before="    end;\n    axiom refl_eq end  # last #\n",
            ~after="    end  # last #\n",
            program,
          ),
        z =>
        Patches.remove(Util.ListUtil.last(case(1, z)))
      )
    ),
    test_case("Case adds a case and keeps the others as written", `Quick, () =>
      check_write(
        ~expected=
          Edit.replace_first(
            ~before="  # last #\n  end",
            ~after="  # last #\n  | ? => ?\n  end",
            program,
          ),
        z =>
        switch (
          Patches.add_case(
            induction(z),
            ~marks=[Language.ProofMark.InductionNotExhaustive],
          )
        ) {
        | Some(patch) => patch
        | None => Alcotest.fail("no case to add")
        }
      )
    ),
    test_case(
      "a strategy written in place of a hole keeps the text around it",
      `Quick,
      () =>
      check_write(
        ~src="theorem t = 1 == 1 proof  # todo #\n  ?\nin t",
        ~expected=
          "theorem t = 1 == 1 proof  # todo #\n  induction ? end\nin t",
        z =>
        Patches.write(
          row(Hole, Edit.proof_of("t", zipper_term(z))),
          Proof.fresh(Terms.induction(~scrut=None)),
        )
      )
    ),
    test_case("a second step turns a one-line proof into a block", `Quick, () =>
      check_write(
        ~src="theorem t = 1 == 1 proof  eval # done # in t",
        ~expected="theorem t = 1 == 1 proof\n  eval;\n  eval # done #\nin t",
        z =>
        Patches.write(row(End, Edit.proof_of("t", zipper_term(z))), eval())
      )
    ),
    test_case("Tab keeps steps the user put on one line", `Quick, () =>
      check_write(
        ~src=
          "theorem t = (1 + 1) * (1 + 1) == 4 proof\n  from 2nd 1 + 1 do eval end;   from 1 + 1 do eval end;\n  eval\nin t",
        ~expected=
          "theorem t = (1 + 1) * (1 + 1) == 4 proof\n  from 2nd 1 + 1 do eval end;   from 1 + 1 do eval end;\n  eval;\n  eval\nin t",
        z =>
        Patches.write(
          row(End, Edit.last_step(Edit.proof_of("t", zipper_term(z)))),
          eval(),
        )
      )
    ),
    test_case(
      "flipping an axiom's direction rewrites only its keyword", `Quick, () =>
      check_write(
        ~src="theorem t = 1 == 1 proof\n  axiom  # which #  refl_eq end\nin t",
        ~expected=
          "theorem t = 1 == 1 proof\n  axiomrev  # which #  refl_eq end\nin t",
        z => {
          let step = Edit.proof_of("t", zipper_term(z));
          switch (step.term) {
          | AxiomStep(a) =>
            Haz3lcore.EditorTransform.mk_proof_patch(
              ~target_id=Proof.rep_id(step),
              {
                ...step,
                term:
                  AxiomStep({
                    ...a,
                    direction: Util.Direction.toggle(a.direction),
                  }),
              },
            )
          | _ => Alcotest.fail("not an axiom step")
          };
        },
      )
    ),
    test_case("a rebuilt node keeps its steps' literal spellings", `Quick, () =>
      check_write(
        ~src=
          "theorem t = forall b: Bool -> 1 == 1 proof\n  induction b\n  | true => from 007 + 1 do eval end\n  end\nin t",
        ~expected=
          "theorem t = forall b: Bool -> 1 == 1 proof\n  induction b\n  | true => from 007 + 1 do eval end\n  | ? => ?\n  end\nin t",
        z =>
        switch (
          Patches.add_case(
            Edit.proof_of("t", zipper_term(z)),
            ~marks=[Language.ProofMark.InductionNotExhaustive],
          )
        ) {
        | Some(patch) => patch
        | None => Alcotest.fail("no case to add")
        }
      )
    ),
    test_case(
      "the caret stays by the text it was at",
      `Quick,
      () => {
        let with_caret = (z: Haz3lcore.Zipper.t) =>
          Haz3lcore.Printer.of_zipper(~holes="?", ~caret="¦", z);
        let at = (step: Proof.t, z) =>
          switch (
            Haz3lcore.Move.jump_to_side_of_id(Left, z, Proof.rep_id(step))
          ) {
          | Some(z) => z
          | None => Alcotest.fail("can't put the caret at the step")
          };
        let write = z =>
          Haz3lcore.EditorTransform.apply_patch(
            z,
            Patches.write(
              row(End, Util.ListUtil.last(case(0, z))),
              eval(),
            ),
          );
        let z = parse_zipper(program);
        let edited = Util.ListUtil.last(case(0, z));
        let later = List.nth(case(1, z), 1);
        List.iter(
          ((name, step, before)) =>
            check_contains(
              ~msg=name,
              with_caret(write(at(step, z))),
              "¦" ++ before,
            ),
          [
            (
              "at the step written after",
              edited,
              "axiom refl_eq end;\n    eval",
            ),
            ("at a later step", later, "from rev(rev(t)) do"),
          ],
        );
      },
    ),
  ];

  // Side-condition exits

  module ET = Test_EditorTransform;

  let exits_program = {|let f = fun n ->
  # the body #
  8  /  n
in
theorem t = forall n: Int ->  # any n #
  8 / n == 8 / n proof
  # first #
  from 8 / n do eval end;  # one #
  eval
in t|};

  let first_eval_origin = z =>
    switch (ET.find_proofs(ET.is_eval_step, ET.exits_ctx_of(z).proof)) {
    | [first, ..._] => Proof.rep_id(first)
    | [] => Alcotest.fail("no eval steps")
    };

  let exit_tests = [
    test_case("the have exit wraps the steps and keeps their text", `Quick, () =>
      check_write(
        ~src=exits_program,
        ~expected=
          Edit.replace_first(
            ~before=
              "proof\n  # first #\n  from 8 / n do eval end;  # one #\n  eval\n",
            ~after=
              "proof\n  have n != 0 proof ? =>\n    # first #\n    from 8 / n do eval end;  # one #\n    eval\n",
            exits_program,
          ),
        z =>
        ET.require_patch(
          "have",
          Exits.have_patch(
            ~ctx=ET.exits_ctx_of(z),
            ET.mk_obligation(~origin=first_eval_origin(z), ~goal="n != 0"),
          ),
        )
      )
    ),
    test_case(
      "the split exit wraps the steps and keeps their text", `Quick, () =>
      check_write(
        ~src=exits_program,
        ~expected=
          Edit.replace_first(
            ~before="  from 8 / n do eval end;  # one #\n  eval\n",
            ~after=
              "  induction n != 0\n  | true =>\n    from 8 / n do eval end;  # one #\n    eval\n  | false => ?\n  end\n",
            exits_program,
          ),
        z =>
        ET.require_patch(
          "split",
          Exits.split_patch(
            ~ctx=ET.exits_ctx_of(z),
            ET.mk_obligation(~origin=first_eval_origin(z), ~goal="n != 0"),
          ),
        )
      )
    ),
    test_case("the float exit restricts the binder in place", `Quick, () =>
      check_write(
        ~src=exits_program,
        ~expected=
          Edit.replace_first(
            ~before="forall n: Int ->",
            ~after="forall n: (Int) where n != 0 ->",
            exits_program,
          ),
        z => {
          let ctx = ET.exits_ctx_of(z);
          ET.require_patch(
            "float",
            Exits.float_patch(
              ~ctx,
              ET.mk_obligation(
                ~origin=Proof.rep_id(ctx.proof),
                ~goal="n != 0",
              ),
            ),
          );
        },
      )
    ),
    test_case(
      "the contract exit keeps the function's body as written", `Quick, () =>
      check_write(
        ~src=exits_program,
        ~expected=
          Edit.replace_first(
            ~before="fun n ->",
            ~after="fun n where n != 0 ->",
            exits_program,
          ),
        z => {
          let func = List.hd(ET.find_funs(zipper_term(z)));
          switch (
            Exits.for_contract(
              ~func=Some(func),
              ET.mk_obligation(~origin=Exp.rep_id(func), ~goal="n != 0"),
            )
          ) {
          | [{patch: Some(patch), _}, ..._] => patch
          | _ => Alcotest.fail("no contract exit")
          };
        },
      )
    ),
  ];
};

let tests = [
  ("ProofLayout.Forms", form_tests),
  ("ProofLayout.Examples", examples_tests),
  ("ProofLayout.Edits", Edit.tests),
  ("ProofLayout.KeepText", KeepText.tests @ KeepText.exit_tests),
];
