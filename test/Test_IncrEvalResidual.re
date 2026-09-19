open Alcotest;

/* Indeterminate residuals under caching.
 *
 * A program mid-edit is normally indeterminate -- that is the state a
 * language with holes spends most of its time in, and precisely the state
 * incremental evaluation runs against. Its result is a RESIDUAL: an
 * expression, not a value, and one that only means anything relative to the
 * environment it was produced in.
 *
 * `Transition.wrap_closure_when_done` decides where that environment lives.
 * A node evaluated as the direct child of a Closure hands its result back
 * BARE and signals, through the out-of-band `in_closure` callback, that the
 * enclosing Closure must keep supplying the environment; the same node
 * evaluated anywhere else wraps the environment into the value itself. A
 * cache entry records one of those two forms and a later run replays it
 * without re-running the transition, so the signal cannot be re-raised.
 *
 * Test_IncrEval's `PIN finding 3` pins the original instance. This file
 * broadens it: the same oracle (every calculus must agree with a0 on value,
 * samples, test results and step timeline at every step) over a corpus of
 * residual shapes, because one program is not enough to keep a fix honest.
 * a0 is the reference; aPL is Calculus.default, so a divergence here is a
 * bug in shipped Hazel, not in the experimental calculi. */

let check = Test_IncrEval.check_full_sequence;
let lit = Test_IncrEval.lit;
let id_edit = Test_IncrEval.id_edit;

/* Programs that go indeterminate and whose residual still refers to
 * something the environment binds. Each is replayed with the cache threaded;
 * `id_edit` steps re-evaluate an unchanged program, which is enough on its
 * own to expose a context-dependent cached value. */
let residual_corpus = [
  (
    "free variable in a let rhs, tuple pattern against unit",
    "let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
let (v2, v3) = () in
f0(f0(f0(23)))",
    [id_edit, id_edit],
  ),
  (
    "residual mentions a let-bound value rather than a function",
    "let u = 5 in
let w = k1 in
let (v2, v3) = () in
u + 7",
    [id_edit, id_edit],
  ),
  (
    "two indeterminate lets in a row",
    "let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
let (a, b) = () in
let (c, d) = () in
f0(1)",
    [id_edit],
  ),
  (
    "the free variable is itself in the residual",
    "let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
let (v2, v3) = () in
f0(k1)",
    [id_edit],
  ),
  (
    "residual under a function definition made later",
    "let w = k1 in
let f0 : Int -> Int = fun n -> n + 1 in
let (v2, v3) = () in
f0(f0(2))",
    [id_edit],
  ),
  (
    "determinate first, then edited into a residual, then unchanged",
    "let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
let (v2, v3) = (1, 2) in
f0(f0(f0(23)))",
    [Test_IncrEval.empty_the_tuple, id_edit, id_edit],
  ),
  (
    "residual inside a tuple that is itself returned",
    "let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
let (v2, v3) = () in
(f0(1), f0(2))",
    [id_edit],
  ),
  (
    "free variable applied, so the residual holds an application",
    "let f0 : Int -> Int = fun n -> 3 in
let w = g0(1) in
let (v2, v3) = () in
f0(f0(4))",
    [id_edit],
  ),
  (
    "case with no matching rule leaves a residual",
    "let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
case ()
  | (a, b) => f0(a)
end",
    [id_edit],
  ),
  (
    "residual produced inside a function body",
    "let mk : Int -> Int =
  fun m -> let w = k1 in let (a, b) = () in m + 1 in
mk(3)",
    [id_edit],
  ),
  (
    "edits around a stable residual",
    "let f0 : Int -> Int = fun n -> 3 in
let k = 9 in
let w = k1 in
let (v2, v3) = () in
f0(f0(23))",
    [lit(9, 8), id_edit, lit(8, 9), id_edit],
  ),
  (
    "residual with the binding shadowed after it",
    "let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
let (v2, v3) = () in
let f0 : Int -> Int = fun n -> 4 in
f0(1)",
    [id_edit],
  ),
];

/* Collect every failure rather than stopping at the first, so a regression
 * reports the whole shape of what it broke. */
let test_residual_corpus = () => {
  let failures =
    List.filter_map(
      ((name, src, edits)) =>
        switch (check(~name, ~src, ~edits)) {
        | () =>
          Printf.printf("RESIDUAL ok   %s\n", name);
          None;
        | exception e =>
          Printf.printf("RESIDUAL FAIL %s\n", name);
          Some(name ++ ": " ++ Printexc.to_string(e));
        },
      residual_corpus,
    );
  switch (failures) {
  | [] => ()
  | fs => Alcotest.fail(String.concat("\n", fs))
  };
};

/* Controls. A bare hole in the free variable's place produces no co-context
 * entry, so re-use is not blocked at the outer nodes and the hit never lands
 * on the node whose value is context-dependent. These passed before the fix
 * as well; they are here so that a future change that breaks them is
 * distinguishable from one that breaks the corpus above. */
let test_residual_controls = () => {
  check(
    ~name="control: hole instead of a free variable",
    ~src=
      "let f0 : Int -> Int = fun n -> 3 in
let w = ? in
let (v2, v3) = () in
f0(f0(f0(23)))",
    ~edits=[id_edit],
  );
  check(
    ~name="control: no indeterminate let at all",
    ~src="let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
f0(f0(f0(23)))",
    ~edits=[id_edit],
  );
  check(
    ~name="control: determinate throughout",
    ~src=
      "let f0 : Int -> Int = fun n -> 3 in
let w = 5 in
let (v2, v3) = (1, 2) in
f0(f0(f0(23)))",
    ~edits=[id_edit],
  );
};

let tests = (
  "IncrEvalResidual",
  [
    test_case("residual corpus", `Quick, test_residual_corpus),
    test_case("residual controls", `Quick, test_residual_controls),
  ],
);
