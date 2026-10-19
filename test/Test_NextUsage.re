/**
 * Tests for "Go to Next Usage" (Shift+F12).
 *
 * With the caret on a variable -- either its binding or one of its
 * uses -- NextUsage jumps to the next use of that same binding, in
 * document order, wrapping around at the end.
 *
 * Two layers are tested:
 *   1. `Statics.Map.usage_ids`: which ids count as uses.
 *   2. `Move(Goal(NextUsage(_)))`: the order the caret visits them in.
 */
open Alcotest;
open Util;
open Haz3lcore;
open Action;

let settings = Language.CoreSettings.on;

/* ---------- Helpers ---------- */

let parse = (s: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Exp, s)) {
  | Some(z) => z
  | None => fail("Failed to parse: " ++ s)
  };

/* Statics + syntax caches for a zipper. Mirrors Test_ProblemCollection. */
let caches = (z: Zipper.t) => {
  let editor = Editor.Model.mk(z, ~root=Exp);
  let statics =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=false,
      ~stitch=Fun.id,
      ~root=Exp,
      editor.state.zipper,
    );
  (editor, statics);
};

let try_perform = (z: Zipper.t, a: Action.t) => {
  let (editor, statics) = caches(z);
  Perform.go(
    ~settings,
    ~statics,
    ~syntax=editor.syntax,
    ~root=Exp,
    a,
    {
      zipper: z,
      col_target: None,
    },
  );
};

let perform = (z: Zipper.t, a: Action.t): Zipper.t =>
  switch (try_perform(z, a)) {
  | Ok(z) => z
  | Error(err) => fail("Action failed: " ++ Action.Failure.show(err))
  };

let fails = (z: Zipper.t, a: Action.t): bool =>
  switch (try_perform(z, a)) {
  | Error(_) => true
  | Ok(_) => false
  };

let next_usage = Move(Goal(NextUsage(Right)));
let prev_usage = Move(Goal(NextUsage(Left)));

/* The id of the term under the caret. */
let indicated = (z: Zipper.t): Id.t =>
  switch (Indicated.index(z)) {
  | Some(id) => id
  | None => fail("Nothing indicated")
  };

/* On-screen position of `id`, for sorting into document order. */
let position = (editor: Editor.Model.t, id: Id.t): Point.t =>
  switch (Measured.find_by_id(id, editor.syntax.measured)) {
  | Some({origin, _}) => origin
  | None => fail("Not on screen: " ++ Id.to_string(id))
  };

let by_position = (editor, ids: list(Id.t)): list(Id.t) =>
  List.sort(
    (a, b) => Point.compare(position(editor, a), position(editor, b)),
    ids,
  );

/* Parse `s` and put the caret on the `nth` (1-based, left to right)
 * place where `name` is bound. */
let on_binding = (~nth: int=1, s: string, name: string): Zipper.t => {
  let z = parse(s);
  let (editor, statics) = caches(z);
  let binders =
    Id.Map.fold(
      (id, info: Language.Info.t, acc) =>
        switch (info) {
        | InfoPat({user_term: {term: Var(n), _}, _})
            when n == name && id == Language.Info.id_of(info) => [
            id,
            ...acc,
          ]
        | _ => acc
        },
      statics.info_map,
      [],
    )
    |> by_position(editor);
  switch (List.nth_opt(binders, nth - 1)) {
  | Some(id) => perform(z, Move(Goal(TileId(id))))
  | None => fail(Printf.sprintf("No binding #%d of %s", nth, name))
  };
};

/* Uses of the binding under the caret, in document order. */
let uses_of = (z: Zipper.t): list(Id.t) => {
  let (editor, statics) = caches(z);
  switch (Indicated.ci_of(z, statics.info_map)) {
  | Some(ci) =>
    Language.Statics.Map.usage_ids(statics.info_map, ci)
    |> by_position(editor)
  | None => fail("No info for the indicated term")
  };
};

/* Perform `action` `n` times from `z`, recording where the caret
 * lands after each step. */
let visits = (z: Zipper.t, action: Action.t, n: int): list(Id.t) => {
  let rec go = (z, n) =>
    if (n <= 0) {
      [];
    } else {
      let z = perform(z, action);
      [indicated(z), ...go(z, n - 1)];
    };
  go(z, n);
};

let id = testable(Id.pp, Id.equal);

/* ---------- Programs under test ---------- */

/* One binding, three uses. */
let three_uses = "let x = 1 in x + x + x";

/* Outer x is used once (in `y = x`); inner x is used twice. */
let shadowed = "let x = 1 in let y = x in let x = 2 in x + x";

/* ---------- Layer 1: which ids count as uses ---------- */

let test_finds_uses_not_binding =
  test_case("Finds every use, but not the binding", `Quick, () => {
    let z = on_binding(three_uses, "x");
    let uses = uses_of(z);
    check(int, "three uses", 3, List.length(uses));
    check(bool, "binding excluded", false, List.mem(indicated(z), uses));
  });

let test_shadowing =
  test_case("Shadowed bindings keep their uses separate", `Quick, () => {
    let outer = uses_of(on_binding(~nth=1, shadowed, "x"));
    let inner = uses_of(on_binding(~nth=2, shadowed, "x"));
    check(int, "outer x used once", 1, List.length(outer));
    check(int, "inner x used twice", 2, List.length(inner));
  });

let test_fun_param =
  test_case("Function parameters have uses", `Quick, () => {
    let z = on_binding("let f = fun y -> y * y in f(3)", "y");
    check(int, "y used twice", 2, List.length(uses_of(z)));
  });

/* ---------- Layer 2: where the caret goes ---------- */

let test_forward_wraps =
  test_case("Next visits uses in order, then wraps", `Quick, () => {
    let z = on_binding(three_uses, "x");
    let uses = uses_of(z);
    let expected = uses @ [List.hd(uses)];
    check(list(id), "u1, u2, u3, u1", expected, visits(z, next_usage, 4));
  });

let test_backward =
  test_case("Previous visits uses in reverse order", `Quick, () => {
    let z = on_binding(three_uses, "x");
    let expected = List.rev(uses_of(z));
    check(list(id), "u3, u2, u1", expected, visits(z, prev_usage, 3));
  });

/* The binding is reachable with F12, so NextUsage should only ever
 * land on uses, however many times it is pressed. */
let test_skips_binding =
  test_case("Cycling never lands on the binding", `Quick, () => {
    let z = on_binding(three_uses, "x");
    let landed = visits(z, next_usage, 6);
    check(bool, "binding not visited", false, List.mem(indicated(z), landed));
  });

/* In `shadowed`, the later `x + x` refers to the inner x, not the
 * outer one, even though the name is the same. Starting from the outer
 * x, we should bounce between its single use and never reach those. */
let test_ignores_shadowed_uses =
  test_case("Cycling ignores a shadowing binding's uses", `Quick, () => {
    let z = on_binding(~nth=1, shadowed, "x");
    let uses = uses_of(z);
    check(list(id), "only outer use", uses @ uses, visits(z, next_usage, 2));
  });

let test_unused =
  test_case("Unused variable: action fails cleanly", `Quick, () => {
    let z = on_binding("let x = 1 in 2", "x");
    check(bool, "returns Error", true, fails(z, next_usage));
  });

let test_not_a_variable =
  test_case("Caret not on a variable: action fails cleanly", `Quick, () =>
    check(bool, "returns Error", true, fails(parse("1 + 2"), next_usage))
  );

let tests = (
  "NextUsage",
  [
    test_finds_uses_not_binding,
    test_shadowing,
    test_fun_param,
    test_forward_wraps,
    test_backward,
    test_skips_binding,
    test_ignores_shadowed_uses,
    test_unused,
    test_not_a_variable,
  ],
);
