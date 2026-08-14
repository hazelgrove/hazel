/* Tests for the `let f(args) = body` function-definition sugar.

   Covers:
     - Typechecking parity with the desugared `let f = fun (args) -> body` form
     - Parameter-type annotations (`f(x: Int, y) = ...`)
     - Optional return-type annotations (`f(...) : Ret = ...`)
     - Single- and zero-argument forms
     - Recursive bindings through the sugar
     - Every id in the surface term survives into the info map */

open Alcotest;
open Test_Statics_Prelude;
open Language;
open FTemp;
open Typ;

/* Locate the sugar's binder pattern in a parsed expression and return
   the ids of (a) the optional outer `Asc` wrapper and (b) the inner
   `Ap(Var(f), args)` wrapper. */
let binder_wrapper_ids = (exp: Exp.t): (option(Id.t), Id.t) =>
  switch (IdTagged.term_of(exp)) {
  | Let(p, _, _) =>
    switch (IdTagged.term_of(p)) {
    | Asc(inner, _) => (Some(IdTagged.rep_id(p)), IdTagged.rep_id(inner))
    | _ => (None, IdTagged.rep_id(p))
    }
  | _ => Alcotest.fail("expected a top-level Let")
  };

/* Check that the info entries at the sugar-introduced pattern wrappers
   carry the arrow type directly (so the cursor inspector renders
   `: <arrow>` instead of `: ? consistent with expected type <arrow>`). */
let binder_info_shape = (name, src, expected_arrow) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(src);
      let m = statics(exp);
      let (asc_id_opt, ap_id) = binder_wrapper_ids(exp);
      let check_at = (~label, id) =>
        switch (Statics.Map.lookup(id, m)) {
        | Some(InfoPat({cls, ty, ana, _})) =>
          Alcotest.(check(string))(
            label ++ " — cls",
            Cls.show(Cls.Pat(ApFunc)),
            Cls.show(cls),
          );
          Alcotest.check(
            testable_typ,
            label ++ " — ty",
            expected_arrow,
            ty,
          );
          Alcotest.check(
            testable_typ,
            label ++ " — ana",
            expected_arrow,
            ana,
          );
        | Some(_) => Alcotest.fail(label ++ ": expected InfoPat")
        | None => Alcotest.fail(label ++ ": no info at binder id")
        };
      check_at(~label="Ap wrapper", ap_id);
      Option.iter(check_at(~label="Asc wrapper"), asc_id_opt);
    },
  );

let tests = (
  "Statics.FunctionSugar",
  [
    /* ===== Typechecking: sugar matches the desugared form ===== */
    fully_consistent_typecheck(
      "let f(x, y) = x + y in f(3, 4)",
      "let f(x: Int, y: Int) = x + y in f(3, 4)",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "with return-type annotation",
      "let f(x: Int, y: Int): Int = x + y in f(3, 4)",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "single argument",
      "let inc(x: Int): Int = x + 1 in inc(3)",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "single argument, no annotations",
      "let inc(x) = x + 1 in inc(3)",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "zero arguments",
      "let answer(): Int = 42 in answer()",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "recursive definition through sugar",
      "let fact(n: Int): Int = if n == 0 then 1 else n * fact(n - 1) in fact(3)",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "sugar yields the same arrow type as `fun`",
      "let f(x: Int, y: Int): Int = x + y in f",
      Some(arrow(prod([int(), int()]), int())),
    ),
    fully_consistent_typecheck(
      "nested let inside sugar body",
      "let f(x: Int): Int = let y = x + 1 in y in f(2)",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "sugar chained with another let",
      "let inc(x) = x + 1 in let double(x) = x * 2 in double(inc(3))",
      Some(int()),
    ),
    /* ===== Error surfaces: inconsistent return-type annotation =====
       `f(...) : Int = "nope"` must produce at least one static error. */
    inconsistent_typecheck(
      "return type mismatch is rejected",
      parse_exp({|let f(x: Int): Int = "nope" in f(1)|}),
    ),
    /* ===== ID preservation =====
       Ensure every id in the surface term survives into the info map
       so downstream features (cursor, highlighting, explain-this) work. */
    info_map_preserves_ids(
      "ids preserved: simple sugar",
      "let f(x: Int, y: Int) = x + y in f(3, 4)",
    ),
    info_map_preserves_ids(
      "ids preserved: sugar with return type",
      "let f(x: Int, y: Int): Int = x + y in f(3, 4)",
    ),
    info_map_preserves_ids(
      "ids preserved: single arg",
      "let inc(x) = x + 1 in inc(3)",
    ),
    info_map_preserves_ids(
      "ids preserved: zero args",
      "let answer(): Int = 42 in answer()",
    ),
    info_map_preserves_ids(
      "ids preserved: recursive",
      "let fact(n: Int): Int = if n == 0 then 1 else n * fact(n - 1) in fact(3)",
    ),
    /* ===== Cursor inspector display =====
       The binder `f(args)` (and its outer `Asc` wrapper if present)
       should report the function's arrow type directly, so the
       cursor inspector shows `: <arrow>` rather than
       `: ? consistent with expected type <arrow>`. */
    binder_info_shape(
      "binder info: Ap wrapper carries arrow type",
      "let f(x: Int, y: Int) = x + y in f(3, 4)",
      arrow(prod([int(), int()]), int()),
    ),
    binder_info_shape(
      "binder info: Asc wrapper with return type",
      "let f(x: Int, y: Int): Int = x + y in f(3, 4)",
      arrow(prod([int(), int()]), int()),
    ),
  ],
);
