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

/* Collect every id present in the surface expression by walking all
   sub-term annotations (exp / pat / typ / tpat / ...). */
let collect_ids = (exp: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let collect = (a: IdTagged.IdTag.t) => {
    acc := a.ids @ acc^;
    a;
  };
  let _ = Grammar.map_exp_annotation(collect, exp);
  acc^;
};

/* After rewriting `let f(args) = def` to `let f = fun (args) -> def`,
   every id that appeared in the surface term must still resolve to
   some Info.t in the map produced by statics. */
let info_map_preserves_ids = (name, src) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(src);
      let m = statics(exp);
      let missing =
        collect_ids(exp)
        |> List.filter(id =>
             !Id.equal(id, Id.invalid)
             && Option.is_none(Statics.Map.lookup(id, m))
           );
      Alcotest.(check(list(string)))(
        src ++ " — every surface id appears in the info map",
        [],
        List.map(Id.show, missing),
      );
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
  ],
);
