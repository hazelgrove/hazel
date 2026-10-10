/* Values left unfinished (Evaluator.evaluate_open), and parts taken out of
   them with their environments (MvuShape.peel and the *_open readers).

   Finishing a value substitutes every variable's value into it, recursively
   and unshared: a livelit's definition, or a view whose handlers close over
   the whole livelit, came out with a copy of every helper in every
   function -- 76% of a Polygons click and most of a Kids' Choice redraw in
   Firefox. Left unfinished, a function taken out of the value must still
   find its helpers: these check that it does, and that it answers as the
   finished value's would.

   Today's evaluator wraps each function it makes in its own Closure, so
   stripping the outer wrappers (record_field) answers the same here: the
   re-wrapping in record_field_open and peel is for values whose functions
   sit under a shared Closure instead, as a mid-run sample's do (see
   MvuShape.close_value). These pin the answers, not that difference. */
open Alcotest;
open Haz3lcore;
open Language;

let elab = text =>
  switch (PersistentZipper.parse_text(~source="t", ~root=Exp, text)) {
  | None => fail("did not parse")
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    snd(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));
  };

let int_lit = n => DHExp.fresh(Atom(Int(Bigint.of_int(n))));
let apply = (f, n) =>
  switch (
    MvuShape.safe_evaluate(
      IdTagged.FreshGrammar.Exp.ap(Forward, f, int_lit(n)),
    )
  ) {
  | Ok(v) =>
    switch (MvuShape.strip_wrappers(v).term) {
    | Atom(Int(i)) => Bigint.to_string(i)
    | _ => "not an Int: " ++ DHExp.show(v)
    }
  | Error(e) => "error: " ++ e
  };

/* A module whose field calls a helper beside it and a variable outside it,
   as a livelit's update calls its helpers. */
let module_text = "let k = 2 in { let helper = fun x -> x + k; let f = fun y -> helper(y) * 10 }";

let field_of = (~open_, text, label) => {
  let e = elab(text);
  let v = open_ ? MvuShape.safe_evaluate_open(e) : MvuShape.safe_evaluate(e);
  switch (v) {
  | Error(err) => fail("evaluation: " ++ err)
  | Ok(v) =>
    switch (
      open_
        ? MvuShape.record_field_open(v, label)
        : MvuShape.record_field(v, label)
    ) {
    | Some(f) => f
    | None => fail("no field " ++ label)
    }
  };
};

let tests = (
  "OpenValues",
  [
    test_case("a field of a finished module", `Quick, () =>
      check(
        string,
        "f(1)",
        "30",
        apply(field_of(~open_=false, module_text, "f"), 1),
      )
    ),
    test_case("a field of an unfinished module keeps its helpers", `Quick, () =>
      check(
        string,
        "f(1), as finished",
        "30",
        apply(field_of(~open_=true, module_text, "f"), 1),
      )
    ),
    test_case(
      "a constructor's payload keeps its environment",
      `Quick,
      () => {
        let e =
          elab(
            "type T = + Pair((Int, Int -> Int)) in let k = 5 in Pair((1, fun x -> x + k))",
          );
        switch (MvuShape.safe_evaluate_open(e)) {
        | Error(err) => fail("evaluation: " ++ err)
        | Ok(v) =>
          switch (MvuShape.of_constructor_open(v)) {
          | Some(("Pair", body)) =>
            switch (MvuShape.of_tuple_open(body)) {
            | Some([_, fn]) => check(string, "fn(1)", "6", apply(fn, 1))
            | _ => fail("not a pair")
            }
          | _ => fail("not Pair")
          }
        };
      },
    ),
  ],
);
