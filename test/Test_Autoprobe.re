/**
 * Tests for auto-probe target selection (ProbePerform.current_toplevel_def).
 *
 * Each test specifies an input program containing a `¦` caret marker and
 * the expected text of the probed expression (or "<none>" if no probe
 * should be placed).
 *
 * Probe-selection rule (see ProbePerform.toplevel_def_body_id):
 *   Walk the cursor's ancestor chain outermost-to-innermost:
 *     - Let(p, def, body): cursor in body → continue; otherwise probe def.
 *     - Seq(e1, e2): cursor in e1 or e2 → continue; cursor on `;` → probe e1.
 *     - TyAlias(p, ty, body): cursor in body → continue; otherwise no probe.
 *     - non-chain ancestor → probe it (the enclosing bare expression).
 *   If the walk falls through, apply the same rules to the cursor's piece.
 *   Test/HintedTest bodies are unwrapped at the end (probe the condition,
 *   not the unit result).
 *
 * Caret targeting: tries `Indicated.index` first, then walks left through
 * secondaries to find a meaningful piece, then falls back to the cursor's
 * containing zipper ancestor.
 */
open Alcotest;
open Haz3lcore;
open Language;
open Action;

let caret_char = "¦";

let string_to_ltr_actions = (s: string): list(Action.t) =>
  s |> Token.to_list |> List.map(c => Action.Insert(c));

let mv_l = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Left, ByChar)));

let perform = (zip: Zipper.t, actions: list(Action.t)): Zipper.t => {
  let perform = (a: Action.t, z: Zipper.t) =>
    Perform.go(
      ~settings=Language.CoreSettings.off,
      ~statics=CachedStatics.empty,
      ~syntax=CachedSyntax.init(z),
      ~root=Exp,
      a,
      {
        zipper: z,
        col_target: None,
      },
    );
  List.fold_left(
    (z: Zipper.t, a: Action.t) =>
      switch (perform(a, z)) {
      | Ok(z) => z
      | Error(err) =>
        Alcotest.fail("Failed on action: " ++ Action.Failure.show(err))
      },
    zip,
    actions,
  );
};

/* Split input at the caret marker, build the program then move the caret
 * back to the marker position. Mirrors Test_Indication. */
let mk = (init: string): list(Action.t) => {
  let rec split =
          (before: list(string), rest: list(string))
          : (list(string), list(string)) =>
    switch (rest) {
    | [] => Alcotest.fail("No caret in: " ++ init)
    | [hd, ...tl] =>
      hd == caret_char
        ? (List.rev(before), tl) : split([hd, ...before], tl)
    };
  let (before, after) = split([], Token.to_list(init));
  let s = Token.of_list(before @ after);
  string_to_ltr_actions(s) @ mv_l(List.length(after));
};

/* Convert a probed id to its source text via TermData + Printer. */
let probed_str = (z: Zipper.t): string => {
  let root_segment = Zipper.unselect_and_zip(z);
  let MakeTerm.{term, _} = MakeTerm.go(root_segment);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  switch (ProbePerform.current_toplevel_def(info_map, z)) {
  | None => "<none>"
  | Some(id) =>
    let syntax = CachedSyntax.mk(~info_map, ~dyn_map=Id.Map.empty, z);
    switch (TermData.segment(id, syntax.term_data)) {
    | Some(seg) =>
      Printer.of_segment(~holes=" ", ~indent="", ~is_single_line=false, seg)
    | None => "<id not in term_data: " ++ Id.to_string(id) ++ ">"
    };
  };
};

let auto = (~name, ~input, ~probed) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = mk(input) |> perform(Zipper.init());
      check(
        testable(Fmt.string, String.equal),
        probed,
        probed,
        probed_str(z),
      );
    },
  );

/* Render the function-sugar parameter anchor (if any) that auto-probe
 * adds alongside the def body. "<none>" when the enclosing definition is
 * not function-definition sugar. */
let param_anchor_str = (z: Zipper.t): string => {
  let root_segment = Zipper.unselect_and_zip(z);
  let MakeTerm.{term, _} = MakeTerm.go(root_segment);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  switch (ProbePerform.current_toplevel_def(info_map, z)) {
  | None => "<no def>"
  | Some(def_id) =>
    switch (ProbePerform.function_sugar_param_anchor(info_map, def_id)) {
    | None => "<none>"
    | Some(id) =>
      let syntax = CachedSyntax.mk(~info_map, ~dyn_map=Id.Map.empty, z);
      switch (TermData.segment(id, syntax.term_data)) {
      | Some(seg) =>
        Printer.of_segment(~holes=" ", ~indent="", ~is_single_line=false, seg)
      | None => "<id not in term_data>"
      };
    }
  };
};

let param = (~name, ~input, ~param) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = mk(input) |> perform(Zipper.init());
      check(
        testable(Fmt.string, String.equal),
        param,
        param,
        param_anchor_str(z),
      );
    },
  );

/* ==================================================================
 * TEST SUITES
 * ================================================================== */

let basic_let_tests = [
  auto(
    ~name="cursor in let body (single line) probes body",
    ~input="let x = 5 in ¦x + 1",
    ~probed="x + 1",
  ),
  auto(
    ~name="cursor in def probes def",
    ~input="let x = ¦5 in x + 1",
    ~probed="5",
  ),
  auto(
    ~name="cursor on pat probes def",
    ~input="let ¦x = 5 in x + 1",
    ~probed="5",
  ),
  auto(
    ~name="cursor on `in` keyword probes def",
    ~input="let x = 5 ¦in x + 1",
    ~probed="5",
  ),
  auto(
    ~name="cursor after `in ` then space then body probes body",
    ~input="let x = 5 in ¦ x + 1",
    ~probed="x + 1",
  ),
];

let nested_let_tests = [
  auto(
    ~name="cursor in innermost body of nested let probes that body",
    ~input="let x = 5 in let y = 2 in ¦x + y",
    ~probed="x + y",
  ),
  auto(
    ~name="cursor in middle let's def probes that def",
    ~input="let x = 5 in let y = ¦2 in x + y",
    ~probed="2",
  ),
  auto(
    ~name="cursor on `let` of second let probes second let's def",
    ~input="let x = 5 in ¦let y = 2 in x + y",
    ~probed="2",
  ),
];

let seq_tests = [
  auto(
    ~name="cursor in e1 of seq probes e1",
    ~input="¦foo(); bar()",
    ~probed="foo()",
  ),
  auto(
    ~name="cursor on `;` probes e1",
    ~input="foo()¦; bar()",
    ~probed="foo()",
  ),
  auto(
    ~name="cursor in e2 of seq probes e2",
    ~input="foo(); ¦bar()",
    ~probed="bar()",
  ),
];

let bare_expression_tests = [
  auto(
    ~name="cursor on bare expression probes it",
    ~input="¦1 + 2",
    ~probed="1 + 2",
  ),
];

/* Function-definition sugar: `let f(args) = body` desugars (in statics) to
 * `let f = fun args -> body` while reusing the surface Let's id. That reuse
 * duplicates the Let in the cursor's ancestor chain, which used to make
 * auto-probe target the function body even when the cursor was in the let
 * body. Guards the dedup_adjacent workaround in
 * ProbePerform.toplevel_def_body_id. */
let function_sugar_tests = [
  auto(
    ~name="sugar: cursor in let body probes let body (not the function body)",
    ~input="let f(x: Int): Int = x + 1 in ¦f(5)",
    ~probed="f(5)",
  ),
  auto(
    ~name="sugar: cursor in function body probes function body",
    ~input="let f(x: Int): Int = ¦x + 1 in f(5)",
    ~probed="x + 1",
  ),
  auto(
    ~name="sugar inside let-body call probes let body",
    ~input="let f(x: Int): Int = x + 1 in f¦(5)",
    ~probed="f(5)",
  ),
  auto(
    ~name="sugar no return type: cursor in let body probes let body",
    ~input="let f(x: Int) = x + 1 in ¦f(5)",
    ~probed="f(5)",
  ),
  auto(
    ~name="sugar no return type: cursor in function body probes function body",
    ~input="let f(x: Int) = ¦x + 1 in f(5)",
    ~probed="x + 1",
  ),
];

let tyalias_tests = [
  auto(
    ~name="cursor in tyalias body probes body (transparent)",
    ~input="type T = Int in ¦x + 1",
    ~probed="x + 1",
  ),
  auto(
    ~name="cursor in tyalias type does not probe",
    ~input="type T = ¦Int in x + 1",
    ~probed="<none>",
  ),
];

/* Auto-probe parameter anchoring: for function-definition sugar, the def
 * body is anchored as before, AND the parameter pattern is anchored
 * separately so params are probed on the header line(s). These check the
 * second anchor that update_autoprobe adds (function_sugar_param_anchor). */
let param_anchor_tests = [
  param(
    ~name="sugar one-line: param anchor is the param tuple",
    ~input="let f(x: Int, y: Int): Int = ¦x + y in f(1, 2)",
    ~param="x: Int, y: Int",
  ),
  param(
    ~name="sugar no return type: param anchor is the param",
    ~input="let f(x: Int) = ¦x + 1 in f(1)",
    ~param="x: Int",
  ),
  param(
    ~name="sugar: cursor in let body still resolves param anchor",
    ~input="let f(x: Int): Int = x + 1 in ¦f(5)",
    ~param="<none>",
  ),
  param(
    ~name="plain let: no param anchor",
    ~input="let x = ¦5 in x + 1",
    ~param="<none>",
  ),
  param(
    ~name="bare expression: no param anchor",
    ~input="¦1 + 2",
    ~param="<none>",
  ),
];

/* Step-into resolution: mirror what step_into_call_stack computes — from a
   call `fn(...)`, resolve fn's binding, then the enclosing-let def body
   (enclosing_let_of_binding) and, for sugar, the parameter anchor. Returns
   (body_text, param_text). Before the fix, sugar definitions resolved to
   "<no body>" because Pat.bindings dropped the function name. */
let step_into_resolution = (~fn: string, program: string): (string, string) => {
  let zipper =
    switch (Parser.to_zipper(~root=Exp, program)) {
    | Some(z) => z
    | None => fail("parse: " ++ program)
    };
  let root_segment = Zipper.unselect_and_zip(zipper);
  let MakeTerm.{term, _} = MakeTerm.go(root_segment);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let syntax = CachedSyntax.mk(~info_map, ~dyn_map=Id.Map.empty, zipper);
  let render = id =>
    switch (TermData.segment(id, syntax.term_data)) {
    | Some(seg) =>
      Printer.of_segment(~holes=" ", ~indent="", ~is_single_line=true, seg)
    | None => "<not-in-syntax>"
    };
  /* Find the call `fn(...)` and resolve fn's binding site, as step-into does. */
  let binding_id =
    Id.Map.fold(
      (_id, info, acc) =>
        switch (acc, info) {
        | (Some(_), _) => acc
        | (None, Info.InfoExp({user_term: {term: Ap(_, fexpr, _), _}, _})) =>
          switch (IdTagged.term_of(fexpr)) {
          | Var(n) when n == fn =>
            switch (Statics.Map.lookup(IdTagged.rep_id(fexpr), info_map)) {
            | Some(ci_var) => Info.get_binding_site(ci_var)
            | None => None
            }
          | _ => None
          }
        | _ => acc
        },
      info_map,
      None,
    );
  switch (binding_id) {
  | None => ("<no binding>", "<no binding>")
  | Some(bid) =>
    switch (
      Statics.Map.enclosing_let_of_binding(~statics=info_map, ~binding_id=bid)
    ) {
    | None => ("<no body>", "<no body>")
    | Some(body_id) =>
      let param =
        switch (ProbePerform.function_sugar_param_anchor(info_map, body_id)) {
        | Some(p) => render(p)
        | None => "<none>"
        };
      (render(body_id), param);
    }
  };
};

let step_into = (~name, ~fn, ~program, ~body, ~param) =>
  test_case(
    name,
    `Quick,
    () => {
      let (b, p) = step_into_resolution(~fn, program);
      check(testable(Fmt.string, String.equal), "body", body, b);
      check(testable(Fmt.string, String.equal), "param", param, p);
    },
  );

let step_into_tests = [
  step_into(
    ~name="sugar: resolves body + params",
    ~fn="f",
    ~program="let f(x: Int, y: Int): Int = x + y in f(1, 2)",
    ~body="x + y",
    ~param="x: Int, y: Int",
  ),
  step_into(
    ~name="sugar no return type: resolves body + param",
    ~fn="f",
    ~program="let f(x: Int) = x + 1 in f(2)",
    ~body="x + 1",
    ~param="x: Int",
  ),
  step_into(
    ~name="sugar: call nested under another let still resolves",
    ~fn="f",
    ~program="let f(x: Int): Int = x + 1 in let g = 5 in f(g)",
    ~body="x + 1",
    ~param="x: Int",
  ),
  step_into(
    ~name="fun literal: resolves Fun def body, no sugar param anchor",
    ~fn="f",
    ~program="let f = fun (x: Int, y: Int) -> x + y in f(1, 2)",
    ~body="fun (x: Int, y: Int) -> x + y",
    ~param="<none>",
  ),
];

/* "All" mode: a single multi probe anchored on the program root expands
 * (via MultiProbe.ids_to_multiprobe, ~drill=false) to one probe per source
 * row across the whole program. This mirrors what update_autoprobe(~mode=All)
 * places and what `hazel probe --auto` renders. Distinct from the
 * Test_MultiProbe harness, which narrows the root via target_subterm_ids
 * (so for a let-chain it only covers the first definition). */
let all_probed_strs = (program: string): list(string) => {
  let zipper =
    switch (Parser.to_zipper(~root=Exp, program)) {
    | Some(z) => z
    | None => fail("parse: " ++ program)
    };
  let root_segment = Zipper.unselect_and_zip(zipper);
  let MakeTerm.{term, _} = MakeTerm.go(root_segment);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let syntax = CachedSyntax.mk(~info_map, ~dyn_map=Id.Map.empty, zipper);
  switch (ProbePerform.program_root_id(syntax)) {
  | None => []
  | Some(root_id) =>
    ProbePerform.ids_from_term(~syntax, ~info_map, root_id)
    |> List.filter_map(id =>
         switch (TermData.segment(id, syntax.term_data)) {
         | Some(seg) =>
           Some(
             Printer.of_segment(
               ~holes=" ",
               ~indent="",
               ~is_single_line=true,
               seg,
             ),
           )
         | None => None
         }
       )
  };
};

let all = (~name, ~input, ~probed: list(string)) =>
  test_case(name, `Quick, () =>
    check(list(string), name, probed, all_probed_strs(input))
  );

let all_mode_tests = [
  all(
    ~name="let-chain: one probe per definition + final body",
    ~input="let x = 1 in\nlet y = x + 2 in\nx + y",
    ~probed=["1", "x + 2", "x + y"],
  ),
  all(
    ~name="sequence: each component, function body not the fun value",
    ~input="let f = fun x -> x + 1 in\nf(1);\nf(2)",
    ~probed=["x + 1", "f(1)", "f(2)"],
  ),
  all(
    ~name="bare expression: single probe on the whole expression",
    ~input="1 + 2",
    ~probed=["1 + 2"],
  ),
  all(
    /* Def value pushed to its own row: the header row carries only the
       pattern `n`, so All mode probes the binder there (like the
       function-sugar param anchor), then the def value, then the body. */
    ~name="multi-line def: header pattern, def value, final body",
    ~input="let n =\n  1 + 2 in\nn",
    ~probed=["n", "1 + 2", "n"],
  ),
];

let tests = [
  ("Autoprobe.All", all_mode_tests),
  ("Autoprobe.BasicLet", basic_let_tests),
  ("Autoprobe.ParamAnchor", param_anchor_tests),
  ("Autoprobe.StepInto", step_into_tests),
  ("Autoprobe.NestedLet", nested_let_tests),
  ("Autoprobe.Seq", seq_tests),
  ("Autoprobe.FunctionSugar", function_sugar_tests),
  ("Autoprobe.BareExpression", bare_expression_tests),
  ("Autoprobe.TyAlias", tyalias_tests),
];
