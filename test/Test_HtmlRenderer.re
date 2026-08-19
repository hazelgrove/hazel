open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* HtmlRenderer is the value-side counterpart to HTMLProj, and mirrors its two
 * modes: static HTML, and a running (init, update, view, subs) app. `parse`
 * is the whole of its contract — what it returns is what puts "View as html"
 * in the sample context menu, what ^^probe_html renders, and which of the two
 * commit targets the rendering uses. */

let parse = (program: string): option(Haz3lcore.HtmlRenderer.value) =>
  Haz3lcore.HtmlRenderer.parse(Sort.Exp, parse_and_evaluate(program));

let kind = (program: string): string =>
  switch (parse(program)) {
  | Some(Static(_)) => "static"
  | Some(App(_)) => "app"
  | None => "rejected"
  };

let noop_app = {|(0, fun (m, _) -> (m, CmdNone), fun m -> Int(m), fun _ -> SubNone)|};

/* === Commit shape, without an editor ===
 *
 * A press commits `base |> handler` as SYNTAX. The handler value comes from
 * evaluating the document, so any subterm reused from it still carries the id
 * it has where it was defined — and a document may not hold one id twice. The
 * editor only reports that much later, as `Highlight.of_tile: shard mismatch`,
 * when a tile's shards cannot all be found in the measured map.
 *
 * So the property to hold is local and checkable here: nothing the commit
 * splices may reuse an id already in the document. */

let exp_ids = (e: Language.Exp.t): list(Id.t) => {
  let acc = ref([]);
  let _ =
    Language.Exp.map_term(
      ~f_exp=
        (continue, x) => {
          acc := [Language.Exp.rep_id(x), ...acc^];
          continue(x);
        },
      e,
    );
  acc^;
};

/* The ids a commit would introduce, against the ids already in the document.
   One parse, then elaborate + evaluate THAT term, so the handler value carries
   the document's own ids — which is the situation in the editor. */
let commit_vs_document = (~program: string) => {
  let document = parse_exp(program);
  let msg = evaluate(elaborate(document));
  let committed =
    Haz3lcore.HtmlRenderer.handler_syntax(~bound=_ => true, msg);
  let doc_ids = exp_ids(document);
  let reused =
    exp_ids(committed) |> List.filter(id => List.mem(id, doc_ids));
  (List.length(reused), committed);
};

/* The real commit path: ProjectorInfo.utility.lift_syntax, with the settings
   the editor uses, over a real segment. The failure mode this guards is not a
   wrong value but a malformed segment — the editor reports it much later as
   `Highlight.of_tile: shard mismatch`, when a tile's shards cannot all be
   found in the measured map. That check is reproduced directly below, so this
   path needs no browser. */
let commit_segment =
    (~program: string, ~handler: string): Haz3lcore.Base.segment => {
  let seg =
    switch (Haz3lcore.Parser.to_segment(program, ~root=Exp)) {
    | Some(seg) => seg
    | None => Alcotest.fail("could not parse: " ++ program)
    };
  let handler =
    Haz3lcore.HtmlRenderer.handler_syntax(
      ~bound=_ => true,
      parse_and_evaluate(handler),
    );
  switch (
    Haz3lcore.ProjectorInfo.utility.lift_syntax(
      ~inline=false,
      fun
      | Exp(e) => Exp(Haz3lcore.HtmlRenderer.spliced(~handler, e))
      | other => other,
      seg,
    )
  ) {
  | Some(seg) => seg
  | None => Alcotest.fail("lift_syntax failed for: " ++ program)
  };
};

let rec tiles_of = (seg: Haz3lcore.Base.segment): list(Haz3lcore.Tile.t) =>
  List.concat_map(
    (p: Haz3lcore.Base.piece) =>
      switch (p) {
      | Tile(t) => [t, ...List.concat_map(tiles_of, t.children)]
      | Grout(_)
      | Secondary(_)
      | Projector(_) => []
      },
    seg,
  );

/* Exactly the invariant Highlight.of_tile asserts before drawing: a tile with
   N children must have N+1 measured shards. */
let shard_mismatches = (seg: Haz3lcore.Base.segment): list(string) => {
  let measured =
    Haz3lcore.Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  tiles_of(seg)
  |> List.filter_map((t: Haz3lcore.Tile.t) => {
       let found =
         switch (Haz3lcore.Measured.find_shards(t, measured)) {
         | shards =>
           List.length(
             List.filter(((i, _)) => List.mem(i, t.shards), shards),
           )
         | exception _ => (-1)
         };
       let expected = List.length(t.children) + 1;
       found == expected
         ? None
         : Some(
             String.concat("", t.label)
             ++ ": found "
             ++ string_of_int(found)
             ++ ", expected "
             ++ string_of_int(expected),
           );
     });
};

/* Segment straight from text, no commit involved — isolates whether a piece
   of syntax satisfies the shard invariant at all. */
let parsed_segment = (text: string): Haz3lcore.Base.segment =>
  switch (Haz3lcore.Parser.to_segment(text, ~root=Exp)) {
  | Some(seg) => seg
  | None => Alcotest.fail("could not parse: " ++ text)
  };

/* The whole commit, as ProjectorPerform performs it for a refractor: lift the
   syntax, trim and re-parenthesize it, select the probed term's shard range,
   and replace the selection. Then measure the resulting document and check the
   same invariant over BOTH the document and the leftover selection — the crash
   reported in the editor comes from selection highlighting (`sel_of_tile`),
   which runs before the update cycle clears the selection. */
let commit_into_document =
    (~program: string, ~handler: string)
    : (Haz3lcore.Base.segment, Haz3lcore.Base.segment, Haz3lcore.Zipper.t) => {
  let z =
    switch (Haz3lcore.Parser.to_zipper(~root=Exp, program)) {
    | Some(z) => z
    | None => Alcotest.fail("could not parse: " ++ program)
    };
  let term_data =
    Haz3lcore.MakeTerm.go(Haz3lcore.Zipper.unselect_and_zip(z)).term_data;
  let id =
    switch (z.refractors.manuals) {
    | [(id, _), ..._] => id
    | [] => Alcotest.fail("no refractor in: " ++ program)
    };
  let handler_syn =
    Haz3lcore.HtmlRenderer.handler_syntax(
      ~bound=_ => true,
      parse_and_evaluate(handler),
    );
  let lifted =
    switch (
      Haz3lcore.ProjectorInfo.utility.lift_syntax(
        ~inline=false,
        fun
        | Exp(e) =>
          Exp(Haz3lcore.HtmlRenderer.spliced(~handler=handler_syn, e))
        | other => other,
        switch (Haz3lcore.TermData.segment(id, term_data)) {
        | Some(seg) => seg
        | None => Alcotest.fail("no segment for the probed term")
        },
      )
    ) {
    | Some(seg) => seg
    | None => Alcotest.fail("lift_syntax failed")
    };
  /* exactly ProjectorPerform's refractor branch */
  let piece =
    lifted
    |> Haz3lcore.Segment.unparenthesize
    |> Haz3lcore.Segment.trim_secondary(Right)
    |> Haz3lcore.Segment.trim_secondary(Left)
    |> Haz3lcore.Segment.parenthesize;
  switch (Haz3lcore.TermData.extremes_shards(id, term_data)) {
  | None => Alcotest.fail("no shard extremes for the probed term")
  | Some((l, r)) =>
    switch (Haz3lcore.Select.shard_range(l, r, z)) {
    | None => Alcotest.fail("could not select the probed term")
    | Some(z) =>
      let z = Haz3lcore.Zipper.replace_selection(Right, [piece], z);
      (Haz3lcore.Zipper.unselect_and_zip(z), z.selection.content, z);
    }
  };
};

/* One level further than the segment checks: commit into the document, then
   ELABORATE AND EVALUATE the result. The symptom seen in the editor is not a
   malformed segment but an indeterminate value — the committed pipeline coming
   back as a stuck application instead of html. */
let commit_and_evaluate = (~program: string, ~handler: string): Language.Exp.t => {
  let (doc, _, _) = commit_into_document(~program, ~handler);
  let term = Haz3lcore.MakeTerm.go(doc).term;
  evaluate(elaborate(term));
};

/* Everything in a slide before its trailing `^^probe_html(...)`, so a handler
   expression can be evaluated in the slide's own scope. */
let defs_of = (program: string): string => {
  let needle = "^^probe_html";
  let rec scan = (i: int): int =>
    i < 0
      ? Alcotest.fail("no ^^probe_html in the program")
      : String.length(program)
        - i >= String.length(needle)
        && String.sub(program, i, String.length(needle)) == needle
          ? i : scan(i - 1);
  String.sub(program, 0, scan(String.length(program) - 1));
};

/* Evaluate a committed document the way the editor does: WITH probe
   instrumentation. A probe wraps the expression it watches, and that wrapper
   is the one thing a plain `evaluate` never sees — which is why the CLI and a
   bare evaluate both reduce a program the editor gets stuck on. */
let evaluate_with_probes = (z: Haz3lcore.Zipper.t): Language.Exp.t => {
  let Haz3lcore.MakeTerm.{term, _} =
    Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
  let (info_map, elaborated) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let targets = Test_Evaluator_Prelude.targets_of_zipper(z, info_map);
  let (value, _) =
    Language.Evaluator.evaluate(
      ~eval_info=Language.EvalInfo.of_targets(targets),
      ~env=Language.Builtins.env_init,
      elaborated,
    );
  value;
};

/* The worker does not call `evaluate`. It runs evaluation in SLICES —
   start_yielding_evaluation, then run_yielding_slice with a 5000-step budget,
   resuming a continuation each time (WorkerServer.slice_step_budget). A
   one-shot evaluate never suspends and resumes, so a resumption bug is
   invisible to it and to `hazel run`. This replicates the worker's loop. */
let probe_inputs = (z: Haz3lcore.Zipper.t) => {
  let Haz3lcore.MakeTerm.{term, _} =
    Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
  let (info_map, elaborated) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let targets = Test_Evaluator_Prelude.targets_of_zipper(z, info_map);
  (elaborated, Language.EvalInfo.of_targets(targets));
};

let drive_slices =
    (~step_budget: int=5000, ~max_slices: int=40000, evaluation)
    : result((Language.Exp.t, Language.EvaluatorState.t), string) => {
  let rec go = (n, evaluation) =>
    n > max_slices
      ? Error(
          "never completed in " ++ string_of_int(max_slices) ++ " slices",
        )
      : (
        switch (
          Language.Evaluator.run_yielding_slice(~step_budget, evaluation)
        ) {
        | EvaluationCompleted(pair) => Ok(pair)
        | EvaluationYielded(evaluation) => go(n + 1, evaluation)
        }
      );
  go(0, evaluation);
};

let evaluate_in_slices =
    (~prev=Language.IncrEval.empty, z: Haz3lcore.Zipper.t)
    : result((Language.Exp.t, Language.EvaluatorState.t), string) => {
  let (elaborated, eval_info) = probe_inputs(z);
  drive_slices(
    Language.Evaluator.start_yielding_evaluation(
      ~prev,
      ~eval_info,
      ~env=Language.Builtins.env_init,
      elaborated,
    ),
  );
};

/* The browser evaluates INCREMENTALLY: each pass is handed the previous
   pass's IncrEval so unchanged subterms are reused rather than recomputed. A
   commit rewrites syntax in place, reusing the document's ids, and IncrEval is
   keyed on ids — so a stale entry can be reused for a term whose meaning
   changed. Neither `hazel run` nor a plain evaluate exercises this, which is
   why the editor sticks where they don't. */
let evaluate_incrementally = (~before: Language.Exp.t, ~after: Language.Exp.t) => {
  let (_, state) =
    Language.Evaluator.evaluate(
      ~env=Language.Builtins.env_init,
      elaborate(before),
    );
  let prev = Language.EvaluatorState.get_incr_eval(state);
  let (value, _) =
    Language.Evaluator.evaluate(
      ~prev,
      ~env=Language.Builtins.env_init,
      elaborate(after),
    );
  value;
};

let tests = (
  "HtmlRenderer",
  [
    test_case(
      "recognizes static HTML",
      `Quick,
      () => {
        check(
          string,
          "a literal element",
          "static",
          kind({|Div([], [Text("hi")])|}),
        );
        check(string, "a nullary element", "static", kind({|Br|}));
        check(
          string,
          "a generic SVG node",
          "static",
          kind({|Node("svg", [Create("viewBox", "0 0 1 1")], [])|}),
        );
      },
    ),
    /* The point of the value seam: HTMLProj can only draw syntax that is
       already HTML or a literal 4-tuple, so a computed one shows as code
       there. Here the application has been evaluated before parse sees it. */
    test_case("recognizes HTML that was computed, not written", `Quick, () =>
      check(
        string,
        "an application returning HTML",
        "static",
        kind({|let f = fun n -> Div([], [Int(n)]) in f(3)|}),
      )
    ),
    test_case(
      "recognizes an MVU app",
      `Quick,
      () => {
        check(string, "a literal 4-tuple", "app", kind(noop_app));
        check(
          string,
          "the labeled form",
          "app",
          kind(
            {|(init=0, update=fun (m, _) -> (m, CmdNone),
             view=fun m -> Int(m), subs=fun _ -> SubNone)|},
          ),
        );
      },
    ),
    /* The case this whole seam exists for: an app produced by a function has
       no 4-tuple in its syntax, so only the evaluated value can reveal it. */
    test_case("recognizes an app that was computed", `Quick, () =>
      check(
        string,
        "an application returning an app",
        "app",
        kind(
          {|let mk = fun n -> (n, fun (m, _) -> (m, CmdNone),
                                  fun m -> Int(m), fun _ -> SubNone) in mk(7)|},
        ),
      )
    ),
    /* Syntax commit splices `f(html)` rather than evaluating the handler,
       so what stands in for `f` matters: the binding name keeps the edit
       short and keeps it pointing at the definition. */
    test_case("a let-bound handler carries its name", `Quick, () =>
      check(
        option(string),
        "named bump",
        Some("bump"),
        Haz3lcore.HtmlRenderer.handler_name(
          parse_and_evaluate(
            {|let bump = fun node -> Div([], [node]) in bump|},
          ),
        ),
      )
    ),
    test_case("an inline lambda has no name to splice", `Quick, () =>
      check(
        option(string),
        "anonymous",
        None,
        Haz3lcore.HtmlRenderer.handler_name(
          parse_and_evaluate({|fun node -> Div([], [node])|}),
        ),
      )
    ),
    test_case(
      "a named handler reuses no document id",
      `Quick,
      () => {
        let (reused, _) =
          commit_vs_document(
            ~program={|let bump = fun node -> node in bump|},
          );
        check(int, "no id reused from the document", 0, reused);
      },
    ),
    /* The regression: a deferred handler's arguments come out of the evaluated
       document, so committing them verbatim puts one id on two pieces. */
    test_case(
      "a deferred handler reuses no document id",
      `Quick,
      () => {
        let (reused, committed) =
          commit_vs_document(
            ~program={|let press = fun (node, d) -> node in press(_, "1")|},
          );
        check(int, "no id reused from the document", 0, reused);
        /* and it really is the compact form, not the inlined closure */
        check(
          bool,
          "commits as a deferred application",
          true,
          switch (Haz3lcore.MvuShape.strip_wrappers(committed).term) {
          | DeferredAp(_) => true
          | _ => false
          },
        );
      },
    ),
    test_case("a named handler lifts to a well-formed segment", `Quick, () =>
      check(
        list(string),
        "no shard mismatches",
        [],
        shard_mismatches(
          commit_segment(
            ~program={|Div([], [Li([], [Text("A")])])|},
            ~handler={|let bump = fun node -> node in bump|},
          ),
        ),
      )
    ),
    test_case("a deferred handler lifts to a well-formed segment", `Quick, () =>
      check(
        list(string),
        "no shard mismatches",
        [],
        shard_mismatches(
          commit_segment(
            ~program={|Div([], [Li([], [Text("A")])])|},
            ~handler={|let press = fun (node, d) -> node in press(_, "1")|},
          ),
        ),
      )
    ),
    /* What the probe actually hands over: RefractorView parenthesizes the
       probed piece, and the probed expression is an application, not a
       literal. */
    test_case("a parenthesized application base lifts cleanly", `Quick, () =>
      check(
        list(string),
        "no shard mismatches",
        [],
        shard_mismatches(
          commit_segment(
            ~program={|(calc(0))|},
            ~handler={|let press = fun (node, d) -> node in press(_, "1")|},
          ),
        ),
      )
    ),
    /* A deferred application is the one piece of syntax the committed pipeline
       has that a plain named handler does not. */
    test_case("a deferral in plain parsed syntax is well-formed", `Quick, () => {
      List.iter(
        text =>
          check(
            list(string),
            text,
            [],
            shard_mismatches(parsed_segment(text)),
          ),
        [
          {|f(1)|},
          {|f(_, "1")|},
          {|map(_, g)|},
          {|x |> f(_, "1")|},
          {|Div([], []) |> f(_, "1")|},
        ],
      )
    }),
    /* End to end, no browser: commit into a real document and check the
       invariant the editor checks when it draws. */
    test_case("committing into a document leaves it well-formed", `Quick, () => {
      List.iter(
        ((label, handler)) => {
          let (doc, selection, _) =
            commit_into_document(
              ~program=
                {|let f = fun node -> node in ^^probe_html(Div([], []))|},
              ~handler,
            );
          check(
            list(string),
            label ++ ": document",
            [],
            shard_mismatches(doc),
          );
          check(
            list(string),
            label ++ ": selection",
            [],
            shard_mismatches(selection),
          );
        },
        [
          ("named", {|let bump = fun node -> node in bump|}),
          (
            "deferred",
            {|let press = fun (node, d) -> node in press(_, "1")|},
          ),
        ],
      )
    }),
    /* The editor showed a committed deferred handler coming back as a stuck
       `setState(...)` rather than html. This is that, in-process. Both the
       document and the handler are built from the same definitions, so a name
       the commit splices is bound where it lands. */
    test_case(
      "a committed pipeline evaluates to html",
      `Quick,
      () => {
        let defs = {|let wrap = fun (node, d) ->
            case node
            | Div(a, _) => Div(a, [Text(d)])
            | _ => node
            end in
          let mk = fun n -> Div([Class("c")], [Text("0")]) in
          |};
        let program = defs ++ {|^^probe_html(mk(0))|};
        List.iter(
          ((label, handler)) => {
            let v = commit_and_evaluate(~program, ~handler=defs ++ handler);
            check(
              bool,
              label ++ ": evaluates to html, not a stuck application",
              true,
              Haz3lcore.MvuShape.is_html(v),
            );
          },
          [("named", {|mk|}), ("deferred", {|wrap(_, "1")|})],
        );
      },
    ),
    /* The editor's path: evaluate, commit, then evaluate again reusing the
       first pass's IncrEval. */
    test_case(
      "a committed pipeline survives incremental reuse",
      `Quick,
      () => {
        let defs = {|let wrap = fun (node, d) ->
            case node
            | Div(a, _) => Div(a, [Text(d)])
            | _ => node
            end in
          let mk = fun n -> Div([Class("c")], [Text("0")]) in
          |};
        let program = defs ++ {|^^probe_html(mk(0))|};
        List.iter(
          ((label, handler)) => {
            let (doc, _, _) =
              commit_into_document(~program, ~handler=defs ++ handler);
            let v =
              evaluate_incrementally(
                ~before=parse_exp(program),
                ~after=Haz3lcore.MakeTerm.go(doc).term,
              );
            check(
              bool,
              label ++ ": html after incremental reuse",
              true,
              Haz3lcore.MvuShape.is_html(v),
            );
          },
          [("named", {|mk|}), ("deferred", {|wrap(_, "1")|})],
        );
      },
    ),
    /* The real thing, at real scale: the Calculator slide's own text, with a
       deferred handler committed into it. This is what the editor does and
       what sticks there. */
    test_case(
      "the Calculator slide survives a deferred commit",
      `Quick,
      () => {
        let program =
          switch (
            List.assoc_opt("Charts / Calculator", Charts.Slides.all_slides)
          ) {
          | Some({backup_text, _}: Haz3lcore.PersistentZipper.t) => backup_text
          | None => Alcotest.fail("Calculator slide not registered")
          };
        let (doc, _, z) =
          commit_into_document(
            ~program,
            ~handler=defs_of(program) ++ {|pressDigit(_, "1")|},
          );
        let term = Haz3lcore.MakeTerm.go(doc).term;
        let plain = evaluate(elaborate(term));
        if (!Haz3lcore.MvuShape.is_html(plain)) {
          let txt =
            Language.Exp.show(Haz3lcore.MvuShape.strip_wrappers(plain));
          print_endline(
            "PLAIN-HEAD: "
            ++ String.sub(txt, 0, min(1400, String.length(txt))),
          );
          let t = Haz3lcore.Printer.of_segment(~holes="?", ~indent="", doc);
          print_endline(
            "COMMITTED-TAIL: "
            ++ String.sub(
                 t,
                 max(0, String.length(t) - 260),
                 min(260, String.length(t)),
               ),
          );
        };
        check(
          bool,
          "html after a plain evaluate",
          true,
          Haz3lcore.MvuShape.is_html(plain),
        );
        let incr =
          evaluate_incrementally(~before=parse_exp(program), ~after=term);
        check(
          bool,
          "html after incremental reuse",
          true,
          Haz3lcore.MvuShape.is_html(incr),
        );
        let probed = evaluate_with_probes(z);
        if (!Haz3lcore.MvuShape.is_html(probed)) {
          let txt =
            Language.Exp.show(Haz3lcore.MvuShape.strip_wrappers(probed));
          print_endline(
            "PROBED-HEAD: "
            ++ String.sub(txt, 0, min(900, String.length(txt))),
          );
        };
        check(
          bool,
          "html when evaluated WITH probe instrumentation",
          true,
          Haz3lcore.MvuShape.is_html(probed),
        );
        /* The worker's actual sequence: slice the pre-commit program, then
           slice the committed one reusing the first pass's IncrEval. */
        let z_before =
          switch (Haz3lcore.Parser.to_zipper(~root=Exp, program)) {
          | Some(z) => z
          | None => Alcotest.fail("could not parse the slide")
          };
        switch (evaluate_in_slices(z_before)) {
        | Error(msg) => Alcotest.fail("pre-commit slices: " ++ msg)
        | Ok((_, state)) =>
          let prev = Language.EvaluatorState.get_incr_eval(state);
          switch (evaluate_in_slices(~prev, z)) {
          | Error(msg) => Alcotest.fail("committed slices: " ++ msg)
          | Ok((value, _)) =>
            if (!Haz3lcore.MvuShape.is_html(value)) {
              let txt =
                Language.Exp.show(Haz3lcore.MvuShape.strip_wrappers(value));
              print_endline(
                "SLICED-REUSE-HEAD: "
                ++ String.sub(txt, 0, min(1200, String.length(txt))),
              );
            };
            check(
              bool,
              "html in slices with incremental reuse, as the worker does",
              true,
              Haz3lcore.MvuShape.is_html(value),
            );
          };
        };
      },
    ),
    test_case(
      "rejects values that are neither",
      `Quick,
      () => {
        check(string, "a number", "rejected", kind({|1 + 1|}));
        check(string, "a list", "rejected", kind({|[1, 2, 3]|}));
        check(string, "a string", "rejected", kind({|"Div"|}));
        check(string, "another ADT", "rejected", kind({|Some(3)|}));
        check(string, "a function", "rejected", kind({|fun x -> x|}));
        /* A 4-tuple whose update/view are not functions is just a tuple. */
        check(string, "a plain 4-tuple", "rejected", kind({|(1, 2, 3, 4)|}));
      },
    ),
  ],
);
