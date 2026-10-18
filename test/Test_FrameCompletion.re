open Alcotest;
open Haz3lcore;

/* The inspector's chip lookup and TyDi complete the frame's segment, the
   one the editor's syntax holds (CompletionQuery.engine_seg), so they hit
   CanonicalCompletion's item memo. A fresh zip rebuilds the caret's
   ancestors, so the caret's item would miss. Programs keep the caret
   inside a closed tile, which a zip rebuilds. */

let settings = Language.CoreSettings.on;

let zipper = Test_TabDispatch.padding_zipper;

/* CanonicalCompletion item-memo misses during [f] */
let misses = (f: unit => 'a): ('a, int) => {
  let n0 = CanonicalCompletion.items_missed^;
  let r = f();
  (r, CanonicalCompletion.items_missed^ - n0);
};

/* a frame as the editor builds it: the syntax, then statics on its
   segment and item parses */
let frame = (z: Zipper.t): (CachedSyntax.t, CachedStatics.t) => {
  let syntax = CachedSyntax.init(~root=Exp, z);
  let source = CachedSyntax.semantic_source(z, syntax);
  check(bool, "the syntax's segment is current", true, source != None);
  let statics =
    CachedStatics.init_compositional(
      ~settings,
      ~stitch=Fun.id,
      ~root=Exp,
      ~shared=syntax.t_cache,
      ~source?,
      z,
    );
  (syntax, statics);
};

let engine_seg_cases = [
  test_case(
    "the frame's segment, not a zip",
    `Quick,
    () => {
      let z = zipper("(1 + ¦");
      let syntax = CachedSyntax.init(~root=Exp, z);
      check(
        bool,
        "current: the syntax's own",
        true,
        CompletionQuery.engine_seg(~syntax, z) === syntax.segment,
      );
      let moved = Test_Editing.perform(z, [Move(Local(Left, ByChar))]);
      let seg = CompletionQuery.engine_seg(~syntax, moved);
      check(bool, "moved: a fresh zip", false, seg === syntax.segment);
      check(
        bool,
        "moved: the semantic source",
        true,
        seg == MakeTerm.semantic_source(moved),
      );
      let buffered = {
        ...z,
        selection: Selection.mk_buffer(Unparsed, []),
      };
      check(
        bool,
        "unparsed buffer: the semantic source",
        true,
        CompletionQuery.engine_seg(~syntax, buffered)
        == MakeTerm.semantic_source(buffered),
      );
    },
  ),
];

let expected_type = Test_ImpliedHole.expected_type;

let inspector_case = (input, expected) =>
  test_case(
    "inspector: " ++ String.escaped(input),
    `Quick,
    () => {
      let z = zipper(input);
      let (syntax, statics) = frame(z);
      let (info, n) =
        misses(() => ImpliedHole.at_caret(~syntax, ~statics, z));
      check(int, "no misses on the frame's segment", 0, n);
      check(string, "implied hole", expected, expected_type(info));
      /* the zipper's own zip misses: the counter sees a re-zip */
      let (info', n') = misses(() => ImpliedHole.at_caret(~statics, z));
      check(bool, "a fresh zip misses", true, n' > 0);
      let id = Option.map(Language.Info.id_of);
      check(bool, "the same hole", true, id(info) == id(info'));
      check(
        string,
        "same information",
        expected_type(info),
        expected_type(info'),
      );
    },
  );

let tydi_case = (input, expected) =>
  test_case(
    "TyDi: " ++ String.escaped(input),
    `Quick,
    () => {
      let z = zipper(input);
      let (syntax, _) = frame(z);
      let seg = CompletionQuery.engine_seg(~syntax, z);
      ignore(CanonicalCompletion.for_editor(seg));
      let (suggestions, n) = misses(() => TyDi.suggest_witnesses(~seg, z));
      check(int, "no misses on the frame's segment", 0, n);
      check(
        list(string),
        "witnesses",
        expected,
        List.map((s: TyDiSuggestion.t) => s.content, suggestions),
      );
      let (suggestions', n') = misses(() => TyDi.suggest_witnesses(z));
      check(bool, "a fresh zip misses", true, n' > 0);
      check(bool, "same suggestions", true, suggestions == suggestions');
    },
  );

/* an edit as the editor runs it: update, then calculate with TyDi */
let edit = (input: string, a: Action.t): Editor.Model.t => {
  let model = Editor.Model.mk(zipper(input), ~root=Exp);
  let old_statics =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=true,
      ~stitch=Fun.id,
      ~root=Exp,
      model.state.zipper,
    );
  let model =
    switch (
      Editor.Update.update(
        ~settings,
        a,
        old_statics,
        Language.Dynamics.Map.empty,
        model,
      )
    ) {
    | Ok(m) => m
    | Error(e) => failf("update: %s", Action.Failure.show(e))
    };
  let statics =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=true,
      ~stitch=Fun.id,
      ~root=Exp,
      model.state.zipper,
    );
  Editor.Update.calculate(
    ~settings,
    ~autoprobe_mode=AutoProbe.Off,
    ~is_edited=true,
    statics,
    Language.Dynamics.Map.empty,
    model,
  );
};

let edit_cases = [
  test_case(
    "an edit: TyDi completes the syntax's segment",
    `Quick,
    () => {
      let model = edit("(if 1 < 2 then 3 ¦ 4)", Insert("x"));
      let z = model.state.zipper;
      check(bool, "no buffer", false, Selection.is_buffer(z.selection));
      check(
        bool,
        "the syntax holds the zipper's zip",
        true,
        model.syntax.segment == Zipper.unselect_and_zip(z),
      );
      let seg = CompletionQuery.engine_seg(~syntax=model.syntax, z);
      check(bool, "current", true, seg === model.syntax.segment);
      let (_, n) =
        misses(() => CanonicalCompletion.complete_items(~sort=Exp, seg));
      check(int, "TyDi completed the same pieces", 0, n);
    },
  ),
  test_case(
    "an edit: a TyDi buffer is the syntax's own zip",
    `Quick,
    () => {
      let model = edit("(if 1 < 2 then 3 e¦ 4)", Insert("l"));
      let z = model.state.zipper;
      check(
        option(string),
        "the witness remainder",
        Some("se"),
        TyDi.get_unparsed_buffer(z),
      );
      check(
        bool,
        "the syntax holds the buffered zip",
        true,
        model.syntax.segment == Zipper.unselect_and_zip(z),
      );
    },
  ),
];

/* Tab's query and its ApplyCompletion(Next) read the frame's segment */
let tab_case = (input, expected) =>
  test_case(
    "Tab: " ++ String.escaped(input),
    `Quick,
    () => {
      let z = zipper(input);
      let (syntax, statics) = frame(z);
      let seg = CompletionQuery.engine_seg(~syntax, z);
      ignore(CanonicalCompletion.for_editor(seg));
      let apply = syntax =>
        switch (
          Perform.go(
            ~settings,
            ~statics,
            ~syntax,
            ApplyCompletion(Next),
            {
              zipper: z,
              col_target: None,
            },
            ~root=Exp,
          )
        ) {
        | Ok(z) => Printer.of_zipper(~holes="?", ~caret="|", z)
        | Error(e) => Action.Failure.show(e)
        };
      let (a, n) = misses(() => CompletionQuery.tab_action(~seg, z));
      check(int, "tab_action: no misses", 0, n);
      let (applied, n) = misses(() => apply(syntax));
      check(int, "ApplyCompletion(Next): no misses", 0, n);
      check(string, "applied", expected, applied);
      /* the zipper's own zips (they miss, so they go last) */
      check(
        bool,
        "the same action",
        true,
        a == CompletionQuery.tab_action(z),
      );
      /* a syntax zipped from another zipper: Perform zips [z] itself */
      let stale = CachedSyntax.init(~root=Exp, Zipper.init());
      let (applied', n') = misses(() => apply(stale));
      check(bool, "a fresh zip misses", true, n' > 0);
      check(string, "the same edit", applied, applied');
    },
  );

let tests = [
  (
    "FrameCompletion",
    engine_seg_cases
    @ [
      inspector_case("(let x : Int = ¦\nlet y = 2 in y)", "Int"),
      inspector_case("(let f(x : Int) : Bool = ¦\nlet y = 2 in y)", "Bool"),
      tydi_case("(if 1 < 2 then 3 el¦ 4)", ["else"]),
      tab_case("(let x = 1¦\nx)", "(let x = 1 in|\nx)"),
    ]
    @ edit_cases,
  ),
];
