open Alcotest;
open Web;

/* `Page.Update.calculate` is the root of the app's calculate cycle: it fans out
 * to every mode and decides, via `~dynamics`, whether this frame may evaluate.
 *
 * Driving it needs `WorkerClient.use_worker := false`. With a worker it dies
 * under node on `Worker is not a constructor`, which is why the whole top-level
 * cycle had no coverage; without one, evaluation runs synchronously through
 * `WorkerServer.evaluate_sync` and the decisions are observable in the model.
 *
 * `~dynamics` is not a user setting: `Main.re` passes `false` for exactly one
 * frame, to build the boot model without paying for evaluation before the first
 * render, and `true` for every frame after. So the sequence worth modelling is
 * boot-then-first-real-frame. (A `false` frame after a result exists would blank
 * it, but the app never issues one, so that is left unasserted rather than
 * pinned as intended behaviour.)
 *
 * `apply` threads `Updated.is_edit` into `calculate` the way `Main.re` does,
 * because the statics debounce reads it. */

let mk_page = (): Page.Model.t => {
  let globals = Globals.Model.init();
  let (default_current, slides) = Lazy.force(Init.startup).scratch;
  let default_names = List.map(fst, slides);
  let scratch =
    ScratchMode.Persist.load_all(
      "scratch",
      ~settings=globals.settings.core,
      ~default_names,
      ~default_current,
    );
  let editors: Editors.Model.t = Scratch(scratch);
  {
    globals,
    editors,
    explain_this: ExplainThisModel.init,
    selection: Editors.Selection.default_selection(editors),
  };
};

let calculate = (~dynamics, ~is_edited=true, page) =>
  Page.Update.calculate(
    ~schedule_action=_ => (),
    ~is_edited,
    ~dynamics,
    page,
  );

/* One turn of the app loop: dispatch, then calculate with the flag the dispatch
   produced -- exactly what Main.re does. */
let apply = (action: Page.Update.t, page: Page.Model.t): Page.Model.t => {
  let updated =
    Page.Update.update(
      ~import_log=_ => (),
      ~get_log_and=_ => (),
      ~schedule_action=_ => (),
      action,
      page,
    );
  calculate(~dynamics=true, ~is_edited=updated.is_edit, updated.model);
};

/* Run `f` with evaluation kept in-process. */
let without_worker = f => {
  WorkerClient.use_worker := false;
  let restore = () => WorkerClient.use_worker := true;
  switch (f()) {
  | x =>
    restore();
    x;
  | exception exn =>
    restore();
    raise(exn);
  };
};

/* Replace the current cell's document, with its statics already calculated, so a
   test starts from a known program in the state a hydrated slide would be in. */
let with_program = (text: string, page: Page.Model.t): Page.Model.t => {
  let editor =
    switch (Haz3lcore.Parser.to_zipper(~root=Haz3lcore.Sort.Exp, text)) {
    | None => failwith("could not parse: " ++ text)
    | Some(z) =>
      Haz3lcore.Editor.Model.mk(z, ~root=Haz3lcore.Sort.Exp)
      |> CellEditor.Model.mk
      |> CellEditor.Update.calculate(
           ~settings=page.globals.settings.core,
           ~is_edited=true,
           ~queue_worker=None,
           ~stitch=x =>
           x
         )
    };
  switch (page.editors) {
  | Scratch(m) =>
    let sp: ScratchMode.Scratchpad.t = List.nth(m.scratchpads, m.current);
    let kind =
      switch (sp.kind) {
      | Code({agent, _}) =>
        ScratchMode.Scratchpad.Code({
          editor,
          agent,
        })
      | Drv(_) as k => k
      };
    {
      ...page,
      editors:
        Scratch({
          ...m,
          scratchpads:
            Util.ListUtil.put_nth(
              m.current,
              {
                ...sp,
                kind,
              },
              m.scratchpads,
            ),
        }),
    };
  | _ => failwith("expected a scratch mode")
  };
};

let current_result = (page: Page.Model.t): EvalResult.Model.t =>
  switch (page.editors) {
  | Scratch(m)
  | Documentation(m) =>
    switch (List.nth_opt(m.scratchpads, m.current)) {
    | Some({kind: Code({editor, _}), _}) => editor.result
    | _ => failwith("current scratchpad is not a code cell")
    }
  | Tutorial(_)
  | Exercises(_) => failwith("expected a scratch mode")
  };

let evaluated = (page: Page.Model.t) =>
  !EvalResult.Model.eval_is_pending(current_result(page));

let int_result = (page: Page.Model.t): option(string) =>
  switch (Util.Calc.get_value(current_result(page).result)) {
  | ResultOk({result, _}) =>
    switch (Language.Exp.term_of(result)) {
    | Atom(Int(n)) => Some(Util.Bigint.to_string(n))
    | _ => None
    }
  | _ => None
  };

/* Left, not Right: a freshly parsed zipper leaves the caret at the end of the
   document, so Move(Right) raises Cant_move. */
let move_left: Page.Update.t =
  Globals(ActiveEditor(Move(Local(Left, ByChar))));
let refresh_statics: Page.Update.t = Editors(Scratch(RefreshStatics));

let tests = (
  "Page",
  [
    /* The gate itself: `~dynamics=false` rewrites the core settings handed to
       every mode, so nothing downstream may evaluate. */
    test_case("dynamics off suppresses evaluation", `Quick, () =>
      without_worker(() =>
        check(
          bool,
          "the cell has no result",
          false,
          evaluated(calculate(~dynamics=false, mk_page())),
        )
      )
    ),
    test_case("dynamics on evaluates the current cell", `Quick, () =>
      without_worker(() =>
        check(
          bool,
          "the cell has a result",
          true,
          evaluated(calculate(~dynamics=true, mk_page())),
        )
      )
    ),
    /* The gate must not be sticky. A suppressed frame still runs statics, so the
       elaboration is now cached; if the next frame's request were gated purely
       on "did the elaboration change", turning dynamics on would never evaluate
       and the cell would stay blank until the user typed. */
    test_case("dynamics on after the boot frame still evaluates", `Quick, () =>
      without_worker(() => {
        let page = mk_page() |> calculate(~dynamics=false);
        check(bool, "boot frame has no result", false, evaluated(page));
        let page = calculate(~dynamics=true, page);
        check(bool, "the next frame evaluates", true, evaluated(page));
      })
    ),
    test_case("a known program evaluates through the whole tree", `Quick, () =>
      without_worker(() =>
        check(
          option(string),
          "result",
          Some("2"),
          mk_page()
          |> with_program("1 + 1")
          |> calculate(~dynamics=true, ~is_edited=false)
          |> int_result,
        )
      )
    ),
    /* The typing debounce, at the top level. An edit defers statics
       (`StaticsDebounce.consume` returns StaticsDefer whenever is_edited), so
       for that frame the result the user sees is deliberately the OLD one, and
       the scheduled `RefreshStatics` is what finishes the job. Both halves are
       asserted, because "stale for one frame" and "stale forever" look
       identical from a single sample. */
    test_case(
      "an edit defers statics, then RefreshStatics completes it", `Quick, () =>
      without_worker(() => {
        let page =
          mk_page()
          |> with_program("1 + 1")
          |> calculate(~dynamics=true, ~is_edited=false);
        check(
          option(string),
          "before the edit",
          Some("2"),
          int_result(page),
        );
        let page =
          page
          |> apply(Globals(ActiveEditor(Move(End))))
          |> apply(Globals(ActiveEditor(Insert("0"))));
        check(
          option(string),
          "the edited frame still shows the old result",
          Some("2"),
          int_result(page),
        );
        let page = page |> apply(refresh_statics);
        check(
          option(string),
          "the refresh re-evaluates",
          Some("11"),
          int_result(page),
        );
      })
    ),
    /* A cursor move is not an edit, so it must neither re-evaluate nor lose the
       result already on screen. */
    test_case("a cursor move keeps the result", `Quick, () =>
      without_worker(() =>
        check(
          option(string),
          "result survives",
          Some("2"),
          mk_page()
          |> with_program("1 + 1")
          |> calculate(~dynamics=true, ~is_edited=false)
          |> apply(move_left)
          |> int_result,
        )
      )
    ),
  ],
);
