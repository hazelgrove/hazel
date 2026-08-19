open Alcotest;
open Web;

/* `Page.Update.calculate` is the root of the app's calculate cycle: it fans out
 * to every mode and decides, via its `~dynamics` argument, whether this frame is
 * allowed to evaluate at all.
 *
 * Driving it in a test needs `WorkerClient.use_worker := false`. With a worker,
 * `calculate` dies on `Worker is not a constructor` under node, which is why the
 * whole top-level cycle had no coverage; without one, evaluation runs
 * synchronously through `WorkerServer.evaluate_sync` and the same decisions are
 * observable in the returned model.
 *
 * `~dynamics` is not a user setting: `Main.re` passes `false` for exactly one
 * frame, to build the boot model without paying for evaluation before the first
 * render, and `true` for every frame after that. So the sequence worth modelling
 * is boot-then-first-real-frame, which is what these tests do. (A `false` frame
 * after a result exists would blank it, but the app never issues one, so that is
 * left unasserted rather than pinned as intended behaviour.) */

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

let calculate = (~dynamics, page) =>
  Page.Update.calculate(
    ~schedule_action=_ => (),
    ~is_edited=true,
    ~dynamics,
    page,
  );

/* Run `f` with evaluation kept in-process. */
let without_worker = f => {
  WorkerClient.use_worker := false;
  let result =
    switch (f()) {
    | x => x
    | exception exn =>
      WorkerClient.use_worker := true;
      raise(exn);
    };
  WorkerClient.use_worker := true;
  result;
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
       on "did the elaboration change", turning dynamics back on would never
       evaluate and the cell would stay blank until the user typed. */
    test_case(
      "dynamics on after a suppressed frame still evaluates", `Quick, () =>
      without_worker(() => {
        let page = mk_page() |> calculate(~dynamics=false);
        check(
          bool,
          "suppressed frame has no result",
          false,
          evaluated(page),
        );
        let page = calculate(~dynamics=true, page);
        check(bool, "the next frame evaluates", true, evaluated(page));
      })
    ),
  ],
);
