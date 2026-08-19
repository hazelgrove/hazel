open Alcotest;
open Web;
open Haz3lcore;

/* `CodeEditable.Update.update` is where an action's downstream cost is decided.
 * It returns an `Updated.t`, and three of its flags drive real work:
 *
 *   recalculate  -- whether the whole app redoes its `calculate` pass
 *   is_edit      -- whether the change needs re-evaluation (and an autosave)
 *   historic     -- whether the undo stack gets a new entry
 *
 * Getting these wrong fails quietly in both directions: too eager and every
 * keystroke-adjacent event re-evaluates the program, too lazy and the user
 * looks at stale results. Nothing in the UI reports either one, so they are
 * worth pinning directly. */

let settings = Settings.Model.init;

let with_probe_all = (settings: Settings.Model.t) => {
  ...settings,
  core: {
    ...settings.core,
    probe_all: true,
  },
};

let of_text = (~settings=settings, text: string): CodeEditable.Model.t => {
  let model =
    switch (Parser.to_zipper(~root=Sort.Exp, text)) {
    | None => failwith("could not parse: " ++ text)
    | Some(z) =>
      Editor.Model.mk(z, ~root=Sort.Exp) |> CodeWithStatics.Model.mk
    };
  CodeWithStatics.Update.calculate(
    ~settings=settings.core,
    ~is_edited=true,
    ~stitch=x => x,
    ~dynamics=model.dynamics,
    ~is_dynamic_term=false,
    model,
  );
};

let update = (~settings=settings, action, model) =>
  CodeEditable.Update.update(~settings, action, model);

let program = "1 + 1";

/* (recalculate, is_edit, historic) for an action applied to `program`. */
let flags = (~settings=settings, action) => {
  let u = update(~settings, action, of_text(~settings, program));
  (u.recalculate, u.is_edit, u.historic);
};

let triple = (name, expected, actual) =>
  check(
    triple(bool, bool, bool),
    name ++ " (recalculate, is_edit, historic)",
    expected,
    actual,
  );

let tests = (
  "CodeEditable",
  [
    test_case("an insertion edits, recalculates and is undoable", `Quick, () =>
      triple("Insert", (true, true, true), flags(Perform(Insert("2"))))
    ),
    test_case("a cursor move recalculates but is not an edit", `Quick, () =>
      triple("Move(End)", (true, false, false), flags(Perform(Move(End))))
    ),
    /* Opening the context menu changes nothing the program depends on, so it
       must not trigger an app-wide recalculate. */
    test_case("opening the context menu is quiet", `Quick, () =>
      triple(
        "ContextMenu(Open)",
        (false, false, false),
        flags(ContextMenu(Open)),
      )
    ),
    /* Probe actions normally require re-evaluation, because a new probe needs a
       sample that the last evaluation did not capture. */
    test_case("a probe action is an edit by default", `Quick, () =>
      triple(
        "Probe(RemoveAll)",
        (true, true, true),
        flags(Perform(Probe(RemoveAll))),
      )
    ),
    /* With probe_all on, every subexpression is already sampled, so adding or
       removing a probe needs no re-evaluation. This is the one place the flag
       is computed rather than looked up, and the only thing that would notice
       a regression is the clock. */
    test_case(
      "probe_all makes a probe action not an edit",
      `Quick,
      () => {
        let settings = with_probe_all(settings);
        triple(
          "Probe(RemoveAll) under probe_all",
          (true, false, true),
          flags(~settings, Perform(Probe(RemoveAll))),
        );
      },
    ),
    /* probe_all must not suppress re-evaluation for actions that really do
       change the program. */
    test_case(
      "probe_all still treats an insertion as an edit",
      `Quick,
      () => {
        let settings = with_probe_all(settings);
        triple(
          "Insert under probe_all",
          (true, true, true),
          flags(~settings, Perform(Insert("2"))),
        );
      },
    ),
  ],
);
