open Alcotest;
open Web;

/* `Editors.Update.update` dispatches to the active mode and owns mode
 * switching. Switching a mode means loading it from storage and rebuilding its
 * editors, so the interesting property is when that work is skipped: selecting
 * the mode you are already in must be quiet, or every click on the current
 * mode's button reloads and re-elaborates everything. */

let globals = Globals.Model.init();

let scratch = (): Editors.Model.t =>
  Scratch(
    ScratchMode.Persist.load_all(
      "scratch",
      ~settings=globals.settings.core,
      ~default_names=List.map(fst, snd(Lazy.force(Init.startup).scratch)),
      ~default_current=fst(Lazy.force(Init.startup).scratch),
    ),
  );

let update = (action: Editors.Update.t, model: Editors.Model.t) =>
  Editors.Update.update(~globals, ~schedule_action=_ => (), action, model);

let mode_of = (model: Editors.Model.t) => Editors.Model.mode_string(model);

let tests = (
  "Editors",
  [
    /* Re-selecting the current mode must not reload it. */
    test_case(
      "switching to the current mode is quiet",
      `Quick,
      () => {
        let before = scratch();
        let u = update(SwitchMode(Scratch), before);
        check(bool, "no recalculate", false, u.recalculate);
        check(bool, "model is untouched", true, u.model === before);
      },
    ),
    /* And a real switch has to actually switch, and ask for a recalculate --
       the new mode's editors have no statics yet. */
    test_case(
      "switching to another mode loads it",
      `Quick,
      () => {
        let u = update(SwitchMode(Documentation), scratch());
        check(string, "mode", "Documentation", mode_of(u.model));
        check(bool, "recalculate requested", true, u.recalculate);
      },
    ),
    test_case(
      "switching back returns to scratch",
      `Quick,
      () => {
        let doc = update(SwitchMode(Documentation), scratch()).model;
        let u = update(SwitchMode(Scratch), doc);
        check(string, "mode", "Scratch", mode_of(u.model));
        check(bool, "recalculate requested", true, u.recalculate);
      },
    ),
    /* An action addressed to a mode that is not active is dropped rather than
       applied to the wrong model. */
    test_case(
      "a scratch action reaches documentation too",
      `Quick,
      () => {
        /* Scratch and Documentation share ScratchMode, so a Scratch action is
           deliberately accepted by either -- pinned because the pairing is easy
           to break when a case is added. */
        let doc = update(SwitchMode(Documentation), scratch()).model;
        let u = update(Scratch(RefreshStatics), doc);
        check(
          string,
          "still documentation",
          "Documentation",
          mode_of(u.model),
        );
      },
    ),
    /* The other two modes load from their own stores; exercising the switch
       catches a load-path regression that the scratch-only tests cannot. */
    test_case(
      "switching to exercises loads it",
      `Quick,
      () => {
        let u = update(SwitchMode(Exercises), scratch());
        check(string, "mode", "Exercises", mode_of(u.model));
        check(bool, "recalculate requested", true, u.recalculate);
      },
    ),
    test_case(
      "switching to tutorial loads it",
      `Quick,
      () => {
        let u = update(SwitchMode(Tutorial), scratch());
        check(string, "mode", "Tutorial", mode_of(u.model));
        check(bool, "recalculate requested", true, u.recalculate);
      },
    ),
    /* Asymmetry, pinned as observed rather than endorsed: re-selecting the
       current mode is quiet for Scratch, Documentation and Exercises, but
       Tutorial raises InvalidAction instead. Nothing depends on the difference
       today -- the mode buttons are disabled for the active mode -- but it is a
       trap for anyone adding a mode by copying a neighbouring case. */
    test_case(
      "re-selecting tutorial raises rather than being quiet",
      `Quick,
      () => {
        let tutorial = update(SwitchMode(Tutorial), scratch()).model;
        check_raises("InvalidAction", Updated.InvalidAction, () =>
          ignore(update(SwitchMode(Tutorial), tutorial))
        );
      },
    ),
  ],
);
