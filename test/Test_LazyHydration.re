open Alcotest;
open Web;

/* Boot builds a full editor only for the CURRENT slide; the rest are
   dormant placeholders hydrated on first switch. */

let zip_len = (sp: ScratchMode.Scratchpad.t): int =>
  switch (sp.kind) {
  | Code({editor, _}) =>
    List.length(Haz3lcore.Zipper.zip(editor.editor.editor.state.zipper))
  | Drv(_) => (-1)
  };

let dormant_count = (m: ScratchMode.Model.t): int =>
  m.scratchpads
  |> List.filter((sp: ScratchMode.Scratchpad.t) => sp.dormant)
  |> List.length;

/* The tests above drive `Persist.hydrate_current` directly. These drive the
   action a slide switch actually dispatches, which is a separate question: the
   function can be correct while nothing calls it. */
let doc_names = () =>
  List.map(fst, snd(Lazy.force(Init.startup).documentation));

let loaded = () =>
  ScratchMode.Persist.load_all(
    "doc",
    ~settings=Settings.Model.init.core,
    ~default_names=doc_names(),
    ~default_current=0,
  );

let switch_slide = (i, m: ScratchMode.Model.t) =>
  ScratchMode.Update.update(
    ~schedule_action=_ => (),
    ~settings=Settings.Model.init,
    ~is_documentation=true,
    ScratchMode.Update.SwitchSlide(i),
    m,
  );

let tests = (
  "LazyHydration",
  [
    test_case(
      "boot hydrates only the current slide",
      `Quick,
      () => {
        let settings = Language.CoreSettings.on;
        let names =
          List.map(fst, snd(Lazy.force(Init.startup).documentation));
        let m =
          ScratchMode.Persist.load_all(
            "doc",
            ~settings,
            ~default_names=names,
            ~default_current=0,
          );
        check(
          int,
          "all but current are dormant",
          List.length(names) - 1,
          dormant_count(m),
        );
        check(
          bool,
          "current slide is hydrated",
          true,
          zip_len(List.nth(m.scratchpads, 0)) > 10,
        );
        check(
          bool,
          "another slide is a placeholder",
          true,
          zip_len(List.nth(m.scratchpads, 5)) <= 2,
        );
        let m2 =
          ScratchMode.Persist.hydrate_current(
            ~settings,
            "doc",
            {
              ...m,
              current: 5,
            },
          );
        check(
          int,
          "hydration consumes the dormant entry",
          List.length(names) - 2,
          dormant_count(m2),
        );
        check(
          bool,
          "switched slide is now hydrated",
          true,
          zip_len(List.nth(m2.scratchpads, 5)) > 10,
        );
        /* idempotent: hydrating again is a no-op */
        let m3 = ScratchMode.Persist.hydrate_current(~settings, "doc", m2);
        check(bool, "second hydration is a no-op", true, m3 == m2);
      },
    ),
    test_case(
      "the switch action hydrates the target slide",
      `Quick,
      () => {
        let m = loaded();
        check(
          bool,
          "target starts dormant",
          true,
          zip_len(List.nth(m.scratchpads, 5)) <= 2,
        );
        let m2 = switch_slide(5, m).model;
        check(int, "current follows the switch", 5, m2.current);
        check(
          bool,
          "target is hydrated",
          true,
          zip_len(List.nth(m2.scratchpads, 5)) > 10,
        );
        check(
          int,
          "one fewer dormant slide",
          dormant_count(m) - 1,
          dormant_count(m2),
        );
      },
    ),
    /* Switching slides is navigation, not an edit: if it were historic it would
       land on the undo stack and Ctrl-Z would silently change slides. */
    test_case("the switch action is not historic", `Quick, () =>
      check(bool, "historic", false, switch_slide(5, loaded()).historic)
    ),
    /* Coming back must not undo the work: the slide left behind stays hydrated,
       or every switch pays full parse cost again. */
    test_case(
      "switching back leaves both slides hydrated",
      `Quick,
      () => {
        let m = loaded();
        let m2 =
          switch_slide(5, m).model |> switch_slide(0) |> (u => u.model);
        check(int, "back on the first slide", 0, m2.current);
        check(
          int,
          "still only one slide was hydrated by the round trip",
          dormant_count(m) - 1,
          dormant_count(m2),
        );
        check(
          bool,
          "the slide we left is still hydrated",
          true,
          zip_len(List.nth(m2.scratchpads, 5)) > 10,
        );
      },
    ),
  ],
);
