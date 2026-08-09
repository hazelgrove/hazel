open Alcotest;
open Web;

/* Boot builds a full editor only for the CURRENT slide; the rest are
   blank placeholders registered in ScratchMode.dormant_slides and get
   hydrated on first switch. */

let zip_len = (sp: ScratchMode.Scratchpad.t): int =>
  switch (sp.kind) {
  | Code({editor, _}) =>
    List.length(Haz3lcore.Zipper.zip(editor.editor.editor.state.zipper))
  | Drv(_) => (-1)
  };

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
          Hashtbl.length(ScratchMode.dormant_slides),
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
          Hashtbl.length(ScratchMode.dormant_slides),
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
  ],
);
