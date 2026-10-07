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
        /* Sized against the placeholder rather than a fixed length: slide
           sets vary, and a short slide is still hydrated. */
        let placeholder_len = zip_len(List.nth(m.scratchpads, 5));
        check(
          bool,
          "another slide is a placeholder",
          true,
          placeholder_len <= 2,
        );
        check(
          bool,
          "current slide is hydrated",
          true,
          zip_len(List.nth(m.scratchpads, 0)) > placeholder_len,
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
          zip_len(List.nth(m2.scratchpads, 5)) > placeholder_len,
        );
        /* idempotent: hydrating again is a no-op */
        let m3 = ScratchMode.Persist.hydrate_current(~settings, "doc", m2);
        check(bool, "second hydration is a no-op", true, m3 == m2);
      },
    ),
  ],
);
