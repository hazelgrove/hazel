open Alcotest;

/* the Cmd-hover underline follows bare Cmd: a key event's modifiers give
   whether it shows */

let held = (~meta=false, ~ctrl=false, ~alt=false, ~shift=false, ()) =>
  Web.Page.MetaListener.held(~meta, ~ctrl, ~alt, ~shift);

let shows = (name, expected, actual) =>
  check(bool, name ++ " shows the underline", expected, actual);

let tests = (
  "PageKeys",
  [
    test_case("a Cmd press shows it", `Quick, () =>
      shows("Cmd down", true, held(~meta=true, ()))
    ),
    test_case("a Cmd release clears it", `Quick, () =>
      shows("Cmd up", false, held())
    ),
    test_case(
      "Cmd chords don't show it",
      `Quick,
      () => {
        shows("Cmd+Shift", false, held(~meta=true, ~shift=true, ()));
        shows("Cmd+Alt", false, held(~meta=true, ~alt=true, ()));
        shows("Cmd+Ctrl", false, held(~meta=true, ~ctrl=true, ()));
      },
    ),
    /* Cmd+C: the c's events carry Cmd alone */
    test_case("a key under bare Cmd keeps it", `Quick, () =>
      shows("Cmd+C", true, held(~meta=true, ()))
    ),
    test_case(
      "other keys don't show it",
      `Quick,
      () => {
        shows("c", false, held());
        shows("Shift", false, held(~shift=true, ()));
        shows("Ctrl", false, held(~ctrl=true, ()));
        shows("Alt", false, held(~alt=true, ()));
      },
    ),
  ],
);
