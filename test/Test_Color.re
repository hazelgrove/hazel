open Alcotest;
open Haz3lcore;
open Language;

module C = Language.BuiltinsADT.Color;

/* The colour ADT backs the Colors config slide. Two things need pinning that
   the type system cannot: that a value survives the syntax round-trip
   (exp_of -> of_exp), and that it renders to CSS the browser will accept. */

let colors: list((string, C.t)) = [
  ("opaque", C.Oklch(99., 0.012, 90.)),
  ("zero chroma", C.Oklch(0., 0., 0.)),
  ("hex", C.Hex("#293445")),
  ("transparent", C.Transparent),
  ("faded", C.Fade(C.Oklch(52., 0.03, 220.), 40.)),
  ("faded hex", C.Fade(C.Hex("#293445"), 12.5)),
  ("nested fade", C.Fade(C.Fade(C.Oklch(50., 0.1, 10.), 50.), 50.)),
];

let roundtrips = ((name, c: C.t), ()) =>
  check(
    bool,
    name ++ " survives exp_of -> of_exp",
    true,
    C.of_exp(C.exp_of(c)) == Some(c),
  );

/* Not a full CSS parser — just the properties a malformed colour would break:
   no OCaml-style trailing dot (90. is not valid inside oklch()), and a
   recognisable function or literal head. */
let renders = ((name, c: C.t), ()) => {
  let css = C.to_css(c);
  check(bool, name ++ ": non-empty", true, String.length(css) > 0);
  check(
    bool,
    name ++ ": no OCaml trailing dot in " ++ css,
    false,
    Util.StringUtil.match(Util.StringUtil.regexp("[0-9]\\.[^0-9]"), css)
    || Util.StringUtil.match(Util.StringUtil.regexp("[0-9]\\.$"), css),
  );
  check(
    bool,
    name ++ ": recognisable head in " ++ css,
    true,
    String.starts_with(~prefix="oklch(", css)
    || String.starts_with(~prefix="color-mix(", css)
    || String.starts_with(~prefix="#", css)
    || css == "transparent",
  );
};

/* A hue of 90 must render as "90", never "90." */
let integral_floats_render_clean = () =>
  check(
    string,
    "integral components lose the trailing dot",
    "oklch(90% 0 120)",
    C.to_css(C.Oklch(90., 0., 120.)),
  );

let tests = [
  (
    "Color.roundtrip",
    List.map(
      ((n, _) as c) => test_case(n, `Quick, roundtrips(c)),
      colors,
    ),
  ),
  (
    "Color.to_css",
    List.map(((n, _) as c) => test_case(n, `Quick, renders(c)), colors)
    @ [test_case("integral floats", `Quick, integral_floats_render_clean)],
  ),
];
