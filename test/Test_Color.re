open Alcotest;

module C = Language.BuiltinsADT.Color;

/* The colour ADT backs the Colors config slide. Two things need pinning that
   the type system cannot: that a value survives the syntax round-trip
   (exp_of -> of_exp), and that it renders to CSS the browser will accept. */

let colors: list((string, C.t)) = [
  ("opaque", C.Oklch(99., 0.012, 90.)),
  ("zero chroma", C.Oklch(0., 0., 0.)),
  ("rgb bytes", C.Rgb(41, 52, 69)),
  ("transparent", C.Transparent),
  ("faded", C.Fade(C.Oklch(52., 0.03, 220.), 40.)),
  ("faded rgb", C.Fade(C.Rgb(41, 52, 69), 12.5)),
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
    || String.starts_with(~prefix="rgb(", css)
    || String.starts_with(~prefix="#", css)
    || css == "oklch(0 0 0 / 0)",
  );
};

/* A hue of 90 must render as "90", never "90." */
/* %g would have emitted "1e-05" / "1.23457e+06" here, and string_of_float
   "90." — all invalid inside oklch(), and setProperty swallows an invalid
   value silently, so the variable just keeps its old colour. */
let no_scientific_notation = () => {
  check(
    string,
    "small chroma stays decimal",
    "oklch(50% 0.00001 120)",
    C.to_css(C.Oklch(50., 0.00001, 120.)),
  );
  check(
    string,
    "large value stays decimal",
    "oklch(50% 0 1234567)",
    C.to_css(C.Oklch(50., 0., 1234567.)),
  );
};

/* A hole or a divide-by-zero in the config yields nan/inf. Pin to 0 rather
   than emit CSS the browser will silently reject. */
let non_finite_is_pinned = () => {
  check(string, "nan", "oklch(0% 0 0)", C.to_css(C.Oklch(nan, nan, nan)));
  check(
    string,
    "infinity",
    "oklch(0% 0 0)",
    C.to_css(C.Oklch(infinity, neg_infinity, infinity)),
  );
};

let integral_floats_render_clean = () =>
  check(
    string,
    "integral components lose the trailing dot",
    "oklch(90% 0 120)",
    C.to_css(C.Oklch(90., 0., 120.)),
  );

/* ---- colour arithmetic (BuiltinsColor) ---- */

module M = Language.BuiltinsColor;

let approx = (a: float, b: float) => Float.abs(a -. b) < 0.0001;

let oklch_is = (msg, (l, c, h), actual: C.t) =>
  switch (actual) {
  | C.Oklch(l', c', h') =>
    check(
      bool,
      msg
      ++ " expected ("
      ++ C.num(l)
      ++ ", "
      ++ C.num(c)
      ++ ", "
      ++ C.num(h)
      ++ ") got ("
      ++ C.num(l')
      ++ ", "
      ++ C.num(c')
      ++ ", "
      ++ C.num(h')
      ++ ")",
      true,
      approx(l, l') && approx(c, c') && approx(h, h'),
    )
  | _ => fail(msg ++ ": not an Oklch")
  };

let lighten = (c, by) => M.map_oklch(((l, ch, h)) => (l +. by, ch, h), c);

/* Lightness is a percentage: pushing past either end must saturate rather
   than produce a colour the browser will reject. */
let lightness_clamps = () => {
  oklch_is(
    "over 100",
    (100., 0.05, 120.),
    lighten(C.Oklch(95., 0.05, 120.), 20.),
  );
  oklch_is(
    "under 0",
    (0., 0.05, 120.),
    lighten(C.Oklch(5., 0.05, 120.), -20.),
  );
};

/* Hue is circular, so rotation wraps in both directions. */
let hue_wraps = () => {
  let rot = (c, by) => M.map_oklch(((l, ch, h)) => (l, ch, h +. by), c);
  oklch_is("past 360", (50., 0.1, 10.), rot(C.Oklch(50., 0.1, 350.), 20.));
  oklch_is("below 0", (50., 0.1, 350.), rot(C.Oklch(50., 0.1, 10.), -20.));
};

/* Chroma cannot go negative. */
let chroma_floors = () =>
  oklch_is(
    "negative chroma",
    (50., 0., 120.),
    M.map_oklch(
      ((l, ch, h)) => (l, ch -. 1., h),
      C.Oklch(50., 0.1, 120.),
    ),
  );

/* The interesting case: 350 -> 10 must cross zero, not sweep 340 degrees
   backwards through the wheel. Midpoint is 0, not 180. */
let mix_takes_short_way_round = () => {
  oklch_is(
    "350 -> 10 midpoint",
    (50., 0.1, 0.),
    M.mix(C.Oklch(50., 0.1, 350.), C.Oklch(50., 0.1, 10.), 0.5),
  );
  oklch_is(
    "10 -> 350 midpoint",
    (50., 0.1, 0.),
    M.mix(C.Oklch(50., 0.1, 10.), C.Oklch(50., 0.1, 350.), 0.5),
  );
};

let mix_endpoints = () => {
  oklch_is(
    "t=0 is the first colour",
    (20., 0.02, 90.),
    M.mix(C.Oklch(20., 0.02, 90.), C.Oklch(80., 0.2, 200.), 0.),
  );
  oklch_is(
    "t=1 is the second",
    (80., 0.2, 200.),
    M.mix(C.Oklch(20., 0.02, 90.), C.Oklch(80., 0.2, 200.), 1.),
  );
  oklch_is(
    "t clamps above 1",
    (80., 0.2, 200.),
    M.mix(C.Oklch(20., 0.02, 90.), C.Oklch(80., 0.2, 200.), 5.),
  );
};

/* `Transparent` has nothing to adjust, so it passes through rather than
   erroring or degrading to black. It is now the ONLY form that does: `Rgb` is
   converted into the working space and therefore responds, which is the whole
   difference between it and the `Hex` constructor it replaced. */
let opaque_forms_pass_through = () => {
  check(
    bool,
    "transparent unchanged",
    true,
    lighten(C.Transparent, 20.) == C.Transparent,
  );
  /* Not just "changed": an adjusted Rgb lands as Oklch, because that is the
     space the adjustment happened in. */
  check(
    bool,
    "rgb responds to lightening, as oklch",
    true,
    switch (lighten(C.Rgb(41, 52, 69), 20.)) {
    | Oklch(l, _, _) =>
      let (l0, _, _) = C.oklch_of_rgb((41, 52, 69));
      Float.abs(l -. (l0 +. 20.)) < 0.001;
    | _ => false
    },
  );
};

/* A faded colour still responds to lightening, through the wrapper. */
let fade_is_transparent_to_maths = () =>
  switch (lighten(C.Fade(C.Oklch(50., 0.1, 120.), 40.), 10.)) {
  | C.Fade(inner, a) =>
    check(bool, "alpha preserved", true, approx(a, 40.));
    oklch_is("inner lightened", (60., 0.1, 120.), inner);
  | _ => fail("Fade did not survive the transformation")
  };

let math_tests = [
  test_case("lightness clamps", `Quick, lightness_clamps),
  test_case("hue wraps", `Quick, hue_wraps),
  test_case("chroma floors at 0", `Quick, chroma_floors),
  test_case(
    "mix takes the short way round",
    `Quick,
    mix_takes_short_way_round,
  ),
  test_case("mix endpoints", `Quick, mix_endpoints),
  test_case(
    "hex/transparent pass through",
    `Quick,
    opaque_forms_pass_through,
  ),
  test_case("fade survives maths", `Quick, fade_is_transparent_to_maths),
];

/* The picker converts to sRGB to show a hex code and back when one is typed
   in, so a round trip has to land where it started. It cannot be exact —
   sRGB has 8 bits per channel and OKLCH does not — but it must be closer than
   a quantisation step, or a colour would visibly drift each time someone
   opened the RGB tab. */
let srgb_roundtrips = ((name, l, c, h), ()) => {
  let (l', c', h') = C.oklch_of_rgb(C.rgb_of_oklch((l, c, h)));
  let near = (what, expected, got, tol) =>
    check(
      bool,
      Printf.sprintf(
        "%s: %s %.3f -> %.3f (tol %.3f)",
        name,
        what,
        expected,
        got,
        tol,
      ),
      true,
      Float.abs(expected -. got) < tol,
    );
  near("lightness", l, l', 0.6);
  near("chroma", c, c', 0.006);
  /* Hue is meaningless at zero chroma, where the round trip may return any
     angle for the same colour. */
  if (c > 0.02) {
    near("hue", h, h', 2.0);
  };
};

/* In-gamut colours only: OKLCH describes colours sRGB cannot hold, and those
   clamp on the way through — which is correct, but not a round trip. */
let srgb_colors = [
  ("mid grey", 52., 0.0, 0.),
  ("sand", 99., 0.012, 90.),
  ("stone", 52., 0.03, 220.),
  ("moss", 70., 0.15, 150.),
  ("clay", 97., 0.025, 90.),
  ("shale dark", 30., 0.04, 250.),
];

let hex_parses = () => {
  let eq = (what, a, b) => check(string, what, a, b);
  /* white and black are the corners the conversion is easiest to get wrong */
  eq("white", "#ffffff", C.hex_of_oklch((100., 0., 0.)));
  eq("black", "#000000", C.hex_of_oklch((0., 0., 0.)));
  let hex = s =>
    switch (C.oklch_of_css(s)) {
    | Some(t) => C.hex_of_oklch(t)
    | None => "unparsed"
    };
  eq("#rrggbb", "#3366cc", hex("#3366cc"));
  eq("shorthand expands", "#3366cc", hex("#36c"));
  eq("no hash", "#3366cc", hex("3366cc"));
  eq("uppercase", "#3366cc", hex("#3366CC"));
  eq("rgb()", "#3366cc", hex("rgb(51, 102, 204)"));
  check(bool, "garbage rejected", true, C.oklch_of_css("nope") == None);
};

/* --- HSV ---------------------------------------------------------------

   Unlike the OKLCH round trip above, this one has no excuse to lose anything:
   both ends are the same 8-bit cube, so it must be exact. */
let show_rgb = ((r, g, b): (int, int, int)): string =>
  Printf.sprintf("(%d,%d,%d)", r, g, b);

let hsv_roundtrips_exactly = () => {
  let grey = (0., 0., 0.);
  /* Every corner and edge midpoint of the cube, plus a stride across it: the
     sector arithmetic changes branch at each face, and an off-by-one in
     `mod 6` only shows at a boundary. */
  let edges = [0, 1, 127, 128, 254, 255];
  let cube =
    List.concat_map(
      r => List.concat_map(g => List.map(b => (r, g, b), edges), edges),
      edges,
    )
    @ List.init(52, i => (i * 5, 255 - i * 5, i * 37 mod 256));
  let bad =
    List.filter_map(
      rgb => {
        let back = C.rgb_of_hsv(C.hsv_of_rgb(~like=grey, rgb));
        back == rgb
          ? None : Some(show_rgb(rgb) ++ " -> " ++ show_rgb(back));
      },
      cube,
    );
  check(list(string), "every rgb survives a trip through hsv", [], bad);
};

/* Why `hsv_of_rgb` takes `~like`: a grey has no hue and black no saturation,
   and inventing zero is what makes a picker's hue jump home the moment value
   reaches the bottom. These pin that the caller's angle comes back. */
let degenerate_colours_keep_their_handles = () => {
  let like = (200., 0.6, 0.5);
  let hs = (what, expected, rgb) => {
    let (h, s, _) = C.hsv_of_rgb(~like, rgb);
    check(
      bool,
      Printf.sprintf("%s: h %.1f s %.2f", what, h, s),
      true,
      (h, s) == expected,
    );
  };
  hs("black keeps hue and saturation", (200., 0.6), (0, 0, 0));
  hs("grey keeps hue", (200., 0.), (128, 128, 128));
  hs("white keeps hue", (200., 0.), (255, 255, 255));
  /* A real colour ignores `like` entirely. */
  hs("red overrides both", (0., 1.), (255, 0, 0));
};

let hsv_hits_the_primaries = () => {
  let eq = (what, expected, hsv) => {
    let got = C.rgb_of_hsv(hsv);
    check(bool, what ++ " -> " ++ show_rgb(got), true, got == expected);
  };
  eq("red", (255, 0, 0), (0., 1., 1.));
  eq("green", (0, 255, 0), (120., 1., 1.));
  eq("blue", (0, 0, 255), (240., 1., 1.));
  eq("hue wraps at 360", (255, 0, 0), (360., 1., 1.));
  eq("negative hue wraps", (0, 0, 255), ((-120.), 1., 1.));
  eq("white", (255, 255, 255), (0., 0., 1.));
  eq("black", (0, 0, 0), (0., 1., 0.));
  /* Out of range clamps rather than wrapping or overshooting. */
  eq("saturation clamps", (255, 0, 0), (0., 4., 1.));
  eq("value clamps", (0, 0, 0), (0., 1., (-1.)));
};

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
    @ [
      test_case("integral floats", `Quick, integral_floats_render_clean),
      test_case("no scientific notation", `Quick, no_scientific_notation),
      test_case("non-finite pinned", `Quick, non_finite_is_pinned),
    ],
  ),
  ("Color.math", math_tests),
  (
    "Color.srgb",
    List.map(
      ((n, _, _, _) as c) => test_case(n, `Quick, srgb_roundtrips(c)),
      srgb_colors,
    )
    @ [test_case("hex and rgb parsing", `Quick, hex_parses)],
  ),
  (
    "Color.hsv",
    [
      test_case("rgb round-trips exactly", `Quick, hsv_roundtrips_exactly),
      test_case(
        "degenerate colours keep their handles",
        `Quick,
        degenerate_colours_keep_their_handles,
      ),
      test_case("primaries and clamping", `Quick, hsv_hits_the_primaries),
    ],
  ),
];
