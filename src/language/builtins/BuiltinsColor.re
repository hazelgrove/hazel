open BuiltinsUtil;
module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;

/* Arithmetic over the ColorValue ADT (declared in BuiltinsADT).

   These are FUNCTIONS rather than constructors on purpose: it means a config
   program evaluates every role down to a canonical `Oklch(l, c, h)`, which is
   the form the CSS applier and a colour-picker projector both want. Were
   mixing a constructor, a role would hold an unevaluated tree instead.

   All of them work in OKLCH, which is perceptually uniform — lightening by
   ten points looks like the same step whatever the hue, which is what makes a
   derived dark scheme viable at all.

   `Transparent` passes through unchanged rather than erroring: it has nothing
   to adjust. `Fade` is transformed through to its inner colour, so a faded
   colour still responds to lightening. Every other form -- `Oklch` and `Rgb`
   -- has components, so every other form responds. */
module C = BuiltinsADT.Color;

let ty = C.typ.term;

/* Lightness and alpha are percentages; chroma is unbounded in principle but
   negative values are meaningless. Hue wraps rather than clamps. */
let clamp = (lo, hi, x) => Float.min(hi, Float.max(lo, x));
let wrap_hue = h => {
  let r = Float.rem(h, 360.);
  r < 0. ? r +. 360. : r;
};

/* The OKLCH components of a colour, where they can be known -- THE one place
   `Rgb` is resolved. Every operation below goes through this, so a palette
   written in sRGB bytes ramps, mixes and reports its lightness exactly as an
   OKLCH one does; miss a site and that operation quietly degrades instead of
   failing. */
let components = (c: C.t): option((float, float, float)) =>
  switch (c) {
  | Oklch(l, ch, h) => Some((l, ch, h))
  | Rgb(r, g, b) => Some(C.oklch_of_rgb((r, g, b)))
  | Fade(_)
  | Transparent => None
  };

/* Apply a transformation to the Oklch components, recursing through Fade. */
let rec map_oklch = (f, c: C.t): C.t =>
  switch (c) {
  | Fade(inner, a) => Fade(map_oklch(f, inner), a)
  | Transparent => c
  | Oklch(_)
  | Rgb(_) =>
    switch (components(c)) {
    | Some(t) =>
      let (l, ch, h) = f(t);
      Oklch(clamp(0., 100., l), Float.max(0., ch), wrap_hue(h));
    | None => c
    }
  };

/* Read the L axis back out. */
let rec lightness = (c: C.t): float =>
  switch (components(c)) {
  | Some((l, _, _)) => l
  | None =>
    switch (c) {
    | Fade(inner, _) => lightness(inner)
    | _ => 0.
    }
  };

/* Shortest-path hue interpolation: going from 350 to 10 should cross 0, not
   sweep backwards through the whole wheel. */
let mix_hue = (h1, h2, t) => {
  let d = Float.rem(h2 -. h1 +. 540., 360.) -. 180.;
  wrap_hue(h1 +. d *. t);
};

let mix = (c1: C.t, c2: C.t, t: float): C.t => {
  let t = clamp(0., 1., t);
  switch (components(c1), components(c2)) {
  | (Some((l1, ch1, h1)), Some((l2, ch2, h2))) =>
    Oklch(
      l1 +. (l2 -. l1) *. t,
      ch1 +. (ch2 -. ch1) *. t,
      mix_hue(h1, h2, t),
    )
  /* Nothing sensible to interpolate: pick the nearer endpoint. */
  | _ => t < 0.5 ? c1 : c2
  };
};

/* --- builtin wrappers --- */

let color_unary = (name, f): BuiltinsUtil.fn => {
  name,
  arg: Prod([C.typ, float()]),
  ret: ty,
  imp:
    binary((d1, d2) => {
      let-unbox amount = (Atom(Float), d2);
      switch (C.of_exp(d1)) {
      | Some(c) => Some(C.exp_of(f(c, amount)))
      | None => None
      };
    }),
  custom_statics: None,
};

let builtins: list(BuiltinsUtil.fn) = [
  /* Lightness in percentage POINTS, not a ratio: lighten(c, 10.) is ten
     points up the L axis. Negative darkens, so darken is the mirror. */
  color_unary("color_lighten", (c, by) =>
    map_oklch(((l, ch, h)) => (l +. by, ch, h), c)
  ),
  color_unary("color_darken", (c, by) =>
    map_oklch(((l, ch, h)) => (l -. by, ch, h), c)
  ),
  /* Chroma is scaled, not offset — chroma ranges differ by hue, so a
     multiplier keeps a saturation change proportionate. */
  color_unary("color_saturate", (c, by) =>
    map_oklch(((l, ch, h)) => (l, ch *. by, h), c)
  ),
  color_unary("color_rotate", (c, by) =>
    map_oklch(((l, ch, h)) => (l, ch, h +. by), c)
  ),
  /* Sets the lightness outright, for pinning a colour to a known step. */
  color_unary("color_with_lightness", (c, l') =>
    map_oklch(((_, ch, h)) => (l', ch, h), c)
  ),
  {
    name: "color_fade",
    arg: Prod([C.typ, float()]),
    ret: ty,
    imp:
      binary((d1, d2) => {
        let-unbox a = (Atom(Float), d2);
        switch (C.of_exp(d1)) {
        | Some(c) => Some(C.exp_of(C.Fade(c, clamp(0., 100., a))))
        | None => None
        };
      }),
    custom_statics: None,
  },
  {
    /* The only component a derivation actually needs to read: it is what lets a
       program place a colour RELATIVE to a scheme's own background and text,
       instead of against hardcoded lightnesses that only suit one polarity. */

    name: "color_lightness",
    arg: ty,
    ret: Atom(Float),
    imp: d =>
      switch (C.of_exp(d)) {
      | Some(c) => Some(Fresh.Exp.float(lightness(c)))
      | None => None
      },
    custom_statics: None,
  },
  {
    name: "color_mix",
    arg: Prod([C.typ, C.typ, float()]),
    ret: ty,
    imp:
      ternary((d1, d2, d3) => {
        let-unbox t = (Atom(Float), d3);
        switch (C.of_exp(d1), C.of_exp(d2)) {
        | (Some(c1), Some(c2)) => Some(C.exp_of(mix(c1, c2, t)))
        | _ => None
        };
      }),
    custom_statics: None,
  },
];
