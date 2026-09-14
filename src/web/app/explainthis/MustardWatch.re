/* An easter egg.

   A Fumola instance named `mustardWatch` gets Girard's line about watches
   underneath the ordinary explanation of what an instance is, and a picture
   of the watch in question.

   The joke is Girard's, from the Ludics-era writing he signed Yann-Joachim
   Ringard: a device that only tells you the time is a device you cannot get
   mustard out of. The dial here is marked with the connectives instead of
   hours, which is the other half of it -- a mustard watch is, after all,
   logical.

   Drawn to the house style in docs/illustration-style.md: an engraved plate,
   tone from hatching rather than from fills, spot colour rationed. It is
   meant to work at two distances. At panel size only the silhouette and the
   mustard carry; closer up the dial turns out to be signed, and to have been
   made in Dijon. */

open Virtual_dom.Vdom;

/* Plain paragraphs, not a blockquote: the markdown translator here has no
   Blockquote case, so a `>` block is dropped on the floor. */
let quote = "*\"Classical watches display time, but can hardly do anything else. This limitation is artificial: for instance several people confessed to be often in want of mustard...and what is the point of knowing time if you cannot get mustard?\"*\n\n-- Jean-Yves Girard (alias Yann-Joachim Ringard)";

let svg = (tag, attrs, children) =>
  Node.create_svg(
    tag,
    ~attrs=List.map(((k, v)) => Attr.create(k, v), attrs),
    children,
  );

/* An element with no children, which is most of them. */
let e = (tag, attrs) => svg(tag, attrs, []);

/* The object's own ink stays warm brown, so it reads as an engraving whatever
   the panel is doing. The page furniture around it -- the shelf it stands on,
   the shadows, the caption -- is drawn in currentColor instead, so it follows
   the panel's text colour and does not disappear if the panel goes dark. */
let ink = "#2b2117";
let leather = "#8a6740";
let leather_ink = "#33230f";
let stitch = "#f2e3c1";
let steel = "#cbc2ae";
let steel_lit = "#bdb3a0";
let steel_ink = "#3f3a2e";
let dial = "#dba828";
let dial_ink = "#2a1d05";
let dial_edge = "#8a6214";
let mustard = "#e0aa1e";
let seed = "#5e4108";
let highlight = "#fffaf0";
let lume = "#fbe9b0";
let hand = "#1f1707";
let red = "#a8392e";

/* Tone comes from these: parallel rules at a few densities, clipped to the
   region being shaded. `lit` runs the other way in a pale ink, for highlights
   on dark material. */
let hatch = (id, step, stroke, angle) =>
  svg(
    "pattern",
    [
      ("id", id),
      ("width", step),
      ("height", step),
      ("patternUnits", "userSpaceOnUse"),
      ("patternTransform", "rotate(" ++ angle ++ ")"),
    ],
    [
      e(
        "line",
        [
          ("x1", "0"),
          ("y1", "0"),
          ("x2", "0"),
          ("y2", step),
          ("stroke", stroke),
          ("stroke-width", "0.5"),
        ],
      ),
    ],
  );

let cross_hatch = (id, step) =>
  svg(
    "pattern",
    [
      ("id", id),
      ("width", step),
      ("height", step),
      ("patternUnits", "userSpaceOnUse"),
      ("patternTransform", "rotate(38)"),
    ],
    [
      e(
        "line",
        [
          ("x1", "0"),
          ("y1", "0"),
          ("x2", "0"),
          ("y2", step),
          ("stroke", ink),
          ("stroke-width", "0.45"),
        ],
      ),
      e(
        "line",
        [
          ("x1", "0"),
          ("y1", "0"),
          ("x2", step),
          ("y2", "0"),
          ("stroke", ink),
          ("stroke-width", "0.45"),
        ],
      ),
    ],
  );

let clip = (id, shape) => svg("clipPath", [("id", id)], [shape]);

let path = (d, attrs) => e("path", [("d", d), ...attrs]);

/* Shading is always a crescent: clip to the object, then fill a circle pushed
   away from the light. The intersection has a curved boundary, which is what
   an engraver would have cut. A clipped rectangle leaves a straight edge
   running through a round thing and the eye catches it at once. */
let shade = (cx, cy, r, fill, opacity) =>
  e(
    "circle",
    [
      ("cx", cx),
      ("cy", cy),
      ("r", r),
      ("fill", "url(#" ++ fill ++ ")"),
      ("opacity", opacity),
    ],
  );

let strap_upper = "M45 31 L71 31 L68 6 L48 6 Z";
let strap_lower = "M45 89 C42.5 103 41.5 114 44 126 L62 126 C65 114 68.5 103 71 89 Z";

/* Bead at the nozzle, two drops falling, and what has collected so far. */
let bead = "M101.2 73.4 C106.4 74.6 110.4 77.6 110.4 81.3 A4.35 4.35 0 0 1 101.7 81.8 C101.5 79 100.6 76 101.2 73.4 Z";
let drop_hi = "M107.4 93 q3.7 5.4 3.7 7.9 a3.8 3.8 0 0 1 -7.6 0 q0 -2.5 3.9 -7.9 z";
let drop_lo = "M110 108.5 q2.5 3.7 2.5 5.3 a2.6 2.6 0 0 1 -5.1 0 q0 -1.6 2.6 -5.3 z";
let puddle = "M96 127 q-2.5 -5 4.2 -5.6 q3.6 -3.8 8.4 -1.5 q7 -1.5 8.6 2.3 q5.7 .8 4.5 4.8 z";

let defs =
  svg(
    "defs",
    [],
    [
      hatch("mw-h1", "2.4", ink, "38"),
      hatch("mw-h2", "1.5", ink, "38"),
      cross_hatch("mw-hx", "1.6"),
      hatch("mw-lit", "2.2", "#f0e2c2", "-52"),
      /* The case ring, as an annulus: two circles, evenodd, so the dial is a
         hole and hatching over the ring does not spill onto it. */
      svg(
        "clipPath",
        [("id", "mw-ring"), ("clip-rule", "evenodd")],
        [
          path(
            "M25 60 a33 33 0 1 0 66 0 a33 33 0 1 0 -66 0 Z M30.5 60 a27.5 27.5 0 1 0 55 0 a27.5 27.5 0 1 0 -55 0 Z",
            [],
          ),
        ],
      ),
      clip(
        "mw-dial",
        e("circle", [("cx", "58"), ("cy", "60"), ("r", "27")]),
      ),
      clip("mw-strap-u", path(strap_upper, [])),
      clip("mw-strap-l", path(strap_lower, [])),
      clip(
        "mw-mustard",
        path(bead ++ " " ++ drop_hi ++ " " ++ drop_lo ++ " " ++ puddle, []),
      ),
    ],
  );

/* The shelf. currentColor, so it survives a change of panel. */
let shelf =
  e(
    "line",
    [
      ("x1", "30"),
      ("y1", "127"),
      ("x2", "126"),
      ("y2", "127"),
      ("stroke", "currentColor"),
      ("stroke-width", "0.8"),
      ("opacity", "0.5"),
    ],
  );

let leather_of = (outline, clip_id, tone_x, dark_x, lit_x, y, h) =>
  svg(
    "g",
    [],
    [
      path(outline, [("fill", leather)]),
      svg(
        "g",
        [("clip-path", "url(#" ++ clip_id ++ ")")],
        [
          e(
            "rect",
            [
              ("x", tone_x),
              ("y", y),
              ("width", "36"),
              ("height", h),
              ("fill", "url(#mw-h1)"),
              ("opacity", "0.55"),
            ],
          ),
          e(
            "rect",
            [
              ("x", dark_x),
              ("y", y),
              ("width", "20"),
              ("height", h),
              ("fill", "url(#mw-h2)"),
              ("opacity", "0.6"),
            ],
          ),
          e(
            "rect",
            [
              ("x", lit_x),
              ("y", y),
              ("width", "8"),
              ("height", h),
              ("fill", "url(#mw-lit)"),
              ("opacity", "0.24"),
            ],
          ),
        ],
      ),
      path(
        outline,
        [
          ("fill", "none"),
          ("stroke", leather_ink),
          ("stroke-width", "1.1"),
          ("stroke-linejoin", "round"),
        ],
      ),
    ],
  );

let stitching = d =>
  path(
    d,
    [
      ("fill", "none"),
      ("stroke", stitch),
      ("stroke-width", "0.85"),
      ("stroke-dasharray", "1.5 1.7"),
      ("opacity", "0.85"),
    ],
  );

let hole = cy =>
  e(
    "ellipse",
    [
      ("cx", "58"),
      ("cy", cy),
      ("rx", "1.15"),
      ("ry", "1.5"),
      ("fill", "#2a1c0a"),
    ],
  );

let straps =
  svg(
    "g",
    [],
    [
      leather_of(strap_upper, "mw-strap-u", "40", "61", "44", "2", "34"),
      stitching("M50.7 30 L52.3 8"),
      stitching("M65.3 30 L63.7 8"),
      hole("11"),
      hole("16.5"),
      hole("22"),
      leather_of(strap_lower, "mw-strap-l", "38", "56", "42", "86", "44"),
      stitching("M47.6 90 C45.5 103 45 113.5 47 124.5"),
      stitching("M68.4 90 C66.5 103 63 113.5 59 124.5"),
    ],
  );

let lug = y =>
  e(
    "rect",
    [
      ("x", "44"),
      ("y", y),
      ("width", "28"),
      ("height", "6"),
      ("rx", "1.8"),
      ("fill", steel_lit),
      ("stroke", steel_ink),
      ("stroke-width", "0.9"),
    ],
  );

let case_ =
  svg(
    "g",
    [],
    [
      e(
        "circle",
        [("cx", "58"), ("cy", "60"), ("r", "33"), ("fill", steel)],
      ),
      svg(
        "g",
        [("clip-path", "url(#mw-ring)")],
        [
          shade("70", "72", "30", "mw-h1", "0.5"),
          shade("78", "80", "26", "mw-hx", "0.45"),
          shade("44", "46", "28", "mw-lit", "0.45"),
        ],
      ),
      /* Knurling. One circle, divided into 60 by pathLength: fine repeated
         detail for the price of one element. */
      e(
        "circle",
        [
          ("cx", "58"),
          ("cy", "60"),
          ("r", "30.3"),
          ("fill", "none"),
          ("stroke", steel_ink),
          ("stroke-width", "5.2"),
          ("pathLength", "60"),
          ("stroke-dasharray", "0.34 0.66"),
          ("opacity", "0.38"),
        ],
      ),
      e(
        "circle",
        [
          ("cx", "58"),
          ("cy", "60"),
          ("r", "33"),
          ("fill", "none"),
          ("stroke", steel_ink),
          ("stroke-width", "1.3"),
        ],
      ),
      e(
        "circle",
        [
          ("cx", "58"),
          ("cy", "60"),
          ("r", "28"),
          ("fill", "none"),
          ("stroke", steel_ink),
          ("stroke-width", "0.9"),
          ("opacity", "0.8"),
        ],
      ),
    ],
  );

let guilloche = r =>
  e("circle", [("cx", "58"), ("cy", "60"), ("r", r), ("fill", "none")]);

let track = (r, width, divisions, dash, opacity) =>
  e(
    "circle",
    [
      ("cx", "58"),
      ("cy", "60"),
      ("r", r),
      ("fill", "none"),
      ("stroke", dial_ink),
      ("stroke-width", width),
      ("pathLength", divisions),
      ("stroke-dasharray", dash),
      ("opacity", opacity),
    ],
  );

let dial_face =
  svg(
    "g",
    [],
    [
      e(
        "circle",
        [
          ("cx", "58"),
          ("cy", "60"),
          ("r", "27"),
          ("fill", dial),
          ("stroke", dial_edge),
          ("stroke-width", "1"),
        ],
      ),
      svg(
        "g",
        [("clip-path", "url(#mw-dial)")],
        [
          svg(
            "g",
            [
              ("fill", "none"),
              ("stroke", dial_edge),
              ("stroke-width", "0.32"),
              ("opacity", "0.3"),
            ],
            [
              guilloche("5"),
              guilloche("8.5"),
              guilloche("12"),
              guilloche("15.5"),
              guilloche("19"),
            ],
          ),
          shade("76", "78", "21", "mw-h1", "0.2"),
          shade("82", "84", "18", "mw-h1", "0.18"),
        ],
      ),
      track("23.5", "2", "60", "0.3 0.7", "0.6"),
      track("23.5", "4.2", "12", "0.16 0.84", "0.55"),
    ],
  );

/* The connectives, where 12, 3, 6 and 9 would be.

   Tensor and lollipop are drawn rather than set: U+2297 and U+22B8 are missing
   from enough font stacks to be a real risk, and a tofu box in the middle of
   the joke would be worse than no joke. Par is a turned ampersand, which is
   exactly what U+214B is, so an ampersand rotated 180 degrees is both safe and
   honest. Bang is an exclamation mark and every font has one. */
let markers =
  svg(
    "g",
    [],
    [
      svg(
        "g",
        [
          ("stroke", dial_ink),
          ("fill", "none"),
          ("stroke-width", "1.15"),
          ("stroke-linecap", "round"),
        ],
        [
          e("circle", [("cx", "58"), ("cy", "42.4"), ("r", "4.3")]),
          path("M55 39.4 L61 45.4 M55 45.4 L61 39.4", []),
          e("circle", [("cx", "73.2"), ("cy", "60"), ("r", "2.3")]),
          path("M75.5 60 L80.8 60", []),
        ],
      ),
      svg(
        "g",
        [
          ("text-anchor", "middle"),
          ("dominant-baseline", "central"),
          ("font-family", "Georgia, 'Times New Roman', serif"),
          ("font-size", "11.5"),
          ("fill", dial_ink),
        ],
        [
          svg(
            "text",
            [
              ("x", "58"),
              ("y", "77.6"),
              ("transform", "rotate(180 58 77.6)"),
            ],
            [Node.text("&")],
          ),
          svg("text", [("x", "40.4"), ("y", "60")], [Node.text("!")]),
        ],
      ),
      /* Where a dial says who made it and where. Illegible at panel size; that
         is the point of it. */
      svg(
        "g",
        [
          ("text-anchor", "middle"),
          ("font-family", "'Source Code Pro', ui-monospace, monospace"),
          ("font-size", "3.2"),
          ("letter-spacing", "0.35"),
          ("fill", dial_ink),
          ("opacity", "0.7"),
        ],
        [
          svg(
            "text",
            [("x", "58"), ("y", "50.6")],
            [Node.text("RINGARD")],
          ),
          svg(
            "text",
            [("x", "58"), ("y", "71.4")],
            [Node.text("DIJON")],
          ),
        ],
      ),
    ],
  );

/* 10:09 -- the pose every watch advertisement uses, except that the hour hand
   is parked exactly on the 10 instead of the nine minutes past it ought to
   have travelled. Nobody has to notice. */
let hands =
  svg(
    "g",
    [],
    [
      svg(
        "g",
        [
          ("fill", hand),
          ("stroke", hand),
          ("stroke-width", "0.35"),
          ("stroke-linejoin", "round"),
        ],
        [
          svg(
            "g",
            [("transform", "rotate(300 58 60)")],
            [
              path(
                "M56.85 63.6 L56.85 50.4 L58 46 L59.15 50.4 L59.15 63.6 Z",
                [],
              ),
            ],
          ),
          svg(
            "g",
            [("transform", "rotate(54 58 60)")],
            [
              path(
                "M57.25 63.6 L57.25 43.6 L58 39.4 L58.75 43.6 L58.75 63.6 Z",
                [],
              ),
            ],
          ),
        ],
      ),
      svg(
        "g",
        [("transform", "rotate(228 58 60)")],
        [
          e(
            "line",
            [
              ("x1", "58"),
              ("y1", "66.5"),
              ("x2", "58"),
              ("y2", "38.5"),
              ("stroke", red),
              ("stroke-width", "0.85"),
              ("stroke-linecap", "round"),
            ],
          ),
          e(
            "circle",
            [("cx", "58"), ("cy", "68"), ("r", "1.6"), ("fill", red)],
          ),
        ],
      ),
      e(
        "circle",
        [("cx", "58"), ("cy", "60"), ("r", "1.8"), ("fill", hand)],
      ),
    ],
  );

let crystal =
  svg(
    "g",
    [("clip-path", "url(#mw-dial)")],
    [
      path(
        "M29 52 Q43 31 81 34 Q53 46 44 70 Q37 90 29 80 Z",
        [("fill", highlight), ("opacity", "0.28")],
      ),
      path(
        "M32 44 Q46 30 68 32 Q47 41 39 60 Z",
        [("fill", highlight), ("opacity", "0.22")],
      ),
      path(
        "M76 82 Q84 70 86 58 Q88 76 80 86 Z",
        [("fill", highlight), ("opacity", "0.16")],
      ),
    ],
  );

/* Where the crown would be. A ribbed collar and a tapered tip, which is what a
   condiment bottle has and a watch does not. */
let nozzle =
  svg(
    "g",
    [("transform", "translate(86 68) rotate(26)")],
    [
      e(
        "rect",
        [
          ("x", "0"),
          ("y", "-5.6"),
          ("width", "7.5"),
          ("height", "11.2"),
          ("rx", "1.4"),
          ("fill", steel_lit),
          ("stroke", steel_ink),
          ("stroke-width", "1"),
        ],
      ),
      svg(
        "g",
        [
          ("stroke", steel_ink),
          ("stroke-width", "0.55"),
          ("opacity", "0.6"),
        ],
        [
          e(
            "line",
            [("x1", "2.1"), ("y1", "-4.8"), ("x2", "2.1"), ("y2", "4.8")],
          ),
          e(
            "line",
            [("x1", "3.8"), ("y1", "-4.8"), ("x2", "3.8"), ("y2", "4.8")],
          ),
          e(
            "line",
            [("x1", "5.5"), ("y1", "-4.8"), ("x2", "5.5"), ("y2", "4.8")],
          ),
        ],
      ),
      path(
        "M7.5 -4.8 L17.6 -2 Q19.6 0 17.6 2 L7.5 4.8 Z",
        [
          ("fill", steel),
          ("stroke", steel_ink),
          ("stroke-width", "1"),
          ("stroke-linejoin", "round"),
        ],
      ),
      path(
        "M7.8 2.2 L17.2 1.1",
        [
          ("fill", "none"),
          ("stroke", ink),
          ("stroke-width", "0.7"),
          ("opacity", "0.35"),
        ],
      ),
      path(
        "M7.8 -2.4 L16.6 -1.2",
        [
          ("fill", "none"),
          ("stroke", highlight),
          ("stroke-width", "0.7"),
          ("opacity", "0.45"),
        ],
      ),
    ],
  );

let speck = (cx, cy, rx) =>
  e(
    "ellipse",
    [("cx", cx), ("cy", cy), ("rx", rx), ("ry", "0.45"), ("fill", seed)],
  );

let condiment =
  svg(
    "g",
    [],
    [
      svg(
        "g",
        [("fill", mustard)],
        [
          path(bead, []),
          path(drop_hi, []),
          path(drop_lo, []),
          path(puddle, []),
        ],
      ),
      svg(
        "g",
        [("clip-path", "url(#mw-mustard)")],
        [
          e(
            "rect",
            [
              ("x", "94"),
              ("y", "70"),
              ("width", "32"),
              ("height", "60"),
              ("fill", "url(#mw-h1)"),
              ("opacity", "0.2"),
            ],
          ),
          e(
            "rect",
            [
              ("x", "106"),
              ("y", "70"),
              ("width", "20"),
              ("height", "60"),
              ("fill", "url(#mw-h2)"),
              ("opacity", "0.28"),
            ],
          ),
          /* Wholegrain. */
          svg(
            "g",
            [("opacity", "0.75")],
            [
              speck("99.5", "124.4", "0.6"),
              speck("104", "125.4", "0.55"),
              speck("108.5", "123.2", "0.6"),
              speck("113", "125", "0.55"),
              speck("117.5", "124", "0.6"),
              speck("106", "99.5", "0.5"),
            ],
          ),
        ],
      ),
      svg(
        "g",
        [("fill", lume), ("opacity", "0.6")],
        [
          e(
            "ellipse",
            [
              ("cx", "103.8"),
              ("cy", "77.2"),
              ("rx", "1.6"),
              ("ry", "1"),
              ("transform", "rotate(-28 103.8 77.2)"),
            ],
          ),
          e(
            "ellipse",
            [
              ("cx", "105.6"),
              ("cy", "97.6"),
              ("rx", "0.9"),
              ("ry", "1.5"),
            ],
          ),
          e(
            "ellipse",
            [
              ("cx", "101.8"),
              ("cy", "122.6"),
              ("rx", "2.6"),
              ("ry", "0.9"),
            ],
          ),
        ],
      ),
      svg(
        "g",
        [
          ("fill", "none"),
          ("stroke", dial_edge),
          ("stroke-width", "0.9"),
          ("stroke-linejoin", "round"),
        ],
        [
          path(bead, []),
          path(drop_hi, []),
          path(drop_lo, []),
          path(puddle, []),
        ],
      ),
    ],
  );

/* Cast shadows: a few strokes on the shelf, thinning as they recede. Drawn in
   currentColor with the rest of the page furniture. */
let shadow = (x1, x2, y) =>
  e("line", [("x1", x1), ("y1", y), ("x2", x2), ("y2", y)]);

let shadows =
  svg(
    "g",
    [
      ("stroke", "currentColor"),
      ("stroke-width", "0.5"),
      ("opacity", "0.32"),
    ],
    [
      shadow("93", "126", "129.2"),
      shadow("97", "122", "131.2"),
      shadow("102", "117", "133"),
      shadow("43", "66", "129.2"),
      shadow("46", "62", "131.2"),
    ],
  );

/* "LA MONTRE A MOUTARDE", with the grave accent written as escaped UTF-8 to
   keep this file ASCII. Girard published the mustard line under a French
   pseudonym; a French plate caption seemed only polite. */
let caption =
  svg(
    "text",
    [
      ("x", "70"),
      ("y", "143"),
      ("text-anchor", "middle"),
      ("font-family", "'Source Code Pro', ui-monospace, monospace"),
      ("font-size", "5.4"),
      ("letter-spacing", "0.6"),
      ("fill", "currentColor"),
      ("opacity", "0.8"),
    ],
    [Node.text("LA MONTRE \xC3\x80 MOUTARDE")],
  );

let art: Node.t =
  svg(
    "svg",
    [
      ("viewBox", "0 0 140 150"),
      /* The explanation panel gives about 367px of content width, so 140 was
         leaving the figure stranded in the middle of it. 182 is 140 * 1.3,
         which is where the dial's signatures and the connectives start to
         read; `max-width: 100%` in explainthis.css still shrinks it if the
         sidebar is dragged narrow. */
      ("width", "182"),
      ("height", "195"),
      ("role", "img"),
      ("aria-label", "a wristwatch dispensing mustard"),
    ],
    [
      defs,
      shelf,
      straps,
      lug("25.5"),
      lug("88.5"),
      case_,
      dial_face,
      markers,
      hands,
      crystal,
      nozzle,
      condiment,
      shadows,
      caption,
    ],
  );
