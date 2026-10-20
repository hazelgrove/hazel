/* An easter egg.

   A Fumola instance named `mustardWatch` gets Girard's line about watches
   underneath the ordinary explanation of what an instance is, and a picture
   of the watch in question.

   The joke is Girard's, from the Ludics-era writing he signed Yann-Joachim
   Ringard: a device that only tells you the time is a device you cannot get
   mustard out of. The dial here is marked with the connectives instead of
   hours, which is the other half of it -- a mustard watch is, after all,
   logical.

   The picture is a cafe: Hazel -- the project's hazelnut, given a body -- is
   dispensing mustard from the watch onto toast while reading the dial, with
   the knife in the other hand. Drawn to the house style in
   docs/illustration-style.md.

   At panel size only the silhouette and the mustard carry. The connectives
   are far too small to read on a wrist at this scale, so they live in the
   medallion at the bottom right, which is magnified the way a nineteenth
   century plate shows a detail. It is drawn breaking the picture's frame on
   purpose: sitting inside the frame it read as a second clock standing on the
   table, because it shared the scene's picture plane. Breaking the border and
   sitting on its own disc of paper is what makes it an annotation about the
   room rather than an object in it.

   The whole drawing carries its own paper, so it does not depend on the panel
   behind it being any particular colour. */

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

/* Tone is carried by the `c-h*` hatch patterns in `defs`, clipped to whatever
   is being shaded, and shading is always a crescent -- a circle pushed away
   from the window -- so no straight edge runs through a round form. The leaf
   in `defs` is drawn once and placed with `use`; the plants are all the same
   leaf at different angles. */
let art: Node.t =
  svg(
    "svg",
    [
      ("viewBox", "0 0 680 480"),
      /* The explanation panel gives about 367px of content width. 340 is the
         most a landscape plate can take and still survive the sidebar being
         dragged narrower; `max-width: 100%` in explainthis.css does the rest. */
      ("width", "340"),
      ("height", "240"),
      ("role", "img"),
      (
        "aria-label",
        "Hazel, a hazelnut-headed person, dispensing mustard from a wristwatch onto toast in a plant-filled cafe",
      ),
    ],
    [
      svg(
        "defs",
        [],
        [
          svg(
            "pattern",
            [
              ("id", "c-h1"),
              ("width", "5.5"),
              ("height", "5.5"),
              ("patternUnits", "userSpaceOnUse"),
              ("patternTransform", "rotate(40)"),
            ],
            [
              e(
                "line",
                [
                  ("x1", "0"),
                  ("y1", "0"),
                  ("x2", "0"),
                  ("y2", "5.5"),
                  ("stroke", "#2b2117"),
                  ("stroke-width", "1"),
                ],
              ),
            ],
          ),
          svg(
            "pattern",
            [
              ("id", "c-h2"),
              ("width", "3.4"),
              ("height", "3.4"),
              ("patternUnits", "userSpaceOnUse"),
              ("patternTransform", "rotate(40)"),
            ],
            [
              e(
                "line",
                [
                  ("x1", "0"),
                  ("y1", "0"),
                  ("x2", "0"),
                  ("y2", "3.4"),
                  ("stroke", "#2b2117"),
                  ("stroke-width", "1"),
                ],
              ),
            ],
          ),
          svg(
            "pattern",
            [
              ("id", "c-hx"),
              ("width", "3.8"),
              ("height", "3.8"),
              ("patternUnits", "userSpaceOnUse"),
              ("patternTransform", "rotate(40)"),
            ],
            [
              e(
                "line",
                [
                  ("x1", "0"),
                  ("y1", "0"),
                  ("x2", "0"),
                  ("y2", "3.8"),
                  ("stroke", "#2b2117"),
                  ("stroke-width", ".95"),
                ],
              ),
              e(
                "line",
                [
                  ("x1", "0"),
                  ("y1", "0"),
                  ("x2", "3.8"),
                  ("y2", "0"),
                  ("stroke", "#2b2117"),
                  ("stroke-width", ".95"),
                ],
              ),
            ],
          ),
          svg(
            "g",
            [("id", "lf")],
            [
              e(
                "path",
                [
                  ("d", "M0 0 C13 -11 33 -12 48 -1 C33 11 13 11 0 0 Z"),
                  ("fill", "#3c5a45"),
                  ("stroke", "#2b2117"),
                  ("stroke-width", "2"),
                  ("stroke-linejoin", "round"),
                ],
              ),
              e(
                "path",
                [
                  ("d", "M3 0 L44 -1"),
                  ("stroke", "#16241a"),
                  ("stroke-width", "1.1"),
                  ("opacity", ".7"),
                ],
              ),
              svg(
                "g",
                [
                  ("stroke", "#16241a"),
                  ("stroke-width", ".8"),
                  ("opacity", ".45"),
                ],
                [
                  e("path", [("d", "M12 -1 L16 -6")]),
                  e("path", [("d", "M22 -1 L26 -7")]),
                  e("path", [("d", "M32 -1 L35 -6")]),
                  e("path", [("d", "M12 0 L16 5")]),
                  e("path", [("d", "M22 0 L26 6")]),
                  e("path", [("d", "M32 0 L35 5")]),
                ],
              ),
            ],
          ),
          svg(
            "clipPath",
            [("id", "c-win")],
            [
              e(
                "rect",
                [
                  ("x", "390"),
                  ("y", "80"),
                  ("width", "244"),
                  ("height", "196"),
                ],
              ),
            ],
          ),
          svg(
            "clipPath",
            [("id", "c-head")],
            [
              e(
                "path",
                [
                  (
                    "d",
                    "M240 106 C262 102 282 114 292 134 C302 154 300 178 286 194 C270 212 242 218 220 210 C202 203 192 186 191 166 C190 142 200 120 216 112 C224 108 232 107 240 106 Z",
                  ),
                ],
              ),
            ],
          ),
          svg(
            "clipPath",
            [("id", "c-body")],
            [
              e(
                "path",
                [
                  (
                    "d",
                    "M218 254 C194 258 176 276 170 302 L160 424 L336 424 L324 300 C318 274 300 258 276 254 Z",
                  ),
                ],
              ),
            ],
          ),
          svg(
            "clipPath",
            [("id", "c-table")],
            [
              e(
                "path",
                [("d", "M0 378 C180 354 500 354 680 378 L680 446 L0 446 Z")],
              ),
            ],
          ),
          svg(
            "clipPath",
            [("id", "c-wall")],
            [
              e(
                "rect",
                [
                  ("x", "0"),
                  ("y", "0"),
                  ("width", "680"),
                  ("height", "358"),
                ],
              ),
            ],
          ),
        ],
      ),
      e(
        "rect",
        [
          ("x", "0"),
          ("y", "0"),
          ("width", "680"),
          ("height", "480"),
          ("fill", "#f2e8d4"),
        ],
      ),
      svg(
        "g",
        [("clip-path", "url(#c-wall)")],
        [
          e(
            "rect",
            [
              ("x", "0"),
              ("y", "0"),
              ("width", "680"),
              ("height", "358"),
              ("fill", "url(#c-h1)"),
              ("opacity", ".14"),
            ],
          ),
          e(
            "rect",
            [
              ("x", "0"),
              ("y", "0"),
              ("width", "300"),
              ("height", "358"),
              ("fill", "url(#c-h1)"),
              ("opacity", ".18"),
            ],
          ),
          e(
            "rect",
            [
              ("x", "0"),
              ("y", "0"),
              ("width", "140"),
              ("height", "358"),
              ("fill", "url(#c-h2)"),
              ("opacity", ".22"),
            ],
          ),
          e(
            "rect",
            [
              ("x", "0"),
              ("y", "0"),
              ("width", "60"),
              ("height", "358"),
              ("fill", "url(#c-hx)"),
              ("opacity", ".18"),
            ],
          ),
        ],
      ),
      e(
        "rect",
        [
          ("x", "382"),
          ("y", "72"),
          ("width", "260"),
          ("height", "212"),
          ("fill", "#faf4e6"),
          ("stroke", "#2b2117"),
          ("stroke-width", "4.5"),
        ],
      ),
      svg(
        "g",
        [("clip-path", "url(#c-win)")],
        [
          svg(
            "g",
            [("transform", "translate(396 258) rotate(-14) scale(.95)")],
            [e("use", [("href", "#lf")])],
          ),
          svg(
            "g",
            [("transform", "translate(446 268) rotate(6)")],
            [e("use", [("href", "#lf")])],
          ),
          svg(
            "g",
            [("transform", "translate(506 262) rotate(-8) scale(1.05)")],
            [e("use", [("href", "#lf")])],
          ),
          svg(
            "g",
            [("transform", "translate(566 266) rotate(12)")],
            [e("use", [("href", "#lf")])],
          ),
          svg(
            "g",
            [("transform", "translate(470 236) rotate(-4) scale(.9)")],
            [e("use", [("href", "#lf")])],
          ),
          svg(
            "g",
            [("transform", "translate(536 240) rotate(9) scale(.85)")],
            [e("use", [("href", "#lf")])],
          ),
          e(
            "rect",
            [
              ("x", "390"),
              ("y", "80"),
              ("width", "244"),
              ("height", "196"),
              ("fill", "url(#c-h1)"),
              ("opacity", ".07"),
            ],
          ),
          svg(
            "g",
            [
              ("stroke", "#2b2117"),
              ("stroke-width", "1"),
              ("opacity", ".18"),
            ],
            [
              e(
                "line",
                [
                  ("x1", "390"),
                  ("y1", "104"),
                  ("x2", "634"),
                  ("y2", "92"),
                ],
              ),
              e(
                "line",
                [
                  ("x1", "390"),
                  ("y1", "124"),
                  ("x2", "634"),
                  ("y2", "112"),
                ],
              ),
              e(
                "line",
                [
                  ("x1", "390"),
                  ("y1", "144"),
                  ("x2", "634"),
                  ("y2", "132"),
                ],
              ),
            ],
          ),
        ],
      ),
      svg(
        "g",
        [("stroke", "#2b2117"), ("stroke-width", "3.6")],
        [
          e(
            "line",
            [("x1", "512"), ("y1", "72"), ("x2", "512"), ("y2", "284")],
          ),
          e(
            "line",
            [("x1", "382"), ("y1", "144"), ("x2", "642"), ("y2", "144")],
          ),
          e(
            "line",
            [("x1", "382"), ("y1", "214"), ("x2", "642"), ("y2", "214")],
          ),
        ],
      ),
      e(
        "rect",
        [
          ("x", "366"),
          ("y", "284"),
          ("width", "292"),
          ("height", "16"),
          ("fill", "#9a7448"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3"),
        ],
      ),
      e(
        "rect",
        [
          ("x", "366"),
          ("y", "284"),
          ("width", "292"),
          ("height", "16"),
          ("fill", "url(#c-h1)"),
          ("opacity", ".22"),
        ],
      ),
      e(
        "line",
        [
          ("x1", "0"),
          ("y1", "322"),
          ("x2", "680"),
          ("y2", "322"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.6"),
        ],
      ),
      e(
        "line",
        [
          ("x1", "0"),
          ("y1", "330"),
          ("x2", "680"),
          ("y2", "330"),
          ("stroke", "#2b2117"),
          ("stroke-width", "1.2"),
          ("opacity", ".5"),
        ],
      ),
      svg(
        "g",
        [
          ("stroke", "#2b2117"),
          ("stroke-width", "1.2"),
          ("opacity", ".35"),
        ],
        [
          e(
            "line",
            [("x1", "56"), ("y1", "330"), ("x2", "56"), ("y2", "376")],
          ),
          e(
            "line",
            [("x1", "150"), ("y1", "330"), ("x2", "150"), ("y2", "372")],
          ),
          e(
            "line",
            [("x1", "566"), ("y1", "330"), ("x2", "566"), ("y2", "364")],
          ),
          e(
            "line",
            [("x1", "652"), ("y1", "330"), ("x2", "652"), ("y2", "362")],
          ),
        ],
      ),
      e(
        "path",
        [
          ("d", "M440 284 L508 284 L500 270 L448 270 Z"),
          ("fill", "#8a5a34"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.6"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "rect",
        [
          ("x", "440"),
          ("y", "270"),
          ("width", "68"),
          ("height", "14"),
          ("fill", "url(#c-h1)"),
          ("opacity", ".3"),
        ],
      ),
      svg(
        "g",
        [
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-linecap", "round"),
        ],
        [
          e(
            "path",
            [
              (
                "d",
                "M474 270 C468 258 470 246 462 238 C456 232 452 226 454 218",
              ),
              ("stroke-width", "5.5"),
            ],
          ),
          e(
            "path",
            [
              ("d", "M468 250 C458 246 448 246 440 240"),
              ("stroke-width", "3.6"),
            ],
          ),
          e(
            "path",
            [
              ("d", "M470 258 C482 252 492 248 502 244"),
              ("stroke-width", "3.6"),
            ],
          ),
          e(
            "path",
            [
              ("d", "M458 232 C450 230 444 226 440 222"),
              ("stroke-width", "2.6"),
            ],
          ),
        ],
      ),
      svg(
        "g",
        [("transform", "translate(424 234) rotate(-6) scale(.8)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(478 240) rotate(5) scale(.72)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(444 214) rotate(-3) scale(.86)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("stroke", "#2b2117"), ("stroke-width", "1.8"), ("fill", "none")],
        [
          e(
            "line",
            [("x1", "78"), ("y1", "0"), ("x2", "90"), ("y2", "40")],
          ),
          e(
            "line",
            [("x1", "132"), ("y1", "0"), ("x2", "120"), ("y2", "40")],
          ),
          e(
            "line",
            [("x1", "105"), ("y1", "0"), ("x2", "105"), ("y2", "40")],
          ),
        ],
      ),
      e(
        "path",
        [
          ("d", "M80 40 L130 40 L122 78 L88 78 Z"),
          ("fill", "#a8392e"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "rect",
        [
          ("x", "80"),
          ("y", "40"),
          ("width", "50"),
          ("height", "38"),
          ("fill", "url(#c-h1)"),
          ("opacity", ".26"),
        ],
      ),
      svg(
        "g",
        [
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.4"),
          ("stroke-linecap", "round"),
        ],
        [
          e("path", [("d", "M100 78 C86 110 74 150 66 192")]),
          e("path", [("d", "M108 78 C108 116 114 156 124 198")]),
          e("path", [("d", "M116 78 C130 106 144 136 152 172")]),
        ],
      ),
      svg(
        "g",
        [("transform", "translate(94 104) rotate(104) scale(.82)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(82 142) rotate(98) scale(.78)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(70 178) rotate(94) scale(.72)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(108 112) rotate(78) scale(.84)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(115 152) rotate(72) scale(.78)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(122 188) rotate(68) scale(.7)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(128 104) rotate(54) scale(.82)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(142 134) rotate(48) scale(.74)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("stroke", "#2b2117"), ("stroke-width", "1.8"), ("fill", "none")],
        [
          e(
            "line",
            [("x1", "596"), ("y1", "0"), ("x2", "606"), ("y2", "26")],
          ),
          e(
            "line",
            [("x1", "648"), ("y1", "0"), ("x2", "638"), ("y2", "26")],
          ),
        ],
      ),
      e(
        "path",
        [
          ("d", "M598 26 L646 26 L638 58 L606 58 Z"),
          ("fill", "#a8392e"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "rect",
        [
          ("x", "598"),
          ("y", "26"),
          ("width", "48"),
          ("height", "32"),
          ("fill", "url(#c-h1)"),
          ("opacity", ".26"),
        ],
      ),
      svg(
        "g",
        [
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.4"),
          ("stroke-linecap", "round"),
        ],
        [
          e("path", [("d", "M614 58 C604 84 596 114 592 144")]),
          e("path", [("d", "M630 58 C636 86 644 112 648 138")]),
        ],
      ),
      svg(
        "g",
        [("transform", "translate(610 76) rotate(102) scale(.76)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(600 110) rotate(98) scale(.7)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(633 78) rotate(68) scale(.74)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(642 108) rotate(62) scale(.68)")],
        [e("use", [("href", "#lf")])],
      ),
      /* potted fern, floor left */
      svg(
        "g",
        [
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.4"),
          ("stroke-linecap", "round"),
        ],
        [
          e("path", [("d", "M58 344 C46 312 36 278 32 246")]),
          e("path", [("d", "M66 344 C66 308 70 274 78 242")]),
          e("path", [("d", "M76 344 C90 314 102 286 112 260")]),
        ],
      ),
      svg(
        "g",
        [("transform", "translate(48 306) rotate(250) scale(.8)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(36 258) rotate(256) scale(.74)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(68 302) rotate(272) scale(.82)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(76 252) rotate(278) scale(.74)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(86 300) rotate(296) scale(.8)")],
        [e("use", [("href", "#lf")])],
      ),
      svg(
        "g",
        [("transform", "translate(106 268) rotate(302) scale(.72)")],
        [e("use", [("href", "#lf")])],
      ),
      e(
        "path",
        [
          ("d", "M32 344 L104 344 L96 392 L40 392 Z"),
          ("fill", "#a8392e"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "rect",
        [
          ("x", "32"),
          ("y", "344"),
          ("width", "72"),
          ("height", "48"),
          ("fill", "url(#c-h1)"),
          ("opacity", ".26"),
        ],
      ),
      e(
        "line",
        [
          ("x1", "34"),
          ("y1", "356"),
          ("x2", "102"),
          ("y2", "356"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.2"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M150 266 L332 266 L328 282 L154 282 Z"),
          ("fill", "#9a7448"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "rect",
        [
          ("x", "150"),
          ("y", "266"),
          ("width", "182"),
          ("height", "16"),
          ("fill", "url(#c-h1)"),
          ("opacity", ".28"),
        ],
      ),
      svg(
        "g",
        [("stroke", "#2b2117"), ("stroke-width", "2.8")],
        [
          e(
            "line",
            [("x1", "164"), ("y1", "282"), ("x2", "164"), ("y2", "356")],
          ),
          e(
            "line",
            [("x1", "200"), ("y1", "282"), ("x2", "200"), ("y2", "356")],
          ),
          e(
            "line",
            [("x1", "284"), ("y1", "282"), ("x2", "284"), ("y2", "356")],
          ),
          e(
            "line",
            [("x1", "318"), ("y1", "282"), ("x2", "318"), ("y2", "356")],
          ),
        ],
      ),
      e(
        "path",
        [
          (
            "d",
            "M218 254 C194 258 176 276 170 302 L160 424 L336 424 L324 300 C318 274 300 258 276 254 Z",
          ),
          ("fill", "#6b7a58"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3.2"),
          ("stroke-linejoin", "round"),
        ],
      ),
      svg(
        "g",
        [("clip-path", "url(#c-body)")],
        [
          e(
            "circle",
            [
              ("cx", "150"),
              ("cy", "330"),
              ("r", "120"),
              ("fill", "url(#c-h1)"),
              ("opacity", ".34"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "120"),
              ("cy", "350"),
              ("r", "100"),
              ("fill", "url(#c-h2)"),
              ("opacity", ".26"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "96"),
              ("cy", "366"),
              ("r", "78"),
              ("fill", "url(#c-hx)"),
              ("opacity", ".22"),
            ],
          ),
        ],
      ),
      e(
        "path",
        [
          ("d", "M216 256 L247 286 L278 254"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M230 216 L266 216 L268 256 L228 256 Z"),
          ("fill", "#c79a63"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
        ],
      ),
      e(
        "rect",
        [
          ("x", "228"),
          ("y", "216"),
          ("width", "22"),
          ("height", "42"),
          ("fill", "url(#c-h2)"),
          ("opacity", ".34"),
        ],
      ),
      e(
        "path",
        [
          (
            "d",
            "M240 106 C262 102 282 114 292 134 C302 154 300 178 286 194 C270 212 242 218 220 210 C202 203 192 186 191 166 C190 142 200 120 216 112 C224 108 232 107 240 106 Z",
          ),
          ("fill", "#c79a63"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3.4"),
          ("stroke-linejoin", "round"),
        ],
      ),
      svg(
        "g",
        [("clip-path", "url(#c-head)")],
        [
          e(
            "path",
            [
              (
                "d",
                "M240 106 C256 126 272 154 286 194 C300 178 302 154 292 134 C282 114 262 102 240 106 Z",
              ),
              ("fill", "#8a5a34"),
              ("stroke", "#2b2117"),
              ("stroke-width", "2.8"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "176"),
              ("cy", "196"),
              ("r", "62"),
              ("fill", "url(#c-h1)"),
              ("opacity", ".3"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "160"),
              ("cy", "210"),
              ("r", "52"),
              ("fill", "url(#c-h2)"),
              ("opacity", ".24"),
            ],
          ),
          svg(
            "g",
            [
              ("stroke", "#2b2117"),
              ("stroke-width", "2.4"),
              ("stroke-linecap", "round"),
              ("opacity", ".45"),
            ],
            [
              e(
                "line",
                [
                  ("x1", "204"),
                  ("y1", "188"),
                  ("x2", "220"),
                  ("y2", "170"),
                ],
              ),
              e(
                "line",
                [
                  ("x1", "212"),
                  ("y1", "198"),
                  ("x2", "232"),
                  ("y2", "178"),
                ],
              ),
              e(
                "line",
                [
                  ("x1", "224"),
                  ("y1", "204"),
                  ("x2", "238"),
                  ("y2", "190"),
                ],
              ),
            ],
          ),
        ],
      ),
      svg(
        "g",
        [("fill", "#2b2117")],
        [
          e(
            "ellipse",
            [("cx", "250"), ("cy", "156"), ("rx", "4.6"), ("ry", "6")],
          ),
          e(
            "ellipse",
            [("cx", "280"), ("cy", "153"), ("rx", "4.6"), ("ry", "6")],
          ),
        ],
      ),
      svg(
        "g",
        [
          ("stroke", "#2b2117"),
          ("stroke-width", "2"),
          ("stroke-linecap", "round"),
          ("fill", "none"),
        ],
        [
          e("path", [("d", "M242 145 C247 141 254 141 258 144")]),
          e("path", [("d", "M273 142 C277 139 284 139 288 142")]),
        ],
      ),
      e(
        "path",
        [
          ("d", "M256 179 C264 187 276 186 282 177"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.6"),
          ("stroke-linecap", "round"),
        ],
      ),
      e(
        "ellipse",
        [
          ("cx", "230"),
          ("cy", "172"),
          ("rx", "10"),
          ("ry", "6"),
          ("fill", "#a8392e"),
          ("opacity", ".32"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M0 378 C180 354 500 354 680 378 L680 446 L0 446 Z"),
          ("fill", "#9a7448"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3.4"),
          ("stroke-linejoin", "round"),
        ],
      ),
      svg(
        "g",
        [("clip-path", "url(#c-table)")],
        [
          e(
            "rect",
            [
              ("x", "0"),
              ("y", "354"),
              ("width", "680"),
              ("height", "92"),
              ("fill", "url(#c-h1)"),
              ("opacity", ".18"),
            ],
          ),
          e(
            "rect",
            [
              ("x", "0"),
              ("y", "412"),
              ("width", "680"),
              ("height", "36"),
              ("fill", "url(#c-hx)"),
              ("opacity", ".26"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "90"),
              ("cy", "440"),
              ("r", "150"),
              ("fill", "url(#c-h2)"),
              ("opacity", ".2"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "640"),
              ("cy", "446"),
              ("r", "130"),
              ("fill", "url(#c-h2)"),
              ("opacity", ".16"),
            ],
          ),
          svg(
            "g",
            [
              ("stroke", "#5c3d1e"),
              ("stroke-width", "1.5"),
              ("opacity", ".45"),
              ("fill", "none"),
            ],
            [
              e("path", [("d", "M0 392 C180 369 500 369 680 392")]),
              e("path", [("d", "M0 404 C180 382 500 382 680 404")]),
              e("path", [("d", "M0 420 C180 399 500 399 680 420")]),
            ],
          ),
        ],
      ),
      e(
        "path",
        [
          ("d", "M0 378 C180 354 500 354 680 378"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3.4"),
        ],
      ),
      e(
        "ellipse",
        [
          ("cx", "432"),
          ("cy", "402"),
          ("rx", "66"),
          ("ry", "17"),
          ("fill", "#faf4e6"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3"),
        ],
      ),
      e(
        "ellipse",
        [
          ("cx", "432"),
          ("cy", "400"),
          ("rx", "48"),
          ("ry", "11"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "1.3"),
          ("opacity", ".4"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M398 396 L462 388 L468 402 L404 410 Z"),
          ("fill", "#d9a95c"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M400 394 L462 386 L466 394 L404 402 Z"),
          ("fill", "#e0aa1e"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "ellipse",
        [
          ("cx", "172"),
          ("cy", "408"),
          ("rx", "38"),
          ("ry", "11.5"),
          ("fill", "#faf4e6"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M153 404 L191 404 L186 378 L158 378 Z"),
          ("fill", "#faf4e6"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "ellipse",
        [
          ("cx", "172"),
          ("cy", "378"),
          ("rx", "13"),
          ("ry", "4.2"),
          ("fill", "#4a3520"),
          ("stroke", "#2b2117"),
          ("stroke-width", "1.8"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M191 385 C202 383 204 396 193 396"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.6"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M308 282 L336 372 L392 306"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "30"),
          ("stroke-linecap", "round"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M308 282 L336 372 L392 306"),
          ("fill", "none"),
          ("stroke", "#6b7a58"),
          ("stroke-width", "23"),
          ("stroke-linecap", "round"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M314 292 L339 368"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "7"),
          ("stroke-linecap", "round"),
          ("opacity", ".16"),
        ],
      ),
      e(
        "path",
        [
          (
            "d",
            "M400 292 C414 284 424 290 422 302 C420 314 408 320 398 314 Z",
          ),
          ("fill", "#c79a63"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M404 292 C400 284 392 284 388 290"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.6"),
          ("stroke-linecap", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M362 340 L396 314 L386 300 L352 326 Z"),
          ("fill", "#8a5a34"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.6"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "circle",
        [
          ("cx", "374"),
          ("cy", "322"),
          ("r", "24"),
          ("fill", "#cbc2ae"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3.2"),
        ],
      ),
      e(
        "circle",
        [
          ("cx", "374"),
          ("cy", "322"),
          ("r", "24"),
          ("fill", "url(#c-h1)"),
          ("opacity", ".18"),
        ],
      ),
      e(
        "circle",
        [
          ("cx", "374"),
          ("cy", "322"),
          ("r", "16.5"),
          ("fill", "#dba828"),
          ("stroke", "#8a6214"),
          ("stroke-width", "1.8"),
        ],
      ),
      svg(
        "g",
        [
          ("stroke", "#2b2117"),
          ("stroke-width", "2.2"),
          ("stroke-linecap", "round"),
        ],
        [
          e(
            "line",
            [("x1", "374"), ("y1", "322"), ("x2", "374"), ("y2", "310")],
          ),
          e(
            "line",
            [("x1", "374"), ("y1", "322"), ("x2", "383"), ("y2", "326")],
          ),
        ],
      ),
      e(
        "circle",
        [("cx", "374"), ("cy", "322"), ("r", "2"), ("fill", "#2b2117")],
      ),
      e(
        "path",
        [
          ("d", "M386 336 L400 348 Q404 355 397 357 L383 345 Z"),
          ("fill", "#cbc2ae"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.4"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "circle",
        [
          ("cx", "374"),
          ("cy", "322"),
          ("r", "33"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "1.3"),
          ("stroke-dasharray", "4 4"),
          ("opacity", ".45"),
        ],
      ),
      /* the stream */
      e(
        "path",
        [
          ("d", "M400 354 C410 366 418 378 424 388"),
          ("fill", "none"),
          ("stroke", "#e0aa1e"),
          ("stroke-width", "8"),
          ("stroke-linecap", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M400 354 C410 366 418 378 424 388"),
          ("fill", "none"),
          ("stroke", "#8a6214"),
          ("stroke-width", "1.3"),
          ("stroke-linecap", "round"),
          ("opacity", ".45"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M182 288 L170 374 L266 398"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "30"),
          ("stroke-linecap", "round"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M182 288 L170 374 L266 398"),
          ("fill", "none"),
          ("stroke", "#6b7a58"),
          ("stroke-width", "23"),
          ("stroke-linecap", "round"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M178 296 L168 372"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "7"),
          ("stroke-linecap", "round"),
          ("opacity", ".16"),
        ],
      ),
      e(
        "path",
        [
          (
            "d",
            "M272 388 C288 386 296 394 292 404 C288 414 274 414 268 406 Z",
          ),
          ("fill", "#c79a63"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.8"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M292 392 L350 384 L372 386 L372 394 L350 396 L294 404 Z"),
          ("fill", "#cbc2ae"),
          ("stroke", "#2b2117"),
          ("stroke-width", "2.6"),
          ("stroke-linejoin", "round"),
        ],
      ),
      e(
        "path",
        [
          ("d", "M296 394 L352 387"),
          ("fill", "none"),
          ("stroke", "#faf4e6"),
          ("stroke-width", "1.5"),
          ("opacity", ".55"),
        ],
      ),
      /* the plate's own frame; the medallion breaks it, which is what says "detail, magnified" rather than "a second clock on the table" */
      e(
        "rect",
        [
          ("x", "5"),
          ("y", "5"),
          ("width", "670"),
          ("height", "442"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "3.6"),
        ],
      ),
      e(
        "rect",
        [
          ("x", "10.5"),
          ("y", "10.5"),
          ("width", "659"),
          ("height", "431"),
          ("fill", "none"),
          ("stroke", "#2b2117"),
          ("stroke-width", "1.1"),
          ("opacity", ".4"),
        ],
      ),
      e(
        "circle",
        [("cx", "612"), ("cy", "416"), ("r", "57"), ("fill", "#f2e8d4")],
      ),
      svg(
        "g",
        [
          (
            "transform",
            "translate(612 416) scale(0.794) translate(-596 -392)",
          ),
        ],
        [
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "63"),
              ("fill", "#f2e8d4"),
              ("stroke", "#2b2117"),
              ("stroke-width", "3.6"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "57.5"),
              ("fill", "none"),
              ("stroke", "#2b2117"),
              ("stroke-width", "1.2"),
              ("opacity", ".5"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "51"),
              ("fill", "#cbc2ae"),
              ("stroke", "#2b2117"),
              ("stroke-width", "2.4"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "47.5"),
              ("fill", "none"),
              ("stroke", "#2b2117"),
              ("stroke-width", "5.6"),
              ("pathLength", "60"),
              ("stroke-dasharray", ".34 .66"),
              ("opacity", ".4"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "51"),
              ("fill", "url(#c-h1)"),
              ("opacity", ".16"),
            ],
          ),
          svg(
            "g",
            [("transform", "translate(636 414) rotate(32)")],
            [
              e(
                "rect",
                [
                  ("x", "0"),
                  ("y", "-8"),
                  ("width", "11"),
                  ("height", "16"),
                  ("rx", "2"),
                  ("fill", "#cbc2ae"),
                  ("stroke", "#2b2117"),
                  ("stroke-width", "2.2"),
                ],
              ),
              e(
                "path",
                [
                  ("d", "M11 -7 L25 -3 Q28 0 25 3 L11 7 Z"),
                  ("fill", "#cbc2ae"),
                  ("stroke", "#2b2117"),
                  ("stroke-width", "2.2"),
                  ("stroke-linejoin", "round"),
                ],
              ),
            ],
          ),
          e(
            "path",
            [
              (
                "d",
                "M664 424 C672 428 674 436 669 441 A6.4 6.4 0 0 1 658 437 C657 433 660 428 664 424 Z",
              ),
              ("fill", "#e0aa1e"),
              ("stroke", "#8a6214"),
              ("stroke-width", "1.6"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "44"),
              ("fill", "#dba828"),
              ("stroke", "#8a6214"),
              ("stroke-width", "1.8"),
            ],
          ),
          svg(
            "g",
            [
              ("fill", "none"),
              ("stroke", "#8a6214"),
              ("stroke-width", ".6"),
              ("opacity", ".3"),
            ],
            [
              e("circle", [("cx", "596"), ("cy", "392"), ("r", "8")]),
              e("circle", [("cx", "596"), ("cy", "392"), ("r", "14")]),
              e("circle", [("cx", "596"), ("cy", "392"), ("r", "20")]),
              e("circle", [("cx", "596"), ("cy", "392"), ("r", "26")]),
              e("circle", [("cx", "596"), ("cy", "392"), ("r", "32")]),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "38"),
              ("fill", "none"),
              ("stroke", "#33250a"),
              ("stroke-width", "3.2"),
              ("pathLength", "60"),
              ("stroke-dasharray", ".3 .7"),
              ("opacity", ".55"),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "38"),
              ("fill", "none"),
              ("stroke", "#33250a"),
              ("stroke-width", "6.8"),
              ("pathLength", "12"),
              ("stroke-dasharray", ".16 .84"),
              ("opacity", ".5"),
            ],
          ),
          svg(
            "g",
            [
              ("stroke", "#2a1d05"),
              ("fill", "none"),
              ("stroke-width", "1.9"),
              ("stroke-linecap", "round"),
            ],
            [
              e("circle", [("cx", "596"), ("cy", "363"), ("r", "7.3")]),
              e(
                "path",
                [
                  ("d", "M590.8 357.8 L601.2 368.2 M590.8 368.2 L601.2 357.8"),
                ],
              ),
              e("circle", [("cx", "621"), ("cy", "392"), ("r", "4.2")]),
              e("path", [("d", "M625.4 392 L634.6 392")]),
            ],
          ),
          svg(
            "g",
            [
              ("text-anchor", "middle"),
              ("dominant-baseline", "central"),
              ("font-family", "Georgia, 'Times New Roman', serif"),
              ("font-size", "19"),
              ("fill", "#2a1d05"),
            ],
            [
              svg(
                "text",
                [
                  ("x", "596"),
                  ("y", "421"),
                  ("transform", "rotate(180 596 421)"),
                ],
                [Node.text("&")],
              ),
              svg(
                "text",
                [("x", "567"), ("y", "392")],
                [Node.text("!")],
              ),
            ],
          ),
          svg(
            "g",
            [
              ("text-anchor", "middle"),
              ("font-family", "'Source Code Pro', ui-monospace, monospace"),
              ("font-size", "5.2"),
              ("letter-spacing", "0.5"),
              ("fill", "#2a1d05"),
              ("opacity", ".7"),
            ],
            [
              svg(
                "text",
                [("x", "596"), ("y", "377")],
                [Node.text("RINGARD")],
              ),
              svg(
                "text",
                [("x", "596"), ("y", "411")],
                [Node.text("DIJON")],
              ),
            ],
          ),
          svg(
            "g",
            [
              ("fill", "#1f1707"),
              ("stroke", "#1f1707"),
              ("stroke-width", ".5"),
              ("stroke-linejoin", "round"),
            ],
            [
              svg(
                "g",
                [("transform", "rotate(300 596 392)")],
                [
                  e(
                    "path",
                    [
                      (
                        "d",
                        "M593.2 398 L593.2 374 L596 366 L598.8 374 L598.8 398 Z",
                      ),
                    ],
                  ),
                ],
              ),
              svg(
                "g",
                [("transform", "rotate(54 596 392)")],
                [
                  e(
                    "path",
                    [
                      ("d", "M594 398 L594 367 L596 359 L598 367 L598 398 Z"),
                    ],
                  ),
                ],
              ),
            ],
          ),
          svg(
            "g",
            [("transform", "rotate(228 596 392)")],
            [
              e(
                "line",
                [
                  ("x1", "596"),
                  ("y1", "403"),
                  ("x2", "596"),
                  ("y2", "353"),
                  ("stroke", "#a8392e"),
                  ("stroke-width", "1.5"),
                  ("stroke-linecap", "round"),
                ],
              ),
              e(
                "circle",
                [
                  ("cx", "596"),
                  ("cy", "405"),
                  ("r", "2.8"),
                  ("fill", "#a8392e"),
                ],
              ),
            ],
          ),
          e(
            "circle",
            [
              ("cx", "596"),
              ("cy", "392"),
              ("r", "3.2"),
              ("fill", "#1f1707"),
            ],
          ),
        ],
      ),
      svg(
        "g",
        [("fill", "none"), ("stroke", "#2b2117")],
        [
          e(
            "circle",
            [
              ("cx", "612"),
              ("cy", "416"),
              ("r", "57"),
              ("stroke-width", "1.2"),
              ("opacity", ".45"),
            ],
          ),
        ],
      ),
      svg(
        "text",
        [
          ("x", "340"),
          ("y", "468"),
          ("text-anchor", "middle"),
          ("font-family", "'Source Code Pro', ui-monospace, monospace"),
          ("font-size", "13"),
          ("letter-spacing", "1.5"),
          ("fill", "#2b2117"),
          ("opacity", ".7"),
        ],
        [Node.text("WHAT IS THE POINT OF KNOWING TIME")],
      ),
    ],
  );
