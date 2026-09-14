/* An easter egg.

   A Fumola instance named `mustardWatch` gets Girard's line about watches
   underneath the ordinary explanation of what an instance is, and a picture
   of the watch in question.

   The joke is Girard's, from the Ludics-era writing he signed Yann-Joachim
   Ringard: a device that only tells you the time is a device you cannot get
   mustard out of. The dial here is marked with the connectives instead of
   hours, which is the other half of it -- a mustard watch is, after all,
   logical. */

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

/* Hour markers: the connectives, where 12, 3, 6 and 9 would be. */
let marker = (x, y, glyph) =>
  svg(
    "text",
    [
      ("x", x),
      ("y", y),
      ("text-anchor", "middle"),
      ("dominant-baseline", "middle"),
      ("font-size", "11"),
      ("font-family", "serif"),
      ("fill", "#6b4f12"),
    ],
    [Node.text(glyph)],
  );

let art: Node.t =
  svg(
    "svg",
    [
      ("viewBox", "0 0 140 150"),
      ("width", "140"),
      ("height", "150"),
      ("role", "img"),
      ("aria-label", "a wristwatch dispensing mustard"),
    ],
    [
      /* Straps, above and below the case. */
      svg(
        "path",
        [
          ("d", "M52 14 h36 l-3 26 h-30 z"),
          ("fill", "#5a4632"),
          ("stroke", "#3b2e20"),
          ("stroke-width", "1.5"),
        ],
        [],
      ),
      svg(
        "path",
        [
          ("d", "M55 110 h30 l3 26 h-36 z"),
          ("fill", "#5a4632"),
          ("stroke", "#3b2e20"),
          ("stroke-width", "1.5"),
        ],
        [],
      ),
      /* The crown, reworked as a nozzle on the right of the case. */
      svg(
        "rect",
        [
          ("x", "103"),
          ("y", "68"),
          ("width", "12"),
          ("height", "9"),
          ("rx", "2"),
          ("fill", "#9aa0a6"),
          ("stroke", "#6b7075"),
          ("stroke-width", "1.5"),
        ],
        [],
      ),
      /* The dollop, mid-squeeze. */
      svg(
        "path",
        [
          (
            "d",
            "M115 72.5 c8 0 10 -6 15 -6 c6 0 9 5 9 10 c0 6 -5 10 -11 10 c-7 0 -10 -5 -13 -8 z",
          ),
          ("fill", "#e3b505"),
          ("stroke", "#a67c00"),
          ("stroke-width", "1.5"),
          ("stroke-linejoin", "round"),
        ],
        [],
      ),
      svg(
        "circle",
        [("cx", "132"), ("cy", "95"), ("r", "4"), ("fill", "#e3b505")],
        [],
      ),
      /* Case. */
      svg(
        "circle",
        [
          ("cx", "70"),
          ("cy", "75"),
          ("r", "36"),
          ("fill", "#c8ccd1"),
          ("stroke", "#7d8288"),
          ("stroke-width", "3"),
        ],
        [],
      ),
      /* Dial: mustard, of course. */
      svg(
        "circle",
        [
          ("cx", "70"),
          ("cy", "75"),
          ("r", "29"),
          ("fill", "#e8c33d"),
          ("stroke", "#a67c00"),
          ("stroke-width", "1.5"),
        ],
        [],
      ),
      marker("70", "54", "\xE2\x8A\x97"),
      marker("91", "75", "\xE2\x8A\xB8"),
      marker("70", "96", "\xE2\x85\x8B"),
      marker("49", "75", "!"),
      /* Hands. */
      svg(
        "line",
        [
          ("x1", "70"),
          ("y1", "75"),
          ("x2", "70"),
          ("y2", "60"),
          ("stroke", "#3b2e20"),
          ("stroke-width", "2.5"),
          ("stroke-linecap", "round"),
        ],
        [],
      ),
      svg(
        "line",
        [
          ("x1", "70"),
          ("y1", "75"),
          ("x2", "84"),
          ("y2", "82"),
          ("stroke", "#3b2e20"),
          ("stroke-width", "2"),
          ("stroke-linecap", "round"),
        ],
        [],
      ),
      svg(
        "circle",
        [("cx", "70"), ("cy", "75"), ("r", "2.5"), ("fill", "#3b2e20")],
        [],
      ),
    ],
  );
