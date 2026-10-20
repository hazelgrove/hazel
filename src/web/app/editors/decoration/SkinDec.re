open Js_of_ocaml;
open Virtual_dom.Vdom;

/* Ornament from the page's optional skin script (www/skin/skin.js). With no
   skin active this draws nothing, so the default look is unchanged. */

let active = (): bool =>
  Js.Unsafe.js_expr(
    "!!(typeof window !== 'undefined' && window.HazelSkin && window.HazelSkin.on)",
  )
  |> Js.to_bool;

/* A brace grown beside a term spanning rows first_row..last_row, whose
   content starts at column `col`. The skin returns SVG markup in pixels,
   relative to the code origin; it caches by geometry. Terms at column 0
   get none: there is no room left of them, and they are usually whole
   programs. */
let brace =
    (
      ~font_metrics: FontMetrics.t,
      ~col: int,
      ~first_row: int,
      ~last_row: int,
      ~cls: string,
    )
    : list(Node.t) =>
  if (last_row <= first_row || col == 0 || !active()) {
    [];
  } else {
    let svg: Js.t(Js.js_string) =
      Js.Unsafe.fun_call(
        Js.Unsafe.js_expr("window.HazelSkin.braceSVG"),
        [|
          Js.Unsafe.inject(Float.of_int(col) *. font_metrics.col_width),
          Js.Unsafe.inject(
            Float.of_int(first_row) *. font_metrics.row_height,
          ),
          Js.Unsafe.inject(
            Float.of_int(last_row + 1) *. font_metrics.row_height,
          ),
          Js.Unsafe.inject(font_metrics.row_height),
          Js.Unsafe.inject(font_metrics.col_width),
          Js.Unsafe.inject(Js.string(cls)),
        |],
      );
    [
      Node.create_svg(
        "svg",
        ~attrs=[
          Attr.classes(["orn-brace"]),
          Attr.string_property("innerHTML", Js.to_string(svg)),
        ],
        [],
      ),
    ];
  };
