open Util;
open WebUtil;

/* Another user's caret (collaborative editing, docs/collab-modular.md):
   a thin bar in their colour with their name above it. Positioned like
   the local caret, from a Point in the editor's measured space. */

let caret_width = 0.2;

/* first word, at most 8 characters */
let truncate_name = (name: string): string => {
  let name =
    switch (String.index_opt(name, ' ')) {
    | Some(i) => String.sub(name, 0, i)
    | None => name
    };
  Unicode.length(name) > 8
    ? Unicode.of_list(ListUtil.take(7, Unicode.to_list(name)))
      ++ {js|…|js}
    : name;
};

let main =
    (
      ~user_id: string,
      ~user_name: option(string),
      ~font_metrics: FontMetrics.t,
      ~color: string,
      ~origin: Point.t,
    ) => {
  let scale = 1.0;
  let height_fudge = ShardDec.shadow_dy *. font_metrics.row_height;
  let paths =
    ShardDec.chonky_path_base(
      (None, None),
      ShardDec.shape_adjust(Direction.Left, None) +. 0.5 *. caret_width,
      caret_width,
      0.,
    );
  let caret_svg =
    Node.create_svg(
      "svg",
      ~attrs=[
        Attr.classes(["remote-caret-svg"]),
        Attr.create("viewBox", Printf.sprintf("0 0 %f %f", scale, scale)),
        Attr.create("preserveAspectRatio", "none"),
      ],
      [
        SvgUtil.Path.view(
          ~attrs=[
            Attr.classes(["remote-caret-path"]),
            Attr.create("style", "fill: " ++ color ++ ";"),
          ],
          paths,
        ),
      ],
    );
  let label =
    switch (user_name) {
    | None => []
    | Some(name) => [
        Node.div(
          ~attrs=[
            Attr.classes(["remote-caret-label"]),
            Attr.create("style", "background-color: " ++ color ++ ";"),
          ],
          [Node.text(truncate_name(name))],
        ),
      ]
    };
  Node.div(
    ~key="remote-caret-" ++ user_id,
    ~attrs=[
      Attr.classes(["remote-caret"]),
      DecUtil.abs_position(~font_metrics, ~height_fudge, ~scale, origin),
    ],
    label @ [caret_svg],
  );
};
