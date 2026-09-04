open Virtual_dom.Vdom;
open Node;
open Util_web;

/* For Unicode graphemes (emoji, CJK, fullwidth forms) which do not conform
   to the character grid, we render them as a single cell with a width
   that matches the number of columns they occupy. This is currently
   limited to either one or two columns. Widths come from Unicode.Width,
   the same source Measured and the caret arithmetic use, so what is drawn
   and what is measured cannot drift apart.

   Invisible clusters (zero-width and bidi controls, exotic spaces) also
   get an explicit cell, labeled with their codepoint: they are measured
   as a column but would otherwise render as nothing, silently shifting
   the rest of the row off the measured grid. */

type segment =
  | Text(string)
  | Grapheme(string, Unicode.Width.t)
  | Invisible(string, Unicode.Width.t);

let px = (value: float): string => Printf.sprintf("%.3fpx", value);

let segments_for_token = (token: string): list(segment) => {
  let clusters = Unicode.Width.graphemes(token);
  clusters
  |> List.fold_left(
       (acc, cluster) =>
         if (cluster == "") {
           acc;
         } else if (Unicode.is_invisible_cluster(cluster)) {
           [
             Invisible(cluster, Unicode.Width.classify_cluster(cluster)),
             ...acc,
           ];
         } else if (Unicode.Width.is_wide_cluster(cluster)) {
           [
             Grapheme(cluster, Unicode.Width.classify_cluster(cluster)),
             ...acc,
           ];
         } else {
           switch (acc) {
           | [Text(existing), ...rest] => [
               Text(existing ++ cluster),
               ...rest,
             ]
           | _ => [Text(cluster), ...acc]
           };
         },
       [],
     )
  |> List.rev;
};

let to_class = (width: Unicode.Width.t): string =>
  switch (width) {
  | One => "grapheme-1col"
  | Two => "grapheme-2col"
  };

let render = (~font_metrics: FontMetrics.t, token: string): list(Node.t) =>
  segments_for_token(token)
  |> List.map(segment =>
       switch (segment) {
       | Text(str) => Node.text(str)
       | Grapheme(grapheme, width) =>
         let cols = Unicode.Width.columns_of_width(width);
         let width_px = font_metrics.col_width *. float_of_int(cols);
         span(
           ~attrs=[
             Attr.classes(["grapheme-cell", to_class(width)]),
             Attr.create("style", "width: " ++ px(width_px)),
           ],
           [Node.text(grapheme)],
         );
       | Invisible(cluster, width) =>
         let cols = Unicode.Width.columns_of_width(width);
         let width_px = font_metrics.col_width *. float_of_int(cols);
         let hex = Printf.sprintf("%04X", Unicode.codepoint_at(cluster, 0));
         span(
           ~attrs=[
             Attr.classes([
               "grapheme-cell",
               "invisible-cell",
               to_class(width),
             ]),
             Attr.create("style", "width: " ++ px(width_px)),
             Attr.create("title", "Invisible character U+" ++ hex),
           ],
           [Node.text(hex)],
         );
       }
     );
