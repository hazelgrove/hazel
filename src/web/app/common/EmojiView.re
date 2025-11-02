open Virtual_dom.Vdom;
open Node;

module EmojiWidth = Util.EmojiWidth;

type segment =
  | Text(string)
  | Emoji(string, EmojiWidth.width);

let px = (value: float): string => Printf.sprintf("%.3fpx", value);

let segments_for_token = (token: string): list(segment) => {
  let clusters = EmojiWidth.graphemes(token);
  clusters
  |> List.fold_left(
       (acc, cluster) =>
         if (cluster == "") {
           acc;
         } else if (EmojiWidth.is_emoji_cluster(cluster)) {
           [Emoji(cluster, EmojiWidth.classify_cluster(cluster)), ...acc];
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

let to_class = (width: EmojiWidth.width): string =>
  switch (width) {
  | One => "emoji-1col"
  | Two => "emoji-2col"
  };

let render = (~font_metrics: FontMetrics.t, token: string): list(Node.t) =>
  segments_for_token(token)
  |> List.map(segment =>
       switch (segment) {
       | Text(str) => Node.text(str)
       | Emoji(grapheme, width) =>
         let cols = EmojiWidth.columns_of_width(width);
         let width_px = font_metrics.col_width *. float_of_int(cols);
         span(
           ~attrs=[
             Attr.classes(["emoji-cell", to_class(width)]),
             Attr.create("style", "width: " ++ px(width_px)),
           ],
           [Node.text(grapheme)],
         );
       }
     );
