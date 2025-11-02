open Js_of_ocaml;

type unsafe_any = Js.Unsafe.any;

type width =
  | One
  | Two;

let columns_of_width = (width: width): int =>
  switch (width) {
  | One => 1
  | Two => 2
  };

let emoji_re: unsafe_any =
  Js.Unsafe.eval_string("/\\p{Extended_Pictographic}/u");

let segmenter_src =
  "(function () {\n"
  ++ "  if (typeof Intl === 'undefined' || typeof Intl.Segmenter === 'undefined') {\n"
  ++ "    return undefined;\n"
  ++ "  }\n"
  ++ "  var segmenter = new Intl.Segmenter(undefined, { granularity: 'grapheme' });\n"
  ++ "  return function (input) {\n"
  ++ "    return Array.from(segmenter.segment(input), function (result) { return result.segment; });\n"
  ++ "  };\n"
  ++ "})()";

let segmenter_fn: Js.Optdef.t(unsafe_any) =
  Js.Unsafe.eval_string(segmenter_src);

let fallback_segmenter_src = "(function (input) { return Array.from(input); })";

let fallback_segmenter: unsafe_any =
  Js.Unsafe.eval_string(fallback_segmenter_src);

let to_js_array = (value: unsafe_any) => Js.Unsafe.coerce(value);

let graphemes = (s: string): list(string) => {
  let input = Js.string(s);
  let arr =
    switch (Js.Optdef.to_option(segmenter_fn)) {
    | Some(fn) =>
      to_js_array(Js.Unsafe.fun_call(fn, [|Js.Unsafe.inject(input)|]))
    | None =>
      to_js_array(
        Js.Unsafe.fun_call(fallback_segmenter, [|Js.Unsafe.inject(input)|]),
      )
    };
  arr |> Js.to_array |> Array.to_list |> List.map(Js.to_string);
};

let is_emoji_cluster = (cluster: string): bool =>
  Js.to_bool(
    Js.Unsafe.fun_call(
      Js.Unsafe.get(emoji_re, "test"),
      [|Js.Unsafe.inject(Js.string(cluster))|],
    ),
  );

let classify_cluster = (cluster: string): width =>
  is_emoji_cluster(cluster) ? Two : One;

let columns_of_cluster = (cluster: string): int =>
  columns_of_width(classify_cluster(cluster));

let columns_of_string = (s: string): int =>
  graphemes(s)
  |> List.fold_left((acc, cluster) => acc + columns_of_cluster(cluster), 0);

let max_columns = (lines: list(string)): int =>
  lines
  |> List.fold_left(
       (acc, line) => {
         let width = columns_of_string(line);
         width > acc ? width : acc;
       },
       0,
     );

let split_lines = (s: string): list(string) =>
  String.split_on_char('\n', s);

let bounding_box_for = (s: string): (int, int) => {
  let lines = split_lines(s);
  let length = List.length(lines);
  let row = length <= 0 ? 0 : length - 1;
  let col = max_columns(lines);
  (row, col);
};
