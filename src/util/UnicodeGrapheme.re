open Js_of_ocaml;

/* Shared utilities for grapheme-aware string manipulation.
   We rely on Intl.Segmenter when available and fall back to
   simple JS iteration otherwise. */

type unsafe_any = Js.Unsafe.any;

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

let graphemes = (s: string): array(string) => {
  let js_str = Js.string(s);
  let arr =
    switch (Js.Optdef.to_option(segmenter_fn)) {
    | Some(fn) =>
      to_js_array(Js.Unsafe.fun_call(fn, [|Js.Unsafe.inject(js_str)|]))
    | None =>
      to_js_array(
        Js.Unsafe.fun_call(
          fallback_segmenter,
          [|Js.Unsafe.inject(js_str)|],
        ),
      )
    };
  arr |> Js.to_array |> Array.map(Js.to_string);
};

let of_graphemes = (clusters: array(string)): string =>
  clusters |> Array.to_list |> String.concat("");

let length = (s: string): int => Array.length(graphemes(s));

let remove_nth = (s: string, idx: int): string => {
  let clusters = graphemes(s);
  let len = Array.length(clusters);
  if (idx < 0 || idx >= len) {
    invalid_arg("UnicodeGrapheme.remove_nth");
  };
  if (len == 1) {
    "";
  } else {
    let result = Array.make(len - 1, "");
    Array.blit(clusters, 0, result, 0, idx);
    Array.blit(clusters, idx + 1, result, idx, len - idx - 1);
    of_graphemes(result);
  };
};

let insert_nth = (s: string, idx: int, fragment: string): string => {
  let clusters = graphemes(s);
  let insert = graphemes(fragment);
  let len = Array.length(clusters);
  let ins_len = Array.length(insert);
  if (idx < 0 || idx > len) {
    invalid_arg("UnicodeGrapheme.insert_nth");
  };
  if (ins_len == 0) {
    s;
  } else {
    let target = Array.make(len + ins_len, "");
    let pos = idx;
    Array.blit(clusters, 0, target, 0, pos);
    Array.blit(insert, 0, target, pos, ins_len);
    Array.blit(clusters, pos, target, pos + ins_len, len - pos);
    of_graphemes(target);
  };
};

let split_nth = (s: string, idx: int): (string, string) => {
  let clusters = graphemes(s);
  let len = Array.length(clusters);
  if (idx < 0 || idx > len) {
    invalid_arg("UnicodeGrapheme.split_nth");
  };
  let pos = idx;
  let left = Array.make(pos, "");
  let right = Array.make(len - pos, "");
  Array.blit(clusters, 0, left, 0, pos);
  Array.blit(clusters, pos, right, 0, len - pos);
  (of_graphemes(left), of_graphemes(right));
};

let remove_last = (s: string): string => remove_nth(s, length(s) - 1);

let remove_first = (s: string): string => remove_nth(s, 0);

let append = (a: string, b: string): string => {
  let clusters_a = graphemes(a);
  let clusters_b = graphemes(b);
  let len_a = Array.length(clusters_a);
  let len_b = Array.length(clusters_b);
  let target = Array.make(len_a + len_b, "");
  Array.blit(clusters_a, 0, target, 0, len_a);
  Array.blit(clusters_b, 0, target, len_a, len_b);
  of_graphemes(target);
};

let to_array = graphemes;

let of_list = (lst: list(string)): string => String.concat("", lst);
