open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;
open Virtual_dom.Vdom;
open Node;
open JsUtil;
open Js_of_ocaml;

let clss = Attr.classes;

let div_c = cls => div(~attrs=[Attr.class_(cls)]);
let span_c = cls => span(~attrs=[Attr.class_(cls)]);

let div_empty = div(~attrs=[Attr.create("style", "display:none")], []);

let div_if = (p, ats, ns) => p ? div(~attrs=[ats], ns) : div_empty;
let span_if = (p, ats, ns) => p ? span(~attrs=[ats], ns) : span([]);

let unless = (p, a) => p ? Effect.Many([]) : a;

let range = (~attrs=[], ~min="0", ~max="100", value) =>
  Node.input(
    ~attrs=
      [
        Attr.create("type", "range"),
        Attr.create("value", value),
        Attr.create("max", max),
        Attr.create("min", min),
      ]
      @ attrs,
    (),
  );

module TextArea = {
  type t = Js.t(Dom_html.textAreaElement);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos = {
    row: int,
    col: int,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type rel =
    | First
    | Middle
    | Last;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type rel_pos = {
    rows: rel,
    cols: rel,
  };

  let get = (id: string): Js.t(Dom_html.textAreaElement) =>
    id
    |> get_elem_by_id
    |> Dom_html.CoerceTo.textarea
    |> Js.Opt.get(_, _ => failwith("TextArea.get"));

  let content = (textarea: t): string => Js.to_string(textarea##.value);

  let lines = (textarea: t): list(string) =>
    textarea |> content |> StringUtil.to_lines;

  let caret_pos = (textarea: t): pos => {
    let rec find_position = (lines, cur_pos, row, col) => {
      switch (lines) {
      | [] => {row, col}
      | [line, ...rest] =>
        let line_length = String.length(line);
        if (cur_pos <= line_length) {
          {row, col: cur_pos};
        } else {
          find_position(rest, cur_pos - line_length - 1, row + 1, 0);
        };
      };
    };
    let lines = lines(textarea);
    let caret_position =
      try(textarea##.selectionStart) {
      | _ => 0
      };
    find_position(lines, caret_position, 0, 0);
  };

  let rel = (current: int, max: int): rel =>
    if (current == 0) {
      First;
    } else if (current == max) {
      Last;
    } else {
      Middle;
    };

  let caret_rel_pos = (textarea: t): rel_pos => {
    /* precondition: lines nonempty */
    let lines = textarea |> lines;
    let {row, col} = caret_pos(textarea);
    let full_row = List.nth(lines, row);
    {
      rows: rel(row, List.length(lines) - 1),
      cols: rel(col, String.length(full_row)),
    };
  };

  let caret_at_start = (textarea: t): bool => {
    let {rows, cols} = caret_rel_pos(textarea);
    rows == First && cols == First;
  };

  let caret_at_end = (textarea: t): bool => {
    /* precondition: lines nonempty */
    let lines = lines(textarea);
    let {rows, cols} = caret_rel_pos(textarea);
    switch (rows, cols, List.rev(lines)) {
    | (Last, Last, _) => true
    | (Last, First, ["", ..._]) => true
    | (First, Last, [_]) => true
    | (First, First, [""]) => true
    | _ => false
    };
  };

  let set_caret_to_end = (textarea: t): unit => {
    textarea##focus;
    let content_length = String.length(content(textarea));
    textarea##.selectionStart := content_length;
    textarea##.selectionEnd := content_length;
  };
};
