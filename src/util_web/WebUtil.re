open Virtual_dom.Vdom;

module Node = Node;
module Attr = Attr;
open Node;
open Util;
open Util.JsUtil;
open Js_of_ocaml;

let clss = Attr.classes;

let div_c = cls => div(~attrs=[Attr.class_(cls)]);
let span_c = cls => span(~attrs=[Attr.class_(cls)]);

let div_empty = div(~attrs=[Attr.create("style", "display:none")], []);

let unless = (p, a) => p ? Effect.Many([]) : a;

let range = (~attrs=[], ~min="0", ~max="100", value) =>
  Node.input(
    ~attrs=
      [
        Attr.create("type", "range"),
        Attr.string_property("value", value),
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
      | [] => {
          row,
          col,
        }
      | [line, ...rest] =>
        let line_length = String.length(line);
        if (cur_pos <= line_length) {
          {
            row,
            col: cur_pos,
          };
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

  let is_last_pos = id => caret_at_end(get(id));
  let is_first_pos = id => caret_at_start(get(id));

  let set_caret_to_start = (textarea: t): unit => {
    textarea##focus;
    textarea##.selectionStart := 0;
    textarea##.selectionEnd := 0;
  };

  let set_caret_to_end = (textarea: t): unit => {
    textarea##focus;
    let content_length = String.length(content(textarea));
    textarea##.selectionStart := content_length;
    textarea##.selectionEnd := content_length;
  };
};

/* Shared empty-hole geometry. The convex path is built from the same tip
 * primitives as EmptyHoleDec so the table-header version and the in-code
 * decoration version stay byte-identical. */
module EmptyHole = {
  let s_y = 0.28;
  let s_x = s_y *. 1.5;
  let tip_width = 0.32; /* matches ShardDec.tip_width */

  open SvgUtil.Path;

  let tr_bl_north = [
    H_({dx: 0.}),
    L_({
      dx: -. tip_width,
      dy: 0.5,
    }),
  ];
  let tl_br_north = [
    H_({dx: 0.}),
    L_({
      dx: tip_width,
      dy: 0.5,
    }),
  ];
  let tr_bl_south = [
    L_({
      dx: -. tip_width,
      dy: 0.5,
    }),
    H_({dx: 0.}),
  ];
  let tl_br_south = [
    L_({
      dx: tip_width,
      dy: 0.5,
    }),
    H_({dx: 0.}),
  ];

  let bl_tr_north = SvgUtil.Path.reverse(tr_bl_north);
  let bl_tr_south = SvgUtil.Path.reverse(tr_bl_south);
  let br_tl_north = SvgUtil.Path.reverse(tl_br_north);
  let br_tl_south = SvgUtil.Path.reverse(tl_br_south);

  let left_tip_path_convex = br_tl_south @ bl_tr_north;
  let right_tip_path_convex = tl_br_north @ tr_bl_south;

  let left_tip_path_concave =
    [H_({dx: Float.neg(tip_width)}), ...bl_tr_south]
    @ br_tl_north
    @ [H_({dx: tip_width})];
  let right_tip_path_concave =
    [H_({dx: tip_width}), ...tr_bl_north]
    @ tl_br_south
    @ [H_({dx: Float.neg(tip_width)})];

  let path = (tip_l, tip_r): list(SvgUtil.Path.cmd) =>
    List.concat([
      [
        M({
          x: 0.5,
          y: 0.5 -. s_y /. 2.,
        }),
        H_({dx: s_x /. 2.}),
      ],
      SvgUtil.Path.scale_x(s_x, SvgUtil.Path.scale_y(s_y, tip_l)),
      [H_({dx: -. s_x})],
      SvgUtil.Path.scale_x(s_x, SvgUtil.Path.scale_y(s_y, tip_r)),
      [Z],
    ]);

  let path_convex = path(right_tip_path_convex, left_tip_path_convex);
  let path_concave = path(right_tip_path_concave, left_tip_path_concave);
};

let empty_hole_svg = (~attrs=[], ()) =>
  Node.create_svg(
    "svg",
    ~attrs=
      [
        Attr.classes(["empty-hole"]),
        Attr.create("viewBox", "0 0.3 1 0.4"),
        Attr.create("preserveAspectRatio", "none"),
      ]
      @ attrs,
    [SvgUtil.Path.view(~attrs=[], EmptyHole.path_convex)],
  );
