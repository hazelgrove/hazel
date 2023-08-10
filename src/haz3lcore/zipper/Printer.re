open Util;

[@deriving (show({with_path: false}), yojson)]
type t = {
  code: list(string),
  selection: list(string),
  backpack: list(list(string)),
};

let rec of_segment = (seg: Segment.t): string =>
  seg |> List.map(of_piece) |> String.concat("")
and of_piece: Piece.t => string =
  fun
  | Tile(t) => of_tile(t)
  | Grout(_) => " "
  | Secondary(w) =>
    Secondary.is_linebreak(w) ? "\n" : Secondary.get_string(w.content)
and of_tile = (t: Tile.t): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(of_delim(t), of_segment)
  |> String.concat("")
and of_delim = (t: Piece.tile, i: int): string => List.nth(t.label, i);

let to_string_basic = (z: Zipper.t): string => {
  z |> Zipper.unselect_and_zip |> of_segment;
};

let lines_to_list = String.split_on_char('\n');

let caret_str = "░";

let to_rows =
    (
      ~measured: Measured.t,
      ~caret: option(Measured.Point.t),
      ~indent: string,
      ~segment: Segment.t,
    )
    : list(string) => {
  let indent_of = i => Measured.Rows.find(i, measured.rows).indent;
  let mk_indent = (i, r) => StringUtil.repeat(indent_of(i), indent) ++ r;
  let rows = segment |> of_segment |> lines_to_list |> List.mapi(mk_indent);
  switch (caret) {
  | Some({row, col}) =>
    switch (ListUtil.split_nth_opt(row, rows)) {
    | Some((pre, caret_row, suf)) when col < String.length(caret_row) =>
      pre @ [StringUtil.insert_nth(col, caret_str, caret_row)] @ suf
    | Some((pre, caret_row, suf)) => pre @ [caret_row ++ caret_str] @ suf
    | _ => rows
    }
  | None => rows
  };
};

let pretty_print = (~measured: Measured.t, z: Zipper.t): string =>
  to_rows(
    ~measured,
    ~caret=None,
    ~indent=" ",
    ~segment=Zipper.unselect_and_zip(z),
  )
  |> String.concat("\n");

let to_string_editor = (editor: Editor.t): string =>
  to_rows(
    ~measured=editor.state.meta.measured,
    ~caret=None,
    ~indent=" ",
    ~segment=Zipper.unselect_and_zip(editor.state.zipper),
  )
  |> String.concat("\n");

let to_string_selection = (editor: Editor.t): string =>
  to_rows(
    ~measured=editor.state.meta.measured,
    ~caret=None,
    ~indent=" ",
    ~segment=editor.state.zipper.selection.content,
  )
  |> String.concat("\n");

let to_log = (~measured: Measured.t, z: Zipper.t): t => {
  code:
    to_rows(
      ~measured,
      ~caret=Some(Zipper.caret_point(measured, z)),
      ~indent=" ",
      ~segment=Zipper.unselect_and_zip(z),
    ),
  selection: z.selection.content |> of_segment |> lines_to_list,
  backpack:
    List.map(
      (s: Selection.t) => s.content |> of_segment |> lines_to_list,
      z.backpack,
    ),
};

let to_log_flat = (~measured, z: Zipper.t): string => {
  let {code, selection, backpack} = to_log(~measured, z);
  Printf.sprintf(
    "CODE:\n%s\nSELECTION:\n%s\n%s\n",
    String.concat("\n", code),
    String.concat("\n", selection),
    backpack
    |> List.mapi((i, b) =>
         Printf.sprintf("BP(%d):\n %s\n", i, String.concat("\n", b))
       )
    |> String.concat(""),
  );
};

let zipper_of_string =
    (~zipper_init=Zipper.init(), str: string): option(Zipper.t) => {
  let insert_to_zid: (Zipper.t, string) => Zipper.t =
    (z, c) => {
      switch (Insert.go(c == "\n" ? Form.linebreak : c, z)) {
      | None =>
        print_endline("WARNING: zipper_of_string: insert returned none ");
        z;
      | exception exn =>
        print_endline(
          "WARNING: zipper_of_string: exception during insert: "
          ++ Printexc.to_string(exn),
        );
        z;
      | Some(r) => r
      };
    };
  str
  |> Util.StringUtil.to_list
  |> List.fold_left(insert_to_zid, zipper_init)
  |> Option.some;
};
