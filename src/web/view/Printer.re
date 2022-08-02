open Core;
open Util;

[@deriving (show({with_path: false}), yojson)]
type t = {
  code: list(string),
  selection: list(string),
  backpack: list(list(string)),
};

let caret_str: string = "░";

//TODO(andrew): need a version with correct indentation for caret positioning
let rec of_segment = (seg: Segment.t): string =>
  seg |> List.map(of_piece) |> String.concat("")
and of_piece: Piece.t => string =
  fun
  | Tile(t) => of_tile(t)
  | Grout(_) => " "
  | Whitespace(w) => w.content == Whitespace.linebreak ? "\n" : w.content
and of_tile = (t: Tile.t): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(of_delim(t), of_segment)
  |> String.concat("")
and of_delim = (t: Piece.tile, i: int): string => List.nth(t.label, i);

let lines_to_list = String.split_on_char('\n');

let of_zipper = (z: Zipper.t): t => {
  let unselected = Zipper.unselect_and_zip(z);
  let map = Measured.of_segment(unselected);
  let mrows = map.rows;
  let Measured.{row, col} = Caret.point(map, z);
  let rows = unselected |> of_segment |> lines_to_list;
  let rows =
    List.mapi(
      (i, r) => {
        let m = Measured.Rows.find(i, mrows);
        StringUtil.repeat(m.indent, " ") ++ r;
      },
      rows,
    );
  let rows =
    switch (ListUtil.split_nth_opt(row, rows)) {
    | Some((pre, caret_row, suf)) when col < String.length(caret_row) =>
      pre @ [StringUtil.insert_nth(col, caret_str, caret_row)] @ suf
    | Some((pre, caret_row, suf)) => pre @ [caret_row ++ caret_str] @ suf
    | _ => rows
    };
  {
    code: rows, //String.concat("", rows),
    selection: z.selection.content |> of_segment |> lines_to_list,
    backpack:
      List.map(
        (s: Selection.t) => s.content |> of_segment |> lines_to_list,
        z.backpack,
      ),
  };
};

let to_string = (z: Zipper.t): string => {
  let {code, selection, backpack} = of_zipper(z);
  Printf.sprintf(
    "CODE:\n%s\nSELECTION:\n%s\n%s\n",
    String.concat("\n", code),
    String.concat("\n", selection),
    backpack
    |> List.mapi((i, b) =>
         "BP("
         ++ string_of_int(i)
         ++ "):\n"
         ++ String.concat("\n", b)
         ++ "\n"
       )
    |> String.concat(""),
  );
};
