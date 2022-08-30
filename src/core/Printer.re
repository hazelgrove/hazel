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

let of_zipper = (~measured: Measured.t, z: Zipper.t): t => {
  let unselected = Zipper.unselect_and_zip(z);
  let mrows = measured.rows;
  let Measured.Point.{row, col} = Zipper.caret_point(measured, z);
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

let zipper_of_string =
    (~zipper_init=Zipper.init(0), id_gen: IdGen.state, str: string)
    : option((Zipper.t, IdGen.state)) => {
  let insert_to_zid:
    ((Zipper.t, IdGen.state), string) => (Zipper.t, IdGen.state) =
    ((z, id_gen), c) => {
      switch (
        Perform.go_z(Insert(c == "\n" ? Whitespace.linebreak : c), z, id_gen)
      ) {
      | Error(err) =>
        print_endline(
          "WARNING: zipper_of_string: insert: " ++ Action.Failure.show(err),
        );
        (z, id_gen);
      | Ok(r) => r
      };
    };
  try(
    str
    |> Util.StringUtil.to_list
    |> List.fold_left(insert_to_zid, (zipper_init, id_gen))
    |> Option.some
  ) {
  | e =>
    print_endline(
      "WARNING: zipper_of_string: exception during parse: "
      ++ Printexc.to_string(e),
    );
    None;
  };
};

let to_string_log = (~measured, z: Zipper.t): string => {
  let {code, selection, backpack} = of_zipper(~measured, z);
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

let to_string_basic = (z: Zipper.t): string => {
  z |> Zipper.unselect_and_zip |> of_segment;
};

let to_string_selection = (z: Zipper.t): string => {
  z.selection.content |> of_segment;
};
