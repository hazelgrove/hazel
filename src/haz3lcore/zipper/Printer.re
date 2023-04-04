open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), yojson)]
type t = {
  code: list(string),
  selection: list(string),
  backpack: list(list(string)),
};

let rec of_segment = (~holes, seg: Segment.t): string =>
  seg |> List.map(of_piece(~holes)) |> String.concat("")
and of_piece = (~holes, p: Piece.t): string =>
  switch (p) {
  | Tile(t) => of_tile(~holes, t)
  | Grout({shape: Concave, _}) => " "
  | Grout({shape: Convex, _}) when holes != None => Option.get(holes)
  | Grout({shape: Convex, _}) => " "
  | Secondary(w) =>
    Secondary.is_linebreak(w) ? "\n" : Secondary.get_string(w.content)
  }
and of_tile = (~holes, t: Tile.t): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(of_delim(t), of_segment(~holes))
  |> String.concat("")
and of_delim = (t: Piece.tile, i: int): string => List.nth(t.label, i);

let to_string_basic = (z: Zipper.t): string => {
  z
  |> Zipper.unselect_and_zip(~ignore_selection=true)
  |> of_segment(~holes=None);
};

let lines_to_list = String.split_on_char('\n');

let caret_str = "░";

let to_rows =
    (
      ~holes: option(string),
      ~measured: Measured.t,
      ~caret: option(Measured.Point.t),
      ~indent: string,
      ~segment: Segment.t,
    )
    : list(string) => {
  let indent_of = i => Measured.Rows.find(i, measured.rows).indent;
  let mk_indent = (i, r) => StringUtil.repeat(indent_of(i), indent) ++ r;
  let rows =
    segment |> of_segment(~holes) |> lines_to_list |> List.mapi(mk_indent);
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
    ~holes=None,
    ~measured,
    ~caret=None,
    ~indent=" ",
    ~segment=Zipper.unselect_and_zip(~ignore_selection=true, z),
  )
  |> String.concat("\n");

let to_string_editor =
    (~holes: option(string)=None, editor: Editor.t): string =>
  to_rows(
    ~holes,
    ~measured=editor.state.meta.measured,
    ~caret=None,
    ~indent=" ",
    ~segment=
      Zipper.unselect_and_zip(~ignore_selection=true, editor.state.zipper),
  )
  |> String.concat("\n");

let to_string_selection = (editor: Editor.t): string =>
  to_rows(
    ~measured=editor.state.meta.measured,
    ~caret=None,
    ~indent=" ",
    ~holes=None,
    ~segment=editor.state.zipper.selection.content,
  )
  |> String.concat("\n");

let to_log = (~measured: Measured.t, z: Zipper.t): t => {
  code:
    to_rows(
      ~holes=None,
      ~measured,
      ~caret=Some(Zipper.caret_point(measured, z)),
      ~indent=" ",
      ~segment=Zipper.unselect_and_zip(~ignore_selection=true, z),
    ),
  selection: z.selection.content |> of_segment(~holes=None) |> lines_to_list,
  backpack:
    List.map(
      (s: Selection.t) =>
        s.content |> of_segment(~holes=None) |> lines_to_list,
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
    (~zipper_init=Zipper.init(0), id_gen: IdGen.state, str: string)
    : option((Zipper.t, IdGen.state)) => {
  let insert_to_zid:
    ((Zipper.t, IdGen.state), string) => (Zipper.t, IdGen.state) =
    ((z, id_gen), c) => {
      switch (
        Perform.go_z(Insert(c == "\n" ? Form.linebreak : c), z, id_gen)
      ) {
      | Error(err) =>
        print_endline(
          "WARNING: zipper_of_string: insert: " ++ Action.Failure.show(err),
        );
        (z, id_gen);
      | exception exn =>
        print_endline(
          "WARNING: zipper_of_string: exception during insert: "
          ++ Printexc.to_string(exn),
        );
        (z, id_gen);
      | Ok(r) => r
      };
    };
  str
  |> Util.StringUtil.to_list
  |> List.fold_left(insert_to_zid, (zipper_init, id_gen))
  |> Option.some;
};

let paste_into_zip =
    (z: Zipper.t, id: Id.t, str: string): option((Zipper.t, Id.t)) => {
  /* HACK(andrew): These two perform calls are a hack to
     deal with the fact that pasting something like "let a = b in"
     won't trigger the barfing of the "in"; to trigger this, we
     insert a space, and then we immediately delete it. */
  let* (z, id) = zipper_of_string(~zipper_init=z, id, str);
  switch (Perform.go_z(Insert(" "), z, id)) {
  | Error(_) => None
  | Ok((z, id)) =>
    switch (Perform.go_z(Destruct(Left), z, id)) {
    | Error(_) => None
    | Ok((z, id)) => Some((z, id))
    }
  };
};
