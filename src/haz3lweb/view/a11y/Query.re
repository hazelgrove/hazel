open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | CursorPos
  | Expression;

let to_string = (query: t): string =>
  switch (query) {
  | CursorPos => "CursorPos"
  | Expression => "Expression"
  };

let get_position = (id: Id.t, measured: Measured.t) =>
  switch (Measured.find_by_id(id, measured)) {
  | Some(m) => Some(m.last)
  | None => None
  };

let query_reply = (query: t, editor: Editor.t) => {
  switch (query) {
  | CursorPos =>
    let zipper = editor.state.zipper;
    let index = Indicated.index(zipper);
    let position =
      switch (index) {
      | Some(id) => get_position(id, editor.state.meta.measured)
      | None => None
      };
    switch (position) {
    | Some({row, col}) =>
      "It is on row "
      ++ string_of_int(row)
      ++ " column "
      ++ string_of_int(col)
      ++ "."
    | None => "Error occurs when querying cursor position."
    };
  | Expression => "TODO"
  };
};
