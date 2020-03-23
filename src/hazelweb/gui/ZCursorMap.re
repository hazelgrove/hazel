module MoveKey = JSUtil.MoveKey;

type t = {
  mutable z: (int, int),
  map: CursorMap.t,
};

let mk = (~z: (int, int), ~map: CursorMap.t) => {z, map};

let get_cursor = zmap => (zmap.z, zmap.map |> CursorMap.find(zmap.z));

let move = (move_key: MoveKey.t, zmap) => {
  let (row, col) = zmap.z;
  switch (move_key) {
  | ArrowLeft =>
    switch (zmap.map |> CursorMap.find_before_within_row(row, col)) {
    | Some((row_col, _)) =>
      zmap.z = row_col;
      true;
    | None =>
      if (row == 0) {
        false;
      } else {
        let (row_col, _) = zmap.map |> CursorMap.end_of_row(row - 1);
        zmap.z = row_col;
        true;
      }
    }
  | ArrowRight =>
    switch (zmap.map |> CursorMap.find_after_within_row(row, col)) {
    | Some((row_col, _)) =>
      zmap.z = row_col;
      true;
    | None =>
      if (row == CursorMap.num_rows(zmap.map) - 1) {
        false;
      } else {
        let (row_col, _) = zmap.map |> CursorMap.start_of_row(row + 1);
        zmap.z = row_col;
        true;
      }
    }
  | _ => failwith("unimplemented")
  };
};
