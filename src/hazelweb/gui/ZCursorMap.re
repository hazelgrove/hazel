module MoveKey = JSUtil.MoveKey;

type t = {
  mutable z: ((int, int), CursorPath.rev_t),
  map: CursorMap.t,
};

let mk = (~z: ((int, int), CursorPath.rev_t), ~map: CursorMap.t) => {
  z,
  map,
};

let move = (move_key: MoveKey.t, zmap) => {
  let ((row, col), (pos, rev_steps)) = zmap.z;
  switch (move_key) {
  | ArrowLeft =>
    switch (pos, zmap.map |> CursorMap.find_before_within_row((row, col))) {
    | (OnText(j), _) when j > 0 =>
      zmap.z = ((row, col - 1), (OnText(j - 1), rev_steps));
      true;
    | (_, Some(z)) =>
      zmap.z = z;
      true;
    | (_, None) =>
      if (row == 0) {
        false;
      } else {
        zmap.z = zmap.map |> CursorMap.end_of_row(row - 1);
        true;
      }
    }
  | ArrowRight =>
    switch (pos, zmap.map |> CursorMap.find_after_within_row((row, col))) {
    | (OnText(j), Some((_, (OnText(_), rev_steps_after))))
        when rev_steps === rev_steps_after || rev_steps == rev_steps_after =>
      zmap.z = ((row, col + 1), (OnText(j + 1), rev_steps));
      true;
    | (_, Some(z)) =>
      zmap.z = z;
      true;
    | (_, None) =>
      if (row == CursorMap.num_rows(zmap.map) - 1) {
        false;
      } else {
        zmap.z = zmap.map |> CursorMap.start_of_row(row + 1);
        true;
      }
    }
  | ArrowUp =>
    if (row <= 0) {
      false;
    } else {
      zmap.z = zmap.map |> CursorMap.find_nearest_within_row((row - 1, col));
      true;
    }
  | ArrowDown =>
    if (row > CursorMap.num_rows(zmap.map)) {
      false;
    } else {
      zmap.z = zmap.map |> CursorMap.find_nearest_within_row((row + 1, col));
      true;
    }
  };
};
