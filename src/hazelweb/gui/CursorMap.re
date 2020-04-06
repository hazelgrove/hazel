module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;

module Row = {
  type t = int;
  let compare = Int.compare;
};
module RowMap = Map.Make(Row);

module Col = {
  type t = int;
  let compare = Int.compare;
};
module ColMap = {
  include Map.Make(Col);

  let find_before = (col, map) =>
    map |> find_last_opt(c => Int.compare(c, col) < 0);
  let find_before_eq = (col, map) =>
    map |> find_last_opt(c => Int.compare(c, col) <= 0);

  let find_after = (col, map) =>
    map |> find_first_opt(c => Int.compare(c, col) > 0);
  let find_after_eq = (col, map) =>
    map |> find_first_opt(c => Int.compare(c, col) >= 0);
};

type t = RowMap.t(ColMap.t(CursorPath.rev_t));
type binding = ((Row.t, Col.t), CursorPath.rev_t);

let of_layout = (l: UHLayout.t): (t, option(binding)) => {
  let row = ref(0);
  let col = ref(0);
  let z = ref(None);
  let rec go = (~indent, ~rev_steps, l: UHLayout.t) => {
    let go' = go(~indent, ~rev_steps);
    switch (l) {
    | Text(s) =>
      col := col^ + StringUtil.utf8_length(s);
      RowMap.empty;
    | Linebreak =>
      row := row^ + 1;
      col := indent;
      RowMap.empty;
    | Align(l) => go(~indent=col^, ~rev_steps, l)
    | Cat(l1, l2) =>
      let map1 = go'(l1);
      let map2 = go'(l2);
      RowMap.union(
        (_, col_map1, col_map2) => {
          Some(ColMap.union((_, _, _) => None, col_map1, col_map2))
        },
        map1,
        map2,
      );

    | Annot(Step(step), l) =>
      go(~rev_steps=[step, ...rev_steps], ~indent, l)

    | Annot(CursorPosition({has_cursor, cursor}), _) =>
      if (has_cursor) {
        z := Some(((row^, col^), (cursor, rev_steps)));
      };
      RowMap.singleton(row^, ColMap.singleton(col^, (cursor, rev_steps)));

    | Annot(Text({has_cursor, _}), l) =>
      switch (has_cursor) {
      | None => ()
      | Some(j) => z := Some(((row^, col^ + j), (OnText(j), rev_steps)))
      };
      let col_before = col^;
      let _ = go'(l);
      let col_after = col^;
      RowMap.singleton(
        row^,
        ColMap.(
          empty
          |> add(col_before, (CursorPosition.OnText(0), rev_steps))
          |> add(
               col_after,
               (CursorPosition.OnText(col_after - col_before), rev_steps),
             )
        ),
      );

    | Annot(_, l) => go'(l)
    };
  };
  let map = go(~indent=0, ~rev_steps=[], l);
  (map, z^);
};

let num_rows = cmap => RowMap.cardinal(cmap);

let find = ((row, col), cmap) =>
  cmap |> RowMap.find(row) |> ColMap.find(col);

let start_of_row = (row, cmap) =>
  cmap
  |> RowMap.find(row)
  |> ColMap.min_binding
  |> (
    fun
    | (col, rev_path) => ((row, col), rev_path)
  );

let end_of_row = (row, cmap) =>
  cmap
  |> RowMap.find(row)
  |> ColMap.max_binding
  |> (
    fun
    | (col, rev_path) => ((row, col), rev_path)
  );

let find_before_within_row = ((row, col), cmap) =>
  cmap
  |> RowMap.find(row)
  |> ColMap.find_before(col)
  |> Option.map(((col, rev_path)) => ((row, col), rev_path));

let find_after_within_row = ((row, col), cmap) =>
  cmap
  |> RowMap.find(row)
  |> ColMap.find_after(col)
  |> Option.map(((col, rev_path)) => ((row, col), rev_path));

// TODO standardize whether CursorMap is aware of text cursor positions
let find_nearest_within_row = ((row, col), cmap) => {
  let col_map = cmap |> RowMap.find(row);
  switch (
    col_map |> ColMap.find_before_eq(col),
    col_map |> ColMap.find_after_eq(col),
  ) {
  | (None, None) =>
    failwith(
      "CursorMap has row with no caret positions: " ++ string_of_int(row),
    )
  | (Some((nearest_col, rev_path)), None)
  | (None, Some((nearest_col, rev_path))) => ((row, nearest_col), rev_path)
  | (Some((col', rev_path)), _) when col' == col => ((row, col), rev_path)
  | (
      Some((col_before, (CursorPosition.OnText(_), rev_steps_before))),
      Some((_, (CursorPosition.OnText(_), rev_steps_after))),
    )
      when
        rev_steps_before === rev_steps_after
        || rev_steps_before == rev_steps_after => (
      (row, col),
      (OnText(col - col_before), rev_steps_before),
    )
  | (Some((col_before, rev_path_before)), Some((col_after, rev_path_after))) =>
    col - col_before <= col_after - col
      ? ((row, col_before), rev_path_before)
      : ((row, col_after), rev_path_after)
  };
};

let move = (move_key: JSUtil.MoveKey.t, z: binding, map: t): option(binding) => {
  let ((row, col), (pos, rev_steps)) = z;
  switch (move_key) {
  | ArrowLeft =>
    switch (pos, map |> find_before_within_row((row, col))) {
    | (OnText(j), _) when j > 0 =>
      Some(((row, col - 1), (OnText(j - 1), rev_steps)))
    | (_, Some(z)) => Some(z)
    | (_, None) => row == 0 ? None : Some(map |> end_of_row(row - 1))
    }
  | ArrowRight =>
    switch (pos, map |> find_after_within_row((row, col))) {
    | (OnText(j), Some((_, (OnText(_), rev_steps_after))))
        when rev_steps === rev_steps_after || rev_steps == rev_steps_after =>
      Some(((row, col + 1), (OnText(j + 1), rev_steps)))
    | (_, Some(z)) => Some(z)
    | (_, None) =>
      row == num_rows(map) - 1 ? None : Some(map |> start_of_row(row + 1))
    }
  | ArrowUp =>
    row <= 0 ? None : Some(map |> find_nearest_within_row((row - 1, col)))
  | ArrowDown =>
    row >= num_rows(map) - 1
      ? None : Some(map |> find_nearest_within_row((row + 1, col)))
  };
};
