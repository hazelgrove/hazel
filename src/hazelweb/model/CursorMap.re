module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Sexplib.Std;

module Row = {
  [@deriving sexp]
  type t = int;
  let compare = Int.compare;
};
module RowMap = Map.Make(Row);

module Col = {
  [@deriving sexp]
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

  let log_sexp = col_map =>
    col_map
    |> iter((col, rev_path) => {
         JSUtil.log("col = " ++ Sexplib.Sexp.to_string(Col.sexp_of_t(col)));
         JSUtil.log(
           "rev_path = "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_rev_t(rev_path)),
         );
       });
};

type t = RowMap.t(ColMap.t(CursorPath.rev_t));
type with_splices = (t, SpliceMap.t(t));

[@deriving sexp]
type binding = ((Row.t, Col.t), CursorPath.rev_t);

let empty = RowMap.empty;

let compare_overlapping_paths =
    (
      (pos1, rev_steps1): CursorPath.rev_t,
      (pos2, rev_steps2): CursorPath.rev_t,
    ) => {
  let n1 = List.length(rev_steps1);
  let n2 = List.length(rev_steps2);
  if (n1 > n2) {
    1;
  } else if (n1 < n2) {
    (-1);
  } else {
    switch (pos1, pos2) {
    | (OnText(_), OnText(_)) => 0
    | (OnText(_), _) => 1
    | (_, OnText(_)) => (-1)
    | (OnDelim(_), OnDelim(_)) => 0
    | (OnDelim(_), _) => 1
    | (_, OnDelim(_)) => (-1)
    | (OnOp(_), OnOp(_)) => 0
    };
  };
};

type z = (option((MetaVar.t, SpliceName.t)), binding);

let of_layout =
    ((l, splice_ls): UHLayout.with_splices): (with_splices, option(z)) => {
  let z = ref(None);
  let splice_cmaps = ref(SpliceMap.empty);
  let rec of_splice =
          (
            ~splice: option((MetaVar.t, SpliceName.t))=?,
            ~rev_steps,
            l: UHLayout.t,
          )
          : t => {
    let row = ref(0);
    let col = ref(0);
    let rec go = (~indent, ~rev_steps, l: UHLayout.t): t => {
      let go' = go(~indent, ~rev_steps);
      switch (l) {
      | Text(s) =>
        col := col^ + StringUtil.utf8_length(s);
        empty;
      | Linebreak =>
        row := row^ + 1;
        col := indent;
        empty;
      | Align(l) => go(~indent=col^, ~rev_steps, l)
      | Cat(l1, l2) =>
        let cmap1 = go'(l1);
        let cmap2 = go'(l2);
        RowMap.union(
          (_, col_map1, col_map2) => {
            Some(
              ColMap.union(
                (_, rev_path1, rev_path2) =>
                  Some(
                    compare_overlapping_paths(rev_path1, rev_path2) > 0
                      ? rev_path1 : rev_path2,
                  ),
                col_map1,
                col_map2,
              ),
            )
          },
          cmap1,
          cmap2,
        );

      | Annot(Step(step), l) =>
        go(~rev_steps=[step, ...rev_steps], ~indent, l)

      | Annot(LivelitView({llu, _}), l) =>
        let ap_cmaps =
          splice_ls
          |> SpliceMap.get_ap(llu)
          |> SpliceMap.ApMap.bindings
          |> List.map(((splice_name, splice_layout)) =>
               (
                 splice_name,
                 of_splice(
                   ~splice=(llu, splice_name),
                   ~rev_steps=[splice_name, ...rev_steps],
                   splice_layout,
                 ),
               )
             )
          |> List.to_seq
          |> SpliceMap.ApMap.of_seq;
        splice_cmaps := splice_cmaps^ |> SpliceMap.put_ap(llu, ap_cmaps);
        // to update row col
        let _ = go'(l);
        empty;

      | Annot(Token({shape, has_cursor, len}), l) =>
        let col_before = col^;
        let _ = go'(l);
        let col_after = col^;
        switch (has_cursor) {
        | None => ()
        | Some(j) =>
          let pos: CursorPosition.t =
            switch (shape) {
            | Text => OnText(j)
            | Op => OnOp(j == 0 ? Before : After)
            | Delim(k) => OnDelim(k, j == 0 ? Before : After)
            };
          z := Some((splice, ((row^, col_before + j), (pos, rev_steps))));
        };
        let (pos_before, pos_after): (CursorPosition.t, CursorPosition.t) =
          switch (shape) {
          | Text => (OnText(0), OnText(len))
          | Op => (OnOp(Before), OnOp(After))
          | Delim(k) => (OnDelim(k, Before), OnDelim(k, After))
          };
        RowMap.singleton(
          row^,
          ColMap.singleton(col_before, (pos_before, rev_steps))
          |> ColMap.add(col_after, (pos_after, rev_steps)),
        );

      | Annot(_, l) => go'(l)
      };
    };
    go(~indent=0, ~rev_steps, l);
  };
  let cmap = of_splice(~rev_steps=[], l);
  ((cmap, splice_cmaps^), z^);
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

let move_up = ((row, col), cmap): option(binding) =>
  row <= 0 ? None : Some(cmap |> find_nearest_within_row((row - 1, col)));

let move_down = ((row, col), cmap): option(binding) =>
  row >= num_rows(cmap) - 1
    ? None : Some(cmap |> find_nearest_within_row((row + 1, col)));

let move_left =
    (((row, col), (pos, rev_steps)): binding, cmap): option(binding) =>
  switch (pos, cmap |> find_before_within_row((row, col))) {
  | (OnText(j), _) when j > 0 =>
    Some(((row, col - 1), (OnText(j - 1), rev_steps)))
  | (_, Some(z)) => Some(z)
  | (_, None) => row == 0 ? None : Some(cmap |> end_of_row(row - 1))
  };

let move_right =
    (((row, col), (pos, rev_steps)): binding, cmap): option(binding) =>
  switch (pos, cmap |> find_after_within_row((row, col))) {
  | (OnText(j), Some((_, (CursorPosition.OnText(_), rev_steps_after))))
      when rev_steps === rev_steps_after || rev_steps == rev_steps_after =>
    Some(((row, col + 1), (OnText(j + 1), rev_steps)))
  | (_, Some(z)) => Some(z)
  | (_, None) =>
    row == num_rows(cmap) - 1 ? None : Some(cmap |> start_of_row(row + 1))
  };

let move_sol = (row, cmap): binding => cmap |> start_of_row(row);

let move_eol = (row, cmap): binding => cmap |> end_of_row(row);
