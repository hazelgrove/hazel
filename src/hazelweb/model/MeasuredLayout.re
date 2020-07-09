open Sexplib.Std;

[@deriving sexp]
type box = {
  height: int,
  width: int,
};

[@deriving sexp]
type t = {
  layout: t',
  metrics: list(box),
}
and t' =
  | Linebreak
  | Text(string)
  | Align(t)
  | Cat(t, t)
  | Annot(UHAnnot.t, t);

type with_splices = (t, SpliceMap.t(t));

let text = (s: string): t => {
  let box = {height: 1, width: StringUtil.utf8_length(s)};
  {metrics: [box], layout: Text(s)};
};
let empty = text("");

let linebreak: t = {
  let box = {height: 1, width: 0};
  {metrics: [box, box], layout: Linebreak};
};

let cat = (m1: t, m2: t): t => {
  let (leading, last) = ListUtil.split_last(m1.metrics);
  let (first, trailing) = ListUtil.split_first(m2.metrics);
  let mid_box = {
    height: max(last.height, first.height),
    width: last.width + first.width,
  };
  {metrics: leading @ [mid_box, ...trailing], layout: Cat(m1, m2)};
};

let align = (m: t): t => {
  let bounding_box =
    m.metrics
    |> List.fold_left(
         ({height: bh, width: bw}, {height, width}) =>
           {height: bh + height, width: max(bw, width)},
         {height: 0, width: 0},
       );
  {metrics: [bounding_box], layout: Align(m)};
};

let height = (m: t) =>
  m.metrics |> List.map(box => box.height) |> List.fold_left((+), 0);

let width = (m: t) =>
  m.metrics |> List.map(box => box.width) |> List.fold_left(max, 0);

let next_position =
    (~indent: int, {row, col}: CaretPosition.t, m: t): CaretPosition.t => {
  let updated_row = row + height(m) - 1;
  let updated_col = {
    let (leading, last) = ListUtil.split_last(m.metrics);
    last.width
    + (
      switch (leading) {
      | [] => col
      | [_, ..._] => indent
      }
    );
  };
  {row: updated_row, col: updated_col};
};

// flattens away Linebreak and Cat nodes
let flatten = (m: t): list(list(t)) => {
  let rec go = (~tail: list(list(t)), m: t): list(list(t)) => {
    switch (m.layout) {
    | Text(_)
    | Align(_)
    | Annot(_) =>
      switch (tail) {
      | [] => [[m]]
      | [row, ...rows] => [[m, ...row], ...rows]
      }
    | Linebreak => [[], ...tail]
    | Cat(m1, m2) => go(~tail=go(~tail, m2), m1)
    };
  };
  go(~tail=[], m);
};

// peels off top level annotations then flattens
let rec peel_and_flatten = (m: t): list(list(t)) => {
  switch (m.layout) {
  | Annot(_annot, m) => peel_and_flatten(m)
  | _ => flatten(m)
  };
};

let table: WeakMap.t(UHLayout.t, t) = WeakMap.mk();
let rec mk = (l: UHLayout.t): t =>
  switch (WeakMap.get(table, l)) {
  | Some(m) => m
  | None =>
    let m =
      switch (l) {
      | Linebreak => linebreak
      | Text(s) => text(s)
      | Align(l) => align(mk(l))
      | Cat(l1, l2) =>
        let m1 = mk(l1);
        let m2 = mk(l2);
        cat(m1, m2);
      | Annot(annot, l) =>
        let m =
          switch (annot) {
          | LivelitView({shape: Inline(width), _}) =>
            text(StringUtil.replicat(width, UnicodeConstants.nbsp))
          | LivelitView({shape: MultiLine(height), _}) =>
            ListUtil.replicate(height - 1, linebreak)
            |> List.fold_left(cat, empty)
          | _ => mk(l)
          };
        {...m, layout: Annot(annot, m)};
      };
    ignore(WeakMap.set(table, l, m));
    m;
  };

let caret_position_of_path =
    ((steps, cursor): CursorPath_common.t, (m, splice_ms): with_splices)
    : option((CaretPosition.t, option((MetaVar.t, SpliceName.t)))) => {
  let rec go =
          (
            ~splice: option((MetaVar.t, SpliceName.t))=?,
            ~indent: int=0,
            ~start: CaretPosition.t={row: 0, col: 0},
            steps,
            m,
          ) => {
    switch (m.layout) {
    | Linebreak
    | Text(_) => None
    | Align(m) => go(~splice?, ~indent=start.col, ~start, steps, m)
    | Annot(annot, m) =>
      switch (steps, annot) {
      | ([step, ...steps], Step(step')) =>
        step == step' ? go(~splice?, ~indent, ~start, steps, m) : None
      | ([step, splice_name, ...steps], LivelitView({llu, hd_step, _}))
          when step == hd_step =>
        let splice_m = SpliceMap.get_splice(llu, splice_name, splice_ms);
        go(~splice=(llu, splice_name), steps, splice_m);
      | ([], Step(_) | LivelitView(_)) => None
      | ([], Token({shape, len, _})) =>
        switch (cursor, shape) {
        | (OnText(j), Text({start_index}))
            when start_index <= j && j <= start_index + len =>
          Some(({...start, col: start.col + (j - start_index)}, splice))
        | (OnOp(Before), Op) => Some((start, splice))
        | (OnOp(After), Op) =>
          Some(({...start, col: start.col + len}, splice))
        | (OnDelim(k, side), Delim(k')) when k == k' =>
          Some((
            switch (side) {
            | Before => start
            | After => {...start, col: start.col + len}
            },
            splice,
          ))
        | _ => None
        }
      | _ => go(~splice?, ~indent, ~start, steps, m)
      }
    | Cat(m1, m2) =>
      switch (go(~splice?, ~indent, ~start, steps, m1)) {
      | Some(pos) => Some(pos)
      | None =>
        let mid = next_position(~indent, start, m1);
        go(~splice?, ~indent, ~start=mid, steps, m2);
      }
    };
  };
  go(steps, m);
};

let find_path =
    (
      ~rev_steps: CursorPath_common.rev_steps=[],
      ~start: CaretPosition.t={row: 0, col: 0},
      ~indent: int=0,
      ~token:
         (
           ~rev_steps: CursorPath_common.rev_steps,
           ~start: CaretPosition.t,
           UHAnnot.token_data
         ) =>
         option('a),
      ~cat:
         (
           ~go: (CaretPosition.t, t) => option('a),
           ~rev_steps: CursorPath_common.rev_steps,
           ~start: CaretPosition.t,
           ~mid: CaretPosition.t,
           ~end_: CaretPosition.t,
           t,
           t
         ) =>
         option('a),
      m: t,
    )
    : option('a) => {
  let rec go = (rev_steps, indent, start: CaretPosition.t, m) =>
    switch (m.layout) {
    | Linebreak
    | Text(_) => None
    | Align(m) => go(rev_steps, start.col, start, m)
    | Cat(m1, m2) =>
      let mid = next_position(~indent, start, m1);
      let end_ = next_position(~indent, mid, m2);
      cat(
        ~go=go(rev_steps, indent),
        ~rev_steps,
        ~start,
        ~mid,
        ~end_,
        m1,
        m2,
      );
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) => go([step, ...rev_steps], indent, start, m)
      | Token(token_data) => token(~rev_steps, ~start, token_data)
      | _ => go(rev_steps, indent, start, m)
      }
    };
  go(rev_steps, indent, start, m);
};

/**
 * `first_path_in_row(row, m)` returns the first path encountered
 * during left-to-right traversal of row `row` of `m`, paired with
 * the found path's position.
 */
let first_path_in_row =
    (
      ~rev_steps: CursorPath_common.rev_steps=[],
      ~indent=0,
      ~start: CaretPosition.t={row: 0, col: 0},
      row: int,
      m: t,
    )
    : option((CursorPath_common.rev_t, CaretPosition.t)) =>
  m
  |> find_path(
       ~rev_steps,
       ~indent,
       ~start,
       ~token=
         (~rev_steps, ~start, token_data) => {
           let UHAnnot.{shape, _} = token_data;
           let cursor: CursorPosition.t =
             switch (shape) {
             | Text({start_index}) => OnText(start_index)
             | Op => OnOp(Before)
             | Delim(k) => OnDelim(k, Before)
             };
           Some(((cursor, rev_steps), start));
         },
       ~cat=
         (~go, ~rev_steps as _, ~start, ~mid, ~end_ as _, m1, m2) =>
           if (row < mid.row) {
             go(start, m1);
           } else if (row > mid.row) {
             go(mid, m2);
           } else {
             switch (go(start, m1)) {
             | Some(result) => Some(result)
             | None => go(mid, m2)
             };
           },
     );

/**
 * `last_path_in_row(m)` returns the first path encountered
 * during right-to-left traversal of row `row` of `m`, paired
 * with the found path's position.
 */
let last_path_in_row =
    (~rev_steps=[], ~start: CaretPosition.t={row: 0, col: 0}, row: int, m: t)
    : option((CursorPath_common.rev_t, CaretPosition.t)) =>
  m
  |> find_path(
       ~rev_steps,
       ~start,
       ~token=
         (~rev_steps, ~start, token_data) => {
           let UHAnnot.{shape, len, _} = token_data;
           let cursor: CursorPosition.t =
             switch (shape) {
             | Text({start_index}) => OnText(start_index + len)
             | Op => OnOp(After)
             | Delim(k) => OnDelim(k, After)
             };
           Some(((cursor, rev_steps), {...start, col: start.col + len}));
         },
       ~cat=
         (~go, ~rev_steps as _, ~start, ~mid, ~end_ as _, m1, m2) =>
           if (row < mid.row) {
             go(start, m1);
           } else if (row > mid.row) {
             go(mid, m2);
           } else {
             switch (go(mid, m2)) {
             | Some(result) => Some(result)
             | None => go(start, m1)
             };
           },
     );

let arbitrate =
    (
      (pos1, rev_steps1) as rev_path1: CursorPath_common.rev_t,
      (pos2, rev_steps2) as rev_path2: CursorPath_common.rev_t,
    ) => {
  let n1 = List.length(rev_steps1);
  let n2 = List.length(rev_steps2);
  if (n1 > n2) {
    rev_path1;
  } else if (n1 < n2) {
    rev_path2;
  } else {
    switch (pos1, pos2) {
    | (OnText(_), OnText(_))
    | (OnDelim(_), OnDelim(_))
    | (OnOp(_), OnOp(_)) =>
      // break tie in favor of left side
      rev_path1
    | (OnText(_), _) => rev_path1
    | (_, OnText(_)) => rev_path2
    | (OnDelim(_), _) => rev_path1
    | (_, OnDelim(_)) => rev_path2
    };
  };
};

/**
 * `prev_path_within_row(from, m) returns the next encountered path in a
 * right-to-left traversal of row `from.row` starting at (but not including)
 * `from.col`. Returned path is paired with its position.
 */
let prev_path_within_row =
    (from: CaretPosition.t, m: t)
    : option((CursorPath_common.rev_t, CaretPosition.t)) =>
  m
  |> find_path(
       ~token=
         (~rev_steps, ~start, token_data) => {
           let UHAnnot.{shape, len, _} = token_data;
           let from_start = from.col - start.col;
           if (from_start <= 0) {
             None;
           } else {
             let (cursor: CursorPosition.t, offset) =
               switch (shape) {
               | Text({start_index}) => (
                   OnText(start_index + from_start - 1),
                   1,
                 )
               | Op => (OnOp(Before), len)
               | Delim(k) => (OnDelim(k, Before), len)
               };
             Some(((cursor, rev_steps), {...from, col: from.col - offset}));
           };
         },
       ~cat=
         (~go, ~rev_steps, ~start, ~mid, ~end_, m1, m2) =>
           if (CaretPosition.compare(from, mid) <= 0) {
             go(start, m1);
           } else {
             switch (go(mid, m2)) {
             | None =>
               mid.row == end_.row
                 ? last_path_in_row(~rev_steps, ~start, mid.row, m1) : None
             | Some((rev_path2, pos2)) =>
               if (CaretPosition.compare(pos2, mid) == 0) {
                 let rev_path =
                   switch (last_path_in_row(~rev_steps, ~start, mid.row, m1)) {
                   | Some((rev_path1, pos1))
                       when CaretPosition.compare(pos1, mid) == 0 =>
                     arbitrate(rev_path1, rev_path2)
                   | _ => rev_path2
                   };
                 Some((rev_path, mid));
               } else {
                 Some((rev_path2, pos2));
               }
             };
           },
     );

/**
 * `next_path_within_row(from, m) returns the next encountered path in a
 * left-to-right traversal of row `from.row` starting at (but not including)
 * `from.col`. Returned path is paired with its position.
 */
let next_path_within_row =
    (from: CaretPosition.t, m: t)
    : option((CursorPath_common.rev_t, CaretPosition.t)) =>
  m
  |> find_path(
       ~token=
         (~rev_steps, ~start, token_data) => {
           let UHAnnot.{shape, len, _} = token_data;
           let from_start = from.col - start.col;
           if (from_start >= len) {
             None;
           } else {
             let (cursor: CursorPosition.t, offset) =
               switch (shape) {
               | Text({start_index}) => (
                   OnText(start_index + from_start + 1),
                   1,
                 )
               | Op => (OnOp(After), len)
               | Delim(k) => (OnDelim(k, After), len)
               };
             Some(((cursor, rev_steps), {...from, col: from.col + offset}));
           };
         },
       ~cat=
         (~go, ~rev_steps, ~start, ~mid, ~end_ as _, m1, m2) =>
           if (CaretPosition.compare(from, mid) >= 0) {
             go(mid, m2);
           } else {
             switch (go(start, m1)) {
             | None =>
               mid.row == start.row
                 ? first_path_in_row(~rev_steps, ~start=mid, mid.row, m2)
                 : None
             | Some((rev_path1, pos1)) =>
               if (CaretPosition.compare(pos1, mid) == 0) {
                 let rev_path =
                   switch (
                     first_path_in_row(~rev_steps, ~start=mid, mid.row, m2)
                   ) {
                   | Some((rev_path2, pos2))
                       when CaretPosition.compare(pos2, mid) == 0 =>
                     arbitrate(rev_path1, rev_path2)
                   | _ => rev_path1
                   };
                 Some((rev_path, mid));
               } else {
                 Some((rev_path1, pos1));
               }
             };
           },
     );

let nearest_path_within_row =
    (target: CaretPosition.t, m: t)
    : option((CursorPath_common.rev_t, CaretPosition.t)) =>
  m
  |> find_path(
       ~token=
         (~rev_steps, ~start, token_data) => {
           let UHAnnot.{shape, len, _} = token_data;
           let from_start = target.col - start.col;
           let is_left = from_start + from_start <= len;
           let (cursor: CursorPosition.t, offset) =
             switch (shape) {
             | Text({start_index}) => (
                 OnText(start_index + from_start),
                 from_start,
               )
             | Op => is_left ? (OnOp(Before), 0) : (OnOp(After), len)
             | Delim(k) =>
               is_left ? (OnDelim(k, Before), 0) : (OnDelim(k, After), len)
             };
           Some(((cursor, rev_steps), {...start, col: start.col + offset}));
         },
       ~cat=
         (~go, ~rev_steps, ~start, ~mid, ~end_ as _, m1, m2) =>
           if (target.row < mid.row) {
             go(start, m1);
           } else if (target.row > mid.row) {
             go(mid, m2);
           } else if (target.col < mid.col) {
             switch (go(start, m1)) {
             | Some(rev_path) => Some(rev_path)
             | None => first_path_in_row(~rev_steps, ~start=mid, mid.row, m2)
             };
           } else if (target.col > mid.col) {
             switch (go(mid, m2)) {
             | Some(rev_path) => Some(rev_path)
             | None => last_path_in_row(~rev_steps, ~start, mid.row, m1)
             };
           } else {
             // Target is between m1 and m2.
             // Check both sides for path and arbitrate if needed.
             switch (
               last_path_in_row(~rev_steps, ~start, mid.row, m1),
               first_path_in_row(~rev_steps, ~start=mid, mid.row, m2),
             ) {
             | (None, None) => None
             | (Some((rev_path, pos)), None)
             | (None, Some((rev_path, pos))) => Some((rev_path, pos))
             | (Some((rev_path1, pos1)), Some((rev_path2, pos2))) =>
               let offset1 = mid.col - pos1.col;
               let offset2 = pos2.col - mid.col;
               if (offset1 < offset2) {
                 Some((rev_path1, pos1));
               } else if (offset1 > offset2) {
                 Some((rev_path2, pos2));
               } else {
                 offset1 == 0
                   ? Some((arbitrate(rev_path1, rev_path2), mid))
                   : Some((rev_path1, pos1));
               };
             };
           },
     );
