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

let height = (m: t) =>
  m.metrics |> List.map(box => box.height) |> List.fold_left((+), 0);

let width = (m: t) =>
  m.metrics |> List.map(box => box.width) |> List.fold_left(max, 0);

module Position = {
  type m = t;
  type t = {
    row: int,
    col: int,
  };
  let compare = (pos1, pos2) =>
    if (pos1.row < pos2.row) {
      (-1);
    } else if (pos1.row > pos2.row) {
      1;
    } else {
      Int.compare(pos1.col, pos2.col);
    };

  let prev = (m: m, {row, col}: t): t => {
    let (first, trailing) = ListUtil.split_first(m.metrics);
    let total_height =
      trailing
      |> List.map(box => box.height)
      |> List.fold_left((+), first.height);
    let updated_row = row - total_height + 1;
    let updated_col =
      switch (trailing) {
      | [] => col - first.width
      | [_, ..._] => - first.width
      };
    {row: updated_row, col: updated_col};
  };

  let next = ({row, col}: t, m: m): t => {
    let (leading, last) = ListUtil.split_last(m.metrics);
    let total_height =
      leading
      |> List.map(box => box.height)
      |> List.fold_left((+), last.height);
    let updated_row = row + total_height - 1;
    let updated_col =
      switch (leading) {
      | [] => col + last.width
      | [_, ..._] => last.width
      };
    {row: updated_row, col: updated_col};
  };
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

let table: WeakMap.t(UHLayout.t, t) = WeakMap.mk();
let rec mk = (l: UHLayout.t): t => {
  switch (WeakMap.get(table, l)) {
  | Some(m) => m
  | None =>
    let m =
      switch (l) {
      | Linebreak =>
        let box = {height: 1, width: 0};
        {metrics: [box, box], layout: Linebreak};
      | Text(s) => {
          metrics: [{height: 1, width: StringUtil.utf8_length(s)}],
          layout: Text(s),
        }
      | Align(l) =>
        let m = mk(l);
        let bounding_box =
          m.metrics
          |> List.fold_left(
               ({height: bh, width: bw}, {height, width}) =>
                 {height: bh + height, width: max(bw, width)},
               {height: 0, width: 0},
             );
        {metrics: [bounding_box], layout: Align(m)};
      | Cat(l1, l2) =>
        let m1 = mk(l1);
        let m2 = mk(l2);
        let (leading, last) = ListUtil.split_last(m1.metrics);
        let (first, trailing) = ListUtil.split_first(m2.metrics);
        let mid_box = {
          height: max(last.height, first.height),
          width: last.width + first.width,
        };
        {metrics: leading @ [mid_box, ...trailing], layout: Cat(m1, m2)};
      | Annot(annot, l) =>
        let m = mk(l);
        {...m, layout: Annot(annot, m)};
      };
    ignore(WeakMap.set(table, l, m));
    m;
  };
};

/**
 * `first_path_in_first_row(rev_steps, m)` returns the first path encountered
 * during left-to-right traversal of the first row of `m`, paired with
 * how many columns the found path is to the right of the start of `m`.
 */
let first_path_in_first_row =
    (rev_steps, m: t): option((CursorPath_common.rev_t, int)) => {
  let rec go = (rev_steps, offset: int, m) =>
    switch (m.layout) {
    | Linebreak
    | Text(_) => None
    | Align(m) => go(rev_steps, offset, m)
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) => go([step, ...rev_steps], offset, m)
      | Token({shape, _}) =>
        let cursor: CursorPosition.t =
          switch (shape) {
          | Text => OnText(0)
          | Op => OnOp(Before)
          | Delim(k) => OnDelim(k, Before)
          };
        Some(((cursor, rev_steps), offset));
      | _ => go(rev_steps, offset, m)
      }
    | Cat(m1, m2) =>
      switch (go(rev_steps, offset, m1)) {
      | Some(result) => Some(result)
      | None =>
        height(m1) == 1 ? go(rev_steps, offset + width(m1), m2) : None
      }
    };
  go(rev_steps, 0, m);
};

/**
 * `last_path_in_last_row(m)` returns the first path encountered
 * during right-to-left traversal of the last row of `m`, paired with
 * how many columns the found path is to the left of the end of `m`.
 */
let last_path_in_last_row =
    (rev_steps, m: t): option((CursorPath_common.rev_t, int)) => {
  let rec go = (rev_steps, offset: int, m) =>
    switch (m.layout) {
    | Linebreak
    | Text(_) => None
    | Align(m) => go(rev_steps, offset, m)
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) => go([step, ...rev_steps], offset, m)
      | Token({shape, len, _}) =>
        let cursor: CursorPosition.t =
          switch (shape) {
          | Text => OnText(len)
          | Op => OnOp(After)
          | Delim(k) => OnDelim(k, After)
          };
        Some(((cursor, rev_steps), offset));
      | _ => go(rev_steps, offset, m)
      }
    | Cat(m1, m2) =>
      switch (go(rev_steps, offset, m2)) {
      | Some(result) => Some(result)
      | None =>
        height(m2) == 1 ? go(rev_steps, offset + width(m2), m1) : None
      }
    };
  go(rev_steps, 0, m);
};

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
 * `prev_path_in_row(from, m) returns the next encountered path in a
 * right-to-left traversal of row `from.row` starting at (but not including)
 * `from.col`, paired with the position of the found path.
 */
let prev_path_in_row =
    (from: Position.t, m: t): option((CursorPath_common.rev_t, Position.t)) => {
  let rec go =
          (rev_steps: CursorPath_common.rev_steps, start: Position.t, m: t)
          : option((CursorPath_common.rev_t, Position.t)) => {
    let end_ = Position.next(start, m);
    if (Position.compare(from, start) < 0 || Position.compare(from, end_) > 0) {
      None;
    } else {
      switch (m.layout) {
      | Linebreak
      | Text(_) => None
      | Annot(annot, m) =>
        switch (annot) {
        | Step(step) => go([step, ...rev_steps], start, m)
        | Token({shape, len, _}) =>
          assert(start.row == end_.row);
          let from_start = from.col - start.col;
          if (from_start >= len) {
            None;
          } else {
            let (cursor: CursorPosition.t, offset) =
              switch (shape) {
              | Text => (OnText(from_start - 1), 1)
              | Op => (OnOp(Before), len)
              | Delim(k) => (OnDelim(k, Before), len)
              };
            Some(((cursor, rev_steps), {...from, col: from.col - offset}));
          };
        | _ => go(rev_steps, start, m)
        }
      | Align(m) => go(rev_steps, start, m)
      | Cat(m1, m2) =>
        let mid = Position.next(start, m);
        if (Position.compare(from, mid) <= 0) {
          go(rev_steps, start, m1);
        } else {
          switch (go(rev_steps, mid, m2)) {
          | None =>
            if (mid.row == end_.row) {
              last_path_in_last_row(rev_steps, m1)
              |> Option.map(((rev_path, offset)) =>
                   (rev_path, {...mid, col: mid.col - offset})
                 );
            } else {
              None;
            }
          | Some((rev_path2, prev_pos)) =>
            let rev_path =
              switch (last_path_in_last_row(rev_steps, m1)) {
              | Some((rev_path1, 0))
                  when Position.compare(prev_pos, mid) == 0 =>
                arbitrate(rev_path1, rev_path2)
              | _ => rev_path2
              };
            Some((rev_path, prev_pos));
          };
        };
      };
    };
  };
  go([], {row: 0, col: 0}, m);
};

/**
 * `next_path_in_row(from, m) returns the next encountered path in a
 * left-to-right traversal of row `from.row` starting at (but not including)
 * `from.col`, paired with the position of the found path.
 */
let next_path_in_row =
    (from: Position.t, m: t): option((CursorPath_common.rev_t, Position.t)) => {
  let rec go =
          (rev_steps: CursorPath_common.rev_steps, start: Position.t, m: t)
          : option((CursorPath_common.rev_t, Position.t)) => {
    let end_ = Position.next(start, m);
    if (Position.compare(from, start) < 0 || Position.compare(from, end_) > 0) {
      None;
    } else {
      switch (m.layout) {
      | Linebreak
      | Text(_) => None
      | Annot(annot, m) =>
        switch (annot) {
        | Step(step) => go([step, ...rev_steps], start, m)
        | Token({shape, len, _}) =>
          assert(start.row == end_.row);
          let from_start = from.col - start.col;
          if (from_start >= len) {
            None;
          } else {
            let (cursor: CursorPosition.t, offset) =
              switch (shape) {
              | Text => (OnText(from_start + 1), 1)
              | Op => (OnOp(After), len)
              | Delim(k) => (OnDelim(k, After), len)
              };
            Some(((cursor, rev_steps), {...from, col: from.col + offset}));
          };
        | _ => go(rev_steps, start, m)
        }
      | Align(m) => go(rev_steps, start, m)
      | Cat(m1, m2) =>
        let mid = Position.next(start, m);
        if (Position.compare(from, mid) >= 0) {
          go(rev_steps, mid, m2);
        } else {
          switch (go(rev_steps, start, m1)) {
          | None =>
            if (mid.row == start.row) {
              first_path_in_first_row(rev_steps, m2)
              |> Option.map(((rev_path, offset)) =>
                   (rev_path, {...mid, col: mid.col + offset})
                 );
            } else {
              None;
            }
          | Some((rev_path1, next_pos)) =>
            let rev_path =
              switch (first_path_in_first_row(rev_steps, m2)) {
              | Some((rev_path2, 0))
                  when Position.compare(next_pos, mid) == 0 =>
                arbitrate(rev_path1, rev_path2)
              | _ => rev_path1
              };
            Some((rev_path, next_pos));
          };
        };
      };
    };
  };
  go([], {row: 0, col: 0}, m);
};

let nearest_path_within_row =
    (target: Position.t, m: t): option(CursorPath_common.rev_t) => {
  let rec go = (rev_steps, start: Position.t, m) => {
    let end_ = Position.next(start, m);
    if (Position.compare(target, start) < 0
        || Position.compare(target, end_) > 0) {
      None;
    } else {
      switch (m.layout) {
      | Linebreak
      | Text(_) => None
      | Align(m) => go(rev_steps, start, m)
      | Annot(annot, m) =>
        switch (annot) {
        | Step(step) => go([step, ...rev_steps], start, m)
        | Token({shape, len, _}) =>
          assert(start.row == end_.row);
          let from_start = target.col - start.col;
          let is_left = from_start + from_start <= len;
          let cursor: CursorPosition.t =
            switch (shape) {
            | Text => OnText(from_start)
            | Op => OnOp(is_left ? Before : After)
            | Delim(k) => OnDelim(k, is_left ? Before : After)
            };
          Some((cursor, rev_steps));
        | _ => go(rev_steps, start, m)
        }
      | Cat(m1, m2) =>
        let mid = Position.next(start, m);
        if (target.row < mid.row) {
          go(rev_steps, start, m1);
        } else if (target.row > mid.row) {
          go(rev_steps, mid, m2);
        } else if (target.col < mid.col) {
          switch (go(rev_steps, start, m1)) {
          | Some(rev_path) => Some(rev_path)
          | None => first_path_in_first_row(rev_steps, m2) |> Option.map(fst)
          };
        } else if (target.col > mid.col) {
          switch (go(rev_steps, mid, m2)) {
          | Some(rev_path) => Some(rev_path)
          | None => last_path_in_last_row(rev_steps, m1) |> Option.map(fst)
          };
        } else {
          // Target is between m1 and m2.
          // Check both sides for path and arbitrate if needed.
          switch (
            last_path_in_last_row(rev_steps, m1),
            first_path_in_first_row(rev_steps, m2),
          ) {
          | (None, None) => None
          | (Some((rev_path, _)), None)
          | (None, Some((rev_path, _))) => Some(rev_path)
          | (Some((rev_path1, offset1)), Some((rev_path2, offset2))) =>
            if (offset1 < offset2) {
              Some(rev_path2);
            } else if (offset1 > offset2) {
              Some(rev_path1);
            } else {
              offset1 == 0
                ? Some(arbitrate(rev_path1, rev_path2)) : Some(rev_path1);
            }
          };
        };
      };
    };
  };
  go([], {row: 0, col: 0}, m);
};
