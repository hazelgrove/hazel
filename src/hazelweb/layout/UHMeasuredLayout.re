module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

[@deriving sexp]
type t = MeasuredLayout.t(UHAnnot.t);
type with_offset = MeasuredLayout.with_offset(UHAnnot.t);
include MeasuredLayout.Make(WeakMap);

let caret_position_of_path =
    ((steps, cursor): CursorPath_common.t, m: t): option(MeasuredPosition.t) => {
  let rec go = (steps, indent: int, start: MeasuredPosition.t, m: t) =>
    switch (m.layout) {
    | Linebreak
    | Text(_) => None
    | Align(m) => go(steps, start.col, start, m)
    | Annot(annot, m) =>
      switch (steps, annot) {
      | ([], Step(_)) => None
      | ([step, ...steps], Step(step')) =>
        step == step' ? go(steps, indent, start, m) : None
      | ([], Token({shape, len, _})) =>
        switch (cursor, shape) {
        | (OnText(j), Text) => Some({...start, col: start.col + j})
        | (OnOp(Before), Op) => Some(start)
        | (OnOp(After), Op) => Some({...start, col: start.col + len})
        | (OnDelim(k, side), Delim(k')) when k == k' =>
          Some(
            switch (side) {
            | Before => start
            | After => {...start, col: start.col + len}
            },
          )
        | _ => None
        }
      | _ => go(steps, indent, start, m)
      }
    | Cat(m1, m2) =>
      switch (go(steps, indent, start, m1)) {
      | Some(pos) => Some(pos)
      | None =>
        let mid = MeasuredLayout.next_position(~indent, start, m1);
        go(steps, indent, mid, m2);
      }
    };
  go(steps, 0, MeasuredPosition.zero, m);
};

type path_position = (CursorPath_common.rev_t, MeasuredPosition.t);

/**
 * `find_path(~token, ~cat, m)` returns the path associated with
 * the first `Token` annotation that passes the `token` check in
 * the top-down traversal directed by `cat`. For each `Cat(m1, m2)`
 * node encountered, the function `cat` specifies how to continue
 * the traversal based on measured positions of the endpoints of
 * `m1` and `m2`.
 */
let find_path =
    (
      ~rev_steps: CursorPath_common.rev_steps=[],
      ~start: MeasuredPosition.t=MeasuredPosition.zero,
      ~indent: int=0,
      ~token:
         (
           ~rev_steps: CursorPath_common.rev_steps,
           ~start: MeasuredPosition.t,
           UHAnnot.token_data
         ) =>
         option(path_position),
      ~cat:
         (
           ~go: (MeasuredPosition.t, t) => option('a),
           ~rev_steps: CursorPath_common.rev_steps,
           ~start: MeasuredPosition.t,
           ~mid: MeasuredPosition.t,
           ~end_: MeasuredPosition.t,
           t,
           t
         ) =>
         option(path_position),
      m: t,
    )
    : option(path_position) => {
  let rec go = (rev_steps, indent, start: MeasuredPosition.t, m: t) =>
    switch (m.layout) {
    | Linebreak
    | Text(_) => None
    | Align(m) => go(rev_steps, start.col, start, m)
    | Cat(m1, m2) =>
      let mid = MeasuredLayout.next_position(~indent, start, m1);
      let end_ = MeasuredLayout.next_position(~indent, mid, m2);
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

let first_path_in_row =
    (
      ~rev_steps: CursorPath_common.rev_steps=[],
      ~indent=0,
      ~start: MeasuredPosition.t=MeasuredPosition.zero,
      row: int,
      m: t,
    )
    : option(path_position) => {
  let end_ = MeasuredLayout.next_position(~indent, start, m);
  if (row < start.row || row > end_.row) {
    None;
  } else {
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
               | Text => OnText(0)
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
  };
};

let last_path_in_row =
    (
      ~rev_steps=[],
      ~indent=0,
      ~start: MeasuredPosition.t=MeasuredPosition.zero,
      row: int,
      m: t,
    )
    : option(path_position) => {
  let end_ = MeasuredLayout.next_position(~indent, start, m);
  if (row < start.row || row > end_.row) {
    None;
  } else {
    m
    |> find_path(
         ~rev_steps,
         ~start,
         ~token=
           (~rev_steps, ~start, token_data) => {
             let UHAnnot.{shape, len, _} = token_data;
             let cursor: CursorPosition.t =
               switch (shape) {
               | Text => OnText(len)
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
  };
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

let prev_path_within_row =
    (from: MeasuredPosition.t, m: t): option(path_position) =>
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
               | Text => (OnText(from_start - 1), 1)
               | Op => (OnOp(Before), len)
               | Delim(k) => (OnDelim(k, Before), len)
               };
             Some(((cursor, rev_steps), {...from, col: from.col - offset}));
           };
         },
       ~cat=
         (~go, ~rev_steps, ~start, ~mid, ~end_ as _, m1, m2) =>
           if (MeasuredPosition.compare(from, mid) <= 0) {
             go(start, m1);
           } else {
             switch (go(mid, m2)) {
             | None => last_path_in_row(~rev_steps, ~start, from.row, m1)
             | Some((rev_path2, pos2)) =>
               if (MeasuredPosition.compare(pos2, mid) == 0) {
                 let rev_path =
                   switch (last_path_in_row(~rev_steps, ~start, mid.row, m1)) {
                   | Some((rev_path1, pos1))
                       when MeasuredPosition.compare(pos1, mid) == 0 =>
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

let next_path_within_row =
    (from: MeasuredPosition.t, m: t): option(path_position) =>
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
               | Text => (OnText(from_start + 1), 1)
               | Op => (OnOp(After), len)
               | Delim(k) => (OnDelim(k, After), len)
               };
             Some(((cursor, rev_steps), {...from, col: from.col + offset}));
           };
         },
       ~cat=
         (~go, ~rev_steps, ~start, ~mid, ~end_ as _, m1, m2) =>
           if (MeasuredPosition.compare(from, mid) >= 0) {
             go(mid, m2);
           } else {
             switch (go(start, m1)) {
             | None => first_path_in_row(~rev_steps, ~start=mid, from.row, m2)
             | Some((rev_path1, pos1)) =>
               if (MeasuredPosition.compare(pos1, mid) == 0) {
                 let rev_path =
                   switch (
                     first_path_in_row(~rev_steps, ~start=mid, mid.row, m2)
                   ) {
                   | Some((rev_path2, pos2))
                       when MeasuredPosition.compare(pos2, mid) == 0 =>
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
    (target: MeasuredPosition.t, m: t): option(path_position) =>
  m
  |> find_path(
       ~token=
         (~rev_steps, ~start, token_data) => {
           let UHAnnot.{shape, len, _} = token_data;
           let from_start = target.col - start.col;
           let is_left = from_start + from_start <= len;
           let (cursor: CursorPosition.t, offset) =
             switch (shape) {
             | Text =>
               let offset = min(from_start, len);
               (OnText(offset), offset);
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
