open Util;
open OptUtil.Syntax;

/* The dynamic cursor points to a stage in evaluation, associated
 * with probe sample collection. This is primarily reified as a call stack,
 * represented as a list of ids of function application forms which have
 * been called but have not yet returned. The cursor also contains ancilliary
 * information used for navigating between and filtering probe samples */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  stack: Probe.call_stack,
  index: int,
  pinned_stack: option(Probe.call_stack),
  indicated_call: option(Id.t),
  time: option(float),
  iter: int,
};

let init: t = {
  stack: [],
  index: (-1),
  pinned_stack: None,
  indicated_call: None,
  time: None,
  iter: 0,
};

let trimmed_stack = (dyn_cursor: t) =>
  ListUtil.slice(0, dyn_cursor.index + 1, dyn_cursor.stack |> List.rev)
  |> List.rev;

/* If the dynamic cursor is on a call, and the provided
 * call stack is downstream of that call, return how many
 * aps downstream it is */
let depth_in_indicated_calls_stack =
    (dyn_cursor: t, call_stack: Probe.call_stack): option(int) => {
  open OptUtil.Syntax;
  let* cur_ap = dyn_cursor.indicated_call;
  ListUtil.suffix_at_depth(
    [cur_ap] @ trimmed_stack(dyn_cursor),
    call_stack,
  );
};

type relative_level =
  | Above(int)
  | Below(int)
  | Same
  | Unrelated;

/* How is the current sample related to the dynamic cursor? */
type relation = {
  /* Is the current sample at the dynamic cursor? */
  is_call_cursor: bool,
  is_more_precise_than_cursor: bool,
  relative_level_to_cursor: relative_level,
  /* Is the current sample at a call directly above the dynamic cursor? */
  is_call_above_call_cursor: option(int),
  /* Is the current sample below the dynamic cursor, and if so, by how much? */
  is_below_indicated_call: option(int),
  /* Is the current sample a call directly below the dynamic cursor, and if so, by how much? */
  is_before_cursor: int,
};

let is_below = ListUtil.suffix_at_depth;

let relative_level = (cs1, cs2): relative_level =>
  switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
  | (Some(0), Some(0)) => Same
  | (Some(n), None) => Below(n)
  | (None, Some(n)) => Above(n)
  | (_, _) => Unrelated
  };

let cur_call = (ap_id: option(Id.t), sample: Sample.t) => {
  let* ap_id = ap_id;
  let dyn = sample.call_stack;
  Some([ap_id, ...dyn]);
};

let cur_ap = (info: option(Info.t)) =>
  switch (info) {
  | Some(
      InfoExp({term: {term: Ap(_, {term: Constructor(_), _}, _), _}, _}),
    )
  | Some(
      InfoExp({
        term:
          {
            term: Probe({term: Ap(_, {term: Constructor(_), _}, _), _}, _),
            _,
          },
        _,
      }),
    ) => Option.None
  | Some(InfoExp({term: {term: Ap(_), _} as ap, _}))
  | Some(InfoExp({term: {term: Probe({term: Ap(_), _} as ap, _), _}, _})) =>
    Some(Exp.rep_id(ap))
  | _ => None
  };

let relation =
    (~trimmed: bool, ~ap_id: option(Id.t), dyn_cursor: t, sample: Sample.t)
    : relation => {
  let this = sample.call_stack;
  let cursor = trimmed ? trimmed_stack(dyn_cursor) : dyn_cursor.stack;
  {
    is_call_cursor: cursor == this,
    is_more_precise_than_cursor:
      List.length(dyn_cursor.stack) > List.length(sample.call_stack),
    relative_level_to_cursor: relative_level(cursor, this),
    is_call_above_call_cursor: {
      let* cur_call = cur_call(ap_id, sample);
      is_below(cur_call, cursor);
    },
    is_below_indicated_call: {
      let* cur_ap = dyn_cursor.indicated_call;
      is_below([cur_ap] @ cursor, this);
    },
    is_before_cursor: sample.iter - dyn_cursor.iter,
  };
};

let is_related = relation =>
  switch (relation.relative_level_to_cursor) {
  | Above(_)
  | Below(_) => true
  | Same => true
  | Unrelated => false
  };
