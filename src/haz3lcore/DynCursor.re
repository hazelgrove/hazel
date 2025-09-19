open Util;

open Language;

/* Manages shared state between probes */

[@deriving (show({with_path: false}), sexp, yojson)]
type closure = Dynamics.Probe.Closure.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type call_cursor = Dynamics.Cursor.call_cursor;

// type call_cursor = {
//   stack: Probe.call_stack,
//   index: int,
// };

// type t = {
//   mutable call_cursor,
//   mutable indicated_call: option(Id.t),
//   mutable pinned_call: option(Probe.call_stack),
// };

// let s: t = {
//   call_cursor: {
//     stack: [],
//     index: (-1),
//   },
//   indicated_call: None,
//   pinned_call: None,
// };

// let reset = () => {
//   s.call_cursor = {
//     stack: [],
//     index: (-1),
//   };
//   s.indicated_call = None;
//   s.pinned_call = None;
// };

let trimmed_stack = (call_cursor: call_cursor) =>
  ListUtil.slice(0, call_cursor.index + 1, call_cursor.stack |> List.rev)
  |> List.rev;

// let capture_cursor = (call_cursor: call_cursor, closure: closure): unit =>
//   // print_endline(
//   //   "capture_cursor: closure.call_Stack="
//   //   ++ String.concat(", ", List.map(Id.str3, closure.call_stack)),
//   // );
//   if (!ListUtil.is_suffix_of(closure.call_stack, call_cursor.stack)) {
//     call_cursor =
//       {
//         stack: closure.call_stack,
//         index: List.length(closure.call_stack) - 1,
//       };
//       // print_endline("case A: call_stack not suffix of call_cursor");
//       // print_endline("index is now: " ++ string_of_int(s.call_cursor.index));
//       // print_endline(
//       //   "call_cursor is now: "
//       //   ++ String.concat(", ", List.map(Id.str3, s.call_cursor.stack)),
//       // );
//       // print_endline(
//       //   "trimmed_stack is now: "
//       //   ++ String.concat(", ", List.map(Id.str3, trimmed_stack())),
//       // );
//   } else {
//     s.call_cursor =
//       {
//         stack: s.call_cursor.stack,
//         index: List.length(closure.call_stack) - 1,
//       };
//       // print_endline("case B: call_stack is suffix of call_cursor");
//       // print_endline(
//       //   "call_cursor is now: "
//       //   ++ String.concat(", ", List.map(Id.str3, s.call_cursor.stack)),
//       // );
//       // print_endline("index is now: " ++ string_of_int(s.call_cursor.index));
//       // print_endline(
//       //   "trimmed_stack is now: "
//       //   ++ String.concat(", ", List.map(Id.str3, trimmed_stack())),
//       // );
//   };
// // s.call_cursor = {
// //   stack:
// //     ListUtil.is_suffix_of(closure.call_stack, s.call_cursor.stack)
// //       ? s.call_cursor.stack : closure.call_stack,
// //   index: List.length(closure.call_stack) - 1,
// // };

// let capture_ap = (info: info): unit => {
//   s.indicated_call = cur_ap(info);
// };

// let capture = (info: info, closure: closure): unit => {
//   capture_cursor(closure);
//   capture_ap(info);
// };

let is_in = (di: Dynamics.Info.t): option(closure) =>
  //TODO(andrew): maybe should use call_cursor suffix trimmed to index?
  List.find_opt(
    (closure: closure) =>
      trimmed_stack(di.dyn_cursor.call_cursor) == closure.call_stack,
    di.closures,
  );

/* If the closure cursor is on a call, and the provided
 * call stack is downstream of that call, return how many
 * aps downstream it is */
let depth_in_indicated_calls_stack =
    (cc: Dynamics.Cursor.t, call_stack: Probe.call_stack): option(int) => {
  open OptUtil.Syntax;
  let* cur_ap = cc.indicated_call;
  ListUtil.suffix_at_depth(
    [cur_ap] @ trimmed_stack(cc.call_cursor),
    call_stack,
  );
};

type relative_level =
  | Above(int)
  | Below(int)
  | Same
  | Unrelated;

/* How is the current closure related to the closure cursor? */
type relation = {
  /* Is the current closure the call cursor? */
  is_call_cursor: bool,
  is_more_precise_than_cursor: bool,
  relative_level_to_cursor: relative_level,
  /* Is the current closure a call directly above the call cursor? */
  is_call_above_call_cursor: option(int),
  /* Is the current closure below the call cursor, and if so, by how much? */
  is_below_indicated_call: option(int),
  /* Is the current closure a call directly below the call cursor, and if so, by how much? */
};

let is_below = ListUtil.suffix_at_depth;

let relative_level = (cs1, cs2): relative_level =>
  switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
  | (Some(0), Some(0)) => Same
  | (Some(n), None) => Below(n)
  | (None, Some(n)) => Above(n)
  | (_, _) => Unrelated
  };

let cur_call = (ap_id: option(Id.t), closure: closure) => {
  open OptUtil.Syntax;
  let* ap_id = ap_id;
  let dyn = closure.call_stack;
  Some([ap_id, ...dyn]);
};

let relation =
    (ap_id: option(Id.t), di: Dynamics.Info.t, closure: closure): relation => {
  open OptUtil.Syntax;
  let this = closure.call_stack;
  // print_endline("this: " ++ String.concat(", ", List.map(Id.str3, this)));
  let cursor = trimmed_stack(di.dyn_cursor.call_cursor);
  {
    // print_endline(
    //   "cursor: " ++ String.concat(", ", List.map(Id.str3, cursor)),
    // );

    is_call_cursor: cursor == this,
    is_more_precise_than_cursor:
      List.length(di.dyn_cursor.call_cursor.stack)
      > List.length(closure.call_stack),
    relative_level_to_cursor: relative_level(cursor, this),
    is_call_above_call_cursor: {
      let* cur_call = cur_call(ap_id, closure);
      is_below(cur_call, cursor);
    },
    is_below_indicated_call: {
      let* cur_ap = di.dyn_cursor.indicated_call;
      is_below([cur_ap] @ cursor, this);
    },
  };
};

let is_related = relation =>
  switch (relation.relative_level_to_cursor) {
  | Above(_)
  | Below(_) => true
  | Same => true
  | Unrelated => false
  };

let first_cursor_closure =
    (ap_id: option(Id.t), di: Dynamics.Info.t): option(closure) => {
  open OptUtil.Syntax;
  let find_cursor =
    List.find_opt(
      (closure: closure) => relation(ap_id, di, closure).is_call_cursor,
      di.closures,
    );
  switch (find_cursor) {
  | Some(closure) => Some(closure)
  | None => None
  };
};

// let pin_call = (info: info): unit =>
//   switch (
//     OptUtil.and_then((di: Dynamics.Info.t) => is_in(di), info.dynamics)
//   ) {
//   | Some(closure_cursor) => s.pinned_call = cur_call(info, closure_cursor)
//   | _ => ()
//   };

// let unpin_call = (): unit => {
//   s.pinned_call = None;
// };

// let toggle_pinned_call = (info: info) =>
//   switch (s.pinned_call) {
//   | Some(pinned_ap) when ListUtil.hd_opt(pinned_ap) == cur_ap(info) =>
//     /* already pinned case */
//     unpin_call()
//   | Some(_)
//   | None => pin_call(info)
//   };

module Debug = {
  let stack = (stack: Probe.call_stack): string =>
    stack |> List.map(Id.str3) |> String.concat("\n");

  let str = (~ap_id: option(Id.t), closure: closure): string =>
    "closure_id: "
    ++ string_of_int(closure.closure_id)
    ++ "\n"
    ++ "ap:"
    ++ (
      switch (cur_call(ap_id, closure)) {
      | Some([ap_id, ..._]) => Id.str3(ap_id)
      | _ => "None"
      }
    )
    // ++ "\nvalue:\n"
    // ++ DHExp.show(closure.value)
    ++ "\nstack:\n"
    ++ stack(closure.call_stack)
    ++ "\ntime: "
    ++ string_of_float(closure.time /. 10000.0);
};
