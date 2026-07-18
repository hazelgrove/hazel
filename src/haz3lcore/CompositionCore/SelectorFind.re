open Util;
open Language;
open OptUtil.Syntax;

type measured_match = {
  match_result: Selector.match_result,
  start: option(Point.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type session = {
  selector: string,
  matches: [@opaque] list(Selector.match_result),
  active: int,
  anchor_point: Point.t,
};

let measure_start =
    (id: Id.t, term_data: TermData.t, measured: Measured.t): option(Point.t) =>
  switch (TermData.extreme_measures(id, term_data, measured)) {
  | Some((start, _)) => Some(start)
  | None => None
  };

/* A semantic term can carry several source IDs. Its representative ID is
   stable for semantic editing, but is not always a measurable/indicateable
   surface shard (notably for atoms). */
let ids_of_focus = (focused: Selector.focus_target): list(Id.t) =>
  switch (focused) {
  | FocusExp(e) => e.annotation.ids
  | FocusPat(p) => p.annotation.ids
  | FocusTyp(t) => t.annotation.ids
  | FocusMod(m) => m.annotation.ids
  };

let visual_id =
    (match_result: Selector.match_result, syntax: CachedSyntax.t)
    : option(Id.t) =>
  ids_of_focus(match_result.focused)
  |> List.find_opt(id =>
       TermData.extreme_measures(id, syntax.term_data, syntax.measured)
       != None
     );

let measure_match_start =
    (match_result: Selector.match_result, syntax: CachedSyntax.t)
    : option(Point.t) =>
  switch (visual_id(match_result, syntax)) {
  | Some(id) => measure_start(id, syntax.term_data, syntax.measured)
  | None => None
  };

/* Move the caret to a selector match. Try every source ID attached to the
   semantic match; if none can become Zipper.Indicated, move to the measured
   start of its visible range. This avoids leaving rebuilt zippers at EOF. */
let jump_to_match =
    (z: Zipper.t, match_result: Selector.match_result): Zipper.t => {
  let syntax = CachedSyntax.init(z);
  let ids = ids_of_focus(match_result.focused);
  switch (measure_match_start(match_result, syntax)) {
  | Some(goal) =>
    /* Rebuilt term zippers default to EOF. Point navigation is directional,
       so restart from the program's left edge before moving to a selector
       target that precedes EOF. */
    let from_start = Zipper.zip(z) |> Zipper.unzip(~direction=Left);
    Move.to_point(~measured=syntax.measured, ~goal, from_start)
    |> Option.value(~default=from_start);
  | None =>
    ids
    |> List.find_map(id => Move.jump_to_id_indicated(z, id))
    |> Option.value(~default=z)
  };
};

let with_positions =
    (matches: list(Selector.match_result), syntax: CachedSyntax.t)
    : list(measured_match) =>
  matches
  |> List.map((match_result: Selector.match_result) =>
       {
         match_result,
         start: measure_match_start(match_result, syntax),
       }
     );

let compare_start = (a: measured_match, b: measured_match): int =>
  switch (a.start, b.start) {
  | (Some(pa), Some(pb)) => Point.compare(pa, pb)
  | (Some(_), None) => (-1)
  | (None, Some(_)) => 1
  | (None, None) => 0
  };

let dedup_by_id = (matches: list(measured_match)): list(measured_match) => {
  let seen = Hashtbl.create(List.length(matches));
  List.filter(
    ({match_result: {focused_id, _}, _}) =>
      if (Hashtbl.mem(seen, focused_id)) {
        false;
      } else {
        Hashtbl.add(seen, focused_id, ());
        true;
      },
    matches,
  );
};

let sort_by_position =
    (matches: list(Selector.match_result), syntax: CachedSyntax.t)
    : list(Selector.match_result) =>
  matches
  |> with_positions(_, syntax)
  |> dedup_by_id
  |> List.sort(compare_start)
  |> List.map(({match_result, _}) => match_result);

let index_after_cursor =
    (
      matches: list(Selector.match_result),
      cursor: Point.t,
      syntax: CachedSyntax.t,
    )
    : int => {
  let positioned = with_positions(matches, syntax);
  switch (
    List.find_index(
      ({start, _}) =>
        switch (start) {
        | Some(start) => Point.compare(start, cursor) > 0
        | None => false
        },
      positioned,
    )
  ) {
  | Some(idx) => idx
  | None => 0
  };
};

let start =
    (
      ~selector: string,
      ~root: Exp.t,
      ~caret_point: Point.t,
      ~syntax: CachedSyntax.t,
    )
    : result(session, string) => {
  let selector = String.trim(selector);
  if (String.length(selector) == 0) {
    Error("Enter a selector.");
  } else {
    let matches =
      Selector.query(selector, root) |> sort_by_position(_, syntax);
    switch (matches) {
    | [] => Error(Selector.diagnose_no_match(selector, root))
    | matches =>
      Ok({
        selector,
        matches,
        active: index_after_cursor(matches, caret_point, syntax),
        anchor_point: caret_point,
      })
    };
  };
};

let length = (session: session): int => List.length(session.matches);

let clamp_active = (idx: int, len: int): int =>
  if (len <= 0) {
    0;
  } else if (idx < 0) {
    len - 1;
  } else if (idx >= len) {
    0;
  } else {
    idx;
  };

let next = (session: session): session => {
  let len = length(session);
  {
    ...session,
    active: clamp_active(session.active + 1, len),
  };
};

let prev = (session: session): session => {
  let len = length(session);
  {
    ...session,
    active: clamp_active(session.active - 1, len),
  };
};

let active_match = (session: session): option(Selector.match_result) =>
  List.nth_opt(session.matches, session.active);

let active_id = (session: session): option(Id.t) =>
  active_match(session)
  |> Option.map((m: Selector.match_result) => m.focused_id);

let active_display_index = (session: session): int =>
  length(session) == 0 ? 0 : session.active + 1;

let summary = (session: session): string =>
  string_of_int(active_display_index(session))
  ++ " of "
  ++ string_of_int(length(session));

let print_active = (session: session): option(string) =>
  active_match(session) |> Option.map(Selector.print_match);

let canonical_selector_for_active =
    (session: session, root: Exp.t): option(string) => {
  let* id = active_id(session);
  let* path = Selector.canonical_numeric(id, root);
  Some(Selector.deparse(path));
};
