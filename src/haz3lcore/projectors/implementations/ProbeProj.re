open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;
open Js_of_ocaml;

[@deriving (show({with_path: false}), sexp, yojson)]
type closure = Dynamics.Probe.Closure.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type model = {
  /* Max col length for value display, indexed by closure id */
  display_lengths: Id.Map.t(int),
  /* Max number of closures to display */
  //max_closures: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ChangeLength(Id.t, int)
  | MoveCursor(int)
  | ToggleShowAllVals(int)
  | PinAp;

let init = {display_lengths: Id.Map.empty};

let model_of_sexp = (sexp): model =>
  switch (model_of_sexp(sexp)) {
  | exception _ => init
  | x => x
  };

module State = {
  type view_mode =
    | Single
    | Many;

  let view_mode = ref(Many);

  let max_closures = () =>
    switch (view_mode^) {
    | Single => 1
    | Many => 30
    };

  let get_view_mode = () => view_mode^;

  let home = Hashtbl.create(100);

  let get_home = (k: Id.t): int =>
    switch (Hashtbl.find_opt(home, k)) {
    | Some(v) => v
    | None => 0
    };

  let set_home = (k: Id.t, v: int) => Hashtbl.add(home, k, v);

  let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

  let click_coords: ref(option(Point.t)) = ref(Option.None);
};

/* Remove opaque values like function literals */
let rm_opaques:
  list(Dynamics.Probe.Env.entry) => list(Dynamics.Probe.Env.entry) =
  List.filter_map((en: Dynamics.Probe.Env.entry) =>
    switch (en.value) {
    | Opaque => None
    | Val(_) => Some(en)
    }
  );

/* Don't redundantly show an env for variable references, patterns */
let hide_env = (info: info): bool =>
  switch (info.statics) {
  | Some(InfoExp({term: {term: Var(_) | Parens({term: Var(_), _}), _}, _})) =>
    true
  | Some(InfoPat(_)) => true
  | _ => false
  };

let cur_ap = (info: info) =>
  switch (info.statics) {
  | Some(InfoExp({term: {term: Ap(_), _} as ap, _}))
  | Some(InfoExp({term: {term: Parens({term: Ap(_), _} as ap), _}, _})) =>
    Some(Term.Exp.rep_id(ap))
  | Some(
      InfoExp({
        term: {term: Parens({term: Parens({term: Ap(_), _} as ap), _}), _},
        _,
      }),
    ) =>
    Some(Term.Exp.rep_id(ap))
  | _ => None
  };

let is_value = (exp: Exp.t) =>
  ValueChecker.check_value((), ClosureEnvironment.empty, exp) == Value;

module DynCursor = {
  /* Manages shared state between probes */

  type t = {
    mutable call_cursor: Probe.call_stack,
    mutable indicated_call: option(Id.t),
    mutable pinned_call: option(Probe.call_stack),
  };

  let s: t = {call_cursor: [], indicated_call: None, pinned_call: None};

  let reset = () => s;

  let capture_cursor = (closure: closure): unit => {
    s.call_cursor = closure.call_stack;
  };

  let capture_ap = (info: info): unit => {
    s.indicated_call = cur_ap(info);
  };

  let capture = (info: info, closure: closure): unit => {
    capture_cursor(closure);
    capture_ap(info);
  };

  let is_in = (closures: list(closure)): option(closure) =>
    List.find_opt(
      (closure: closure) => s.call_cursor == closure.call_stack,
      closures,
    );

  /* If one of the current probe's cells is not already selected,
   * select the first one */
  let probe_default = (info: info): unit =>
    switch (info.dynamics) {
    | Some(closures) when is_in(closures) != None => capture_ap(info)
    | Some(closures) =>
      capture_ap(info);
      switch (closures) {
      | [fst, ..._] => capture_cursor(fst)
      | [] => ()
      };
    | None => ()
    };

  /* If the closure cursor is on a call, and the provided
   * call stack is downstream of that call, return how many
   * aps downstream it is */
  let depth_in_indicated_calls_stack =
      (call_stack: Probe.call_stack): option(int) => {
    open OptUtil.Syntax;
    let* cur_ap = s.indicated_call;
    ListUtil.suffix_at_depth([cur_ap] @ s.call_cursor, call_stack);
  };

  type relative_level =
    | Above(int)
    | Below(int)
    | Same;

  /* How is the current closure related to the closure cursor? */
  type relation = {
    /* Is the current closure the call cursor? */
    is_call_cursor: bool,
    relative_level_to_cursor: relative_level,
    /* Is the current closure a call directly above the call cursor? */
    is_call_above_call_cursor: option(int),
    /* Is the current closure below the call cursor, and if so, by how much? */
    is_below_indicated_call: option(int),
  };

  let is_below = ListUtil.suffix_at_depth;

  let relative_level = (cs1, cs2): relative_level =>
    switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
    | (Some(0), Some(0)) => Same
    | (Some(n), None) => Below(n)
    | (None, Some(n)) => Above(n)
    | (_, _) => Same
    };

  let cur_call = (info: info, closure: closure) => {
    open OptUtil.Syntax;
    let* lex = cur_ap(info);
    let dyn = closure.call_stack;
    Some([lex, ...dyn]);
  };

  let relation = (info: info, closure: closure): relation => {
    open OptUtil.Syntax;
    let this = closure.call_stack;
    let cursor = s.call_cursor;
    {
      is_call_cursor: cursor == this,
      relative_level_to_cursor: relative_level(cursor, this),
      is_call_above_call_cursor: {
        let* cur_call = cur_call(info, closure);
        is_below(cur_call, cursor);
      },
      is_below_indicated_call: {
        let* cur_ap = s.indicated_call;
        is_below([cur_ap] @ cursor, this);
      },
    };
  };

  let clss = (info: info, closure: closure): list(string) => {
    let relation = relation(info, closure);
    (
      switch (
        relation.is_call_cursor,
        relation.is_call_above_call_cursor,
        relation.is_below_indicated_call,
      ) {
      | (true, _, _) => ["cursor"]
      | (_, Some(0), _) => ["cursor-caller", "direct"]
      | (_, Some(_), _) => ["cursor-caller", "indirect"]
      | (_, _, Some(0)) => ["cursor-callee", "direct"]
      | (_, _, Some(_)) => ["cursor-callee", "indirect"]
      | (_, _, None) => ["cursor-unrelated"]
      }
    )
    @ (
      switch (relation.relative_level_to_cursor) {
      | Same => ["level0"]
      | Below(n) => ["below", "L" ++ string_of_int(n)]
      | Above(n) => ["above", "L" ++ string_of_int(n)]
      }
    );
  };

  let first_index_of_interest = (info, closures: list(closure)): option(int) => {
    let find = (rel: relation => bool): option(int) =>
      List.find_index(
        (closure: closure) => rel(relation(info, closure)),
        closures,
      );
    switch (find(relation => relation.is_call_cursor)) {
    | Some(idx) => Some(idx)
    | None =>
      switch (find(relation => relation.is_below_indicated_call == Some(0))) {
      | Some(idx) => Some(idx)
      | None => find(relation => relation.is_below_indicated_call != None)
      }
    };
  };

  let show_pin = info => {
    s.pinned_call != None
    && s.pinned_call
    |> OptUtil.and_then(ListUtil.hd_opt) == cur_ap(info);
  };

  let is_pinned = (info: info): bool =>
    switch (OptUtil.and_then(is_in, info.dynamics)) {
    | Some(closure_cursor) => s.pinned_call == cur_call(info, closure_cursor)
    | _ => false
    };

  let pin_call = (info: info): unit =>
    switch (OptUtil.and_then(is_in, info.dynamics)) {
    | Some(closure_cursor) => s.pinned_call = cur_call(info, closure_cursor)
    | _ => ()
    };

  let unpin_call = (): unit => {
    s.pinned_call = None;
  };

  let toggle_pinned_call = (info: info) =>
    switch (s.pinned_call) {
    | Some(pinned_ap) when ListUtil.hd_opt(pinned_ap) == cur_ap(info) =>
      /* already pinned case */
      unpin_call()
    | Some(_)
    | None => pin_call(info)
    };

  module Debug = {
    let stack = (stack: Probe.call_stack): string =>
      stack |> List.map(Id.str3) |> String.concat("\n");

    let str = (info, closure: closure): string =>
      "ap:"
      ++ (
        switch (cur_call(info, closure)) {
        | Some([ap_id, ..._]) => Id.str3(ap_id)
        | _ => "None"
        }
      )
      ++ "\nstack:\n"
      ++ stack(closure.call_stack);
    // ++ "\ntime: "
    // ++ string_of_float(closure.time /. 10000.0);
  };
};

module Closures = {
  let filter_frames_by_pin =
      (info: info, frames: list(closure)): list(closure) =>
    switch (DynCursor.s.pinned_call) {
    | Some(pinned_ap) =>
      List.filter(
        (closure: closure) =>
          ListUtil.hd_opt(pinned_ap) == cur_ap(info)
          || ListUtil.is_suffix_of(pinned_ap, closure.call_stack),
        frames,
      )
    | None => frames
    };

  let total = (info: info): int =>
    switch (info.dynamics) {
    | Some(closures) => List.length(filter_frames_by_pin(info, closures))
    | None => 0
    };

  let new_home = (cursor_idx: int, home: int, max_closures: int): int =>
    if (cursor_idx < home) {
      cursor_idx;
    } else if (cursor_idx >= home + max_closures) {
      cursor_idx - max_closures + 1;
    } else {
      home;
    };

  let select_frames =
      (_model: model, info: info, closures: list(closure)): list(closure) => {
    let closures = filter_frames_by_pin(info, closures);
    let cursor_idx =
      switch (DynCursor.first_index_of_interest(info, closures)) {
      | Some(idx) => idx
      | None => 0
      };
    let home = State.get_home(info.id);
    let max_closures = State.max_closures();
    let new_home = new_home(cursor_idx, home, max_closures);
    State.set_home(info.id, new_home);
    ListUtil.slice(new_home, max_closures, closures);
  };

  let group_by_predicate =
      /* Precondition: Items to be grouped are contigious in list */
      (should_group: ('a, 'a) => bool, xs: list('a)): list(list('a)) => {
    List.fold_left(
      (acc: list(list('a)), item: 'a) => {
        switch (acc) {
        | [] => [[item]]
        | [[rep, ..._] as first, ...init] when should_group(rep, item) => [
            first @ [item],
            ...init,
          ]
        | _ => [[item]] @ acc
        }
      },
      [],
      xs,
    );
  };

  let is_same_call = ((_, c1: closure), (_, c2: closure)): bool => {
    switch (List.rev(c2.call_stack), List.rev(c1.call_stack)) {
    | ([], _)
    | (_, []) => false
    | ([f1, ..._], [f2, ..._]) => f1 == f2
    };
  };

  let group =
      (closures: list((int, closure))): list(list((int, closure))) => {
    let grouped =
      closures |> group_by_predicate(is_same_call) |> List.map(List.rev);
    /* Flatten if all groups are singletons */
    List.for_all(group => List.length(group) == 1, grouped)
      ? [List.concat(grouped)] : grouped;
  };

  let collate =
      (closures: list(closure)): (int, list(list((int, closure)))) => {
    let numbered_closures =
      List.mapi((i, c) => (List.length(closures) - i - 1, c), closures);
    (List.length(closures), group(numbered_closures));
  };
};

let seg_view = (view_seg, utility: utility, available: int, exp: Exp.t) => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_casts |> Abbreviate.abbreviate_exp(~available);
  let seg = utility.term_to_seg(Exp(abbr_exp));
  let len = seg |> Printer.of_segment(~holes=Some("?")) |> String.length;
  (view_seg(Sort.Exp, seg), len);
};

let pos_rel_to_target = (e: Js.t(Dom_html.mouseEvent)): Point.t => {
  open Float;
  let row_height = 10.0;
  let col_width = 10.0;
  let text_box =
    e##.currentTarget
    |> Js.Opt.get(_, _ => failwith(""))
    |> JsUtil.get_child_with_class(_, "code")
    |> Option.get;
  let x_rel = of_int(e##.clientX) -. text_box##getBoundingClientRect##.left;
  let y_rel = of_int(e##.clientY) -. text_box##getBoundingClientRect##.top;
  let row = to_int(y_rel /. row_height);
  let col = to_int(round(x_rel /. col_width));
  {row, col};
};

let display_length = (model: model, closure: closure): int =>
  Id.Map.find_opt(closure.closure_id, model.display_lengths)
  |> Option.value(
       ~default=
         !is_value(closure.value)
           ? 5 : State.get_view_mode() == Single ? 36 : 12,
     );

let length_cls = (length: int): string =>
  if (length > 10) {
    "extra";
  } else if (length > 9) {
    "s6";
  } else if (length > 8) {
    "s5";
  } else if (length > 7) {
    "s4";
  } else if (length > 6) {
    "s3";
  } else if (length > 5) {
    "s2";
  } else if (length > 4) {
    "s1";
  } else {
    "s0";
  };

let value_view =
    (
      info: info,
      model: model,
      utility: utility,
      view_seg,
      local,
      closure: closure,
      index: int,
    ) => {
  let val_pointerdown = (e: Js.t(Dom_html.pointerEvent)) => {
    if (Js.to_bool(e##.shiftKey)) {
      let target =
        e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
      JsUtil.setPointerCapture(target, e##.pointerId);
      State.mousedown := Some(target);
      State.click_coords := Some({row: e##.clientY, col: e##.clientX});
    };
    DynCursor.capture(info, closure);
    Effect.Ignore;
  };

  let val_pointerup = (e: Js.t(Dom_html.pointerEvent)) => {
    let target =
      e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
    if (JsUtil.hasPointerCapture(target, e##.pointerId)) {
      JsUtil.releasePointerCapture(target, e##.pointerId);
    };
    State.mousedown := None;
    State.click_coords := None;
    Effect.Ignore;
  };

  let val_mousemove = (e: Js.t(Dom_html.mouseEvent)) => {
    switch (State.mousedown^) {
    | Some(_) when Js.to_bool(e##.shiftKey) =>
      let goal = pos_rel_to_target(e);
      local(ChangeLength(closure.closure_id, goal.col));
    | _ => Effect.Ignore
    };
  };

  let (view, length) =
    seg_view(
      view_seg,
      utility,
      display_length(model, closure),
      closure.value,
    );

  div(
    ~attrs=[
      //Attr.title(DynCursor.Debug.str(info, closure)),
      Attr.classes(
        ["value", length_cls(length)]
        @ DynCursor.clss(info, closure)
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        @ (!is_value(closure.value) ? ["indet"] : []),
      ),
      Attr.on_double_click(_ => local(ToggleShowAllVals(index))),
      Attr.on_pointerdown(val_pointerdown),
      Attr.on_pointerup(val_pointerup),
      Attr.on_mousemove(val_mousemove),
    ],
    [view],
  );
};

let env_val =
    (
      model: model,
      closure_id,
      view_seg,
      utility: utility,
      en: Dynamics.Probe.Env.entry,
    )
    : Node.t => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.binding.name ++ "="),
      switch (en.value) {
      | Opaque => Node.text("Opaque")
      | Val(d) =>
        seg_view(view_seg, utility, display_length(model, closure_id), d)
        |> fst
      },
    ],
  );
};

let env_view = (model, closure: closure, view_seg, utility: utility): Node.t =>
  Node.div(
    ~attrs=[Attr.classes(["live-env"])],
    closure.env
    |> ListUtil.dedup
    |> rm_opaques
    |> List.map(env_val(model, closure, view_seg, utility)),
  );

let closure_view =
    (
      info: info,
      utility: utility,
      view_seg,
      model: model,
      local,
      (index: int, closure: closure),
    ) =>
  div(
    ~attrs=[Attr.classes(["closure"])],
    [value_view(info, model, utility, view_seg, local, closure, index)]
    @ (hide_env(info) ? [] : [env_view(model, closure, view_seg, utility)]),
  );

let closure_group_view =
    (
      info,
      utility,
      view_seg,
      model,
      local,
      groups: list(list((int, closure))),
    ) => {
  let group_views =
    List.map(
      closures =>
        Node.div(
          ~attrs=[Attr.classes(["closure-group"])],
          List.map(
            closure_view(info, utility, view_seg, model, local),
            closures,
          ),
        ),
      groups,
    );
  group_views == []
    ? [] : [div(~attrs=[Attr.classes(["closure-groups"])], group_views)];
};

let ellipsis_view = (local): Node.t =>
  div(
    ~attrs=[
      Attr.classes(["ellipsis"]),
      Attr.on_double_click(_ => {local(ToggleShowAllVals(0))}),
    ],
    [text("⋯")],
  );

let nav_bar_view = (_model: model, num_total: int, local) => {
  let nav_arrow = (cond: bool, offset: int): Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["nav-arrow"] @ (cond ? ["disabled"] : [])),
        Attr.on_click(_ => local(MoveCursor(offset))),
      ],
      [],
    );
  // TODO: better logic
  let show_left = num_total < State.max_closures();
  let show_right = num_total < State.max_closures();
  div(
    ~attrs=[Attr.classes(["nav-bar"])],
    [nav_arrow(show_left, 1), nav_arrow(show_right, -1)],
  );
};

let equals_view =
  div(~attrs=[Attr.classes(["live-equals"])], [text("=")]);

let offside_view =
    (
      model: model,
      info: info,
      local,
      view_seg: (~background: bool=?, Sort.t, list(syntax)) => Node.t,
      utility: utility,
    ) =>
  Node.div(
    ~attrs=[Attr.classes(["live-offside"])],
    switch (info.dynamics) {
    | Some(closures) =>
      let num_total = Closures.total(info);
      let closures = Closures.select_frames(model, info, closures);
      let (num_shown, groups) = Closures.collate(closures);
      let is_cut_off = num_shown != num_total && num_shown > 0;
      let extras = [
        nav_bar_view(model, num_total, local),
        ellipsis_view(local),
      ];
      (num_shown > 0 ? [equals_view] : [])
      @ closure_group_view(info, utility, view_seg, model, local, groups)
      @ (is_cut_off ? extras : []);
    | _ => []
    },
  );

let num_closures_view = (info: info) => {
  let num_closures = Closures.total(info);
  let description = num_closures < 1000 ? string_of_int(num_closures) : "1k+";
  div(
    ~attrs=[
      Attr.title(string_of_int(num_closures)),
      Attr.classes(["num-closures"]),
    ],
    [text(description)],
  );
};

let pin_view = (info: info) =>
  DynCursor.show_pin(info)
    ? [div(~attrs=[Attr.classes(["pin"])], [])] : [];

let syntax_str =
  Core.Memo.general(seg => {
    let max_len = 30;
    let seg = Segment.unparenthesize(seg);
    let str = Printer.of_segment(~holes=Some("?"), seg);
    let str = Re.Str.global_replace(Re.Str.regexp("\n"), " ", str);
    String.length(str) > max_len
      ? String.sub(str, 0, max_len) ++ "..." : str;
  });
let icon = div(~attrs=[Attr.classes(["icon"])], []);

let view = (local, info: info): Node.t =>
  div(
    ~attrs=[
      Attr.classes(
        ["main"]
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        @ (DynCursor.is_pinned(info) ? ["pinned"] : []),
      ),
      Attr.on_double_click(_ => local(PinAp)),
      Attr.on_pointerdown(_ => {
        /* Select a default cell if one is not already selected */
        DynCursor.probe_default(info);
        Effect.Ignore;
      }),
    ],
    [text(syntax_str(info.syntax)), icon],
  );

let overlay_view = (info: info): Node.t =>
  div(
    ~attrs=[
      Attr.classes(
        ["overlay"]
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        @ (DynCursor.is_pinned(info) ? ["pinned"] : []),
      ),
    ],
    [num_closures_view(info)] @ pin_view(info),
  );

let update = (m: model, info: info, a: action) => {
  switch (a) {
  | ChangeLength(id, len) =>
    if (len > (-1)) {
      {display_lengths: Id.Map.add(id, len, m.display_lengths)};
    } else {
      m;
    }
  | ToggleShowAllVals(_) =>
    switch (State.view_mode^) {
    | Single => State.view_mode := Many
    | Many => State.view_mode := Single
    };
    m;
  | MoveCursor(offset) =>
    switch (info.dynamics) {
    | Some(closures) =>
      let closures = Closures.filter_frames_by_pin(info, closures);
      let cursor_idx = DynCursor.first_index_of_interest(info, closures);
      switch (cursor_idx) {
      /* Cursor would be outside window, reset to next visible closure */
      | Some(idx) =>
        let next_idx_maybe = idx - offset;
        if (next_idx_maybe >= 0 && next_idx_maybe < List.length(closures)) {
          DynCursor.capture_cursor(List.nth(closures, next_idx_maybe));
        };
      | _ => ()
      };
    | None => ()
    };
    m;
  | PinAp =>
    /* Pin a function call, filtering the cells in other probes */
    DynCursor.toggle_pinned_call(info);
    m;
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type m = model;
[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = m;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;

  let init = init;
  let dynamics = true;
  let can_focus = false;
  let focus = _ => ();

  let can_project = (any: Term.Any.t) =>
    switch (any) {
    | Exp(_) => true
    | Pat(_) => true
    | Any(_) => true /* Grout don't have sorts rn */
    | _ => false
    };

  let placeholder = (_, info: info) =>
    ProjectorCore.Shape.inline(2 + String.length(syntax_str(info.syntax)));

  let update = update;

  let view = (model, info, ~local, ~parent as _, ~view_seg) =>
    View.{
      inline: Some(view(local, info)),
      underlay: None,
      overlay: Some(overlay_view(info)),
      offside:
        Some(offside_view(model, info, local, view_seg, info.utility)),
    };
};
