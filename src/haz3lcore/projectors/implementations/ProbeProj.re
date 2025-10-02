open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;
open Js_of_ocaml;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type closure = Dynamics.Probe.Closure.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ChangeLength(int, int)
  | ToggleShowAllVals(int)
  | NoOp;

module Window = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Single
    | Many;

  let mode = ref(Single);
  let offset = Hashtbl.create(100);

  let reset = () => {
    Hashtbl.clear(offset);
    mode := Single;
  };

  let max_closures = () =>
    switch (mode^) {
    | Single => 1
    | Many => 30
    };

  let get_mode = () => mode^;

  let toggle_mode = () =>
    switch (mode^) {
    | Single => mode := Many
    | Many => mode := Single
    };

  let get_offset = (k: Id.t): int =>
    switch (Hashtbl.find_opt(offset, k)) {
    | Some(v) => v
    | None => 0
    };

  /* We are displaying a certain window of closures near the closure cursor.
   * If the closure cursor moves, we want to readjust this window to show the
   * cursor, but only if necessary. Thus we compare the cursor position to the
   * current window bounds, and make the minimum change to the window necessary
   * to show the cursor. As an edge case, if there are less total closures than
   * the window size, we set the window to begin at zero. */
  let new_offest =
      (cursor_idx: int, home: int, max_closures: int, all_closures: int): int =>
    if (all_closures <= max_closures) {
      0;
    } else if (cursor_idx < home) {
      cursor_idx;
    } else if (cursor_idx >= home + max_closures) {
      cursor_idx - max_closures + 1;
    } else {
      home;
    };

  let set_offset = (k: Id.t, v: int) => Hashtbl.add(offset, k, v);

  let reform = (id, all_closures, cursor_idx): (int, int) => {
    let max = max_closures();
    let new_home = new_offest(cursor_idx, get_offset(id), max, all_closures);
    set_offset(id, new_home);
    (new_home, max);
  };
};

let is_value = (exp: Exp.t) =>
  ValueChecker.check_value((), ClosureEnvironment.empty, exp) == Value;
module ClosureLength = {
  let lengths: Hashtbl.t(int, int) = Hashtbl.create(100);

  let reset = () => {
    Hashtbl.clear(lengths);
  };

  let get = (closure: closure): int =>
    Hashtbl.find_opt(lengths, closure.closure_id)
    |> Option.value(
         // TODO(andrew): relax 5, special-case multilines eg `case`
         ~default=
           /*!is_value(closure.value)
             ? 5 :*/ Window.get_mode()
           == Single
             ? 150 : 12,
       );

  let set = (id: int, length: int): unit => Hashtbl.add(lengths, id, length);
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

module Closures = {
  let filter_frames_by_pin =
      (~ap_id: option(Id.t), di: Dynamics.Info.t): list(closure) =>
    switch (di.dyn_cursor.pinned_stack) {
    | Some(pinned_ap) =>
      List.filter(
        (closure: closure) =>
          ListUtil.hd_opt(pinned_ap) == ap_id  //TODO(andrew): should this clause exist?? why does this need to know ap_id..
          || ListUtil.is_suffix_of(pinned_ap, closure.call_stack),
        di.closures,
      )
    | None => di.closures
    };

  let total = (~ap_id: option(Id.t), di: Dynamics.Info.t): int =>
    List.length(filter_frames_by_pin(~ap_id, di));

  let first_index_of_interest =
      (~ap_id: option(Id.t), di: Dynamics.Info.t): option(int) => {
    let find = (rel: Dynamics.Cursor.relation => bool): option(int) =>
      List.find_index(
        (closure: closure) =>
          rel(Dynamics.Cursor.relation(ap_id, di.dyn_cursor, closure)),
        di.closures,
      );
    switch (find(relation => relation.is_call_cursor)) {
    | Some(idx) => Some(idx)
    | None =>
      switch (find(relation => relation.is_below_indicated_call == Some(0))) {
      | Some(idx) => Some(idx)
      | None =>
        let a = find(relation => relation.is_below_indicated_call != None);
        a == None ? find(Dynamics.Cursor.is_related) : a;
      }
    // }
    };
  };

  let select_frames =
      (~id: Id.t, ~ap_id: option(Id.t), di: Dynamics.Info.t): list(closure) => {
    let closures = filter_frames_by_pin(~ap_id, di);
    let cursor_idx =
      switch (first_index_of_interest(~ap_id, di)) {
      | Some(idx) => idx
      | None => 0
      };
    let all_closures = List.length(closures);
    let (l, r) = Window.reform(id, all_closures, cursor_idx);
    ListUtil.slice(l, r, closures);
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

let abbreviate = (exp: Exp.t, available: int): Exp.t => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_ascriptions |> Abbreviate.abbreviate_exp(~available);
  abbr_exp;
};

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> String.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(Exp(exp));
  (seg, len_seg(utility, seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_ascriptions |> Abbreviate.abbreviate_exp(~available);
  seg_of_exp(utility, abbr_exp);
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
  {
    row,
    col,
  };
};

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

module ValueState = {
  let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

  let click_coords: ref(option(Point.t)) = ref(Option.None);
};

let cursor_clss =
    (ap_id: option(Id.t), di: Dynamics.Info.t, closure: closure)
    : list(string) => {
  let relation = Dynamics.Cursor.relation(ap_id, di.dyn_cursor, closure);
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
    | Unrelated => ["incomparable"]
    }
  );
};

module Debug = {
  let stack = (stack: Probe.call_stack): string =>
    stack |> List.map(Id.str3) |> String.concat("\n");

  let str = (~ap_id: option(Id.t), closure: closure): string =>
    "closure_id: "
    ++ string_of_int(closure.closure_id)
    ++ "\n"
    ++ "ap:"
    ++ (
      switch (Dynamics.Cursor.cur_call(ap_id, closure)) {
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

let value_view =
    (
      ~ap_id: option(Id.t),
      di: Dynamics.Info.t,
      utility: utility,
      view_seg,
      local,
      parent: external_action => Ui_effect.t(unit),
      closure: closure,
      index: int,
    ) => {
  let val_pointerdown = (e: Js.t(Dom_html.pointerEvent)) => {
    if (Js.to_bool(e##.shiftKey)) {
      let target =
        e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
      JsUtil.setPointerCapture(target, e##.pointerId);
      ValueState.mousedown := Some(target);
      ValueState.click_coords :=
        Some({
          row: e##.clientY,
          col: e##.clientX,
        });
    };
    parent(DynCursor(Capture(closure, ap_id)));
  };

  let val_pointerup = (e: Js.t(Dom_html.pointerEvent)) => {
    let target =
      e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
    if (JsUtil.hasPointerCapture(target, e##.pointerId)) {
      JsUtil.releasePointerCapture(target, e##.pointerId);
    };
    ValueState.mousedown := None;
    ValueState.click_coords := None;
    Effect.Ignore;
  };

  let val_mousemove = (e: Js.t(Dom_html.mouseEvent)) => {
    switch (ValueState.mousedown^) {
    | Some(_) when Js.to_bool(e##.shiftKey) =>
      let goal = pos_rel_to_target(e);
      local(ChangeLength(closure.closure_id, goal.col));
    | _ => Effect.Ignore
    };
  };

  let (seg, length) =
    abbreviated_seg_of(utility, ClosureLength.get(closure), closure.value);

  div(
    ~attrs=[
      //Attr.title(Debug.str(~ap_id, closure)),
      Attr.classes(
        ["value", length_cls(length)]
        @ cursor_clss(ap_id, di, closure)
        @ (Option.is_some(ap_id) ? ["ap"] : [])
        @ (!is_value(closure.value) ? ["indet"] : []),
      ),
      Attr.on_double_click(_ => local(ToggleShowAllVals(index))),
      Attr.on_pointerdown(evt =>
        Key.meta_held(evt)
          ? switch (ap_id, Dynamics.Info.is_in(di)) {
            | (Some(ap_id), Some(closure_cursor)) =>
              parent(
                DynCursor(
                  TogglePinCall([ap_id, ...closure_cursor.call_stack]),
                ),
              )
            | _ => Effect.Ignore
            }
          : val_pointerdown(evt)
      ),
      Attr.on_pointerup(val_pointerup),
      Attr.on_mousemove(val_mousemove),
    ],
    [view_seg(Sort.Exp, seg)],
  );
};

let env_val =
    (closure, view_seg, utility: utility, en: Dynamics.Probe.Env.entry)
    : Node.t => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.binding.name ++ " ≡ "),
      switch (en.value) {
      | Opaque => Node.text("Opaque")
      | Val(d) =>
        let (seg, _) =
          abbreviated_seg_of(utility, ClosureLength.get(closure), d);
        view_seg(Sort.Exp, seg);
      },
    ],
  );
};

let env_view = (closure: closure, view_seg, utility: utility): Node.t =>
  Node.div(
    ~attrs=[Attr.classes(["live-env"])],
    closure.env
    |> ListUtil.dedup
    |> rm_opaques
    |> List.map(env_val(closure, view_seg, utility)),
  );

let show_pin = (~ap_id: option(Id.t), di: Dynamics.Info.t) => {
  di.dyn_cursor.pinned_stack != None
  && di.dyn_cursor.pinned_stack
  |> OptUtil.and_then(ListUtil.hd_opt) == ap_id;
};

let pin_view = (~ap_id: option(Id.t), di: Dynamics.Info.t) =>
  show_pin(~ap_id, di) ? [div(~attrs=[Attr.classes(["pin"])], [])] : [];

let closure_view =
    (
      ~ap_id: option(Id.t),
      ~hide_env: bool,
      di: Dynamics.Info.t,
      utility: utility,
      view_seg,
      local,
      parent,
      (index: int, closure: closure),
    ) =>
  div(
    ~attrs=[Attr.classes(["closure"])],
    [
      value_view(~ap_id, di, utility, view_seg, local, parent, closure, index),
    ]
    @ pin_view(~ap_id, di)
    @ (hide_env ? [] : [env_view(closure, view_seg, utility)]),
  );

let closure_group_view =
    (
      ~ap_id: option(Id.t),
      ~hide_env: bool,
      di: Dynamics.Info.t,
      utility,
      view_seg,
      local,
      parent,
      groups: list(list((int, closure))),
    ) => {
  let group_views =
    List.map(
      closures =>
        Node.div(
          ~attrs=[Attr.classes(["closure-group"])],
          List.map(
            closure_view(
              ~ap_id,
              ~hide_env,
              di,
              utility,
              view_seg,
              local,
              parent,
            ),
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

let move_cursor =
    (
      ~ap_id: option(Id.t),
      di: Dynamics.Info.t,
      parent: external_action => Ui_effect.t(unit),
      offset: int,
    ) => {
  let closures = Closures.filter_frames_by_pin(~ap_id, di);
  let cursor_idx = Closures.first_index_of_interest(~ap_id, di);
  switch (cursor_idx) {
  /* Cursor would be outside window, reset to next visible closure */
  | Some(idx) =>
    let next_idx_maybe = idx - offset;
    if (next_idx_maybe >= 0 && next_idx_maybe < List.length(closures)) {
      //TODO(andrew): is none appropriate below?
      parent(
        DynCursor(Capture(List.nth(closures, next_idx_maybe), None)),
      );
    } else {
      Effect.Ignore;
    };
  | _ => Effect.Ignore
  };
};

let nav_bar_view =
    (
      ap_id: option(Id.t),
      di: Dynamics.Info.t,
      num_total: int,
      parent: external_action => Ui_effect.t(unit),
    ) => {
  let nav_arrow = (cond: bool, offset: int): Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["nav-arrow"] @ (cond ? ["disabled"] : [])),
        Attr.on_click(_ => move_cursor(~ap_id, di, parent, offset)),
      ],
      [],
    );
  let show_left = num_total < Window.max_closures();
  let show_right = num_total < Window.max_closures();
  div(
    ~attrs=[Attr.classes(["nav-bar"])],
    [nav_arrow(show_left, 1), nav_arrow(show_right, -1)],
  );
};

let equals_view =
  div(~attrs=[Attr.classes(["live-equals"])], [text("≡")]);

let num_closures_view = (~ap_id: option(Id.t), di: Dynamics.Info.t) => {
  let num_closures = Closures.total(~ap_id, di);
  let description = num_closures < 1000 ? string_of_int(num_closures) : "1k+";
  div(
    ~attrs=[
      Attr.title(string_of_int(num_closures)),
      Attr.classes(["num-closures"]),
    ],
    [text(description)],
  );
};

let syntax_str = (utility: utility) =>
  Core.Memo.general(seg => {
    let max_len = 30;
    let seg = Segment.unparenthesize(seg);
    let str = utility.seg_to_string(seg);
    let str = StringUtil.replace(StringUtil.regexp("\n"), str, " ");
    String.length(str) > max_len
      ? String.sub(str, 0, max_len) ++ "..." : str;
  });
let icon = div(~attrs=[Attr.classes(["icon"])], []);

let round_up = (utility: utility, closure): unit => {
  let (_, cur) =
    abbreviated_seg_of(utility, ClosureLength.get(closure), closure.value);
  let goal = cur + 1;
  let (_, max_len) =
    seg_of_exp(utility, DHExp.strip_ascriptions(closure.value));
  let rec find_target = (target: int): int => {
    let attempt_len =
      abbreviated_seg_of(utility, target, closure.value) |> snd;
    if (attempt_len < goal && target <= max_len) {
      find_target(target + 1);
    } else {
      target;
    };
  };
  ClosureLength.set(closure.closure_id, find_target(goal));
};

let round_down = (utility: utility, closure: closure): unit => {
  let (_, cur) =
    abbreviated_seg_of(utility, ClosureLength.get(closure), closure.value);
  let goal = cur - 1;
  let rec find_target = (target: int): int => {
    let attempt_len =
      abbreviated_seg_of(utility, target, closure.value) |> snd;
    if (attempt_len > goal && target > 0) {
      find_target(target - 1);
    } else {
      target;
    };
  };
  ClosureLength.set(closure.closure_id, find_target(goal));
};

let indicated_closure =
    (~ap_id: option(Id.t), di: Dynamics.Info.t): option(closure) =>
  Dynamics.Info.first_cursor_closure(ap_id, di);

let key_handler =
    (
      local,
      ~id: Id.t,
      ~ap_id: option(Id.t),
      di: Dynamics.Info.t,
      utility,
      parent: external_action => Ui_effect.t(unit),
      evt,
    ) => {
  open Effect;
  /* PLAN: inter-probe navigation
      ultimately need to be able to issue a parent action to move to and focus on
     another projector. for now, should be able to use the Project(Focus(id)) action
     to do both in one; will need to rethink when we want to /create/ probes as well.
     the probe that we want to move to is going to depend on the closure cursor, but
     also maybe the row of the closure we're on. alternatively, can maybe avoid
     row based logic by using closure creation time instead. In any case, want a function
     that takes the closure cursor and emits a new closure cursor and the id of a
     probe to jump to. Not sure this is the best approach at all, but for now maybe
     we could add all probe data to a common mutable structure in this module, when
     projectorview.all is called, and use this to calculate the probe id to jump to.
     like basically we're going to treat this mutable cache as a db, and do certain
     queries. specifically, return all probe_ids that have a closure with equal
     closure cursor to current, and take the one with the timestamp closet to but
     before/after the current closure cursor closure timestamp. */
  let key = Key.mk(KeyDown, evt);
  switch (key.key) {
  | D("Escape") when key.shift == Down =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Window.reset();
    ClosureLength.reset();
    parent(DynCursor(Reset));
  | D("Escape") =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Ignore;
  | D("ArrowRight") when key.shift == Down =>
    switch (indicated_closure(~ap_id, di)) {
    | Some(closure) => round_up(utility, closure)
    | None => ()
    };
    Many([local(NoOp), Stop_propagation, Prevent_default]);
  | D("ArrowLeft") when key.shift == Down =>
    switch (indicated_closure(~ap_id, di)) {
    | Some(closure) => round_down(utility, closure)
    | None => ()
    };
    Many([local(NoOp), Stop_propagation, Prevent_default]);
  | D("ArrowRight") =>
    // hack: Prevent_default below stops aggressive horizontal scroll
    // noop to trigger redraw
    Many([
      move_cursor(~ap_id, di, parent, 1),
      Stop_propagation,
      Prevent_default,
    ])
  | D("ArrowLeft") =>
    Many([
      move_cursor(~ap_id, di, parent, -1),
      Stop_propagation,
      Prevent_default,
    ])
  | D(" ") =>
    Window.toggle_mode();
    Many([local(NoOp), Stop_propagation, Prevent_default]); // trigger redraw
  | _ => Many([Stop_propagation])
  };
};

let cur_ap = (info: info) =>
  switch (info.statics) {
  | Some(InfoExp({term: {term: Ap(_), _} as ap, _}))
  | Some(InfoExp({term: {term: Probe({term: Ap(_), _} as ap, _), _}, _})) =>
    Some(Term.Exp.rep_id(ap))
  | _ => None
  };

/* Don't redundantly show an env for variable references, patterns */
let hide_env = (info: info): bool =>
  switch (info.statics) {
  | Some(
      InfoExp({term: {term: Var(_) | Probe({term: Var(_), _}, _), _}, _}),
    ) =>
    true
  | Some(InfoPat(_)) => true
  | _ => false
  };

let offside_view =
    (
      info: info,
      local,
      parent,
      view_seg: (~background: bool=?, Sort.t, list(syntax)) => Node.t,
      utility: utility,
    ) =>
  switch (info.dynamics) {
  | Some(di) =>
    let id = info.id;
    let ap_id = cur_ap(info);
    let hide_env = hide_env(info);
    let num_total = Closures.total(~ap_id, di);
    let closures = Closures.select_frames(~id, ~ap_id, di);
    let (num_shown, groups) = Closures.collate(closures);
    let is_cut_off = num_shown != num_total && num_shown > 0;
    let extras = [
      nav_bar_view(ap_id, di, num_total, parent),
      ellipsis_view(local),
    ];
    Node.div(
      ~attrs=[
        Attr.id(Id.cls(id)),
        Attr.tabindex(0),
        Attr.on_keydown(
          key_handler(local, ~id, ~ap_id, di, utility, parent),
        ),
        Attr.classes(["live-offside", Window.get_mode() |> Window.show_mode]),
      ],
      (num_shown > 0 ? [equals_view] : [])
      @ closure_group_view(
          ~ap_id,
          ~hide_env,
          di,
          utility,
          view_seg,
          local,
          parent,
          groups,
        )
      @ (is_cut_off ? extras : []),
    );
  | _ => Node.div([])
  };

let update = (() as m, _info: info, a: action) => {
  switch (a) {
  | ChangeLength(id, len) => ClosureLength.set(id, len)
  | ToggleShowAllVals(_) => Window.toggle_mode()
  | NoOp => m
  };
};

/* If one of the current probe's cells is not already selected,
 * select the first one */
let probe_default =
    (parent: external_action => Ui_effect.t(unit), info: info)
    : Effect.t(unit) =>
  switch (info.dynamics) {
  //| Some(di) when DynCursor.is_in(di) != None =>
  // DynCursor.capture_ap(info)
  //TODO(andrew): capture ap only
  | Some(di) =>
    //DynCursor.capture_ap(info);
    switch (di.closures) {
    | [fst, ..._] => parent(DynCursor(Capture(fst, cur_ap(info))))
    | [] => Effect.Ignore
    }
  | None => Effect.Ignore
  };

let is_pinned = (ap_id: option(Id.t), di: Dynamics.Info.t): bool =>
  switch (Dynamics.Info.is_in(di)) {
  | Some(closure_cursor) =>
    di.dyn_cursor.pinned_stack
    == Dynamics.Cursor.cur_call(ap_id, closure_cursor)
  | _ => false
  };

let view = (local, parent, info: info): Node.t =>
  div(
    ~attrs=[
      Attr.id(Id.cls(info.id)),
      Attr.tabindex(0),
      Attr.on_keydown(
        key_handler(
          local,
          ~id=info.id,
          ~ap_id=cur_ap(info),
          Option.value(info.dynamics, ~default=Dynamics.Info.init),
          info.utility,
          parent,
        ),
      ),
      Attr.classes(
        ["main"]
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        @ (
          switch (info.dynamics) {
          | Some(di) => is_pinned(cur_ap(info), di) ? ["pinned"] : []
          | None => []
          }
        ),
      ),
      Attr.on_double_click(_ =>
        switch (
          cur_ap(info),
          info.dynamics |> OptUtil.and_then(Dynamics.Info.is_in),
        ) {
        | (Some(ap_id), Some(closure_cursor)) =>
          parent(
            DynCursor(TogglePinCall([ap_id, ...closure_cursor.call_stack])),
          )
        | _ => Effect.Ignore
        }
      ),
      Attr.on_pointerdown(_
        /* Select a default cell if one is not already selected */
        => probe_default(parent, info)),
      Attr.on_pointerup(_ => {
        JsUtil.get_elem_by_id(Id.cls(info.id))##blur;
        Effect.Ignore;
      }),
    ],
    [text(syntax_str(info.utility, info.syntax)) /*, icon*/],
  );

let overlay_view = (info: info): Node.t =>
  switch (info.dynamics) {
  | Some(di) =>
    let ap_id = cur_ap(info);
    div(
      ~attrs=[
        Attr.classes(
          ["overlay"]
          @ (Option.is_some(ap_id) ? ["ap"] : [])
          @ (is_pinned(ap_id, di) ? ["pinned"] : []),
        ),
      ],
      [num_closures_view(~ap_id, di)] /*@ pin_view(info)*/,
    );
  | None => Node.div([])
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  let model_of_sexp = _ => ();
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;

  let init = (any: Term.Any.t) =>
    switch (any) {
    | Exp(_)
    | Pat(_) => Some()
    | Any(_) => Some() /* Grout don't have sorts rn */
    | _ => None
    };

  let dynamics = true;

  let focusable =
    Focusable.{
      pointer: Some(id => JsUtil.get_elem_by_id(Id.cls(id))##focus),
      keyboard: None,
    };

  let placeholder = (_, info: info) =>
    ProjectorCore.Shape.inline(
      /*2 +*/ String.length(syntax_str(info.utility, info.syntax)),
    );

  let update = update;

  let view = (_model, info, ~local, ~parent, ~view_seg) => {
    View.{
      inline:
        switch (info.syntax) {
        | [Grout({id, _})] when id == Id.invalid => Node.div([])
        | _ => view(local, parent, info)
        },
      overlay:
        switch (info.syntax) {
        | [Grout({id, _})] when id == Id.invalid =>
          Some(overlay_view(info))
        | _ => Some(overlay_view(info))
        },
      offside:
        Some(offside_view(info, local, parent, view_seg, info.utility)),
    };
  };
};
