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
  max_closures: int,
  /* Index offset for closure display if over max */
  index_offset: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | PinAp(Id.t)
  | ChangeLength(Id.t, int)
  | Offset(int)
  | ToggleShowAllVals(int);

let init = {display_lengths: Id.Map.empty, max_closures: 30, index_offset: 0};

let model_of_sexp = (sexp): model =>
  switch (model_of_sexp(sexp)) {
  | exception _ => init
  | x => x
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

/* Is the underlying syntax a variable reference? */
let is_var_ref = (info: info): bool =>
  switch (info.statics) {
  | Some(InfoExp({term: {term: Var(_), _}, _}))
  | Some(InfoPat({term: {term: Var(_), _}, _})) => true
  | _ => false
  };

let cur_ap_id = (info: info): option(Id.t) =>
  switch (info.statics) {
  | Some(InfoExp({term: {term: Ap(_), _} as ap, _})) =>
    Some(Term.Exp.rep_id(ap))
  | Some(InfoExp({term: {term: Wrap({term: Ap(_), _} as ap, _), _}, _})) =>
    Some(Term.Exp.rep_id(ap))
  | _ => None
  };

let cur_outer_ap_id = (_info: info, dyn_stack: Probe.stack): option(Id.t) =>
  switch (dyn_stack) {
  | [frame, ..._] => Some(frame.ap_id)
  | _ => None
  };

module State = {
  /* Manages shared state between probes */

  type t = {
    mutable pinned_ap: option(Id.t),
    mutable env_cursor: list(Id.t),
    mutable dyn_env_cursor: list(Probe.frame),
    mutable cur_ap: option(Id.t),
    mutable outer_ap_id: option(Id.t),
  };

  let s: t = {
    pinned_ap: None,
    env_cursor: [],
    dyn_env_cursor: [],
    cur_ap: None,
    outer_ap_id: None,
  };

  let reset = () => {
    s.pinned_ap = None;
    s.env_cursor = [];
    s.dyn_env_cursor = [];
    s.cur_ap = None;
    s.outer_ap_id = None;
  };

  let capture = (info: info, closure: closure) => {
    s.env_cursor = Probe.env_stack(closure.stack);
    s.dyn_env_cursor = closure.dyn_stack;
    s.cur_ap = cur_ap_id(info);
    s.outer_ap_id = cur_outer_ap_id(info, closure.dyn_stack);
  };
};

module Closures = {
  let num = (info: info): int =>
    switch (info.dynamics) {
    | Some(di) => List.length(di)
    | None => 0
    };

  let filter_frames_by_pin =
      (info: info, frames: list(closure)): list(closure) =>
    //TODO(andrew): make this logic work more generally...
    switch (State.s.pinned_ap) {
    | Some(pinned_ap) =>
      frames
      |> List.filter((closure: closure) =>
           switch (closure.dyn_stack |> List.rev) {
           | _ when Some(pinned_ap) == cur_ap_id(info) => true
           | [frame, ..._] => frame.ap_id == pinned_ap
           | [] => false
           }
         )
    | None => frames
    };

  let comparor = (a: closure, b: closure): int => {
    compare(
      ListUtil.common_suffix_length(
        State.s.env_cursor,
        Probe.env_stack(b.stack),
      ),
      ListUtil.common_suffix_length(
        State.s.env_cursor,
        Probe.env_stack(a.stack),
      ),
    );
  };

  let select_frames =
      (info: info, model: model, closures: list(closure)): list(closure) => {
    switch (List.sort(comparor, closures)) {
    | [] => []
    | _ =>
      closures
      |> filter_frames_by_pin(info)
      |> ListUtil.slice(model.index_offset, model.max_closures)
    };
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
    switch (List.rev(c2.dyn_stack), List.rev(c1.dyn_stack)) {
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
      (info: info, model: model, di: list(closure))
      : list(list((int, closure))) => {
    let closures = select_frames(info, model, di);
    let numbered_closures =
      List.mapi((i, c) => (List.length(closures) - i - 1, c), closures);
    group(numbered_closures);
  };
};

module Debug = {
  let of_id = (id: Id.t): string => String.sub(Id.to_string(id), 0, 3);

  let stack = (stack: Probe.stack): string =>
    stack
    |> List.rev
    |> List.map(({env_id, ap_id}: Probe.frame) =>
         "" ++ of_id(env_id) ++ " : " ++ of_id(ap_id)
       )
    |> String.concat("\n");

  let str = (closure: closure): string =>
    "closure_id: "
    ++ of_id(closure.closure_id)
    ++ "\nenv_id: "
    ++ of_id(closure.env_id)
    ++ "\ndyn_stack:\n"
    ++ stack(closure.dyn_stack)
    ++ "\nstack:\n"
    ++ stack(closure.stack);
};

let depth_in_cur_ap_stack = (dyn_stack: list(Probe.frame)): option(int) =>
  List.find_index(
    ({ap_id, _}: Probe.frame) => Some(ap_id) == State.s.cur_ap,
    dyn_stack,
  );

let seg_view = (utility: utility, available: int, seg: Exp.t): Node.t =>
  seg
  |> DHExp.strip_casts
  |> Abbreviate.abbreviate_exp(~available)
  |> fst
  |> utility.exp_to_seg
  |> utility.view_seg(Exp);

let get_goal = (utility: utility, e: Js.t(Dom_html.mouseEvent)): Point.t =>
  FontMetrics.get_goal(
    ~font_metrics=utility.font_metrics,
    e##.currentTarget
    |> Js.Opt.get(_, _ => failwith(""))
    |> JsUtil.get_child_with_class(_, "code")
    |> Option.get,
    e |> Js.Unsafe.coerce,
  );

let on_outer_ap = (info: info, closure: closure): bool =>
  switch (cur_ap_id(info), State.s.outer_ap_id) {
  | (Some(ap_id), Some(outer_ap_id)) =>
    ap_id == outer_ap_id
    && closure.env_id
    == Option.value(
         ~default={ap_id: Id.invalid, env_id: Id.invalid},
         ListUtil.hd_opt(State.s.dyn_env_cursor),
       ).
         env_id
  | _ => false
  };

let show_indicator = (stack: Probe.stack): bool => {
  let local = Probe.env_stack(stack);
  State.s.env_cursor == []
  && local == []
  || State.s.env_cursor != []
  && (
    ListUtil.is_suffix_of(local, State.s.env_cursor)
    || ListUtil.is_suffix_of(State.s.env_cursor, local)
  );
};

let dynamic_cursor_cls = (info: info, closure: closure): list(string) =>
  switch (depth_in_cur_ap_stack(closure.dyn_stack)) {
  | _ when on_outer_ap(info, closure) => ["cursor-outer-ap"]
  | Some(depth)
      when ListUtil.is_suffix_of(State.s.dyn_env_cursor, closure.dyn_stack) =>
    ["cursor-ap-lex"] @ (depth == 0 ? [] : ["light"])
  | Some(depth) => ["cursor-ap"] @ (depth == 0 ? [] : ["light"])
  | _ when show_indicator(closure.stack) => ["cursor-lex"]
  | None => ["cursor-none"]
  };

let display_length = (model: model, id: Id.t): int =>
  Id.Map.find_opt(id, model.display_lengths) |> Option.value(~default=12);

let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

let value_view =
    (
      info: info,
      model: model,
      utility: utility,
      local,
      closure: closure,
      index: int,
    ) => {
  let val_pointerdown = (e: Js.t(Dom_html.pointerEvent)) => {
    let target = e##.target |> Js.Opt.get(_, _ => failwith("no target"));
    JsUtil.setPointerCapture(target, e##.pointerId) |> ignore;
    mousedown := Some(target);
    State.capture(info, closure);
    Effect.Ignore;
  };

  let val_pointerup = (e: Js.t(Dom_html.pointerEvent)) => {
    switch (mousedown^) {
    | Some(target) =>
      JsUtil.releasePointerCapture(target, e##.pointerId) |> ignore
    | None => ()
    };
    mousedown := None;
    Effect.Ignore;
  };

  //TODO: refactor to pointermove when supported
  let val_mousemove = (e: Js.t(Dom_html.mouseEvent)) =>
    switch (mousedown^) {
    | Some(_elem) when Js.to_bool(e##.shiftKey) =>
      /* Ideally we could just use hasPointerCapture... */
      let goal = get_goal(utility, e);
      local(ChangeLength(closure.closure_id, goal.col));
    | _ => Effect.Ignore
    };

  div(
    ~attrs=[
      Attr.title(Debug.str(closure)),
      Attr.classes(
        ["val-resize"]
        @ dynamic_cursor_cls(info, closure)
        @ (Option.is_some(cur_ap_id(info)) ? ["ap"] : []),
      ),
      Attr.on_double_click(_ => local(ToggleShowAllVals(index))),
      Attr.on_pointerdown(val_pointerdown),
      Attr.on_pointerup(val_pointerup),
      Attr.on_mousemove(val_mousemove),
    ],
    [
      seg_view(
        utility,
        display_length(model, closure.closure_id),
        closure.value,
      ),
    ],
  );
};

let env_val = (utility: utility, en: Dynamics.Probe.Env.entry): Node.t => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.binding.name ++ "="),
      switch (en.value) {
      | Opaque => Node.text("Opaque")
      | Val(d) => seg_view(utility, 12, d)
      },
    ],
  );
};

let env_view = (closure: closure, utility: utility): Node.t =>
  Node.div(
    ~attrs=[Attr.classes(["live-env"])],
    closure.env |> ListUtil.dedup |> rm_opaques |> List.map(env_val(utility)),
  );

let closure_view =
    (
      info: info,
      utility: utility,
      model: model,
      local,
      (index: int, closure: closure),
    ) =>
  div(
    ~attrs=[
      Attr.classes(
        ["closure"] @ (show_indicator(closure.stack) ? ["cursor"] : []),
      ),
    ],
    [value_view(info, model, utility, local, closure, index)]
    @ (is_var_ref(info) ? [] : [env_view(closure, utility)]),
  );

let closure_group_view =
    (info, utility, model, local, groups: list(list((int, closure)))) => {
  let group_views =
    List.map(
      closures =>
        Node.div(
          ~attrs=[Attr.classes(["closure-group"])],
          List.map(closure_view(info, utility, model, local), closures),
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

let nav_bar_view = (model: model, di: list(closure), local) => {
  let nav_arrow = (cond: bool, offset: int): Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["nav-arrow"] @ (cond ? ["disabled"] : [])),
        Attr.on_click(_ => cond ? Effect.Ignore : local(Offset(offset))),
      ],
      [],
    );
  let show_left = model.index_offset >= List.length(di) - model.max_closures;
  let show_right = model.index_offset <= 0;
  let view =
    div(
      ~attrs=[Attr.classes(["nav-bar"])],
      [nav_arrow(show_left, 1), nav_arrow(show_right, -1)],
    );
  List.length(di) > model.max_closures ? [view] : [];
};

let offside_view = (model: model, info: info, local, utility: utility) =>
  Node.div(
    ~attrs=[Attr.classes(["live-offside"])],
    switch (info.dynamics) {
    | Some(di) =>
      let groups = Closures.collate(info, model, di);
      let ellipsis =
        List.length(di) > model.max_closures ? [ellipsis_view(local)] : [];
      nav_bar_view(model, di, local)
      @ closure_group_view(info, utility, model, local, groups)
      @ ellipsis;
    | _ => []
    },
  );

let num_closures_view = (info: info) => {
  let num_closures = Closures.num(info);
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
  State.s.pinned_ap != None && State.s.pinned_ap == cur_ap_id(info)
    ? [div(~attrs=[Attr.classes(["pin"])], [])] : [];

let syntax_str = (info: info) => {
  let max_len = 30;
  let str = Printer.of_segment(~holes=None, [info.syntax]);
  let str = Re.Str.global_replace(Re.Str.regexp("\n"), " ", str);
  String.length(str) > max_len ? String.sub(str, 0, max_len) ++ "..." : str;
};

let syntax_view = (info: info) => info |> syntax_str |> text;

let placeholder = (_m, info) =>
  ProjectorShape.inline(3 + String.length(syntax_str(info)));

let icon = div(~attrs=[Attr.classes(["icon"])], []);

let view = (info: info): Node.t => {
  let on_double_click = _ => {
    //State.reset();
    switch (State.s.pinned_ap) {
    | Some(pinned_ap) when Some(pinned_ap) == cur_ap_id(info) =>
      State.s.pinned_ap = None
    | Some(_)
    | None => State.s.pinned_ap = cur_ap_id(info)
    };
    Effect.Ignore;
  };

  let on_pointerdown = _ => {
    switch (info.dynamics) {
    | Some(di) =>
      switch (di) {
      | [first_closure, ..._] => State.capture(info, first_closure)
      | [] => ()
      }
    | None => ()
    };
    Effect.Ignore;
  };

  div(
    ~attrs=[
      Attr.classes(
        ["main"]
        @ (Option.is_some(cur_ap_id(info)) ? ["ap"] : [])
        @ (State.s.pinned_ap == cur_ap_id(info) ? ["pinned"] : []),
      ),
      Attr.on_double_click(on_double_click),
      Attr.on_pointerdown(on_pointerdown),
    ],
    [syntax_view(info), icon, num_closures_view(info)] @ pin_view(info),
  );
};

let update = (m: model, _info: info, a: action) => {
  //print_endline("update: action:" ++ show_action(a));
  switch (a) {
  | ChangeLength(id, len) =>
    if (len > (-1)) {
      {...m, display_lengths: Id.Map.add(id, len, m.display_lengths)};
    } else {
      m;
    }
  | ToggleShowAllVals(offset) => {
      ...m,
      index_offset: offset,
      max_closures: m.max_closures == 1 ? init.max_closures : 1,
    }
  | Offset(offset) =>
    let index_offset = m.index_offset + offset;
    let index_offset = index_offset < 0 ? 0 : index_offset;
    {...m, index_offset};
  | PinAp(id) =>
    switch (State.s.pinned_ap) {
    | Some(_) => State.s.pinned_ap = None
    | None => State.s.pinned_ap = Some(id)
    };
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
  let can_project = _ => true;
  let can_focus = false;
  let dynamics = true;
  let placeholder = placeholder;
  let update = update;
  let view = (_model, info, ~local as _, ~parent as _, ~utility as _) =>
    view(info);
  let offside_view =
    Some(
      (model, info, ~local, ~parent as _, ~utility) =>
        offside_view(model, info, local, utility),
    );
  let focus = _ => ();
};
