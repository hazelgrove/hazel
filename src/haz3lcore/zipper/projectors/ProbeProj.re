open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;
open Js_of_ocaml;

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
  | ToggleShowAllVals;

let init = {display_lengths: Id.Map.empty, max_closures: 30, index_offset: 0};

let model_of_sexp = (sexp): model =>
  switch (model_of_sexp(sexp)) {
  | exception _ => init
  | x => x
  };

let display_length = (model: model, id: Id.t): int =>
  Id.Map.find_opt(id, model.display_lengths) |> Option.value(~default=12);

module Debug = {
  let of_id = (id: Id.t) => String.sub(Id.to_string(id), 0, 3);

  let stack = stack =>
    stack
    |> List.rev
    |> List.map(({env_id, ap_id}: Probe.frame) =>
         "" ++ of_id(env_id) ++ " : " ++ of_id(ap_id)
       )
    |> String.concat("\n");

  let str = (closure: Dynamics.Probe.Closure.t) =>
    "closure_id: "
    ++ of_id(closure.closure_id)
    ++ "\nenv_id: "
    ++ of_id(closure.env_id)
    ++ "\ndyn_stack:\n"
    ++ stack(closure.dyn_stack)
    ++ "\nstack:\n"
    ++ stack(closure.stack);
};

let cur_ap_id = (info: info) =>
  switch (info.statics) {
  | Some(InfoExp({term: {term: Ap(_), _} as ap, _})) =>
    Some(Term.Exp.rep_id(ap))
  | Some(InfoExp({term: {term: Wrap({term: Ap(_), _} as ap, _), _}, _})) =>
    Some(Term.Exp.rep_id(ap))
  | _ => None
  };

let cur_outer_ap_id = (_info: info, dyn_stack: Probe.stack) =>
  switch (dyn_stack) {
  | [frame, ..._] => Some(frame.ap_id)
  | _ => None
  };

module State = {
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

  let capture = (info: info, closure: Dynamics.Probe.Closure.t) => {
    s.env_cursor = Probe.env_stack(closure.stack);
    s.dyn_env_cursor = closure.dyn_stack;
    s.cur_ap = cur_ap_id(info);
    s.outer_ap_id = cur_outer_ap_id(info, closure.dyn_stack);
  };
};

let comparor = (a: Dynamics.Probe.Closure.t, b: Dynamics.Probe.Closure.t) => {
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

let depth_in_cur_ap_stack = (dyn_stack): option(int) =>
  List.find_index(
    ({ap_id, _}: Probe.frame) => Some(ap_id) == State.s.cur_ap,
    dyn_stack,
  );

let seg_view = (utility, available, seg) =>
  seg
  |> DHExp.strip_casts
  |> Abbreviate.abbreviate_exp(~available)
  |> fst
  |> utility.exp_to_seg
  |> utility.view(Exp);

let get_goal = (utility: utility, e: Js.t(Dom_html.mouseEvent)) =>
  FontMetrics.get_goal(
    ~font_metrics=utility.font_metrics,
    e##.currentTarget
    |> Js.Opt.get(_, _ => failwith(""))
    |> JsUtil.get_child_with_class(_, "code")
    |> Option.get,
    e |> Js.Unsafe.coerce,
  );

let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

let on_outer_ap = (info: info, closure: Dynamics.Probe.Closure.t): bool =>
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

let show_indicator = stack => {
  let local = Probe.env_stack(stack);
  State.s.env_cursor == []
  && local == []
  || State.s.env_cursor != []
  && (
    ListUtil.is_suffix_of(local, State.s.env_cursor)
    || ListUtil.is_suffix_of(State.s.env_cursor, local)
  );
};

let dynamic_cursor_cls = (info: info, closure: Dynamics.Probe.Closure.t) =>
  switch (depth_in_cur_ap_stack(closure.dyn_stack)) {
  | _ when on_outer_ap(info, closure) => ["cursor-outer-ap"]
  | Some(depth)
      when ListUtil.is_suffix_of(State.s.dyn_env_cursor, closure.dyn_stack) =>
    ["cursor-ap-lex"] @ (depth == 0 ? [] : ["light"])
  | Some(depth) => ["cursor-ap"] @ (depth == 0 ? [] : ["light"])
  | _ when show_indicator(closure.stack) => ["cursor-lex"]
  | None => ["cursor-none"]
  };

let value_view =
    (info: info, model, utility, local, closure: Dynamics.Probe.Closure.t) => {
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
      Attr.on_double_click(_ => local(ToggleShowAllVals)),
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

let env_val = (utility: utility, en: Dynamics.Probe.Env.entry) => {
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

let env_view = (closure: Dynamics.Probe.Closure.t, utility: utility): Node.t =>
  Node.div(
    ~attrs=[Attr.classes(["live-env"])],
    closure.env |> ListUtil.dedup |> rm_opaques |> List.map(env_val(utility)),
  );

let closure_view =
    (
      info,
      utility: utility,
      model: model,
      local,
      closure: Dynamics.Probe.Closure.t,
    ) =>
  div(
    ~attrs=[
      Attr.classes(
        ["closure"] @ (show_indicator(closure.stack) ? ["cursor"] : []),
      ),
    ],
    [value_view(info, model, utility, local, closure)]
    @ (is_var_ref(info) ? [] : [env_view(closure, utility)]),
  );

let select_frames = (model: model, vals) => {
  switch (List.sort(comparor, vals)) {
  | [] => []
  | _ =>
    vals
    |> ListUtil.remove_first_n(model.index_offset)
    |> ListUtil.truncate(model.max_closures)
  };
};

let nav_back = (di, model, local, left_cond) =>
  List.length(di) > model.max_closures
    ? [
      Node.div(
        ~attrs=[
          Attr.classes(
            ["closures-header"] @ (left_cond ? ["disabled"] : []),
          ),
          Attr.on_click(_ => left_cond ? Effect.Ignore : local(Offset(1))),
        ],
        [Node.text("<")],
      ),
    ]
    : [];

let nav_forward = (di, model, local, right_cond) =>
  List.length(di) > model.max_closures
    ? [
      Node.div(
        ~attrs=[
          Attr.classes(
            ["closures-tail"] @ (right_cond ? ["disabled"] : []),
          ),
          Attr.on_click(_ => right_cond ? Effect.Ignore : local(Offset(-1))),
        ],
        [Node.text(">")],
      ),
    ]
    : [];

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

let group_closures =
    (closures: list(Dynamics.Probe.Closure.t))
    : list(list(Dynamics.Probe.Closure.t)) => {
  let is_same_call =
      (c1: Dynamics.Probe.Closure.t, c2: Dynamics.Probe.Closure.t) => {
    switch (List.rev(c2.dyn_stack), List.rev(c1.dyn_stack)) {
    | ([], _)
    | (_, []) => false
    | ([f1, ..._], [f2, ..._]) => f1 == f2
    };
  };
  let grouped =
    closures |> group_by_predicate(is_same_call) |> List.map(List.rev);
  /* Flatten if all groups are singletons */
  List.for_all(group => List.length(group) == 1, grouped)
    ? [List.concat(grouped)] : grouped;
};

let closure_group_view =
    (info, utility, model, local, closures: list(Dynamics.Probe.Closure.t)) =>
  group_closures(closures)
  |> List.map(closures =>
       Node.div(
         ~attrs=[Attr.classes(["closure-group"])],
         List.map(closure_view(info, utility, model, local), closures),
       )
     );

let offside_view =
    (model: model, ~info, ~local, ~parent as _, ~utility: utility) => {
  Node.div(
    ~attrs=[Attr.classes(["live-offside"])],
    switch (info.dynamics) {
    | Some(di) =>
      let frames = select_frames(model, di);
      let left_cond =
        model.index_offset >= List.length(di) - model.max_closures;
      let right_cond = model.index_offset <= 0;
      nav_back(di, model, local, left_cond)
      @ nav_forward(di, model, local, right_cond)
      @ closure_group_view(info, utility, model, local, frames);
    | _ => []
    },
  );
};

let num_closures = (info: info) =>
  switch (info.dynamics) {
  | Some(di) => List.length(di)
  | None => 0
  };

let num_closures_view = (info: info) => {
  let num_closures = num_closures(info);
  let description = num_closures < 1000 ? string_of_int(num_closures) : "1k+";
  div(
    ~attrs=[
      Attr.title(string_of_int(num_closures)),
      Attr.classes(["num-closures"]),
    ],
    [text(description)],
  );
};

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

let view =
    (_: model, ~info, ~local as _, ~parent as _, ~utility as _: utility) =>
  div(
    ~attrs=[
      Attr.classes(
        ["main"] @ (Option.is_some(cur_ap_id(info)) ? ["ap"] : []),
      ),
      Attr.on_click(_ => {
        //State.reset();
        switch (State.s.pinned_ap) {
        | Some(_) => State.s.pinned_ap = None
        | None => State.s.pinned_ap = cur_ap_id(info)
        };
        Effect.Ignore;
      }),
    ],
    [syntax_view(info), icon, num_closures_view(info)],
  );

let update = (m: model, a: action) => {
  //print_endline("update: action:" ++ show_action(a));
  switch (a) {
  | ChangeLength(id, len) =>
    if (len > (-1)) {
      {...m, display_lengths: Id.Map.add(id, len, m.display_lengths)};
    } else {
      m;
    }
  | ToggleShowAllVals => {
      ...m,
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
  let view = view;
  let offside_view = Some(offside_view);
  let focus = _ => ();
};
