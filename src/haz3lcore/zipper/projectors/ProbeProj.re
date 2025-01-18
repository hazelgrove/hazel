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
  | PinAp(list(Id.t))
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

let cur_ap = (info: info) =>
  switch (info.statics) {
  | Some(InfoExp({term: {term: Ap(_), _} as ap, _}))
  | Some(InfoExp({term: {term: Wrap({term: Ap(_), _} as ap, _), _}, _})) =>
    Some(Term.Exp.rep_id(ap))
  | _ => None
  };

let cur_call = (info: info, closure: closure) => {
  open OptUtil.Syntax;
  let* lex = cur_ap(info);
  let dyn = closure.call_stack;
  Some([lex, ...dyn]);
};

module State = {
  /* Manages shared state between probes */

  type t = {
    mutable call_cursor: list(Id.t),
    mutable indicated_call: option(Id.t),
    mutable pinned_call: option(list(Id.t)),
  };

  let s: t = {call_cursor: [], indicated_call: None, pinned_call: None};

  let reset = () => s;

  let capture = (info: info, closure: closure) => {
    s.call_cursor = closure.call_stack;
    s.indicated_call = cur_ap(info);
  };
};

module Closures = {
  let num = (info: info): int =>
    switch (info.dynamics) {
    | Some(di) => List.length(di)
    | None => 0
    };

  let filter_frames_by_pin = (frames: list(closure)): list(closure) =>
    switch (State.s.pinned_call) {
    | Some(pinned_ap) =>
      List.filter(
        (closure: closure) =>
          ListUtil.is_suffix_of(pinned_ap, closure.call_stack),
        frames,
      )
    | None => frames
    };

  let comparor = (a: closure, b: closure): int => {
    compare(
      ListUtil.common_suffix_length(State.s.call_cursor, b.call_stack),
      ListUtil.common_suffix_length(State.s.call_cursor, a.call_stack),
    );
  };

  let select_frames = (model: model, closures: list(closure)): list(closure) => {
    switch (List.sort(comparor, closures)) {
    | [] => []
    | _ =>
      closures
      |> filter_frames_by_pin
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
      (model: model, di: list(closure))
      : (int, list(list((int, closure)))) => {
    let closures = select_frames(model, di);
    let numbered_closures =
      List.mapi((i, c) => (List.length(closures) - i - 1, c), closures);
    (List.length(closures), group(numbered_closures));
  };
};

module Debug = {
  let of_id = (id: Id.t): string => String.sub(Id.to_string(id), 0, 3);

  let stack = (stack: Probe.call_stack): string =>
    stack |> List.map(of_id) |> String.concat("\n");

  let str = (info, closure: closure): string =>
    //"closure_id: "
    //++ of_id(closure.closure_id)
    // ++ "\nenv_id: "
    // ++ of_id(closure.env_id)
    //++
    "ap:"
    ++ (
      switch (cur_call(info, closure)) {
      | Some([ap_id, ..._]) => of_id(ap_id)
      | _ => "None"
      }
    )
    ++ "\nstack:\n"
    ++ stack(closure.call_stack);
  // ++ "\nstack:\n"
  // ++ stack(closure.stack);
};

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

let depth_in_cur_call_stack = (call_stack: Probe.call_stack): option(int) => {
  open OptUtil.Syntax;
  let* cur_ap = State.s.indicated_call;
  let cur_ap = [cur_ap] @ State.s.call_cursor;
  let rec go = (depth: int, stack: list(Id.t)): option(int) =>
    if (stack == cur_ap) {
      Some(depth);
    } else {
      switch (stack) {
      | [] => None
      | [_, ...rest] => go(depth + 1, rest)
      };
    };
  go(0, call_stack);
};

let dynamic_cursor_cls = (info: info, closure: closure): list(string) => {
  let this = closure.call_stack;
  let is_call_cursor = State.s.call_cursor == this;
  let is_desc_of_call_cursor =
    ListUtil.is_suffix_of(State.s.call_cursor, this);
  let is_call_directly_creating_call_cursor =
    cur_call(info, closure) == Some(State.s.call_cursor);
  let is_downstream_of_indicated_call = depth_in_cur_call_stack(this);
  is_call_directly_creating_call_cursor
    ? ["cursor-outer-ap"]
    : (
      switch (is_downstream_of_indicated_call) {
      | Some(depth) =>
        (is_desc_of_call_cursor ? ["cursor-ap-lex"] : ["cursor-ap"])
        @ (depth == 0 ? [] : ["light"])
      | None => is_call_cursor ? ["cursor-lex"] : ["cursor-none"]
      }
    );
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
      Attr.title(Debug.str(info, closure)),
      Attr.classes(
        ["val-resize"]
        @ dynamic_cursor_cls(info, closure)
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : []),
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
        ["closure"]
        @ (closure.call_stack == State.s.call_cursor ? ["cursor"] : []),
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
  div(
    ~attrs=[Attr.classes(["nav-bar"])],
    [nav_arrow(show_left, 1), nav_arrow(show_right, -1)],
  );
};

let equals_view =
  div(~attrs=[Attr.classes(["live-equals"])], [text("=")]);

let offside_view = (model: model, info: info, local, utility: utility) =>
  Node.div(
    ~attrs=[Attr.classes(["live-offside"])],
    switch (info.dynamics) {
    | Some(di) =>
      let (num_shown, groups) = Closures.collate(model, di);
      let is_cut_off = num_shown != Closures.num(info) && num_shown > 0;
      let extras = [nav_bar_view(model, di, local), ellipsis_view(local)];
      (num_shown > 0 ? [equals_view] : [])
      @ closure_group_view(info, utility, model, local, groups)
      @ (is_cut_off ? extras : []);
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

let pin_view = (info: info, closure: closure) =>
  State.s.pinned_call != None
  && State.s.pinned_call == cur_call(info, closure)
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
  let first_closure =
    switch (info.dynamics) {
    | Some([first_closure, ..._]) => Some(first_closure)
    | _ => None
    };
  let is_pinned =
    switch (first_closure) {
    | Some(first_closure) =>
      State.s.pinned_call == cur_call(info, first_closure)
    | _ => false
    };
  let on_double_click = _ => {
    //State.reset();
    switch (State.s.pinned_call) {
    | Some(pinned_ap) when ListUtil.hd_opt(pinned_ap) == cur_ap(info) =>
      State.s.pinned_call = None
    | Some(_)
    | None =>
      //TODO(andrew): this should be on the cell not on the ap...
      switch (first_closure) {
      | Some(first_closure) =>
        State.s.pinned_call = cur_call(info, first_closure)
      | _ => ()
      }
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
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        @ (is_pinned ? ["pinned"] : []),
      ),
      Attr.on_double_click(on_double_click),
      Attr.on_pointerdown(on_pointerdown),
    ],
    [syntax_view(info), icon],
  );
};

let overlay_view = (info: info): Node.t => {
  let first_closure =
    switch (info.dynamics) {
    | Some([first_closure, ..._]) => Some(first_closure)
    | _ => None
    };
  let is_pinned =
    switch (first_closure) {
    | Some(first_closure) =>
      State.s.pinned_call == cur_call(info, first_closure)
    | _ => false
    };
  div(
    ~attrs=[
      Attr.classes(
        ["overlay"]
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        @ (is_pinned ? ["pinned"] : []),
      ),
    ],
    [num_closures_view(info)]
    @ (
      switch (first_closure) {
      | Some(first_closure) => pin_view(info, first_closure)
      | _ => []
      }
    ),
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
    switch (State.s.pinned_call) {
    | Some(_) => State.s.pinned_call = None
    | None => State.s.pinned_call = Some(id)
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
  let overlay_view =
    Some(
      (_model, info, ~local as _, ~parent as _, ~utility as _) =>
        overlay_view(info),
    );
  let underlay_view = Option.None; //TODO
  let focus = _ => ();
};
