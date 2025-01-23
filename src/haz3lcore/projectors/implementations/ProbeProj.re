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
  | PinAp(Probe.call_stack)
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
  | Some(
      InfoExp({
        term:
          {term: Wrap({term: Wrap({term: Ap(_), _} as ap, _), _}, _), _},
        _,
      }),
    ) =>
    Some(Term.Exp.rep_id(ap))
  | _ => None
  };

let cur_call = (info: info, closure: closure) => {
  open OptUtil.Syntax;
  let* lex = cur_ap(info);
  let dyn = closure.call_stack;
  Some([lex, ...dyn]);
};

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

  let has = (closures: list(closure)): option(closure) =>
    List.find_opt(
      (closure: closure) => s.call_cursor == closure.call_stack,
      closures,
    );

  let is_pinned = (info: info): bool =>
    switch (OptUtil.and_then(has, info.dynamics)) {
    | Some(closure_cursor) => s.pinned_call == cur_call(info, closure_cursor)
    | _ => false
    };

  let pin_call = (info: info): unit =>
    switch (OptUtil.and_then(has, info.dynamics)) {
    | Some(closure_cursor) => s.pinned_call = cur_call(info, closure_cursor)
    | _ => ()
    };

  let unpin_call = (): unit => {
    s.pinned_call = None;
  };

  /* If the closure cursor is on a call, and the provided
   * call stack is downstream of that call, return how many
   * aps downstream it is */
  let depth_in_indicated_calls_stack =
      (call_stack: Probe.call_stack): option(int) => {
    open OptUtil.Syntax;
    let* cur_ap = s.indicated_call;
    ListUtil.suffix_at_depth([cur_ap] @ s.call_cursor, call_stack, 0);
  };

  /* How is the current closure related to the closure cursor? */
  type relation = {
    /* Is the current closure the call cursor? */
    is_call_cursor: bool,
    /* Is the current closure a call directly above the call cursor? */
    is_call_directly_above_call_cursor: bool,
    /* Is the current closure below the call cursor, and if so, by how much? */
    is_below_indicated_call: option(int),
  };

  //use ListUtil.is_suffix_of
  let is_strictly_above = (xs: Probe.call_stack, ys: Probe.call_stack): bool =>
    ListUtil.is_suffix_of(xs, ys) && xs != ys;

  let relation = (info: info, closure: closure): relation => {
    let this = closure.call_stack;
    let cursor = s.call_cursor;
    //ListUtil.suffix_at_depth(cursor, this, 0) != None
    let _cond =
      if (List.mem(ListUtil.hd_opt(this), cursor |> List.map(Option.some))) {
        cursor == this;
      } else {
        is_strictly_above(this, cursor);
      };
    {
      is_call_cursor: cursor == this,
      is_call_directly_above_call_cursor:
        cur_call(info, closure) == Some(cursor),
      is_below_indicated_call: depth_in_indicated_calls_stack(this),
    };
  };

  let clss = (info: info, closure: closure): list(string) => {
    let relation = relation(info, closure);
    switch (
      relation.is_call_cursor,
      relation.is_call_directly_above_call_cursor,
      relation.is_below_indicated_call,
    ) {
    | (true, _, _) => ["cursor"]
    | (_, true, _) => ["cursor-caller"]
    | (_, _, Some(0)) => ["cursor-callee", "direct"]
    | (_, _, Some(_)) => ["cursor-callee", "indirect"]
    | (_, _, None) => ["cursor-unrelated"]
    };
  };
};

module Closures = {
  let num = (info: info): int =>
    switch (info.dynamics) {
    | Some(di) => List.length(di)
    | None => 0
    };

  let filter_frames_by_pin = (info, frames: list(closure)): list(closure) =>
    switch (DynCursor.s.pinned_call) {
    | Some(pinned_ap) =>
      List.filter(
        (closure: closure) =>
          /* Which do we want to show here? */
          //DynCursor.s.pinned_call == cur_call(info, closure)
          ListUtil.hd_opt(pinned_ap) == cur_ap(info)
          || ListUtil.is_suffix_of(pinned_ap, closure.call_stack),
        frames,
      )
    | None => frames
    };

  let select_frames =
      (model: model, info: info, closures: list(closure)): list(closure) =>
    closures
    |> filter_frames_by_pin(info)
    |> ListUtil.slice(model.index_offset, model.max_closures);

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

  // let sort_by_call_stack_size =
  //     (closures: list((int, closure))): list((int, closure)) => {
  //   List.stable_sort(
  //     ((_, c1: closure), (_, c2: closure)) =>
  //       compare(List.length(c1.call_stack), List.length(c2.call_stack)),
  //     closures,
  //   );
  // };

  let group =
      (closures: list((int, closure))): list(list((int, closure))) => {
    let grouped =
      closures |> group_by_predicate(is_same_call) |> List.map(List.rev);
    // |> List.map(sort_by_call_stack_size);
    /* Flatten if all groups are singletons */
    List.for_all(group => List.length(group) == 1, grouped)
      ? [List.concat(grouped)] : grouped;
  };

  let collate =
      (model: model, info: info, di: list(closure))
      : (int, list(list((int, closure)))) => {
    let closures = select_frames(model, info, di);
    let numbered_closures =
      List.mapi((i, c) => (List.length(closures) - i - 1, c), closures);
    (List.length(closures), group(numbered_closures));
  };
};

module Debug = {
  let stack = (stack: Probe.call_stack): string =>
    stack |> List.map(Id.str3) |> String.concat("\n");

  let str = (info, closure: closure): string =>
    //"closure_id: "
    //++ of_id(closure.closure_id)
    // ++ "\nenv_id: "
    // ++ of_id(closure.env_id)
    //++
    "ap:"
    ++ (
      switch (cur_call(info, closure)) {
      | Some([ap_id, ..._]) => Id.str3(ap_id)
      | _ => "None"
      }
    )
    ++ "\nstack:\n"
    ++ stack(closure.call_stack);
  // ++ "DynCursor:\n"
  // ++ String.concat("\n", DynCursor.clss(info, closure));
  // ++ "\nstack:\n"
  // ++ stack(closure.stack);
};

let seg_view = (view_seg, utility: utility, available: int, seg: Exp.t) =>
  seg
  |> DHExp.strip_casts
  |> Abbreviate.abbreviate_exp(~available)
  |> PairUtil.map_fst(e => TermBase.Exp(e))
  |> PairUtil.map_fst(utility.term_to_seg)
  |> PairUtil.map_fst(view_seg(Sort.Exp));

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

let display_length = (model: model, id: Id.t): int =>
  Id.Map.find_opt(id, model.display_lengths) |> Option.value(~default=12);

let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

let click_coords: ref(option(Point.t)) = ref(Option.None);

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
    let target = e##.target |> Js.Opt.get(_, _ => failwith("no target"));
    JsUtil.setPointerCapture(target, e##.pointerId) |> ignore;
    mousedown := Some(target);
    click_coords := Some({row: e##.clientY, col: e##.clientX});
    DynCursor.capture_cursor(closure);
    DynCursor.capture_ap(info);
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
      let goal = pos_rel_to_target(e);
      local(ChangeLength(closure.closure_id, goal.col));
    | _ => Effect.Ignore
    };

  let (view, length) =
    seg_view(
      view_seg,
      utility,
      display_length(model, closure.closure_id),
      closure.value,
    );

  div(
    ~attrs=[
      Attr.title(Debug.str(info, closure)),
      Attr.classes(
        ["value"]
        @ DynCursor.clss(info, closure)
        @ (length > 5 ? ["long"] : [])
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : []),
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
    (view_seg, utility: utility, en: Dynamics.Probe.Env.entry): Node.t => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.binding.name ++ "="),
      switch (en.value) {
      | Opaque => Node.text("Opaque")
      | Val(d) => seg_view(view_seg, utility, 12, d) |> fst
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
    |> List.map(env_val(view_seg, utility)),
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
    @ (is_var_ref(info) ? [] : [env_view(closure, view_seg, utility)]),
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

let offside_view =
    (model: model, info: info, local, view_seg, utility: utility) =>
  Node.div(
    ~attrs=[Attr.classes(["live-offside"])],
    switch (info.dynamics) {
    | Some(di) =>
      let (num_shown, groups) = Closures.collate(model, info, di);
      let is_cut_off = num_shown != Closures.num(info) && num_shown > 0;
      let extras = [nav_bar_view(model, di, local), ellipsis_view(local)];
      (num_shown > 0 ? [equals_view] : [])
      @ closure_group_view(info, utility, view_seg, model, local, groups)
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
  DynCursor.s.pinned_call != None
  && DynCursor.s.pinned_call == cur_call(info, closure)
    ? [div(~attrs=[Attr.classes(["pin"])], [])] : [];

let syntax_str = (info: info) => {
  let max_len = 30;
  let seg =
    switch (Segment.unparenthesize(info.syntax)) {
    | Some(seg) => seg
    | None => [info.syntax]
    };
  let str = Printer.of_segment(~holes=Some("?"), seg);
  let str = Re.Str.global_replace(Re.Str.regexp("\n"), " ", str);
  String.length(str) > max_len ? String.sub(str, 0, max_len) ++ "..." : str;
};

let syntax_view = (info: info) => info |> syntax_str |> text;

let placeholder = (_m, info) =>
  ProjectorCore.inline(3 + String.length(syntax_str(info)));

let icon = div(~attrs=[Attr.classes(["icon"])], []);

let view = (info: info): Node.t => {
  let on_double_click = _ => {
    //DynCursor.reset();
    switch (DynCursor.s.pinned_call) {
    | Some(pinned_ap) when ListUtil.hd_opt(pinned_ap) == cur_ap(info) =>
      /* already pinned case */
      DynCursor.s.pinned_call = None
    | Some(_)
    | None => DynCursor.pin_call(info)
    };
    Effect.Ignore;
  };

  let on_pointerdown = _ => {
    switch (info.dynamics) {
    | Some(di) when DynCursor.has(di) != None =>
      /* If the cursor is already on one of this probe's closures */
      DynCursor.capture_ap(info)
    | Some(di) =>
      switch (di) {
      | [first_closure, ..._] =>
        DynCursor.capture_cursor(first_closure);
        DynCursor.capture_ap(info);
      | [] => DynCursor.capture_ap(info)
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
        @ (DynCursor.is_pinned(info) ? ["pinned"] : []),
      ),
      Attr.on_double_click(on_double_click),
      Attr.on_pointerdown(on_pointerdown),
    ],
    [syntax_view(info), icon],
  );
};

let overlay_view = (info: info): Node.t => {
  let cursored_closure = OptUtil.and_then(DynCursor.has, info.dynamics);
  div(
    ~attrs=[
      Attr.classes(
        ["overlay"]
        @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        @ (DynCursor.is_pinned(info) ? ["pinned"] : []),
      ),
    ],
    [num_closures_view(info)]
    @ (
      switch (cursored_closure) {
      | Some(cursored_closure) => pin_view(info, cursored_closure)
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
    switch (DynCursor.s.pinned_call) {
    | Some(_) => DynCursor.s.pinned_call = None
    | None => DynCursor.s.pinned_call = Some(id)
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
  let can_project = (_, any: Term.Any.t) =>
    switch (any) {
    | Exp(_) => true
    | Pat(_) => true
    | _ => false
    };

  let can_focus = false;
  let dynamics = true;
  let placeholder = placeholder;
  let update = update;
  let view = (_model, info, ~local as _, ~parent as _, ~view_seg as _) =>
    view(info);
  let offside_view =
    Some(
      (model, info, ~local, ~parent as _, ~view_seg) =>
        offside_view(model, info, local, view_seg, info.utility),
    );
  let overlay_view =
    Some(
      (_model, info, ~local as _, ~parent as _, ~view_seg as _) =>
        overlay_view(info),
    );
  let underlay_view = Option.None; //TODO
  let focus = _ => ();
};
