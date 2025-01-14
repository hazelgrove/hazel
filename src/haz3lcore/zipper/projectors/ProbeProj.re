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

let stack = stack =>
  stack
  |> List.rev
  |> List.map(({env_id, ap_id}: Probe.frame) =>
       ""
       ++ String.sub(Id.to_string(env_id), 0, 2)
       ++ " : "
       ++ String.sub(Id.to_string(ap_id), 0, 2)
     )
  |> String.concat("\n");

let env_cursor: ref(list(Id.t)) = ref([]);
let last_target: ref(list('a)) = ref([]);
let cur_ap: ref(option(Id.t)) = ref(Option.None);
let cur_ap_depth: ref(option(int)) = ref(Option.None);
let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

let comparor = (a: Dynamics.Probe.Closure.t, b: Dynamics.Probe.Closure.t) => {
  compare(
    ListUtil.common_suffix_length(env_cursor^, Probe.env_stack(b.stack)),
    ListUtil.common_suffix_length(env_cursor^, Probe.env_stack(a.stack)),
  );
};

let show_indicator = stack => {
  let local = Probe.env_stack(stack);
  env_cursor^ == []
  && local == []
  || env_cursor^ != []
  && ListUtil.one_is_suffix_of_other(env_cursor^, local);
};

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

let cur_ap_id = (info: info) =>
  switch (info.statics) {
  | Some(InfoExp({term: {term: Ap(_), _} as ap, _})) =>
    Some(Term.Exp.rep_id(ap))
  | Some(InfoExp({term: {term: Wrap({term: Ap(_), _} as ap, _), _}, _})) =>
    Some(Term.Exp.rep_id(ap))
  | _ => None
  };

let depth_in_stack = (dyn_stack): option(int) =>
  List.find_index(
    ({ap_id, _}: Probe.frame) => Some(ap_id) == cur_ap^,
    dyn_stack,
  );

let value_view =
    (info: info, model, utility, local, closure: Dynamics.Probe.Closure.t) => {
  let val_pointerdown = (e: Js.t(Dom_html.pointerEvent)) => {
    let target = e##.target |> Js.Opt.get(_, _ => failwith("no target"));
    JsUtil.setPointerCapture(target, e##.pointerId) |> ignore;
    mousedown := Some(target);
    env_cursor := Probe.env_stack(closure.stack);
    last_target := [target];
    cur_ap := cur_ap_id(info);
    cur_ap_depth := Some(List.length(closure.dyn_stack));
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
      Attr.title(
        "dyn_stack:\n"
        ++ stack(closure.dyn_stack)
        ++ "\nstack:\n"
        ++ stack(closure.stack),
      ),
      Attr.classes(
        ["val-resize"]
        @ (
          switch (depth_in_stack(closure.dyn_stack)) {
          | Some(0) => ["dyn-cursor"]
          | Some(_) => ["dyn-cursor", "light"]
          | None => []
          }
        )
        @ (Option.is_some(cur_ap_id(info)) ? ["ap"] : [])
        @ (
          switch (cur_ap_depth^) {
          | Some(0) => ["top-ap"]
          | _ => []
          }
        )
        @ (show_indicator(closure.stack) ? ["cursor"] : []),
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

let select_vals = (model: model, vals) => {
  switch (List.sort(comparor, vals)) {
  | [] => []
  | _ =>
    vals
    |> ListUtil.remove_first_n(model.index_offset)
    |> ListUtil.truncate(model.max_closures)
  };
};

let offside_pos = utility =>
  Attr.create(
    "style",
    Printf.sprintf("position: absolute; left: %fpx;", utility.offside_offset),
  );

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

let offside_view = (info, ~model: model, ~local, ~utility: utility) => {
  Node.div(
    ~attrs=[Attr.classes(["live-offside"]), offside_pos(utility)],
    switch (info.dynamics) {
    | Some(di) =>
      let vals = select_vals(model, di);
      let left_cond =
        model.index_offset >= List.length(di) - model.max_closures;
      let right_cond = model.index_offset <= 0;
      nav_back(di, model, local, left_cond)
      @ nav_forward(di, model, local, right_cond)
      @ List.map(closure_view(info, utility, model, local), vals);
    | _ => []
    },
  );
};

let num_closures = (info: info) =>
  switch (info.dynamics) {
  | Some(di) => List.length(di)
  | _ => 0
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
  Inline(3 + String.length(syntax_str(info)));

// let icon = div(~attrs=[Attr.classes(["icon"])], [text("🔍")]);
let icon = div(~attrs=[Attr.classes(["icon"])], []);

let view = (model: model, ~info, ~local, ~parent as _, ~utility: utility) => {
  div([
    offside_view(info, ~model, ~local, ~utility),
    div(
      ~attrs=[
        Attr.classes(
          ["main"] @ (Option.is_some(cur_ap_id(info)) ? ["ap"] : []),
        ),
        Attr.on_click(_ => {
          env_cursor := [];
          cur_ap := None;
          Effect.Ignore;
        }),
      ],
      [syntax_view(info), icon, num_closures_view(info)],
    ),
  ]);
};

let update = (m: model, a: action) => {
  print_endline("update: action:" ++ show_action(a));
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
  let focus = _ => ();
};
