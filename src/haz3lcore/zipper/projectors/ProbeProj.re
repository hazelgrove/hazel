open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;
open Js_of_ocaml;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
  len: int,
  show_all_vals: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | ChangeLength(int)
  | ToggleShowAllVals;

let stack = stack =>
  stack
  |> List.rev
  |> List.map(({env_id, ap_id, closure_id}: Probe.frame) =>
       ""
       ++ String.sub(Id.to_string(closure_id), 0, 2)
       ++ "\n"
       ++ String.sub(Id.to_string(env_id), 0, 2)
       ++ "\n"
       ++ String.sub(Id.to_string(ap_id), 0, 2)
     )
  |> String.concat("\n");

let env_cursor: ref(list(Id.t)) = ref([]);
let last_target: ref(list('a)) = ref([]);
let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

let common_suffix_length = (s1, s2) =>
  List.length(ListUtil.max_common_suffix(s1, s2));

let one_is_suffix_of_other = (s1, s2) =>
  common_suffix_length(s1, s2) == List.length(s1)
  || common_suffix_length(s1, s2) == List.length(s2);

let comparor = (a: Dynamics.Probe.Info.t, b: Dynamics.Probe.Info.t) => {
  compare(
    common_suffix_length(env_cursor^, Probe.env_stack(b.stack)),
    common_suffix_length(env_cursor^, Probe.env_stack(a.stack)),
  );
};

let show_indicator = stack => {
  let local = Probe.env_stack(stack);
  env_cursor^ == []
  && local == []
  || env_cursor^ != []
  && one_is_suffix_of_other(env_cursor^, local);
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

let resizable_val =
    (~resizable=false, model, utility, local, pi: Dynamics.Probe.Info.t) => {
  let val_pointerdown = (e: Js.t(Dom_html.pointerEvent)) => {
    let target = e##.target |> Js.Opt.get(_, _ => failwith("no target"));
    JsUtil.setPointerCapture(target, e##.pointerId) |> ignore;
    mousedown := Some(target);
    env_cursor := Probe.env_stack(pi.stack);
    last_target := [target];
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
    | Some(_elem) when Js.to_bool(e##.shiftKey) && resizable =>
      /* Ideally we could just use hasPointerCapture... */
      let goal = get_goal(utility, e);
      local(ChangeLength(goal.col));
    | _ => Effect.Ignore
    };

  div(
    ~attrs=[
      Attr.title(stack(pi.stack)),
      Attr.classes(
        ["val-resize"] @ (show_indicator(pi.stack) ? ["cursor"] : []),
      ),
      Attr.on_double_click(_ => local(ToggleShowAllVals)),
      Attr.on_pointerdown(val_pointerdown),
      Attr.on_pointerup(val_pointerup),
      Attr.on_mousemove(val_mousemove),
    ],
    [seg_view(utility, model.len, pi.value)],
  );
};

let env_val = (en: Dynamics.Probe.Env.entry, utility: utility) => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.name ++ "="),
      switch (en.raw) {
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
    switch (en.raw) {
    | Opaque => None
    | Val(_) => Some(en)
    }
  );

/* Is the underlying syntax a variable reference? */
let is_var_ref = (info: info): bool =>
  switch (MakeTerm.go([info.syntax]).term.term) {
  | Var(_) => true
  | _ => false
  };

let env_view = (pi: Dynamics.Probe.Info.t, utility: utility): Node.t =>
  Node.div(
    ~attrs=[Attr.classes(["live-env"])],
    pi.env |> rm_opaques |> List.map(en => env_val(en, utility)),
  );

let closure_view =
    (
      info,
      utility: utility,
      model: t,
      local,
      index,
      pi: Dynamics.Probe.Info.t,
    ) =>
  div(
    ~attrs=[
      Attr.classes(
        ["closure"] @ (show_indicator(pi.stack) ? ["cursor"] : []),
      ),
    ],
    [resizable_val(~resizable=index == 0, model, utility, local, pi)]
    @ (is_var_ref(info) ? [] : [env_view(pi, utility)]),
  );

let select_vals = (model: t, vals) => {
  switch (List.sort(comparor, vals)) {
  | [] => []
  | [hd, ..._] when !model.show_all_vals => [hd]
  | _ => vals
  };
};

let offside_pos = utility =>
  Attr.create(
    "style",
    Printf.sprintf("position: absolute; left: %fpx;", utility.offside_offset),
  );

let offside_view = (info, ~model, ~local, ~utility: utility) => {
  Node.div(
    ~attrs=[Attr.classes(["live-offside"]), offside_pos(utility)],
    switch (info.dynamics) {
    | Some(di) =>
      List.mapi(
        closure_view(info, utility, model, local),
        select_vals(model, di.vals),
      )
    | _ => []
    },
  );
};

let num_closures = (info: info) =>
  switch (info.dynamics) {
  | Some(di) => List.length(di.vals)
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

let icon = div(~attrs=[Attr.classes(["icon"])], [text("🔍")]);
// let icon =
//   img(
//     ~attrs=[
//       Attr.classes(["icon"]),
//       Attr.create("height", "18px"),
//       Attr.create("width", "18px"),
//       Attr.src("img/noun-search-5661270.svg"),
//       Attr.alt("probe"),
//     ],
//     (),
//   );

let view = (model: t, ~info, ~local, ~parent as _, ~utility: utility) =>
  div([
    offside_view(info, ~model, ~local, ~utility),
    div(
      ~attrs=[
        Attr.classes(["main"]),
        Attr.on_click(_ => {
          env_cursor := [];
          Effect.Ignore;
        }),
      ],
      [icon, syntax_view(info), num_closures_view(info)],
    ),
  ]);

let update = (m: t, a: a) => {
  print_endline("update: action:" ++ show_a(a));
  switch (a) {
  | ChangeLength(len) =>
    if (len > (-1)) {
      {...m, len};
    } else {
      m;
    }
  | ToggleShowAllVals => {...m, show_all_vals: !m.show_all_vals}
  };
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;
  let init = {text: "🔍", len: 12, show_all_vals: true};
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = placeholder;
  let update = update;
  let view = view;
  let focus = _ => ();
};
