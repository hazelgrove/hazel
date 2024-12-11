open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;
open Js_of_ocaml;

//let _ = Zipper.base_point; //not cyclical
//let _ = Editor.show; //cyclical

/* Plan for extended dynamics:
   in principle we could track:
   - prev 'stack frame': id of prev expr that began execution?
   - env (or just the part of the env that's relevant to the current expr)
     aka the co-ctx

    */

/* Plan for selectable (editable as well maybe):

   selectable:
   create own mini version of editor, use it as model
   update fn updates model after applying mini editor action
   hopefully can use same actions
    */

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

let stack = (di: option(Dynamics.Info.t)) => {
  switch (di) {
  | Some(di) =>
    List.map(
      (pi: Dynamics.Probe.Info.t) => pi.stack |> Probe.show_stack,
      di.vals,
    )
    |> String.concat(", ")
  | _ => "Nein"
  };
};

/* given two env_cursors,return their maximum common suffix */
let max_common_suffix = (a: list('a), b: list('a)) => {
  let rec loop = (a, b, acc) =>
    switch (a, b) {
    | ([], _)
    | (_, []) => acc
    | ([ha, ...ta], [hb, ...tb]) when ha == hb =>
      loop(ta, tb, [ha, ...acc])
    | _ => acc
    };
  loop(List.rev(a), List.rev(b), []);
};

let env_cursor: ref(list(Id.t)) = ref([]);
let last_target: ref(list('a)) = ref([]);
let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);
let env_cursor_of_stack = List.map((en: Probe.frame) => en.env_id);

let show_indicator = stack => {
  let local = env_cursor_of_stack(stack);
  env_cursor^ == local
  || (max_common_suffix(env_cursor^, local) != [] || local == [])
  && List.length(env_cursor^) != List.length(local);
};

let common_suffix_length = (s1, s2) =>
  List.length(max_common_suffix(s1, s2));

let comparor = (a: Dynamics.Probe.Info.t, b: Dynamics.Probe.Info.t) => {
  compare(
    common_suffix_length(env_cursor^, env_cursor_of_stack(b.stack)),
    common_suffix_length(env_cursor^, env_cursor_of_stack(a.stack)),
  );
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
    (~resizable=false, model, utility, local, pi: Dynamics.Probe.Info.t) =>
  div(
    ~attrs=[
      Attr.classes(
        ["val-resize"] @ (show_indicator(pi.stack) ? ["cursor"] : []),
      ),
      Attr.on_pointerdown(e => {
        // print_endline("pointerdown");
        let target = e##.target |> Js.Opt.get(_, _ => failwith("no target"));
        JsUtil.setPointerCapture(target, e##.pointerId) |> ignore;
        mousedown := Some(target);
        if (last_target^ == [target]) {
          env_cursor := [];
          last_target := [];
        } else {
          env_cursor := env_cursor_of_stack(pi.stack);
          last_target := [target];
        };
        Effect.Ignore;
      }),
      Attr.on_pointerup(e => {
        // print_endline("pointerup");
        switch (mousedown^) {
        | Some(target) =>
          JsUtil.releasePointerCapture(target, e##.pointerId) |> ignore
        | None => ()
        };
        mousedown := None;
        Effect.Ignore;
      }),
      Attr.on_mousemove(e =>
        switch (mousedown^) {
        | Some(_elem) when Js.to_bool(e##.shiftKey) && resizable =>
          /* Ideally this would be onpointermove and we could just use hasPointerCapture... */
          let goal = get_goal(utility, e);
          local(ChangeLength(goal.col));
        | _ =>
          // print_endline("mousemove:up");
          Effect.Ignore
        }
      ),
    ],
    [seg_view(utility, model.len, pi.value)],
  );

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

let rm_opaques =
  List.filter_map((en: Dynamics.Probe.Env.entry) =>
    switch (en.raw) {
    | Opaque => None
    | Val(_) => Some(en)
    }
  );

let is_var_ref = info =>
  switch (MakeTerm.go([info.syntax]).term.term) {
  | Var(_) => true
  | _ => false
  };

let env_view = (pi: Dynamics.Probe.Info.t, utility: utility) =>
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

let num_closures_view = (info: info) =>
  div(
    ~attrs=[Attr.classes(["num-closures"])],
    [text(string_of_int(num_closures(info)))],
  );

let code_str = (info: info) =>
  [info.syntax] |> Printer.of_segment(~holes=None);

let icon = div(~attrs=[Attr.classes(["icon"])], [text("🔍")]);

let view = (model: t, ~info, ~local, ~parent as _, ~utility: utility) =>
  div([
    //Attr.title(stack(info.dynamics)),
    offside_view(info, ~model, ~local, ~utility),
    div(
      ~attrs=[
        Attr.classes(["main"]),
        Attr.on_double_click(_ => local(ToggleShowAllVals)),
      ],
      [icon, text(code_str(info)), num_closures_view(info)],
    ),
  ]);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;
  let init = {text: "🔍", len: 12, show_all_vals: true};
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = (_m, info) =>
    Inline(3 + String.length(code_str(info)));
  let update = (m, a) => {
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
  let view = view;
  let focus = _ => ();
};
