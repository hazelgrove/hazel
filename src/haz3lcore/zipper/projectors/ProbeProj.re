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

/* Proof of concept value exposure. This isn't getting set immediately
   after folding for some reason */
let _vals = (di: option(Dynamics.Info.t)) => {
  switch (di) {
  | Some(di) =>
    List.map(
      (pi: Dynamics.Probe.Info.t) =>
        pi.value
        |> DHExp.strip_casts
        |> (d => d.term)
        |> TermBase.Exp.show_term,
      di.vals,
    )
    |> String.concat(", ")
  | _ => "Nein"
  };
};

let stack = (di: option(Dynamics.Info.t)) => {
  switch (di) {
  | Some(di) =>
    List.map(
      (pi: Dynamics.Probe.Info.t) => pi.stack |> TermBase.show_probe_stack,
      di.vals,
    )
    |> String.concat(", ")
  | _ => "Nein"
  };
};

let vals = (di: option(Dynamics.Info.t), exp_to_seg) => {
  switch (di) {
  | Some(di) =>
    List.map(
      (pi: Dynamics.Probe.Info.t) =>
        pi.value
        |> DHExp.strip_casts
        |> Abbreviate.abbreviate_exp
        |> exp_to_seg
        |> Printer.of_segment(~holes=None),
      di.vals,
    )
    |> String.concat(", ")
  | _ => "Nein"
  };
};

let env_val = (en: Dynamics.Probe.Env.entry, bonus_pack: bonus_pack) => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.name ++ "="),
      switch (en.raw) {
      | Opaque => Node.text("Opaque")
      | Val(d) =>
        d
        |> Abbreviate.abbreviate_exp
        |> fst
        |> bonus_pack.exp_to_seg
        |> bonus_pack.view(Exp)
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

let rm_fst = (xs: list('a)) =>
  switch (xs) {
  | [] => []
  | [_, ...rest] => rest
  };

let env_div2 = (pi: Dynamics.Probe.Info.t, bonus_pack: bonus_pack) => {
  Node.div(
    ~attrs=[Attr.classes(["live-env"])],
    pi.env |> rm_fst |> rm_opaques |> List.map(en => env_val(en, bonus_pack)),
  );
};

let env_div = (di: Dynamics.Info.t, bonus_pack: bonus_pack) => {
  Node.div(
    ~attrs=[Attr.classes(["live-env"])],
    List.concat_map(
      (pi: Dynamics.Probe.Info.t) => {
        pi.env |> List.map(en => env_val(en, bonus_pack))
      },
      di.vals,
    ),
  );
};

let env_cursor: ref(list(Id.t)) = ref([]);

let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

let env_cursor_of_stack = List.map((en: TermBase.probe_frame) => en.env_id);

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

let show_indicator = stack => {
  let local = stack |> env_cursor_of_stack;
  if (env_cursor^ == local) {
    true;
  } else if (max_common_suffix(env_cursor^, local) != []) {
    !(
      List.length(env_cursor^) == List.length(local) && env_cursor^ != local
    );
  } else {
    false;
  };
};

let comparor = (a: Dynamics.Probe.Info.t, b: Dynamics.Probe.Info.t) => {
  compare(
    List.length(
      max_common_suffix(env_cursor^, env_cursor_of_stack(b.stack)),
    ),
    List.length(
      max_common_suffix(env_cursor^, env_cursor_of_stack(a.stack)),
    ),
  );
};

let vals_div =
    (di: option(Dynamics.Info.t), ~model, ~local, ~bonus_pack: bonus_pack) => {
  //dprint_endline("available:" ++ string_of_int(model.len));
  let filter_vals = vals => {
    switch (List.sort(comparor, vals)) {
    | [] => []
    | [hd, ..._] when !model.show_all_vals => [hd]
    | _ => vals
    };
  };
  switch (di) {
  | Some(di) =>
    Node.div(
      ~attrs=[
        Attr.classes(["live-offside"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "position: absolute; left: %fpx;",
            bonus_pack.offside_offset,
          ),
        ),
      ],
      List.map(
        (pi: Dynamics.Probe.Info.t) => {
          // print_endline("pi.env:" ++ Dynamics.Probe.Env.show(pi.env));
          div(
            ~attrs=[
              Attr.classes(
                ["wrap"] @ (show_indicator(pi.stack) ? ["cursor"] : []),
              ),
              Attr.on_pointerdown(e => {
                // print_endline("pointerdown");
                let target =
                  e##.target |> Js.Opt.get(_, _ => failwith("no target"));
                JsUtil.setPointerCapture(target, e##.pointerId) |> ignore;
                mousedown := Some(target);
                env_cursor := pi.stack |> env_cursor_of_stack;
                Effect.Ignore;
              }),
              Attr.on_pointerup(e => {
                // print_endline("pointerup");
                switch (mousedown^) {
                | Some(target) =>
                  JsUtil.releasePointerCapture(target, e##.pointerId)
                  |> ignore
                | _ => ()
                };
                mousedown := None;
                Effect.Ignore;
              }),
              Attr.on_mousemove(e =>
                switch (mousedown^) {
                | Some(_elem) =>
                  /* Ideally this would be onpointermove and we could just use hasPointerCapture... */
                  // print_endline("mousemove:down");
                  let goal =
                    FontMetrics.get_goal(
                      ~font_metrics=bonus_pack.font_metrics,
                      e##.currentTarget
                      |> Js.Opt.get(_, _ => failwith(""))
                      |> JsUtil.get_child_with_class(_, "code")
                      |> Option.get,
                      e |> Js.Unsafe.coerce,
                    );
                  local(ChangeLength(goal.col));
                | _ =>
                  print_endline("mousemove:up");
                  Effect.Ignore;
                }
              ),
            ],
            [
              pi.value
              |> DHExp.strip_casts
              |> Abbreviate.abbreviate_exp(~available=model.len)
              |> fst
              |> bonus_pack.exp_to_seg
              |> bonus_pack.view(Exp),
              //pi.stack |> TermBase.show_probe_stack |> Node.text,
              env_div2(pi, bonus_pack),
            ],
          )
        },
        filter_vals(di.vals),
      ),
      //@ [env_div(di, bonus_pack)],
    )
  | _ =>
    Node.div(~attrs=[Attr.classes(["live-offside"])], [Node.text("?")])
  };
};

let _ = Zipper.base_point;
//let _ = Editor.show;

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

let code_str = (info: info) =>
  [info.syntax] |> Printer.of_segment(~holes=None);

let view = (model: t, ~info, ~local, ~parent as _, ~bonus_pack: bonus_pack) =>
  div(
    ~attrs=[
      Attr.title(stack(info.dynamics)),
      Attr.on_double_click(_ => local(ToggleShowAllVals)),
    ],
    [
      vals_div(info.dynamics, ~model, ~local, ~bonus_pack),
      text("🔍 " ++ code_str(info)),
    ],
  );

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
