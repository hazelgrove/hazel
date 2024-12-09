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
};

[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | ChangeLength(int);

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

let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

let vals_div =
    (di: option(Dynamics.Info.t), ~model, ~local, ~bonus_pack: bonus_pack) => {
  //dprint_endline("available:" ++ string_of_int(model.len));
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
              Attr.classes(["wrap"]),
              Attr.on_pointerdown(e => {
                // print_endline("pointerdown");
                let target =
                  e##.target |> Js.Opt.get(_, _ => failwith("no target"));
                JsUtil.setPointerCapture(target, e##.pointerId) |> ignore;
                mousedown := Some(target);
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
              env_div2(pi, bonus_pack),
            ],
          )
        },
        di.vals,
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
    ~attrs=[], //Attr.on_double_click(_ => parent(Remove)),
    //Attr.title(vals(info.dynamics, bonus_pack.exp_to_seg)),
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
  let init = {text: "🔍", len: 12};
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = (_m, info) =>
    Inline(3 + String.length(code_str(info)));
  let update = (m, a) => {
    print_endline("update: action:" ++ show_a(a));
    let ChangeLength(len) = a;
    if (len > (-1)) {
      {...m, len};
    } else {
      m;
    };
  };
  let view = view;
  let focus = _ => ();
};
