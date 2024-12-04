open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
};

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
      Node.text(en.name ++ " = "),
      switch (en.raw) {
      | Opaque => Node.text("Opaque")
      | Val(d) => d |> bonus_pack.exp_to_seg |> bonus_pack.view(Exp)
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

let vals_div = (di: option(Dynamics.Info.t), bonus_pack: bonus_pack) => {
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
            ~attrs=[Attr.classes(["wrap"])],
            [
              pi.value
              |> DHExp.strip_casts
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

let view = (_m: t, ~info, ~local as _, ~parent, ~bonus_pack: bonus_pack) =>
  div(
    ~attrs=[
      Attr.on_double_click(_ => parent(Remove)),
      Attr.title(vals(info.dynamics, bonus_pack.exp_to_seg)),
    ],
    [vals_div(info.dynamics, bonus_pack), text("🔍 " ++ code_str(info))],
  );

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = {text: "🔍"};
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = (_m, info) =>
    Inline(3 + String.length(code_str(info)));
  let update = (m, _) => m;
  let view = view;
  let focus = _ => ();
};
