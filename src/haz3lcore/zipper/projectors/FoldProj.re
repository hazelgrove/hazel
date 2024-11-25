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
let vals = (di: option(Dynamics.Info.t)) => {
  switch (di) {
  | Some(di) =>
    List.map(
      (d: DHExp.t) =>
        d |> DHExp.strip_casts |> (d => d.term) |> TermBase.Exp.show_term,
      di.vals,
    )
    |> String.concat(", ")
  | _ => "Nein"
  };
};

let vals = (di: option(Dynamics.Info.t)) => {
  switch (di) {
  | Some(di) =>
    List.map(
      (d: DHExp.t) =>
        d
        |> DHExp.strip_casts
        |> ExpToSegment.exp_to_segment(
             ~settings={
               inline: false,
               fold_case_clauses: false,
               fold_fn_bodies: false,
               hide_fixpoints: false,
               fold_cast_types: false,
             },
           )
        |> Printer.of_segment(~holes=None),
      di.vals,
    )
    |> String.concat(", ")
  | _ => "Nein"
  };
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = {text: "⋱"};
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = (m, _) =>
    Inline(m.text == "⋱" ? 2 : m.text |> String.length);
  let update = (m, _) => m;
  let view = (m: model, ~info, ~local as _, ~parent) =>
    div(
      ~attrs=[
        Attr.on_double_click(_ => parent(Remove)),
        Attr.title(vals(info.dynamics)),
      ],
      [text(m.text)],
    );
  let focus = _ => ();
};
