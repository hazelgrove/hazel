open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

/* A theorem's proof, in a drawer below the theorem. The web layer renders
   the proof (View.drawers) and counts its rows (DrawerFit.set_computed).
   Placed only by AutoProbePerform.update_proofs. */

/* the theorem whose proof this drawer shows: its own anchor, unless the
   drawer sits under a theorem cell's statement */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default None]
  theorem: option(Id.t),
};

let model_string = (~theorem=?, ()): string =>
  {theorem: theorem} |> sexp_of_t |> Sexplib.Sexp.to_string;

let theorem_of = (m: t, info: info): Id.t =>
  Option.value(m.theorem, ~default=info.id);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = _ => None;
  let focusable = Focusable.non;
  let elaborate_syntax = false;

  let placeholder = (m: model, info: info) =>
    DrawerFit.shape(theorem_of(m, info));
  let update = (m, _, ()) => m;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({model, info, drawers, _}: View.args(model, action)) => {
    let theorem = theorem_of(model, info);
    View.{
      inline: Node.div([]),
      overlay: None,
      offside: None,
      below:
        Some(
          Node.div(
            ~attrs=
              [
                Attr.classes(["proof-drawer"]),
                Attr.create("data-drawer-id", Id.to_string(theorem)),
              ]
              @ ProbeProj.keep_clicks,
            switch (drawers.proof(theorem)) {
            | Some(proof) => [proof]
            | None => []
            },
          ),
        ),
      error: false,
    };
  };
};
