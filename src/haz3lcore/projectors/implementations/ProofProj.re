open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

/* A theorem's proof, in a drawer below the theorem. The web layer renders
   the proof (Settings.view) and fits the drawer to it (Settings.rows).
   Placed only by AutoProbePerform.update_proofs. */

module Settings = {
  /* Set by the web layer each render: a theorem's proof, by its id */
  let view: ref(Id.t => option(Node.t)) = ref(_ => None);
  /* the rows each theorem's proof takes, as counted from its stepper;
     DrawerFit adds what rendering measures */
  let computed: Hashtbl.t(Id.t, int) = Hashtbl.create(8);
  let rows = (id: Id.t): int =>
    DrawerFit.rows(
      id,
      Hashtbl.find_opt(computed, id) |> Option.value(~default=1),
    );
  /* true when the drawer's rows changed */
  let set_computed = (id: Id.t, n: int): bool => {
    let before = rows(id);
    Hashtbl.replace(computed, id, n);
    if (rows(id) == before) {
      false;
    } else {
      DrawerFit.layout := DrawerFit.layout^ + 1;
      ProbeProj.Settings.version := ProbeProj.Settings.version^ + 1;
      true;
    };
  };
};

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
    ProjectorCore.Shape.{
      horizontal: 0,
      vertical:
        Tab(
          Settings.rows(theorem_of(m, info))
          |> max(1)
          |> min(ProbeProj.DrawerHeight.max_rows),
        ),
    };
  let update = (m, _, ()) => m;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({model, info, _}: View.args(model, action)) => {
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
            switch (Settings.view^(theorem)) {
            | Some(proof) => [proof]
            | None => []
            },
          ),
        ),
      error: false,
    };
  };
};
