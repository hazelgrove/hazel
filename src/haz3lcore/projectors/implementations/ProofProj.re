open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

/* A theorem's proof, in a drawer below the theorem. The web layer renders
   the proof (Settings.view) and fits the drawer to it (Settings.rows).
   Placed only by AutoProbePerform.update_proofs. */

module Settings = {
  /* Set by the web layer each render: a theorem's proof, by its id */
  let view: ref(Id.t => option(Node.t)) = ref(_ => None);
  /* the rows each theorem's proof takes, as computed from its stepper
     and as measured once rendered (which catches what the count misses,
     like induction cases); a drawer takes the larger, so the two never
     undo each other. a change bumps ProbeProj.Settings.layout so drawers
     re-lay out */
  let computed: Hashtbl.t(Id.t, int) = Hashtbl.create(8);
  let measured: Hashtbl.t(Id.t, int) = Hashtbl.create(8);
  let rows = (id: Id.t): int =>
    max(
      Hashtbl.find_opt(computed, id) |> Option.value(~default=1),
      Hashtbl.find_opt(measured, id) |> Option.value(~default=1),
    );
  let set = (table, id: Id.t, n: int): bool => {
    let before = rows(id);
    Hashtbl.replace(table, id, n);
    if (rows(id) == before) {
      false;
    } else {
      ProbeProj.Settings.layout := ProbeProj.Settings.layout^ + 1;
      ProbeProj.Settings.version := ProbeProj.Settings.version^ + 1;
      true;
    };
  };
  let set_computed = set(computed);
  let set_measured = set(measured);
};

let model_string = "()";

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = _ => None;
  let focusable = Focusable.non;
  let elaborate_syntax = false;

  let placeholder = ((), info: info) =>
    ProjectorCore.Shape.{
      horizontal: 0,
      vertical:
        Tab(
          Settings.rows(info.id)
          |> max(1)
          |> min(ProbeProj.DrawerHeight.max_rows),
        ),
    };
  let update = (m, _, ()) => m;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({info, _}: View.args(model, action)) =>
    View.{
      inline: Node.div([]),
      overlay: None,
      offside: None,
      below:
        Some(
          Node.div(
            ~attrs=[
              Attr.classes(["proof-drawer"]),
              Attr.create("data-proof-id", Id.to_string(info.id)),
              /* the proof's own clicks: the editor below would take the
                 pointer (and the caret) first */
              Attr.on_pointerdown(_ => Effect.Stop_propagation),
              Attr.on_mousedown(_ => Effect.Stop_propagation),
            ],
            switch (Settings.view^(info.id)) {
            | Some(proof) => [proof]
            | None => []
            },
          ),
        ),
      error: false,
    };
};
