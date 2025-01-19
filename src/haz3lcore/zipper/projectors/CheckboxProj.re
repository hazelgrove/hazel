open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

let bool_of = (any: Any.t): option(bool) =>
  switch (any) {
  | Exp({term: Bool(b), _}) => Some(b)
  | _ => None
  };

let get = (utility: utility, syntax: syntax): bool =>
  switch (bool_of(utility.seg_to_term([syntax]))) {
  | Some(b) => b
  | None => failwith("Checkbox: not boolean literal")
  };

let toggle = (any: Any.t): Any.t =>
  switch (any) {
  | Exp({term: Bool(b), _} as t) => Exp({...t, term: Bool(!b)})
  | e => e
  };

let view =
    (
      _,
      info,
      ~local as _,
      ~parent: external_action => Ui_effect.t(unit),
      ~utility,
    ) => {
  Node.input(
    ~attrs=
      [
        Attr.create("type", "checkbox"),
        Attr.on_input((_, _) =>
          parent(SetSyntax(utility.lift_syntax(toggle, info.syntax)))
        ),
      ]
      @ (get(utility, info.syntax) ? [Attr.checked] : []),
    (),
  );
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = (_, any: Term.Any.t) => bool_of(any) != None;
  let can_focus = false;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorShape.inline(2);
  let update = (model, _, _) => model;
  let view = view;
  let offside_view = Option.None;
  let overlay_view = Option.None;
  let underlay_view = Option.None;
  let focus = _ => ();
};
