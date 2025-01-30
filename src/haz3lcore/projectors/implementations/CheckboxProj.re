open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = ();

  let bool_of = (any: Any.t): option(bool) =>
    switch (any) {
    | Exp({term: Bool(b), _}) => Some(b)
    | _ => None
    };

  let get = (info: info): bool =>
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(b) =>
      switch (bool_of(b)) {
      | Some(b) => b
      | None => failwith("Checkbox: Get: not boolean literal")
      }
    | None => failwith("Checkbox: Get: not boolean literal")
    };

  let toggle_bool_lit: Any.t => Any.t =
    fun
    | Exp({term: Bool(b), _} as t) => Exp({...t, term: Bool(!b)})
    | _ => failwith("Checkbox: Toggle: not boolean literal");

  let toggle = (info): Base.segment =>
    switch (info.utility.lift_syntax(toggle_bool_lit, info.syntax)) {
    | Some(s) => s
    | None => failwith("Checkbox: Toggle: lift failed")
    };

  let can_project = (_, any: Term.Any.t) => bool_of(any) != None;

  let can_focus = false;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorCore.inline(2);
  let update = (model, _, _) => model;

  let view =
      (
        _,
        info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) =>
    Node.input(
      ~attrs=
        [
          Attr.create("type", "checkbox"),
          Attr.on_input((_, _) => parent(SetSyntax(toggle(info)))),
        ]
        @ (info |> get ? [Attr.checked] : []),
      (),
    );

  let offside_view = Option.None;
  let overlay_view = Option.None;
  let underlay_view = Option.None;
  let focus = _ => ();
};
