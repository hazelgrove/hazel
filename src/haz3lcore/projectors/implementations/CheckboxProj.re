open Util;
open ProjectorInterface;
open Virtual_dom.Vdom;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed_m) = bool;

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | Toggle;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

module M =
       (Editor: ProjectorInterface.EDITOR)

         : (
           ProjectorInterface.PROJECTOR with
             type model' = model(Editor.model) and
             type action' = action(Editor.action) and
             type focus' = focus(Editor.focus) and
             type editor_model = Editor.model
       ) => {
  type editor_model = Editor.model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model' = model(Editor.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action' = action(Editor.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus' = focus(Editor.focus);

  let mk = (any: Term.Any.t, _ed: unit => option(Editor.model)) =>
    switch (any) {
    | Exp({term: Atom(Bool(b)), _}) => Some(b)
    | _ => None
    };

  let dynamics = false;

  let placeholder = (~common as _, ~id as _, _model) =>
    ProjectorShape.inline(2);

  let update = (~common as _, ~sort as _, ~id as _, b, Toggle) => !b;

  let mk_term = (~sort as _, ~prev as _, m): (model('a), Calc.t(Any.t)) => (
    m,
    NewValue(Exp(Atom(Bool(m)) |> Exp.fresh)),
  );

  let calculate = (~common as _, model) => model;

  let get_cursor_info =
      (~common as _, ~inject as _, ~read_only as _, _model, _focus) => Cursor.empty;

  let view =
      (
        ~common as _,
        ~inject,
        ~escape as _,
        ~take_focus as _,
        ~focus as _,
        ~id as _,
        model,
      ) =>
    View.mk(
      Node.input(
        ~attrs=
          [
            Attr.create("type", "checkbox"),
            Attr.on_input((_, _) => inject(Toggle)),
          ]
          @ (model ? [Attr.checked] : []),
        (),
      ),
    );
};
