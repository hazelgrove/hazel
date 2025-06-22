open Util;
open Virtual_dom.Vdom;
open Language;
open ProjectorInterface;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = float;
[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | Set(float);
[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

let init = (~copy_ed as _, any: Term.Any.t, _ed) =>
  switch (any) {
  | Exp({term: Atom(Float(f)), _}) => Some(f)
  | _ => None
  };

let mk_term =
    (~mk_term_ed as _, ~sort as _, ~prev as _, m)
    : (model('a), Calc.t(Any.t)) => (
  m,
  NewValue(Exp(Atom(Float(m)) |> Exp.fresh)),
);

let view =
    (
      ~common as _,
      ~ed_str as _,
      ~view_ed as _,
      ~view_editable as _,
      ~enter_ed as _,
      ~mk_ed as _,
      ~mk_term_ed as _,
      ~calculate_ed as _,
      ~local,
      ~parent as _,
      ~focus as _,
      ~focussed as _,
      model,
      _info,
    )
    : View.t =>
  View.mk(
    Util.WebUtil.range(
      ~attrs=[Attr.on_input((_, v) => local(Set(float_of_string(v))))],
      model |> Printf.sprintf("%.2f"),
    ),
  );

module M =
       (Editor: EditorInterface.EDITOR)

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
    | Exp({term: Atom(Float(f)), _}) => Some(f)
    | _ => None
    };

  let dynamics = false;

  let placeholder = (~common as _, ~id as _, _model) =>
    ProjectorShape.inline(10);

  let update = (~common as _, ~sort as _, ~id as _, _, Set(f)) => f;

  let mk_term = (~sort as _, ~prev as _, m): (model', Calc.t(Any.t)) => (
    m,
    NewValue(Exp(Atom(Float(m)) |> Exp.fresh)),
  );

  let calculate = Defaults.calculate;

  let get_cursor_info = Defaults.get_cursor_info;

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
    ProjectorInterface.View.{
      inline:
        Util.WebUtil.range(
          ~attrs=[
            Attr.on_input((_, v) => inject(Set(float_of_string(v)))),
          ],
          model |> Printf.sprintf("%.2f"),
        ),
      offside: None,
      overlay: None,
      enter_left: None,
      enter_right: None,
    };
};
