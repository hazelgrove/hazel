open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = Bigint.t;
[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | Set(Bigint.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

let init = (~copy_ed as _, any: Term.Any.t, _ed) =>
  switch (any) {
  | Exp({term: Atom(Int(i)), _}) => Some(i)
  | _ => None
  };

let mk_term =
    (~mk_term_ed as _, ~sort as _, ~prev as _, m)
    : (model('a), Calc.t(Any.t)) => (
  m,
  NewValue(Exp(Atom(Int(m)) |> Exp.fresh)),
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
      ~attrs=[Attr.on_input((_, v) => local(Set(Bigint.of_string(v))))],
      model |> Bigint.to_string,
    ),
  );

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

  let mk = (any, ed) => init(~copy_ed=Editor.Model.copy, any, ed);
  let dynamics = false;
  let placeholder = (_, _) => ProjectorShape.inline(10);
  let update = (~common as _, ~sort as _, _info, _, Set(n)) => n;

  let mk_term = mk_term(~mk_term_ed=Editor.Update.make_term);

  let calculate = Calculate.default(~calculate_ed=Editor.Update.calculate);

  let get_cursor_info =
    CursorInfo.default(~get_cursor_info_ed=Editor.Focus.get_cursor_info);

  let view =
    view(
      ~ed_str=Editor.View.print_string,
      ~mk_ed=Editor.Model.mk,
      ~mk_term_ed=Editor.Update.make_term,
      ~calculate_ed=Editor.Update.calculate,
      ~view_ed=Editor.View.view,
      ~view_editable=Editor.View.view_editable,
      ~enter_ed=Editor.Focus.enter,
    );
};

let methods = {
  init,
  dynamics: false,
  placeholder: (~ed_size as _, _, _) => ProjectorShape.inline(10),
  update: (~update_ed as _, ~common as _, ~sort as _, _, _, Set(n)) => n,
  mk_term,
  view,
  calculate: Calculate.default,
  get_cursor_info: CursorInfo.default,
  sexp_of_model,
  model_of_sexp,
  yojson_of_model,
  model_of_yojson,
  sexp_of_action,
  action_of_sexp,
  yojson_of_action,
  action_of_yojson,
  sexp_of_focus,
  focus_of_sexp,
  yojson_of_focus,
  focus_of_yojson,
};
