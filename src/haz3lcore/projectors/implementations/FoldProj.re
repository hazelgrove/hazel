open Util;
open ProjectorInterface;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed_m) = {
  [@default "⋱"]
  text: string,
  ed: 'ed_m,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  |;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

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

  let mk = (_any: Language.Term.Any.t, ed) => {
    open OptUtil.Syntax;
    let+ ed = ed();
    {
      text: "⋱",
      ed,
    };
  };

  let dynamics = false;

  let placeholder = (~common as _, ~id as _, m: model') =>
    ProjectorShape.inline(m.text == "⋱" ? 2 : String.length(m.text));

  let update = (~common as _, ~sort as _, ~id as _, m, _) => m;

  let mk_term = (~sort, ~prev, {text, ed}) => {
    let (ed, t) = Editor.Update.make_term(~sort, ed);
    (
      {
        text,
        ed,
      },
      Calc.update(t, Fun.id, prev),
    );
  };

  let calculate = (~common, {text, ed}) => {
    text,
    ed: Editor.Update.calculate(~common, ed),
  };

  let get_cursor_info = ProjectorInterface.Defaults.get_cursor_info;

  let hover_view = (~common, sort, ed: 'ed_m) =>
    div(
      ~attrs=[Attr.class_("hover-view")],
      [
        Editor.View.view(~common, ~mode=ReadOnly, ~sort, ~background=true, ed),
      ],
    );

  let view =
      (
        ~common: Common.t,
        ~inject as _,
        ~escape,
        ~take_focus as _,
        ~focus as _,
        ~info,
        m,
      )
      : View.t =>
    View.mk(
      div(
        ~attrs=[Attr.on_double_click(_ => escape(Remove))],
        [text(m.text), hover_view(~common, info.sort, m.ed)],
      ),
    );

  let unproject = (model: model(_)) => model.ed;
};
