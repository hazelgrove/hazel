open Js_of_ocaml_tyxml.Tyxml_js;

let make =
    (code_history_rs: Model.code_history_rs): Html.elt([> Html_types.div]) => {
  let make_boxes = (history: CodeHistory.t) => {
    /* We can't copy HTML element values: TyXML is side-effectful, so we need to instantiate individually. */
    let non_move_actions =
      List.filter(
        fun
        | Action.MoveTo(_)
        | Action.MoveToNextHole
        | Action.MoveToPrevHole => false
        | _ => true,
        history.actions,
      );

    List.map(
      action => Html.p([Html.txt(Action.show(action))]),
      non_move_actions,
    );
  };
  let action_list_rs = React.S.map(make_boxes, code_history_rs);

  Html.(
    div(
      ~a=[a_class(["panel", "history-panel"])],
      [
        Panel.main_title_bar("history"),
        R.Html.div(
          ~a=[a_class(["panel-body"])],
          ReactiveData.RList.from_signal(action_list_rs),
        ),
      ],
    )
  );
};
