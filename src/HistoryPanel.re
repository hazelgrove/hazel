open Tyxml_js;
module Dom = Js_of_ocaml.Dom;
module KeyCombos = JSUtil.KeyCombos;

let make =
    (code_history_rs: Model.code_history_rs, undo, redo)
    : Html.elt([> Html_types.div]) => {
  let make_boxes = (history: CodeHistory.t) => {
    /* We can't copy HTML element values: TyXML is side-effectful, so we need to instantiate individually. */
    let non_move_actions =
      List.filter(action => !Action.is_move(action), history.actions);

    List.map(
      action => Html.p([Html.txt(Action.show(action))]),
      non_move_actions,
    );
  };
  let action_list_rs = React.S.map(make_boxes, code_history_rs);

  /* Create event listeners for keyboard shortcuts (these are side-effectful calls) */
  let on_undo = evt => {
    Dom.preventDefault(evt);
    undo();
  };
  let on_redo = evt => {
    Dom.preventDefault(evt);
    redo();
  };
  let _ = JSUtil.listen_for_key(KeyCombos.undo_windows, on_undo);
  let _ = JSUtil.listen_for_key(KeyCombos.undo_mac, on_undo);
  let _ = JSUtil.listen_for_key(KeyCombos.redo_windows, on_redo);
  let _ = JSUtil.listen_for_key(KeyCombos.redo_windows_other, on_redo);
  let _ = JSUtil.listen_for_key(KeyCombos.redo_mac, on_redo);
  let _ = JSUtil.listen_for_key(KeyCombos.redo_mac_other, on_redo);

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
