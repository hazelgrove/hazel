open Tyxml_js;

let make = (redo_stack_rs: Model.redo_stack_rs): Html.elt([> Html_types.div]) => {
  let make_boxes = (redo_stack: Model.redo_stack) => {
    let actions_to_take = 500;

    let non_move_actions =
      switch (redo_stack) {
      | [branch, ..._] =>
        List.filter(
          action => !Action.is_move(action),
          GeneralUtil.take(actions_to_take, branch.actions),
        )
      | [] => assert(false)
      };

    List.map(
      action => Html.p([Html.txt(Action.show(action))]),
      non_move_actions,
    );
  };
  let action_list_rs = React.S.map(make_boxes, redo_stack_rs);

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
