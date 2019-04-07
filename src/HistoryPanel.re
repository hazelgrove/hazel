open Tyxml_js;

let make = (repository_rs: Model.repository_rs): Html.elt([> Html_types.div]) => {
  let make_boxes = (repository: Repository.t) => {
    let actions_to_take = 500;

    let non_move_actions =
      switch (repository.redo_stack) {
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
  let action_list_rs = React.S.map(make_boxes, repository_rs);

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
