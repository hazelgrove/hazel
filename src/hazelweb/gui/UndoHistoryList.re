module Vdom = Virtual_dom.Vdom;
module KeyCombo = JSUtil.KeyCombo;
module ZList = GeneralUtil.ZList;
exception InvalidInstance;

let view = (~inject: Update.Action.t => Vdom.Event.t, model: Model.t) => {
  let shape_to_display_string = (shape: Action.shape): string => {
    switch (shape) {
    | SParenthesized => "parentheise"
    /* type shapes */
    | SNum => "type Num"
    | SBool => "type Bool"
    | SList => "type List"
    /* expression shapes */
    | SAsc => "add ':'"
    | SVar(varstr, _) => "edit var: " ++ varstr // convert int to char? ++ Char.chr(intvar)
    | SLam => "add lambada"
    | SNumLit(value, _) => "edit number: " ++ string_of_int(value)
    | SListNil => "add list"
    | SInj(direction) =>
      switch (direction) {
      | L => "inject left"
      | R => "inject right"
      }
    | SLet => "bulid 'let'"
    | SLine => "add a new line"
    | SCase => "add case"
    | SOp(op) => "add operator " ++ Action.op_shape_to_string(op)
    | SApPalette(_) => "appalette?"
    /* pattern-only shapes */
    | SWild => "wild?"
    };
  };

  let action_to_display_string = (action: Action.t) => {
    switch (action) {
    | UpdateApPalette(_) => "updatePlate?"
    | Delete => "delete"
    | Backspace => "backspace"
    | Construct(shape) => shape_to_display_string(shape)
    | MoveTo(_)
    | MoveToBefore(_)
    | MoveLeft
    | MoveRight
    | MoveToNextHole
    | MoveToPrevHole
    | ShiftLeft
    | ShiftRight
    | ShiftUp
    | ShiftDown => ""
    };
  };

  /*   let rec group_history_view = (history, prev_action: option(Action.t), result) => {
          switch(history){
          | [] => result
          | [head, _] => {
            let (_,action,_)=head;
            switch(action){
            | None => result; //reach init history
            | Some(action_now) =>
            }

          }
          }
        }
       */
  let history_entry_view = undo_history_entry => {
    let (_, action, id) = undo_history_entry;
    switch (action) {
    | None => Vdom.(Node.div([], []))
    | Some(detail_ac) =>
      Vdom.(
        Node.div(
          [
            Attr.classes(["the-history-entry"]),
            Attr.on_click(_ => inject(Update.Action.ShiftHistory(id))),
          ],
          [Node.text(action_to_display_string(detail_ac))],
        )
      )
    };
  };
  let prev_history_view = history => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-prev-history"])],
        List.map(history_entry_view, history),
      )
    );
  };
  let suc_history_view = history => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-suc-history"])],
        List.map(history_entry_view, history),
      )
    );
  };
  let now_history_view = (history: UndoHistory.undo_history_entry) => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-now-history"])],
        [history_entry_view(history)],
      )
    );
  };
  let history_view = (model: Model.t) => {
    let (prev_his, now, suc_his) = model.undo_history;
    switch (now) {
    | (_, None, _) =>
      Vdom.(
        Node.div(
          [Attr.classes(["the-history"])],
          [
            Vdom.(
              Node.div(
                [Attr.classes(["history-is-empty-msg"])],
                [Node.text("no history in scope")],
              )
            ),
          ],
        )
      )
    | (_, Some(_), _) =>
      Vdom.(
        Node.div(
          [Attr.classes(["the-history"])],
          [
            suc_history_view(List.rev(suc_his)),
            now_history_view(now),
            prev_history_view(List.rev(prev_his)),
          ],
        )
      )
    };
  };
  Vdom.(
    Node.div(
      [Attr.classes(["panel", "context-inspector-panel"])],
      [
        Panel.view_of_main_title_bar("history"),
        Node.div(
          [Attr.classes(["panel-body", "context-inspector-body"])],
          [history_view(model)],
        ),
      ],
    )
  );
};
