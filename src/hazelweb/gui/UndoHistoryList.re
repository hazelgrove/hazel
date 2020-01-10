module Vdom = Virtual_dom.Vdom;
module KeyCombo = JSUtil.KeyCombo;
module ZList = GeneralUtil.ZList;
exception InvalidInstance;

let shape_to_string = (shape: Action.shape): string => {
  switch (shape) {
  | SParenthesized => "parentheise"
  /* type shapes */
  | SNum => "number"
  | SBool => "bool"
  | SList => "list"
  /* expression shapes */
  | SAsc => "???"
  | SVar(varstr, _) => "var: " ++ varstr // convert int to char? ++ Char.chr(intvar)
  | SLam => "lambada?"
  | SNumLit(value, _) => "numlit? " ++ string_of_int(value)
  | SListNil => "listNil"
  | SInj(direction) =>
    switch (direction) {
    | L => "inject left"
    | R => "inject right"
    }
  | SLet => "let"
  | SLine => "a new line"
  | SCase => "case"
  | SOp(op) => "operator " ++ Action.op_shape_to_string(op)
  | SApPalette(_) => "appalette?"
  /* pattern-only shapes */
  | SWild => "wild?"
  };
};
let action_to_stirng = (action: Action.t) => {
  switch (action) {
  | UpdateApPalette(_) => "updatePlate?"
  | Delete => "delete"
  | Backspace => "backspace"
  | Construct(shape) => "add " ++ shape_to_string(shape)
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
let history_entry_view = undo_history_entry => {
  let (_, action, _) = undo_history_entry;
  let txt =
    switch (action) {
    | None => "no action"
    | Some(detail_ac) => action_to_stirng(detail_ac)
    };
  Vdom.(Node.div([], [Node.text(txt)]));
};
let history_view = (model: Model.t) => {
  let erase_func =
      (undo_history_entry: UndoHistory.undo_history_entry)
      : UndoHistory.undo_history_entry => undo_history_entry;
  let history = ZList.erase(model.undo_history, erase_func);
  switch (history) {
  | [] =>
    Vdom.(
      Node.div(
        [Attr.classes(["the-context"])],
        [
          Vdom.(
            Node.div(
              [Attr.classes(["context-is-empty-msg"])],
              [Node.text("no variables in scope")],
            )
          ),
        ],
      )
    )
  | his_lst =>
    Vdom.(
      Node.div(
        [Attr.classes(["the-context"])],
        List.map(history_entry_view, List.rev(his_lst)),
      )
    )
  };
};

let view = (model: Model.t) => {
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
