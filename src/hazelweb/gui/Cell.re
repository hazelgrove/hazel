module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;

open ViewUtil;
open Sexplib.Std;

/** maps key combos to actions contextually, depending on cursor info */
let kc_actions:
  Hashtbl.t(HazelKeyCombos.t, CursorInfo_common.t => Action_common.t) =
  [
    (HazelKeyCombos.Backspace, _ => Action_common.Backspace),
    (Delete, _ => Action_common.Delete),
    (ShiftTab, _ => Action_common.MoveToPrevHole),
    (Tab, _ => Action_common.MoveToNextHole),
    (
      HazelKeyCombos.GT,
      fun
      | {CursorInfo_common.typed: OnType, _} =>
        Action_common.Construct(SOp(SArrow))
      | _ => Action_common.Construct(SOp(SGreaterThan)),
    ),
    (Ampersand, _ => Action_common.Construct(SOp(SAnd))),
    (VBar, _ => Action_common.Construct(SOp(SOr))),
    (LeftParen, _ => Action_common.Construct(SParenthesized)),
    (Colon, _ => Action_common.Construct(SAsc)),
    (Equals, _ => Action_common.Construct(SOp(SEquals))),
    (Enter, _ => Action_common.Construct(SLine)),
    (Backslash, _ => Action_common.Construct(SLam)),
    (Plus, _ => Action_common.Construct(SOp(SPlus))),
    (Minus, _ => Action_common.Construct(SOp(SMinus))),
    (Asterisk, _ => Action_common.Construct(SOp(STimes))),
    (Slash, _ => Action_common.Construct(SOp(SDivide))),
    (LT, _ => Action_common.Construct(SOp(SLessThan))),
    (
      Space,
      fun
      | {CursorInfo_common.cursor_term: Line(_, CommentLine(_)), _} =>
        Action_common.Construct(SChar(" "))
      | _ => Action_common.Construct(SOp(SSpace)),
    ),
    (Comma, _ => Action_common.Construct(SOp(SComma))),
    (
      LeftBracket,
      fun
      | {CursorInfo_common.typed: OnType, _} =>
        Action_common.Construct(SList)
      | _ => Action_common.Construct(SListNil),
    ),
    (Semicolon, _ => Action_common.Construct(SOp(SCons))),
    (Alt_L, _ => Action_common.Construct(SInj(L))),
    (Alt_R, _ => Action_common.Construct(SInj(R))),
    (Alt_C, _ => Action_common.Construct(SCase)),
    (Pound, _ => Action_common.Construct(SCommentLine)),
    (Shift_Enter, _ => Action_common.Construct(SCommentLine)),
    (Ctrl_Alt_I, _ => Action_common.SwapUp),
    (Ctrl_Alt_K, _ => Action_common.SwapDown),
    (Ctrl_Alt_J, _ => Action_common.SwapLeft),
    (Ctrl_Alt_L, _ => Action_common.SwapRight),
  ]
  |> List.to_seq
  |> Hashtbl.of_seq;

let focus = () => {
  JSUtil.force_get_elem_by_id("cell")##focus;
};

let view = (~inject, model: Model.t) => {
  TimeUtil.measure_time(
    "Cell.view",
    model.measurements.measurements && model.measurements.cell_view,
    () => {
      open Vdom;
      let program = Model.get_program(model);
      Node.div(
        [Attr.id(cell_id)],
        [
          /* font-specimen used to gather font metrics for caret positioning and other things */
          Node.div([Attr.id("font-specimen")], [Node.text("X")]),
          Node.div(
            [Attr.id("code-container")],
            [
              UHCode.view(
                ~inject,
                ~measure=
                  model.measurements.measurements
                  && model.measurements.uhcode_view,
                ~font_metrics=model.font_metrics,
                ~is_mac=model.is_mac,
                program,
              ),
            ],
          ),
        ],
      );
    },
  );
};
