open Tyxml_js;
open React;
open Model;
module Dom_html = Js_of_ocaml.Dom_html;
module Dom = Js_of_ocaml.Dom;
module Js = Js_of_ocaml.Js;
let make =
    ({edit_state_rs, cursor_info_rs, do_action, _}: Model.t, set_cursor) => {
  module Util = GeneralUtil;
  module Ev = Dom_html.Event;
  module KC = JSUtil.KeyCombo;
  module KCs = JSUtil.KeyCombos;
  let doAction = action => {
    do_action(action);
    set_cursor();
  };
  let action_button = (action, can_perform_rs, lbl_body, key_combo) => {
    let _ =
      JSUtil.listen_for_key(
        key_combo,
        evt => {
          Dom.preventDefault(evt);
          doAction(action);
        },
      );

    let onclick_handler = _ => {
      doAction(action);
      true;
    };
    let lbl_div =
      Html5.(
        div(
          ~a=[a_class(["action-label"]), a_onclick(onclick_handler)],
          [lbl_body],
        )
      );

    let keyboard_shortcut_div =
      Html5.(
        div(
          ~a=[a_class(["keyboard-shortcut"]), a_onclick(onclick_handler)],
          [txt(KC.name(key_combo))],
        )
      );

    let cls_rs =
      S.map(
        can_perform =>
          can_perform ?
            ["action-panel-entry", "action-enabled"] :
            ["action-panel-entry", "action-disabled"],
        can_perform_rs,
      );

    Html5.(
      div(~a=[R.Html5.a_class(cls_rs)], [lbl_div, keyboard_shortcut_div])
    );
  };

  let action_input_button =
      (
        action,
        conv,
        can_perform_rs,
        lbl_body,
        input_id,
        key_combo,
        placeholder_str,
      ) => {
    let ((i_rs, i_rf), i_elt, i_dom) =
      JSUtil.r_input(input_id, placeholder_str);
    let clear_input = () => {
      i_dom##.value := Js.string("");
      i_rf("");
    };

    let onclick_handler = _ => {
      let converted = conv(React.S.value(i_rs));
      switch (converted) {
      | Some(arg) =>
        doAction(action(arg));
        clear_input();
        true;
      | None => true
      };
    };
    let invalid = [
      Html5.(div(~a=[a_class(["invalid-mark"])], [txt("✗")])),
    ];

    let valid = [
      Html5.(div(~a=[a_class(["valid-mark"])], [txt("✔")])),
    ];

    let validity_rs =
      React.S.l2(
        (m, i_str) =>
          switch (conv(i_str)) {
          | None => invalid
          | Some(arg) =>
            let a = action(arg);
            switch (
              Action.perform_syn(
                (VarCtx.empty, Palettes.initial_palette_ctx),
                a,
                m,
              )
            ) {
            | Some(_) => valid
            | None => invalid
            };
          },
        edit_state_rs,
        i_rs,
      );

    let validity_div =
      R.Html5.(
        div(
          ~a=[Html5.a_class(["action-validity-indicator"])],
          ReactiveData.RList.from_signal(validity_rs),
        )
      );

    let lbl_div =
      Html5.(
        div(
          ~a=[
            a_class(["action-label", "action-label-with-input"]),
            a_onclick(onclick_handler),
          ],
          [
            div(~a=[a_class(["action-label-text"])], [lbl_body]),
            div(~a=[a_class(["action-input"])], [i_elt]),
            validity_div,
          ],
        )
      );

    let lbl_dom = To_dom.of_div(lbl_div);

    let _ =
      JSUtil.listen_for_key(
        key_combo,
        evt => {
          Dom_html.stopPropagation(evt);
          Dom.preventDefault(evt);
          clear_input();
          i_dom##focus;
          ();
        },
      );

    let _ =
      JSUtil.listen_to(Ev.keyup, i_dom, evt =>
        if (KC.matches(KCs.enter, evt)) {
          lbl_dom##click;
          clear_input();
          set_cursor();
          Js._false;
        } else if (KC.matches(KCs.escape, evt)) {
          clear_input();
          set_cursor();
          Js._false;
        } else {
          Js._true;
        }
      );

    let _ =
      JSUtil.listen_to(
        Ev.keydown,
        i_dom,
        evt => {
          Dom_html.stopPropagation(evt);
          Js._true;
        },
      );

    let _ =
      JSUtil.listen_to(
        Ev.keypress,
        i_dom,
        evt => {
          Dom_html.stopPropagation(evt);
          Js._true;
        },
      );

    let keyboard_shortcut_div =
      Html5.(
        div(
          ~a=[a_class(["keyboard-shortcut"]), a_onclick(onclick_handler)],
          [txt(KC.name(key_combo))],
        )
      );

    let cls_rs =
      S.map(
        can_perform =>
          can_perform ?
            ["action-panel-entry", "action-enabled"] :
            ["action-panel-entry", "action-disabled"],
        can_perform_rs,
      );

    Html5.(
      div(~a=[R.Html5.a_class(cls_rs)], [lbl_div, keyboard_shortcut_div])
    );
  };

  let info_button = (msg, enabled_rs) => {
    let cls_rs =
      S.map(
        enabled =>
          enabled ?
            ["action-panel-entry", "action-enabled"] :
            ["action-panel-entry", "action-disabled"],
        enabled_rs,
      );
    Html5.(
      div(
        ~a=[R.Html5.a_class(cls_rs)],
        [div(~a=[a_class(["action-label"])], [msg])],
      )
    );
  };

  let twopiece_lbl = (cls, code_txt, post_txt) =>
    Html5.(
      span([
        span(~a=[a_class(["code", cls])], [txt(code_txt)]),
        txt(post_txt),
      ])
    );
  let twopiece_lbl_op = twopiece_lbl("op");
  let twopiece_lbl_kw = twopiece_lbl("kw");

  let check_action_rs = action =>
    S.l2(
      (edit_state, cursor_info) =>
        Action.can_perform(
          (VarCtx.empty, Palettes.initial_palette_ctx),
          edit_state,
          cursor_info,
          action,
        ),
      edit_state_rs,
      cursor_info_rs,
    );

  let checked_action_button = (a, lbl, kc) =>
    action_button(a, check_action_rs(a), lbl, kc);

  let backspace =
    checked_action_button(
      Action.Backspace,
      Html5.txt("backspace"),
      KCs.backspace,
    );

  let delete =
    checked_action_button(Action.Delete, Html5.txt("delete"), KCs.delete);

  let moveToPrevHole =
    checked_action_button(
      Action.MoveToPrevHole,
      Html5.txt("move to previous hole"),
      KCs.shift_tab,
    );

  let moveToNextHole =
    checked_action_button(
      Action.MoveToNextHole,
      Html5.txt("move to next hole"),
      KCs.tab,
    );

  let constructNum =
    checked_action_button(
      Action.Construct(Action.SNum),
      twopiece_lbl_kw("Num", " type"),
      KCs.key_N,
    );

  let constructBool =
    checked_action_button(
      Action.Construct(Action.SBool),
      twopiece_lbl_kw("Bool", " type"),
      KCs.key_B,
    );

  let constructArrow =
    checked_action_button(
      Action.(Construct(SOp(SArrow))),
      twopiece_lbl_op(LangUtil.typeArrowSym, " type operator"),
      KCs.gt,
    );

  let constructSum =
    checked_action_button(
      Action.(Construct(SOp(SVBar))),
      twopiece_lbl_op("|", " type operator"),
      KCs.vbar,
    );

  let constructList =
    checked_action_button(
      Action.(Construct(SList)),
      twopiece_lbl_kw("List", " type"),
      KCs.key_L,
    );

  let constructParenthesized =
    checked_action_button(
      Action.Construct(Action.SParenthesized),
      Html5.txt("parenthesize"),
      KCs.left_parens,
    );

  let constructAsc =
    checked_action_button(
      Action.Construct(Action.SAsc),
      Html5.txt("type ascription"),
      KCs.colon,
    );

  let constructLet =
    checked_action_button(
      Action.Construct(Action.SLet),
      twopiece_lbl_kw("let", ""),
      KCs.equals,
    );

  let constructNewLine =
    checked_action_button(
      Action.Construct(Action.SLine),
      Html5.txt("new line"),
      KCs.enter,
    );

  let can_enter_varchar_rs = S.map(Action.can_enter_varchar, cursor_info_rs);

  let constructVar =
    info_button(Html5.txt("enter variables directly"), can_enter_varchar_rs);

  let constructBoolLit =
    info_button(
      Html5.txt("enter bool literals (true, false) directly"),
      can_enter_varchar_rs,
    );

  let constructLam =
    action_button(
      Action.Construct(Action.SLam),
      check_action_rs(Action.Construct(Action.SLam)),
      twopiece_lbl_kw(LangUtil.lamSym, ""),
      KCs.backslash,
    );

  let can_enter_numeral_rs = S.map(Action.can_enter_numeral, cursor_info_rs);

  let constructLit =
    info_button(
      Html5.txt("enter number literals directly"),
      can_enter_numeral_rs,
    );

  let constructPlus =
    checked_action_button(
      Action.(Construct(SOp(SPlus))),
      twopiece_lbl_op("+", " operator"),
      KCs.plus,
    );

  let constructTimes =
    checked_action_button(
      Action.(Construct(SOp(STimes))),
      twopiece_lbl_op("*", " operator"),
      KCs.asterisk,
    );

  let constructLessThan =
    checked_action_button(
      Action.(Construct(SOp(SLessThan))),
      twopiece_lbl_op("<", " operator"),
      KCs.lt,
    );

  let constructSpace =
    checked_action_button(
      Action.(Construct(SOp(SSpace))),
      Html5.txt("apply"),
      KCs.space,
    );

  let constructComma =
    checked_action_button(
      Action.(Construct(SOp(SComma))),
      twopiece_lbl_op(",", " operator"),
      KCs.comma,
    );

  let constructNil =
    checked_action_button(
      Action.(Construct(SListNil)),
      twopiece_lbl_op("[]", " (nil)"),
      KCs.left_bracket,
    );

  let constructCons =
    checked_action_button(
      Action.(Construct(SOp(SCons))),
      twopiece_lbl_op("::", " operator"),
      KCs.semicolon,
    );

  let constructInjL =
    checked_action_button(
      Action.Construct(Action.SInj(L)),
      Html5.txt("left injection"),
      KCs.alt_L,
    );

  let constructInjR =
    checked_action_button(
      Action.Construct(Action.SInj(R)),
      Html5.txt("right injection"),
      KCs.alt_R,
    );

  let constructCase =
    checked_action_button(
      Action.Construct(Action.SCase),
      twopiece_lbl_kw("case", ""),
      KCs.alt_C,
    );

  let can_insert_ap_palette_rs =
    S.l1(Action.can_construct_palette, cursor_info_rs);

  let constructApPalette =
    action_input_button(
      v => Action.Construct(Action.SApPalette("$" ++ v)),
      s => PaletteName.is_valid("$" ++ s) ? Some(s) : None,
      can_insert_ap_palette_rs,
      Html5.txt("apply palette"),
      "ap_palette_input",
      KCs.dollar,
      "enter palette name",
    );

  let typeConstructionActions =
    Html5.(
      div(
        ~a=[a_class(["sub-panel", "sub-panel-default"])],
        [
          div(
            ~a=[a_class(["sub-panel-title"])],
            [txt("Type Construction")],
          ),
          div(
            ~a=[a_class(["sub-panel-body"])],
            [
              constructNum,
              constructBool,
              constructList,
              constructArrow,
              constructSum,
            ],
          ),
        ],
      )
    );

  let expressionConstructionActions =
    Html5.(
      div(
        ~a=[a_class(["sub-panel", "sub-panel-default"])],
        [
          div(
            ~a=[a_class(["sub-panel-title"])],
            [txt("Expression Construction")],
          ),
          div(
            ~a=[a_class(["sub-panel-body"])],
            [
              constructVar,
              constructNewLine,
              constructLet,
              constructLam,
              constructSpace,
              constructLit,
              constructPlus,
              constructTimes,
              constructLessThan,
              constructBoolLit,
              constructComma,
              constructNil,
              constructCons,
              constructInjL,
              constructInjR,
              constructCase,
              constructAsc,
              constructApPalette,
            ],
          ),
        ],
      )
    );

  let generalActions =
    Html5.(
      div(
        ~a=[a_class(["sub-panel", "sub-panel-default"])],
        [
          div(~a=[a_class(["sub-panel-title"])], [txt("General")]),
          div(
            ~a=[a_class(["sub-panel-body"])],
            [
              constructParenthesized,
              backspace,
              delete,
              moveToPrevHole,
              moveToNextHole,
            ],
          ),
        ],
      )
    );

  Html5.(
    div(
      ~a=[a_class(["panel", "action-panel"])],
      [
        PanelUtils.titlebar("Edit Actions"),
        generalActions,
        expressionConstructionActions,
        typeConstructionActions,
      ],
    )
  );
};
