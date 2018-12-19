open Tyxml_js;
open React;
open Semantics.Core;
open Model;
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
  let action_button = (action, lbl_body, key_combo) => {
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
          [pcdata(KC.name(key_combo))],
        )
      );

    let can_perform_rs =
      S.map(
        edit_state =>
          switch (Action.performSyn((), Ctx.empty, action, edit_state)) {
          | Some(_) => ["action-panel-entry", "action-enabled"]
          | None => ["action-panel-entry", "action-disabled"]
          },
        edit_state_rs,
      );

    Html5.(
      div(
        ~a=[R.Html5.a_class(can_perform_rs)],
        [lbl_div, keyboard_shortcut_div],
      )
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
      Html5.(div(~a=[a_class(["invalid-mark"])], [pcdata("✗")])),
    ];

    let valid = [
      Html5.(div(~a=[a_class(["valid-mark"])], [pcdata("✔")])),
    ];

    let validity_rs =
      React.S.l2(
        (m, i_str) =>
          switch (conv(i_str)) {
          | None => invalid
          | Some(arg) =>
            let a = action(arg);
            switch (Action.performSyn((), Ctx.empty, a, m)) {
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
          Js._false;
        },
      );

    let _ =
      JSUtil.listen_to(
        Ev.keyup,
        i_dom,
        evt => {
          let key = JSUtil.get_key(evt);
          if (key == KC.key(KCs.enter)) {
            lbl_dom##click;
            clear_input();
            set_cursor();
            Js._false;
          } else if (key == KC.key(KCs.esc)) {
            clear_input();
            set_cursor();
            Js._false;
          } else {
            Js._true;
          };
        },
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
          [pcdata(KC.name(key_combo))],
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

  let action_input_input_button =
      (
        action,
        conv,
        can_perform_rs,
        lbl_body,
        input_id,
        key_combo,
        placeholder_str_1,
        placeholder_str_2,
      ) => {
    let input_id_1 = input_id ++ "_1";
    let input_id_2 = input_id ++ "_2";
    let ((i_rs_1, i_rf_1), i_elt_1, i_dom_1) =
      JSUtil.r_input(input_id_1, placeholder_str_1);
    let ((i_rs_2, i_rf_2), i_elt_2, i_dom_2) =
      JSUtil.r_input(input_id_2, placeholder_str_2);
    let clear_input = () => {
      i_dom_1##.value := Js.string("");
      i_rf_1("");
      i_dom_2##.value := Js.string("");
      i_rf_2("");
    };
    let onclick_handler = _ => {
      let converted = conv(React.S.value(i_rs_1), React.S.value(i_rs_2));
      switch (converted) {
      | Some(arg) =>
        doAction(action(arg));
        clear_input();
        true;
      | None => true
      };
    };
    let invalid = [
      Html5.(div(~a=[a_class(["invalid-mark"])], [pcdata("✗")])),
    ];

    let valid = [
      Html5.(div(~a=[a_class(["valid-mark"])], [pcdata("✔")])),
    ];

    let validity_rs =
      React.S.l3(
        (m, i_str_1, i_str_2) =>
          switch (conv(i_str_1, i_str_2)) {
          | None => invalid
          | Some(arg) =>
            let a = action(arg);
            switch (Action.performSyn((), Ctx.empty, a, m)) {
            | Some(_) => valid
            | None => invalid
            };
          },
        edit_state_rs,
        i_rs_1,
        i_rs_2,
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
            a_class(["action-label", "action-label-with-two-inputs"]),
            a_onclick(onclick_handler),
          ],
          [
            div(~a=[a_class(["action-label-text"])], [lbl_body]),
            div(
              ~a=[a_class(["action-input", "action-input-1"])],
              [i_elt_1],
            ),
            div(
              ~a=[a_class(["action-input", "action-input-2"])],
              [i_elt_2],
            ),
            validity_div,
          ],
        )
      );

    let lbl_dom = To_dom.of_div(lbl_div);

    let _ =
      JSUtil.listen_for_key(
        key_combo,
        evt => {
          clear_input();
          i_dom_1##focus;
          Dom_html.stopPropagation(evt);
          Js._false;
        },
      );

    let i_keyup_listener = i_dom =>
      JSUtil.listen_to(
        Ev.keyup,
        i_dom,
        evt => {
          let key = JSUtil.get_key(evt);
          if (key == KC.key(KCs.enter)) {
            lbl_dom##click;
            clear_input();
            set_cursor();
            Js._false;
          } else if (key == KC.key(KCs.esc)) {
            clear_input();
            set_cursor();
            Js._false;
          } else {
            Dom_html.stopPropagation(evt);
            Js._true;
          };
        },
      );

    let _ = i_keyup_listener(i_dom_1);
    let _ = i_keyup_listener(i_dom_2);
    let i_keypress_listener = i_dom => {
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

      ();
    };
    let _ = i_keypress_listener(i_dom_1);
    let _ = i_keypress_listener(i_dom_2);
    let keyboard_shortcut_div =
      Html5.(
        div(
          ~a=[a_class(["keyboard-shortcut"]), a_onclick(onclick_handler)],
          [pcdata(KC.name(key_combo))],
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
      div(
        ~a=[R.Html5.a_class(cls_rs), a_onclick(onclick_handler)],
        [lbl_div, keyboard_shortcut_div],
      )
    );
  };

  let is_hole_rs =
    S.l1(
      ({ZExp.sort, _}) =>
        switch (sort) {
        | ZExp.IsExpr(UHExp.Tm(_, UHExp.EmptyHole(_))) => true
        | _ => false
        },
      cursor_info_rs,
    );

  let can_insert_var_rs =
    S.l1(
      ({ZExp.sort, _}) =>
        switch (sort) {
        | ZExp.IsExpr(_) => true
        | ZExp.IsType => false
        },
      cursor_info_rs,
    );

  let can_insert_let_case_rs =
    S.l1(
      ({ZExp.mode, _}) =>
        switch (mode) {
        | ZExp.TypePosition => false
        | _ => true
        },
      cursor_info_rs,
    );

  let backspace =
    action_button(
      Action.Backspace,
      Html5.pcdata("backspace"),
      KCs.backspace,
    );

  let delete = action_button(Action.Delete, Html5.pcdata("delete"), KCs.del);
  let threepiece = (cls, pre_txt, code_txt, post_txt) =>
    Html5.(
      span([
        pcdata(pre_txt),
        span(~a=[a_class(["code", cls])], [pcdata(code_txt)]),
        pcdata(post_txt),
      ])
    );

  let moveToPrevHole =
    action_button(
      Action.MoveToPrevHole,
      Html5.pcdata("move to previous hole"),
      KCs.backtab,
    );

  let moveToNextHole =
    action_button(
      Action.MoveToNextHole,
      Html5.pcdata("move to next hole"),
      KCs.tab,
    );

  let threepiece_op = threepiece("op");
  let threepiece_kw = threepiece("kw");
  let constructNum =
    action_button(
      Action.Construct(Action.SNum),
      threepiece_kw("", "num", " type"),
      KCs.pound,
    );

  let constructArrow =
    action_button(
      Action.Construct(Action.STyOp(UHTyp.Arrow)),
      threepiece_op("insert ", LangUtil.typeArrowSym, " type operator"),
      KCs.greaterThan,
    );

  let constructSum =
    action_button(
      Action.Construct(Action.STyOp(UHTyp.Sum)),
      threepiece_op("insert ", "|", " type operator"),
      KCs.vbar,
    );

  let constructParenthesized =
    action_button(
      Action.Construct(Action.SParenthesized),
      Html5.pcdata("parenthesize"),
      KCs.openParens,
    );

  let constructAsc =
    action_button(
      Action.Construct(Action.SAsc),
      Html5.pcdata("type ascription"),
      KCs.colon,
    );

  let constructLet =
    action_input_button(
      v => Action.Construct(Action.SLet(v)),
      s =>
        switch (String.compare(s, "")) {
        | 0 => None
        | _ => Some(s)
        },
      can_insert_let_case_rs,
      Html5.div([threepiece_kw("", "let", "")]),
      "let_input",
      KCs.equals,
      "enter var",
    );

  let constructVar =
    action_input_button(
      v => Action.Construct(Action.SVar(v, After)),
      s =>
        switch (String.compare(s, "")) {
        | 0 => None
        | _ => Some(s)
        },
      can_insert_var_rs,
      Html5.pcdata("var"),
      "var_input",
      KCs.alt_V,
      "enter var",
    );

  let constructLam =
    action_input_button(
      v => Action.Construct(Action.SLam(v)),
      s =>
        switch (String.compare(s, "")) {
        | 0 => None
        | _ => Some(s)
        },
      is_hole_rs,
      threepiece_kw("", LangUtil.lamSym, ""),
      "lam_input",
      KCs.backslash,
      "enter var",
    );

  let constructPlus =
    action_button(
      Action.Construct(Action.SOp(UHExp.Plus)),
      threepiece_op("insert ", "+", " operator"),
      KCs.plus,
    );

  let constructTimes =
    action_button(
      Action.Construct(Action.SOp(UHExp.Times)),
      threepiece_op("insert ", "*", " operator"),
      KCs.asterisk,
    );

  let constructSpace =
    action_button(
      Action.Construct(Action.SOp(UHExp.Space)),
      Html5.pcdata("insert application operator"),
      KCs.space,
    );

  let constructInjL =
    action_button(
      Action.Construct(Action.SInj(UHExp.L)),
      Html5.pcdata("left injection"),
      KCs.alt_L,
    );

  let constructInjR =
    action_button(
      Action.Construct(Action.SInj(UHExp.R)),
      Html5.pcdata("right injection"),
      KCs.alt_R,
    );

  let constructCase =
    action_input_input_button(
      ((v1, v2)) =>
        Action.Construct([@implicit_arity] Action.SCase(v1, v2)),
      (s1, s2) => {
        let s1_empty = String.compare(s1, "");
        let s2_empty = String.compare(s2, "");
        switch (s1_empty, s2_empty) {
        | (0, _) => None
        | (_, 0) => None
        | _ => Some((s1, s2))
        };
      },
      can_insert_let_case_rs,
      threepiece_kw("", "case", ""),
      "case_input",
      KCs.alt_C,
      "enter var1",
      "enter var2",
    );

  let typeConstructionActions =
    Html5.(
      div(
        ~a=[a_class(["sub-panel", "sub-panel-default"])],
        [
          div(
            ~a=[a_class(["sub-panel-title"])],
            [pcdata("Type Construction")],
          ),
          div(
            ~a=[a_class(["sub-panel-body"])],
            [constructNum, constructArrow, constructSum],
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
            [pcdata("Expression Construction")],
          ),
          div(
            ~a=[a_class(["sub-panel-body"])],
            [
              constructLet,
              constructVar,
              constructLam,
              constructSpace,
              constructPlus,
              constructTimes,
              constructInjL,
              constructInjR,
              constructCase,
              constructAsc,
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
          div(~a=[a_class(["sub-panel-title"])], [pcdata("General")]),
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
