open Tyxml_js;
open React;
open SemanticsCore;
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
                (),
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
    S.map(
      edit_state =>
        switch (
          Action.perform_syn(
            (),
            (VarCtx.empty, Palettes.initial_palette_ctx),
            action,
            edit_state,
          )
        ) {
        | Some(_) => true
        | None => false
        },
      edit_state_rs,
    );

  let backspace =
    action_button(
      Action.Backspace,
      check_action_rs(Action.Backspace),
      Html5.txt("backspace"),
      KCs.backspace,
    );

  let delete =
    action_button(
      Action.Delete,
      check_action_rs(Action.Delete),
      Html5.txt("delete"),
      KCs.del,
    );

  let moveToPrevHole =
    action_button(
      Action.MoveToPrevHole,
      check_action_rs(Action.MoveToPrevHole),
      Html5.txt("move to previous hole"),
      KCs.backtab,
    );

  let moveToNextHole =
    action_button(
      Action.MoveToNextHole,
      check_action_rs(Action.MoveToNextHole),
      Html5.txt("move to next hole"),
      KCs.tab,
    );

  let constructNum =
    action_button(
      Action.Construct(Action.SNum),
      check_action_rs(Action.Construct(Action.SNum)),
      twopiece_lbl_kw("num", " type"),
      KCs.pound,
    );

  let is_type_rs =
    ZExp.(
      S.map(
        ({sort, _}) =>
          switch (sort) {
          | IsType => true
          | IsExpr(_)
          | IsPat(_) => false
          },
        cursor_info_rs,
      )
    );

  let constructArrow =
    action_button(
      Action.(Construct(SOp(SArrow))),
      is_type_rs,
      twopiece_lbl_op(LangUtil.typeArrowSym, " type operator"),
      KCs.greaterThan,
    );

  let constructSum =
    action_button(
      Action.(Construct(SOp(SVBar))),
      is_type_rs,
      twopiece_lbl_op("|", " type operator"),
      KCs.vbar,
    );

  let is_not_pat_rs =
    ZExp.(
      S.map(
        ({sort, _}) =>
          switch (sort) {
          | IsPat(_) => false
          | IsType
          | IsExpr(_) => true
          },
        cursor_info_rs,
      )
    );

  let constructParenthesized =
    action_button(
      Action.Construct(Action.SParenthesized),
      is_not_pat_rs,
      Html5.txt("parenthesize"),
      KCs.openParens,
    );

  let can_ascribe_rs =
    ZExp.(
      S.map(
        ({sort, _}) =>
          switch (sort) {
          | IsExpr(_) => true
          | IsType => false
          | IsPat(_) => true
          },
        cursor_info_rs,
      )
    );

  let constructAsc =
    action_button(
      Action.Construct(Action.SAsc),
      can_ascribe_rs,
      Html5.txt("type ascription"),
      KCs.colon,
    );

  let expr_not_ana_only_rs =
    ZExp.(
      S.map(
        ({mode, _}) =>
          switch (mode) {
          | AnaOnly(_) => false
          | AnaAnnotatedLambda(_, _)
          | TypeInconsistent(_, _)
          | AnaFree(_)
          | Subsumed(_, _)
          | SynOnly(_)
          | SynFree
          | SynErrorArrow(_)
          | SynMatchingArrow(_, _)
          | SynFreeArrow(_) => true
          | TypePosition => false
          | PatAnaOnly(_)
          | PatTypeInconsistent(_, _)
          | PatSubsumed(_, _)
          | PatSynOnly(_) => false
          },
        cursor_info_rs,
      )
    );

  let constructLet =
    action_button(
      Action.Construct(Action.SLet),
      expr_not_ana_only_rs,
      twopiece_lbl_kw("let", ""),
      KCs.equals,
    );

  let can_enter_var_rs =
    ZExp.(
      S.map(
        ({sort, side, _}) =>
          switch (sort) {
          | IsExpr(UHExp.Tm(_, UHExp.Var(_, _)))
          | IsExpr(UHExp.Tm(_, UHExp.EmptyHole(_)))
          | IsPat(_) => true /* TODO */
          | IsExpr(UHExp.Tm(_, UHExp.NumLit(_))) =>
            switch (side) {
            | Before => true
            | In(_)
            | After => false
            }
          | IsExpr(_)
          | IsType => false
          },
        cursor_info_rs,
      )
    );

  let constructVar =
    info_button(Html5.txt("enter variables directly"), can_enter_var_rs);

  let can_insert_ap_palette_rs =
    S.l1(
      ({ZExp.sort, ZExp.ctx: (_, palette_ctx), _}) =>
        switch (sort) {
        | ZExp.IsExpr(UHExp.Tm(_, UHExp.EmptyHole(_))) => true
        | _ => false
        },
      cursor_info_rs,
    );

  let constructLam =
    action_button(
      Action.Construct(Action.SLam),
      check_action_rs(Action.Construct(Action.SLam)),
      twopiece_lbl_kw(LangUtil.lamSym, ""),
      KCs.backslash,
    );

  let is_lit_or_hole_rs =
    ZExp.(
      S.map(
        ({sort, _}) =>
          switch (sort) {
          | IsExpr(UHExp.Tm(_, UHExp.NumLit(_)))
          | IsExpr(UHExp.Tm(_, UHExp.EmptyHole(_))) => true
          | IsExpr(_)
          | IsPat(_) /* TODO */
          | IsType => false
          },
        cursor_info_rs,
      )
    );
  let constructLit =
    info_button(
      Html5.txt("enter number literals directly"),
      is_lit_or_hole_rs,
    );

  let constructPlus =
    action_button(
      Action.(Construct(SOp(SPlus))),
      expr_not_ana_only_rs,
      twopiece_lbl_op("+", " operator"),
      KCs.plus,
    );

  let constructTimes =
    action_button(
      Action.(Construct(SOp(STimes))),
      expr_not_ana_only_rs,
      twopiece_lbl_op("*", " operator"),
      KCs.asterisk,
    );

  let constructSpace =
    action_button(
      Action.(Construct(SOp(SSpace))),
      expr_not_ana_only_rs,
      Html5.txt("apply"),
      KCs.space,
    );

  let constructNil =
    action_button(
      Action.(Construct(SListNil)),
      expr_not_ana_only_rs,
      Html5.txt("nil"),
      KCs.nil,
    );

  let constructCons =
    action_button(
      Action.(Construct(SOp(SCons))),
      expr_not_ana_only_rs,
      twopiece_lbl_op(";", " operator"),
      KCs.cons,
    );

  let constructInjL =
    action_button(
      Action.Construct(Action.SInj(L)),
      expr_not_ana_only_rs,
      Html5.txt("left injection"),
      KCs.alt_L,
    );

  let constructInjR =
    action_button(
      Action.Construct(Action.SInj(R)),
      expr_not_ana_only_rs,
      Html5.txt("right injection"),
      KCs.alt_R,
    );

  let constructCase =
    action_button(
      Action.Construct(Action.SCase),
      expr_not_ana_only_rs,
      twopiece_lbl_kw("case", ""),
      KCs.alt_C,
    );

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
            [txt("Expression Construction")],
          ),
          div(
            ~a=[a_class(["sub-panel-body"])],
            [
              constructVar,
              constructLet,
              constructLam,
              constructSpace,
              constructLit,
              constructPlus,
              constructTimes,
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
