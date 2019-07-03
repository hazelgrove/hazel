module Vdom = Virtual_dom.Vdom;
module KeyCombo = JSUtil.KeyCombo;

let view = (~inject: Update.Action.t => Vdom.Event.t, model: Model.t) => {
  let edit_state = model.edit_state;
  let cursor_info = model.cursor_info;

  let action_button = (a: Action.t, lbl, key_combo) => {
    let can_perform =
      Action.can_perform(
        (VarCtx.empty, Palettes.initial_palette_ctx),
        edit_state,
        cursor_info,
        a,
      );
    Vdom.(
      Node.div(
        [
          Attr.classes(
            can_perform
              ? ["action-panel-entry", "action-enabled"]
              : ["action-panel-entry", "action-disabled"],
          ),
          Attr.on_click(_ => inject(Update.Action.EditAction(a))),
          Attr.on_keydown(evt =>
            if (KeyCombo.Details.matches(key_combo, evt)) {
              Event.Many([
                inject(Update.Action.EditAction(a)),
                Event.Prevent_default,
              ]);
            } else {
              Event.Prevent_default;
            }
          ),
        ],
        [
          Node.div([Attr.classes(["action-label"])], [lbl]),
          Node.div(
            [Attr.classes(["keyboard-shortcut"])],
            [Node.text(KeyCombo.Details.name(key_combo))],
          ),
        ],
      )
    );
  };

  let info_button = (~can_perform, lbl) =>
    Vdom.(
      Node.div(
        [
          Attr.classes(
            can_perform
              ? ["action-panel-entry", "action-enabled"]
              : ["action-panel-entry", "action-disabled"],
          ),
        ],
        [Node.div([Attr.classes(["action-label", "info-label"])], [lbl])],
      )
    );

  let twopiece_lbl = (cls, code_txt, post_txt) =>
    Vdom.(
      Node.span(
        [],
        [
          Node.span([Attr.classes(["code", cls])], [Node.text(code_txt)]),
          Node.text(post_txt),
        ],
      )
    );
  let twopiece_lbl_op = twopiece_lbl("op");
  let twopiece_lbl_kw = twopiece_lbl("kw");

  let backspace =
    action_button(
      Action.Backspace,
      Vdom.Node.text("backspace"),
      KeyCombo.Details.backspace,
    );

  let delete =
    action_button(
      Action.Delete,
      Vdom.Node.text("delete"),
      KeyCombo.Details.delete,
    );

  let moveToPrevHole =
    action_button(
      Action.MoveToPrevHole,
      Vdom.Node.text("move to previous hole"),
      KeyCombo.Details.shift_tab,
    );

  let moveToNextHole =
    action_button(
      Action.MoveToNextHole,
      Vdom.Node.text("move to next hole"),
      KeyCombo.Details.tab,
    );

  let constructNum =
    action_button(
      Action.Construct(Action.SNum),
      twopiece_lbl_kw("Num", " type"),
      KeyCombo.Details.key_N,
    );

  let constructBool =
    action_button(
      Action.Construct(Action.SBool),
      twopiece_lbl_kw("Bool", " type"),
      KeyCombo.Details.key_B,
    );

  let constructArrow =
    action_button(
      Action.(Construct(SOp(SArrow))),
      twopiece_lbl_op(LangUtil.typeArrowSym, " type operator"),
      KeyCombo.Details.gt,
    );

  let constructSum =
    action_button(
      Action.(Construct(SOp(SVBar))),
      twopiece_lbl_op("|", " type operator"),
      KeyCombo.Details.vbar,
    );

  let constructList =
    action_button(
      Action.(Construct(SList)),
      twopiece_lbl_kw("List", " type"),
      KeyCombo.Details.key_L,
    );

  let constructParenthesized =
    action_button(
      Action.Construct(Action.SParenthesized),
      Vdom.Node.text("parenthesize"),
      KeyCombo.Details.left_parens,
    );

  let constructAsc =
    action_button(
      Action.Construct(Action.SAsc),
      Vdom.Node.text("type ascription"),
      KeyCombo.Details.colon,
    );

  let constructLet =
    action_button(
      Action.Construct(Action.SLet),
      twopiece_lbl_kw("let", ""),
      KeyCombo.Details.equals,
    );

  let constructNewLine =
    action_button(
      Action.Construct(Action.SLine),
      Vdom.Node.text("new line"),
      KeyCombo.Details.enter,
    );

  let constructVar =
    info_button(
      ~can_perform=Action.can_enter_varchar(cursor_info),
      Vdom.Node.text("enter variables directly"),
    );

  let constructBoolLit =
    info_button(
      ~can_perform=Action.can_enter_varchar(cursor_info),
      Vdom.Node.text("enter bool literals (true, false) directly"),
    );

  let constructNumLit =
    info_button(
      ~can_perform=Action.can_enter_numeral(cursor_info),
      Vdom.Node.text("enter number literals directly"),
    );

  let constructLam =
    action_button(
      Action.Construct(Action.SLam),
      twopiece_lbl_kw(LangUtil.lamSym, ""),
      KeyCombo.Details.backslash,
    );

  let constructPlus =
    action_button(
      Action.(Construct(SOp(SPlus))),
      twopiece_lbl_op("+", " operator"),
      KeyCombo.Details.plus,
    );

  let constructTimes =
    action_button(
      Action.(Construct(SOp(STimes))),
      twopiece_lbl_op("*", " operator"),
      KeyCombo.Details.asterisk,
    );

  let constructLessThan =
    action_button(
      Action.(Construct(SOp(SLessThan))),
      twopiece_lbl_op("<", " operator"),
      KeyCombo.Details.lt,
    );

  let constructSpace =
    action_button(
      Action.(Construct(SOp(SSpace))),
      Vdom.Node.text("apply"),
      KeyCombo.Details.space,
    );

  let constructComma =
    action_button(
      Action.(Construct(SOp(SComma))),
      twopiece_lbl_op(",", " operator"),
      KeyCombo.Details.comma,
    );

  let constructNil =
    action_button(
      Action.(Construct(SListNil)),
      twopiece_lbl_op("[]", " (nil)"),
      KeyCombo.Details.left_bracket,
    );

  let constructCons =
    action_button(
      Action.(Construct(SOp(SCons))),
      twopiece_lbl_op("::", " operator"),
      KeyCombo.Details.semicolon,
    );

  let constructInjL =
    action_button(
      Action.Construct(Action.SInj(L)),
      Vdom.Node.text("left injection"),
      KeyCombo.Details.alt_L,
    );

  let constructInjR =
    action_button(
      Action.Construct(Action.SInj(R)),
      Vdom.Node.text("right injection"),
      KeyCombo.Details.alt_R,
    );

  let constructCase =
    action_button(
      Action.Construct(Action.SCase),
      twopiece_lbl_kw("case", ""),
      KeyCombo.Details.alt_C,
    );

  /*
   let can_insert_ap_palette_rs =
     S.l1(Action.can_construct_palette, cursor_info_rs);

   let constructApPalette =
     action_input_button(
       v => Action.Construct(Action.SApPalette("$" ++ v)),
       s => PaletteName.is_valid("$" ++ s) ? Some(s) : None,
       can_insert_ap_palette_rs,
       Vdom.Node.text("apply palette"),
       "ap_palette_input",
       KeyCombo.Details.dollar,
       "enter palette name",
     );
   */

  let type_construction_actions =
    Vdom.(
      Node.div(
        [Attr.classes(["sub-panel", "sub-panel-default"])],
        [
          Node.div(
            [Attr.classes(["sub-panel-title"])],
            [Node.text("Type Construction")],
          ),
          Node.div(
            [Attr.classes(["sub-panel-body"])],
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

  let expression_construction_actions =
    Vdom.(
      Node.div(
        [Attr.classes(["sub-panel", "sub-panel-default"])],
        [
          Node.div(
            [Attr.classes(["sub-panel-title"])],
            [Node.text("Expression Construction")],
          ),
          Node.div(
            [Attr.classes(["sub-panel-body"])],
            [
              constructVar,
              constructNewLine,
              constructLet,
              constructLam,
              constructSpace,
              constructNumLit,
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
              /* constructApPalette, */
            ],
          ),
        ],
      )
    );

  let general_actions =
    Vdom.(
      Node.div(
        [Attr.classes(["sub-panel", "sub-panel-default"])],
        [
          Node.div(
            [Attr.classes(["sub-panel-title"])],
            [Node.text("General")],
          ),
          Node.div(
            [Attr.classes(["sub-panel-body"])],
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

  Vdom.(
    Node.div(
      [Attr.classes(["panel", "action-panel"])],
      [
        Panel.view_of_main_title_bar("Edit Actions"),
        Node.div(
          [Attr.classes(["panel-body", "action-panel-body"])],
          [
            general_actions,
            expression_construction_actions,
            type_construction_actions,
          ],
        ),
      ],
    )
  );
};

/*
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
       Html5.(div([Attr.classes(["invalid-mark"])], [txt("✗")])),
     ];

     let valid = [
       Html5.(div([Attr.classes(["valid-mark"])], [txt("✔")])),
     ];

     let validity_rs =
       React.S.l2(
         (m, i_str) =>
           switch (conv(i_str)) {
           | None => invalid
           | Some(arg) =>
             let a = action(arg);
             switch (
               Action.syn_perform_block(
                 (VarCtx.empty, Palettes.initial_palette_ctx),
                 a,
                 m,
               )
             ) {
             | Succeeded(_) => valid
             | CursorEscaped(_)
             | Failed => invalid
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
             div([Attr.classes(["action-label-text"])], [lbl_body]),
             div([Attr.classes(["action-input"])], [i_elt]),
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
         if (KeyCombo.Details.matches(KeyCombo.Details.enter, evt)) {
           lbl_dom##click;
           clear_input();
           set_cursor();
           Js._false;
         } else if (KeyCombo.Details.matches(KeyCombo.Details.escape, evt)) {
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
           [Attr.classes(["keyboard-shortcut"]), a_onclick(onclick_handler)],
           [txt(KeyCombo.Details.name(key_combo))],
         )
       );

     let cls_rs =
       S.map(
         can_perform =>
           can_perform
             ? ["action-panel-entry", "action-enabled"]
             : ["action-panel-entry", "action-disabled"],
         can_perform_rs,
       );

     Html5.(
       div(~a=[R.Html5.a_class(cls_rs)], [lbl_div, keyboard_shortcut_div])
     );
   };

   let constructApPalette =
     action_input_button(
       v => Action.Construct(Action.SApPalette("$" ++ v)),
       s => PaletteName.is_valid("$" ++ s) ? Some(s) : None,
       can_insert_ap_palette_rs,
       Vdom.Node.text("apply palette"),
       "ap_palette_input",
       KeyCombo.Details.dollar,
       "enter palette name",
     );
   */
