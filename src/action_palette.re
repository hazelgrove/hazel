open Semantics.Core;

module Ev = Dom_html.Event;

open Tyxml_js;

open React;

module Util = General_util;

let make_palette ((ms, es, do_action): Model.mt) set_cursor => {
  /* start by defining a bunch of helpers */
  /* performs the top-level action and updates the signal */
  let doAction action => {
    do_action action;
    set_cursor ()
  };
  module KC = Js_util.KeyCombo;
  module KCs = Js_util.KeyCombos;
  /* helper function for constructing action buttons with no textbox */
  let action_button action btn_label key_combo => {
    let _ =
      Js_util.listen_to_t
        Ev.keydown
        Dom_html.document
        (
          fun evt => {
            let key = Js_util.get_key evt;
            /* Js_util.log key; */
            if (key == KC.key key_combo) {
              doAction action;
              Dom.preventDefault evt
            } else {
              ()
            }
          }
        );
    Html5.(
      button
        a::[
          a_class ["btn", "btn-outline-primary"],
          a_onclick (
            fun _ => {
              doAction action;
              true
            }
          ),
          R.filter_attrib
            (a_disabled ())
            (
              S.map
                (
                  fun m =>
                    /* switch (Action.performSyn () Ctx.empty action m) {
                       | Some _ => false
                       | None => true
                       } */ false
                )
                ms
            )
        ]
        [pcdata (btn_label ^ " [" ^ KC.name key_combo ^ "]")]
    )
  };
  /* actions that take an input. the conversion function
   * goes from a string (the input value) to an arg option
   * where arg is the action argument. */
  let action_input_button action conv btn_label input_id key_combo placeholder_str => {
    /* create reactive input box */
    let ((i_rs, i_rf), i_elt, i_dom) = Js_util.r_input input_id placeholder_str;
    let clear_input () => {
      i_dom##.value := Js.string "";
      i_rf "" /* need to manually update rf because r_input only reacts to UI input */
    };
    let button_elt =
      Html5.(
        button
          a::[
            a_class ["btn", "btn-default"],
            a_id (input_id ^ "_button"),
            a_onclick (
              fun _ => {
                let arg = Util.force_opt (conv (React.S.value i_rs));
                doAction (action arg);
                clear_input ();
                true
              }
            ),
            R.filter_attrib /* filters out the disabled attribute */
              (a_disabled ())
              (
                S.l2
                  (
                    fun s m => {
                      /* S.l2 creates a signal from two signals */
                      let converted = conv s;
                      switch converted {
                      | Some arg =>
                        switch (Action.performSyn () Ctx.empty (action arg) m) {
                        | Some _ => false
                        | None =>
                          true /* filter disbled attr out if invalid action */
                        }
                      | _ => true
                      }
                    }
                  )
                  i_rs
                  ms
              )
          ]
          [pcdata (btn_label ^ " [" ^ KC.name key_combo ^ "]")]
      );
    let button_dom = To_dom.of_button button_elt;
    /* listen for the key combo at the document level */
    let _ =
      Js_util.listen_to
        Ev.keypress
        Dom_html.document
        (
          fun evt => {
            let key = Js_util.get_key evt;
            /* let _ = Firebug.console##log evt_key in */
            if (key == KC.key key_combo) {
              i_dom##focus;
              Dom_html.stopPropagation evt;
              Js._false
            } else {
              Js._true
            }
          }
        );
    /* respond to enter and esc inside the input box */
    let _ =
      Js_util.listen_to
        Ev.keyup
        i_dom
        (
          fun evt => {
            let key = Js_util.get_key evt;
            if (key == KC.key KCs.enter) {
              button_dom##click;
              i_dom##blur;
              Js._false
            } else if (
              key == KC.key KCs.esc
            ) {
              i_dom##blur;
              Js._false
            } else {
              Js._true
            }
          }
        );
    /* stop propagation of keys when focus is in input box */
    let _ =
      Js_util.listen_to
        Ev.keypress
        i_dom
        (
          fun evt => {
            Dom_html.stopPropagation evt;
            Js._true
          }
        );
    Html5.(
      div a::[a_class ["input-group"]] [span a::[a_class ["input-group-btn"]] [button_elt], i_elt]
    )
  };
  /* actions that take two inputs. the conversion function
   * goes from a pair of strings to an arg option where arg is
   * the action argument. */
  let action_input_input_button
      action
      conv
      btn_label
      input_id
      key_combo
      placeholder_str_1
      placeholder_str_2 => {
    /* analagous to action_input_button, but with two input boxes.
     * could define an n-ary version of this, but this is probably more clear for now */
    let input_id_1 = input_id ^ "_1";
    let input_id_2 = input_id ^ "_2";
    let ((i_rs_1, i_rf_1), i_elt_1, i_dom_1) = Js_util.r_input input_id_1 placeholder_str_1;
    let ((i_rs_2, i_rf_2), i_elt_2, i_dom_2) = Js_util.r_input input_id_2 placeholder_str_2;
    let clear_input () => {
      i_dom_1##.value := Js.string "";
      i_rf_1 "";
      i_dom_2##.value := Js.string "";
      i_rf_2 ""
    };
    let button_elt =
      Html5.(
        button
          a::[
            a_class ["btn", "btn-default"],
            a_id (input_id ^ "_button"),
            a_onclick (
              fun _ => {
                let i1 = React.S.value i_rs_1;
                let i2 = React.S.value i_rs_2;
                let arg = Util.force_opt (conv (i1, i2));
                doAction (action arg);
                clear_input ();
                true
              }
            ),
            R.filter_attrib
              (a_disabled ())
              (
                S.l3
                  (
                    fun s1 s2 m =>
                      switch (conv (s1, s2)) {
                      | Some arg =>
                        switch (Action.performSyn () Ctx.empty (action arg) m) {
                        | Some _ => false
                        | None => true
                        }
                      | None => true
                      }
                  )
                  i_rs_1
                  i_rs_2
                  ms
              )
          ]
          [pcdata (btn_label ^ " [" ^ KC.name key_combo ^ "]")]
      );
    let button_dom = To_dom.of_button button_elt;
    let _ =
      Js_util.listen_to
        Ev.keypress
        Dom_html.document
        (
          fun evt => {
            let key = Js_util.get_key evt;
            if (key == KC.key key_combo) {
              Firebug.console##log "in c";
              i_dom_1##focus;
              Dom_html.stopPropagation evt;
              Js._false
            } else {
              Js._true
            }
          }
        );
    let i_keyup_listener i_dom =>
      Js_util.listen_to
        Ev.keyup
        i_dom
        (
          fun evt => {
            let key = Js_util.get_key evt;
            if (key == KC.key KCs.enter) {
              button_dom##click;
              i_dom##blur;
              Js._false
            } else if (
              key == KC.key KCs.esc
            ) {
              i_dom##blur;
              Js._false
            } else {
              Dom_html.stopPropagation evt;
              Js._true
            }
          }
        );
    let _ = i_keyup_listener i_dom_1;
    let _ = i_keyup_listener i_dom_2;
    let i_keypress_listener i_dom =>
      Js_util.listen_to
        Ev.keypress
        i_dom
        (
          fun evt => {
            Dom_html.stopPropagation evt;
            Js._true
          }
        );
    let _ = i_keypress_listener i_dom_1;
    let _ = i_keypress_listener i_dom_2;
    Html5.(
      div
        a::[a_class ["input-group"]]
        [span a::[a_class ["input-group-btn"]] [button_elt], i_elt_1, i_elt_2]
    )
  };
  /* now construct the action palette entries*/
  /* movement */
  /* let moveChild1 = action_button (Action.Move (Action.Child 1)) "move child 1" KCs.number_1;
     let moveChild2 = action_button (Action.Move (Action.Child 2)) "move child 2" KCs.number_2;
     let moveChild3 = action_button (Action.Move (Action.Child 3)) "move child 3" KCs.number_3;
     let moveParent = action_button (Action.Move Action.Parent) "move parent" KCs.p; */
  /* deletion */
  let delete = action_button Action.Delete "delete" KCs.del;
  let backspace = action_button Action.Backspace "backspace" KCs.backspace;
  /* type construction */
  let constructArrow =
    action_button (Action.Construct Action.SArrow) "construct arrow" KCs.greaterThan;
  let constructNum = action_button (Action.Construct Action.SNum) "construct num" KCs.n;
  let constructSum = action_button (Action.Construct Action.SSum) "construct sum" KCs.s;
  /* expression construction */
  let constructAsc = action_button (Action.Construct Action.SAsc) "construct asc" KCs.colon;
  let constructLet =
    action_input_button
      (fun v => Action.Construct (Action.SLet v))
      (
        fun s =>
          switch (String.compare s "") {
          | 0 => None
          | _ => Some s
          }
      )
      "construct let"
      "var_input"
      KCs.equals
      "Enter var + press Enter";
  let constructVar =
    action_input_button
      (fun v => Action.Construct (Action.SVar v))
      (
        fun s =>
          switch (String.compare s "") {
          | 0 => None
          | _ => Some s
          }
      )
      "construct var"
      "var_input"
      KCs.v
      "Enter var + press Enter";
  let constructLam =
    action_input_button
      (fun v => Action.Construct (Action.SLam v))
      (
        fun s =>
          switch (String.compare s "") {
          | 0 => None
          | _ => Some s
          }
      )
      "construct lam"
      "lam_input"
      KCs.backslash
      "Enter var + press Enter";
  let constructAp = action_button (Action.Construct Action.SAp) "construct ap" KCs.openParens;
  let constructLit =
    action_input_button
      (fun n => Action.Construct (Action.SLit n))
      (
        fun s =>
          switch (String.compare s "") {
          | 0 => None
          | _ =>
            try (Some (int_of_string s)) {
            | _ => None
            }
          }
      )
      "construct lit"
      "lit_input"
      KCs.pound
      "Enter num + press Enter";
  let constructPlus =
    action_button (Action.Construct (Action.SOp UHExp.Plus)) "construct plus" KCs.plus;
  let constructTimes =
    action_button
      (Action.Construct (Action.SOp UHExp.Times)) "construct explicit times" KCs.asterisk;
  let constructSpace =
    action_button (Action.Construct (Action.SOp UHExp.Space)) "construct implicit times" KCs.space;
  let constructInjL =
    action_button (Action.Construct (Action.SInj UHExp.L)) "construct inj L" KCs.l;
  let constructInjR =
    action_button (Action.Construct (Action.SInj UHExp.R)) "construct inj R" KCs.r;
  let constructCase =
    action_input_input_button
      (fun (v1, v2) => Action.Construct (Action.SCase v1 v2 [@implicit_arity]))
      (
        fun (s1, s2) => {
          let s1_empty = String.compare s1 "";
          let s2_empty = String.compare s2 "";
          switch (s1_empty, s2_empty) {
          | (0, _) => None
          | (_, 0) => None
          | _ => Some (s1, s2)
          }
        }
      )
      "construct case"
      "case_input"
      KCs.c
      "Enter var + press Tab"
      "Enter var + press Enter";
  /* let movementActions =
     Html5.(
       div
         a::[a_class ["panel", "panel-default"]]
         [
           div a::[a_class ["panel-title"]] [pcdata "Movement"],
           div
             a::[a_class ["panel-body"]]
             [moveChild1, br (), moveChild2, br (), moveChild3, br (), moveParent]
         ]
     ); */
  let typeConstructionActions =
    Html5.(
      div
        a::[a_class ["panel", "panel-default"]]
        [
          div a::[a_class ["panel-title"]] [pcdata "Type Construction"],
          div
            a::[a_class ["panel-body"]]
            [constructArrow, br (), constructNum, br (), constructSum, br ()]
        ]
    );
  let expressionConstructionActions =
    Html5.(
      div
        a::[a_class ["panel", "panel-default"]]
        [
          div a::[a_class ["panel-title"]] [pcdata "Expression Construction"],
          div
            a::[a_class ["panel-body"]]
            [
              constructAsc,
              br (),
              constructLet,
              br (),
              constructVar,
              constructLam,
              constructAp,
              br (),
              constructLit,
              constructPlus,
              constructTimes,
              constructSpace,
              br (),
              constructInjL,
              br (),
              constructInjR,
              br (),
              constructCase
            ]
        ]
    );
  let deleteActions =
    Html5.(
      div
        a::[a_class ["panel", "panel-default"]]
        [
          div a::[a_class ["panel-title"]] [pcdata "Deletion"],
          div a::[a_class ["panel-body"]] [delete, backspace]
        ]
    );
  /* finally, put it all together into the action palette */
  Html5.(
    div
      a::[a_class ["action-palette"]]
      [
        /* movementActions, */
        typeConstructionActions,
        expressionConstructionActions,
        deleteActions
      ]
  )
};
