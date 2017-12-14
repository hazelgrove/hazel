open View;

open Tyxml_js;

open Semantics.Core;

let view ((rs, rf): Model.rp) => {
  /* helpers */
  let kc = Js_util.KeyCombo.keyCode;
  /* pretty printed view */
  let pp_view_width = 50;
  let pp_sdoc_locs_rs =
    React.S.map
      (
        fun ((zexp, _), _) => {
          let view = PPView.of_zexp [] zexp;
          Pretty.PP.sdoc_of_doc pp_view_width view
        }
      )
      rs;
  let pp_rs =
    React.S.map
      (
        fun (sdoc, _) => {
          let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc;
          [prettified]
        }
      )
      pp_sdoc_locs_rs;
  let pp_view =
    R.Html5.div
      a::
        Html5.[
          a_contenteditable true,
          a_onkeypress (
            fun evt => {
              /* when pressing letters, don't actually insert them */
              Dom.preventDefault evt;
              true
            }
          ),
          a_onkeydown (
            fun evt => {
              /* prevent backspace and delete, which doesn't trigger keypress */
              let which = Js_util.get_keyCode evt;
              let is_backspace = which == kc Js_util.KeyCombos.backspace;
              let is_del = which == kc Js_util.KeyCombos.del;
              if (is_backspace || is_del) {
                Dom.preventDefault evt;
                false
              } else {
                true
              }
            }
          ),
          a_ondrop (
            fun evt => {
              /* prevent dropping dragged stuff into view */
              Dom.preventDefault evt;
              false
            }
          )
        ]
      (ReactiveData.RList.from_signal pp_rs);
  /* need to prevent cut and paste differently because
     a_onpaste and a_oncut are not defined by Html5 */
  let pp_view_dom = Tyxml_js.To_dom.of_div pp_view;
  let preventDefault_handler evt => {
    Dom.preventDefault evt;
    false
  };
  let _ = Js_util.listen_to_t (Dom.Event.make "paste") pp_view_dom preventDefault_handler;
  let _ = Js_util.listen_to_t (Dom.Event.make "cut") pp_view_dom preventDefault_handler;
  let pp_view_parent = Html5.(div a::[a_id "pp_view", a_class ["ModelExp"]] [pp_view]);
  /* Construct a simple DOM change listener to trigger cursor
     movements (we could also just listen directly to the DOM
     changes for pp_view, but this avoids the browser needlessly
     computing the diffs for us */
  let num_changes = ref 0;
  let num_changes_str_rs =
    React.S.map
      (
        fun _ => {
          let num_changes' = !num_changes + 1;
          num_changes := num_changes';
          string_of_int num_changes'
        }
      )
      rs;
  let num_changes_counter =
    Html5.(div a::[a_id "num_changes_counter"] [R.Html5.pcdata num_changes_str_rs]);
  let num_changes_counter_dom = Tyxml_js.To_dom.of_div num_changes_counter;
  let set_cursor () => {
    let cursors = Dom_html.document##getElementsByClassName (Js.string "cursor");
    let cursor_opt = cursors##item 0;
    let cursor = Js.Opt.get cursor_opt (fun () => assert false);
    let cursor': Js.t Dom.node = Js.Unsafe.coerce cursor;
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    range##setStartBefore cursor';
    range##setEndBefore cursor';
    selection##removeAllRanges;
    selection##addRange range
  };
  let _ =
    MutationObserver.observe
      child_list::false
      attributes::false
      node::num_changes_counter_dom
      subtree::true
      character_data::true
      f::(fun _ _ => set_cursor ())
      ();
  /* type view */
  let htype_rs =
    React.S.map
      (
        fun ((_, htype), _) => {
          let pp_view = PPView.of_htype [] htype;
          let (sdoc, _) = Pretty.PP.sdoc_of_doc pp_view_width pp_view;
          let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc;
          [prettified]
        }
      )
      rs;
  let htype_view = R.Html5.div (ReactiveData.RList.from_signal htype_rs);
  /* result view */
  let result_rs =
    React.S.map
      (
        fun ((zexp, _), _) => {
          let e = ZExp.erase zexp;
          let expanded = Dynamics.DHExp.syn_expand () Ctx.empty e;
          switch expanded {
          | Dynamics.DHExp.DoesNotExpand => [Html5.(pcdata "(does not expand)")] /* should never happen! */
          | Dynamics.DHExp.Expands d ty delta =>
            let result = Dynamics.Evaluator.evaluate () delta d;
            switch result {
            | Dynamics.Evaluator.InvalidInput => [Html5.pcdata "(internal error: invalid input)"]
            | Dynamics.Evaluator.CastError => [Html5.pcdata "(cast error)"]
            | Dynamics.Evaluator.Value d_val
            | Dynamics.Evaluator.Indet d_val =>
              let pp_view = PPView.of_dhexp [] d_val;
              let (sdoc, _) = Pretty.PP.sdoc_of_doc pp_view_width pp_view;
              let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc;
              [prettified]
            }
          }
        }
      )
      rs;
  let result_view = R.Html5.div (ReactiveData.RList.from_signal result_rs);
  /* checkboxes */
  let ((show_hole_names_checkbox_rs, _), show_hole_names_checkbox, _) =
    Js_util.r_checkbox "show_hole_names_checkbox" "Show hole names" true;
  let ((show_hole_envs_checkbox_rs, _), show_hole_envs_checkbox, _) =
    Js_util.r_checkbox "show_hole_envs_checkbox" "Show hole environments" false;
  let root_classes =
    React.S.l2
      (
        fun show_hole_names show_hole_envs => {
          let show_hole_names_class =
            if show_hole_names {"show-hole-names"} else {"hide-hole-names"};
          let show_hole_envs_class = if show_hole_envs {"show-hole-envs"} else {"hide-hole-envs"};
          [show_hole_names_class, show_hole_envs_class]
        }
      )
      show_hole_names_checkbox_rs
      show_hole_envs_checkbox_rs;
  /* final chrome */
  let chrome =
    Tyxml_js.To_dom.of_div
      Html5.(
        div
          a::[a_id "root", R.Html5.a_class root_classes]
          [
            div a::[a_class ["top-bar"]] [span a::[a_class ["logo-text"]] [pcdata "Hazel"]],
            div
              a::[a_class ["main-area"]]
              [
                div
                  a::[a_class ["page-area"]]
                  [
                    div
                      a::[a_class ["page"]]
                      [
                        h1 [pcdata "Welcome to Hazel"],
                        hr (),
                        p [
                          pcdata "Hazel is an experimental structure editor for a simple typed functional programming language."
                        ],
                        pp_view_parent,
                        div
                          a::[a_class ["cell-status"]]
                          [
                            div a::[a_class ["result-label"]] [pcdata "Result: "],
                            div
                              a::[a_class ["type-indicator"]]
                              [
                                div a::[a_class ["type-label"]] [pcdata "Type: "],
                                div a::[a_class ["htype-view"]] [htype_view]
                              ]
                          ],
                        div a::[a_class ["result-view"]] [result_view]
                      ]
                  ],
                div
                  a::[a_class ["sidebar"]]
                  [
                    Action_palette.make_palette (rs, rf),
                    div a::[a_class ["panel-title"]] [pcdata "Options"],
                    show_hole_names_checkbox,
                    show_hole_envs_checkbox,
                    num_changes_counter
                  ]
              ]
          ]
      );
  (chrome, set_cursor)
};
