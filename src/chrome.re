open View;

open Tyxml_js;

open Semantics.Core;

let view ((ms, es, do_action): Model.mt) => {
  /* helpers */
  let kc = Js_util.KeyCombo.keyCode;
  /* # pretty printed view */
  let pp_view_width = 50;
  let view_rs =
    React.S.map
      (
        fun e => {
          let view = PPView.of_hexp [] e;
          /* returns a pair of a doc and rev_paths table */
          Pretty.PP.sdoc_of_doc pp_view_width view
        }
      )
      es; /* only updates when the underlying erasure changes */
  /* ## stream of HTML from the above doc stream */
  let pp_rs = React.S.map (fun (sdoc, _) => [Pretty.HTML_Of_SDoc.html_of_sdoc sdoc]) view_rs;
  let pp_view =
    /* container div is content editable, but with input disabled */
    R.Html5.div
      a::
        Html5.[
          a_contenteditable true,
          a_onkeypress (
            fun evt => {
              /* when pressing letters, don't actually insert them */
              let charCode = Js.Optdef.get evt##.charCode (fun () => assert false);
              let key = Js.to_string (Js.Optdef.get evt##.key (fun () => assert false));
              if (charCode != 0 || String.equal key "Enter" || String.equal key "Tab") {
                Dom.preventDefault evt;
                true
              } else {
                true
              }
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
  /* set_cursor is called when we need to put the cursor at the appropriate place given
     the current Z-expression */
  let first_leaf node => {
    let cur = ref node;
    let found = ref false;
    while (not !found) {
      let cur_node = !cur;
      switch (Js.Opt.to_option cur_node##.firstChild) {
      | Some firstChild => cur := firstChild
      | None => found := true
      }
    };
    !cur
  };
  let last_leaf node => {
    let cur = ref node;
    let found = ref false;
    while (not !found) {
      let cur_node = !cur;
      switch (Js.Opt.to_option cur_node##.lastChild) {
      | Some lastChild => cur := lastChild
      | None => found := true
      }
    };
    !cur
  };
  let move_cursor_before node => {
    let cursor_leaf = first_leaf node;
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    range##setStartBefore cursor_leaf;
    range##setEndBefore cursor_leaf;
    selection##removeAllRanges;
    selection##addRange range
  };
  let move_cursor_after node => {
    let cursor_leaf = last_leaf node;
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    range##setStartAfter cursor_leaf;
    range##setEndAfter cursor_leaf;
    selection##removeAllRanges;
    selection##addRange range
  };
  let set_cursor () => {
    let ((ze, _), _) = React.S.value ms;
    let cursor_path = Semantics.Core.Path.of_zexp ze;
    let id = PPView.id_of_rev_path (List.rev cursor_path);
    let cursor_elem = Js_util.forceGetElementById id;
    let cursor_node: Js.t Dom.node = Js.Unsafe.coerce cursor_elem;
    move_cursor_before cursor_node
  };
  /* Construct a simple DOM change listener to trigger cursor
     movements. we could also just listen directly to the DOM
     changes for pp_view, but this avoids the browser needlessly
     computing the diffs for us. instead we make a simple hidden
     counter div that increments on every change to the erasure */
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
      es;
  let num_changes_counter =
    Html5.(div a::[a_id "num_changes_counter"] [R.Html5.pcdata num_changes_str_rs]);
  let num_changes_counter_dom = Tyxml_js.To_dom.of_div num_changes_counter;
  let _ =
    MutationObserver.observe
      child_list::false
      attributes::false
      node::num_changes_counter_dom
      subtree::true
      character_data::true
      f::(fun _ _ => set_cursor ())
      ();
  /* listen to selection change events and respond */
  let rev_paths_rs = React.S.map (fun (_, rev_paths) => rev_paths) view_rs;
  let fix_anchor selection anchor =>
    switch anchor##.nodeType {
    | Dom.TEXT =>
      let anchor' = Js.Opt.get (Dom.CoerceTo.text anchor) (fun () => assert false);
      let length = anchor'##.length;
      let anchor_offset = selection##.anchorOffset;
      if (anchor_offset == length) {
        switch (Js.Opt.to_option anchor##.parentNode) {
        | Some parent =>
          switch parent##.nodeType {
          | Dom.ELEMENT =>
            let parent_elem =
              Js.Opt.get (Dom_html.CoerceTo.element parent) (fun () => assert false);
            let classList = parent_elem##.classList;
            let is_space = Js.to_bool (classList##contains (Js.string "space"));
            if is_space {
              switch (Js.Opt.to_option parent##.nextSibling) {
              | Some sibling => first_leaf sibling
              | None => anchor
              }
            } else {
              anchor
            }
          | _ => anchor
          }
        | None => anchor
        }
      } else if (
        anchor_offset == 0
      ) {
        switch (Js.Opt.to_option anchor##.parentNode) {
        | Some parent =>
          switch parent##.nodeType {
          | Dom.ELEMENT =>
            let parent_elem =
              Js.Opt.get (Dom_html.CoerceTo.element parent) (fun () => assert false);
            let classList = parent_elem##.classList;
            if (Js.to_bool (classList##contains (Js.string "space"))) {
              switch (Js.Opt.to_option parent##.previousSibling) {
              | Some sibling => last_leaf sibling
              | None => anchor
              }
            } else {
              anchor
            }
          | _ => anchor
          }
        | None => anchor
        }
      } else {
        anchor
      }
    | Dom.ELEMENT =>
      let anchor_elem = Js.Opt.get (Dom_html.CoerceTo.element anchor) (fun () => assert false);
      let classList = anchor_elem##.classList;
      if (not (Js.to_bool (classList##contains (Js.string "SText")))) {
        let anchorOffset = selection##.anchorOffset;
        let children = anchor##.childNodes;
        let child_opt = children##item anchorOffset;
        switch (Js.Opt.to_option child_opt) {
        | Some child =>
          switch (Js.Opt.to_option (Dom_html.CoerceTo.element child)) {
          | Some child_element =>
            let tagName = Js.to_string child_element##.tagName;
            if (String.equal tagName "BR") {
              switch (Js.Opt.to_option child_element##.previousSibling) {
              | Some sibling => last_leaf sibling
              | None => first_leaf child
              }
            } else {
              first_leaf child
            }
          | None => first_leaf child
          }
        | None => anchor
        }
      } else {
        anchor
      }
    | _ => anchor
    };
  let clear_cursors () => {
    let cursors = Dom_html.document##getElementsByClassName (Js.string "cursor");
    let num_cursors = cursors##.length;
    for i in 0 to (num_cursors - 1) {
      let cursor = Js.Opt.get (cursors##item i) (fun () => assert false);
      cursor##.classList##remove (Js.string "cursor")
    }
  };
  let has_class classList cls => Js.to_bool (classList##contains (Js.string cls));
  let do_transport () => {
    let selection = Dom_html.window##getSelection;
    let anchor = selection##.anchorNode;
    let parent_elem =
      switch anchor##.nodeType {
      | Dom.TEXT =>
        switch (Js.Opt.to_option anchor##.parentNode) {
        | Some parentNode => Js.Opt.to_option (Dom_html.CoerceTo.element parentNode)
        | None => None
        }
      | Dom.ELEMENT => Js.Opt.to_option (Dom_html.CoerceTo.element anchor)
      | _ =>
        Js_util.log "BAD ANCHOR";
        None
      };
    switch parent_elem {
    | Some parent_elem =>
      let classList = parent_elem##.classList;
      let has_class = has_class classList;
      if (has_class "hole-before-1") {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          /* Js_util.log ("moving after " ^ string_of_int anchorOffset); */
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some super_parent =>
            switch (Js.Opt.to_option super_parent##.lastChild) {
            | Some lastChild => move_cursor_after lastChild
            | None => ()
            }
          | None => ()
          }
        } else if (
          anchorOffset == 2
        ) {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some super_parent =>
            switch (Js.Opt.to_option super_parent##.firstChild) {
            | Some firstChild => move_cursor_before firstChild
            | None => ()
            }
          | None => ()
          }
        }
      } else if (
        has_class "holeName"
      ) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 0) {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some super_parent =>
            switch (Js.Opt.to_option super_parent##.firstChild) {
            | Some firstChild => move_cursor_before firstChild
            | None => ()
            }
          | None => ()
          }
        } else {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some super_parent =>
            switch (Js.Opt.to_option super_parent##.lastChild) {
            | Some lastChild => move_cursor_after lastChild
            | None => ()
            }
          | None => ()
          }
        }
      } else if (
        has_class "hole-after-1" || has_class "hole-before-2"
      ) {
        /* Js_util.log "moving before"; */
        switch (Js.Opt.to_option parent_elem##.parentNode) {
        | Some super_parent =>
          switch (Js.Opt.to_option super_parent##.firstChild) {
          | Some firstChild => move_cursor_before firstChild
          | None => ()
          }
        | None => ()
        }
      } else if (
        has_class "nonEmptyHole-after-inner"
      ) {
        let anchor_offset = selection##.anchorOffset;
        if (anchor_offset > 0) {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some super_parent =>
            switch (Js.Opt.to_option super_parent##.parentNode) {
            | Some super_parent' =>
              switch (Js.Opt.to_option super_parent'##.lastChild) {
              | Some lastChild =>
                Js_util.log "moving after";
                Js_util.log lastChild;
                move_cursor_after lastChild
              | None => ()
              }
            | None => ()
            }
          | None => ()
          }
        }
      } else if (
        has_class "nonEmptyHole-after-outer"
      ) {
        let anchor_offset = selection##.anchorOffset;
        Js_util.log ("anchor_offset = " ^ string_of_int anchor_offset);
        if (anchor_offset == 2) {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some super_parent =>
            switch (Js.Opt.to_option super_parent##.firstChild) {
            | Some firstChild =>
              switch (Js.Opt.to_option firstChild##.firstChild) {
              | Some firstChild => move_cursor_after firstChild
              | _ => ()
              }
            | _ => ()
            }
          | _ => ()
          }
        }
      } else if (
        has_class "nonEmptyHoleName"
      ) {
        let anchor_offset = selection##.anchorOffset;
        if (anchor_offset == 0) {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some super_parent =>
            switch (Js.Opt.to_option super_parent##.firstChild) {
            | Some firstChild =>
              switch (Js.Opt.to_option firstChild##.firstChild) {
              | Some firstChild => move_cursor_after firstChild
              | _ => ()
              }
            | _ => ()
            }
          | _ => ()
          }
        } else {
          switch (Js.Opt.to_option parent_elem##.nextSibling) {
          | Some sibling => move_cursor_after sibling
          | None => ()
          }
        }
      } else if (
        has_class "SIndentation"
      ) {
        let anchor_text = Js.Opt.get (Dom.CoerceTo.text anchor) (fun () => assert false);
        let len = anchor_text##.length;
        let anchor_offset = selection##.anchorOffset;
        Js_util.log ("indentation: " ^ string_of_int len ^ string_of_int anchor_offset);
        if (len == anchor_offset) {
          switch (Js.Opt.to_option parent_elem##.nextSibling) {
          | Some sibling => move_cursor_before (first_leaf sibling)
          | None => ()
          }
        }
      } else {
        ()
      }
    | None => ()
    }
  };
  let _ =
    Js_util.listen_to_t
      (Dom.Event.make "selectionchange")
      Dom_html.document
      (
        fun evt => {
          /* transport */
          do_transport ();
          /* get effective anchor node (where selection began) */
          let selection = Dom_html.window##getSelection;
          let anchor = fix_anchor selection selection##.anchorNode;
          Js_util.log selection;
          /* get current paths hash table */
          let rev_paths = React.S.value rev_paths_rs;
          /* traverse up the DOM until we find an element with an id in the paths table */
          let cur = ref (Js.some anchor);
          let found = ref false;
          while (Js.Opt.test !cur && not !found) {
            let cur_node = Js.Opt.get !cur (fun () => assert false);
            switch cur_node##.nodeType {
            | Dom.ELEMENT =>
              let cur_element' = Dom_html.CoerceTo.element cur_node;
              let cur_element = Js.Opt.get cur_element' (fun () => assert false);
              let cur_id = Js.to_string cur_element##.id;
              switch (Hashtbl.find rev_paths cur_id) {
              | rev_path =>
                found := true;
                do_action (Semantics.Core.Action.MoveTo (List.rev rev_path));
                clear_cursors ();
                let elem = Js_util.forceGetElementById cur_id;
                elem##.classList##add (Js.string "cursor")
              | exception Not_found => ()
              };
              ()
            | _ => ()
            };
            cur := cur_node##.parentNode;
            ()
          };
          ()
        }
      );
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
      ms;
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
      ms;
  let result_view = R.Html5.div (ReactiveData.RList.from_signal result_rs);
  /* checkboxes */
  let ((show_hole_envs_checkbox_rs, _), show_hole_envs_checkbox, _) =
    Js_util.r_checkbox "show_hole_envs_checkbox" "Show hole environments" false;
  let root_classes =
    React.S.l1
      (
        fun show_hole_envs => {
          let show_hole_envs_class = if show_hole_envs {"show-hole-envs"} else {"hide-hole-envs"};
          [show_hole_envs_class]
        }
      )
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
                          pcdata "Hazel is an experiment in ",
                          strong [pcdata "hole-driven development"],
                          pcdata ". Use the actions on the right to construct a lambda term. Navigate using the standard text cursor."
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
                    Action_palette.make_palette (ms, es, do_action) set_cursor,
                    div a::[a_class ["panel-title"]] [pcdata "Options"],
                    show_hole_envs_checkbox
                    /* num_changes_counter */
                  ]
              ]
          ]
      );
  (chrome, set_cursor)
};
