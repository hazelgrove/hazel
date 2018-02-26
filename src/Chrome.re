open View;

open Tyxml_js;

open Semantics.Core;

let view ((ms, es, do_action): Model.mt) => {
  /* helpers */
  let kc = JSUtil.KeyCombo.key;
  /* # pretty printed view */
  let pp_view_width = 50;
  let prefix = "view";
  let view_rs =
    React.S.map
      (
        fun e => {
          let view = View.of_hexp prefix [] e;
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
              let key = JSUtil.get_key evt;
              let is_backspace = key == kc JSUtil.KeyCombos.backspace;
              let is_del = key == kc JSUtil.KeyCombos.del;
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
  let _ = JSUtil.listen_to_t (Dom.Event.make "paste") pp_view_dom preventDefault_handler;
  let _ = JSUtil.listen_to_t (Dom.Event.make "cut") pp_view_dom preventDefault_handler;
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
  let node_length node => {
    let text_node = Js.Opt.get (Dom.CoerceTo.text node) (fun () => assert false);
    text_node##.length
  };
  let move_cursor_before node => {
    let cursor_leaf = first_leaf node;
    /* JSUtil.log "moving_cursor_before "; */
    /* JSUtil.log cursor_leaf; */
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    range##setStart cursor_leaf 0;
    range##setEnd cursor_leaf 0;
    selection##removeAllRanges;
    selection##addRange range
  };
  let move_cursor_after node => {
    let cursor_leaf = last_leaf node;
    /* JSUtil.log "moving_cursor_after "; */
    /* JSUtil.log cursor_leaf; */
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    let len = node_length cursor_leaf;
    range##setStart cursor_leaf len;
    range##setEnd cursor_leaf len;
    selection##removeAllRanges;
    selection##addRange range
  };
  let set_cursor () => {
    let ((ze, _), _) = React.S.value ms;
    let (cursor_path, cursor_side) = Semantics.Core.Path.of_zexp ze;
    let id = View.id_of_rev_path prefix (List.rev cursor_path);
    let cursor_elem = JSUtil.forceGetElementById id;
    let cursor_node: Js.t Dom.node = Js.Unsafe.coerce cursor_elem;
    switch cursor_side {
    | Before
    | On => move_cursor_before (first_leaf cursor_node)
    | After => move_cursor_after (last_leaf cursor_node)
    }
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
  let fix_anchor selection anchor => {
    let anchorOffset = selection##.anchorOffset;
    let anchor' =
      switch anchor##.nodeType {
      | Dom.TEXT =>
        let anchor' = Js.Opt.get (Dom.CoerceTo.text anchor) (fun () => assert false);
        let length = anchor'##.length;
        if (anchorOffset == length) {
          switch (Js.Opt.to_option anchor##.parentNode) {
          | Some parent =>
            switch parent##.nodeType {
            | Dom.ELEMENT =>
              let parent_elem =
                Js.Opt.get (Dom_html.CoerceTo.element parent) (fun () => assert false);
              let classList = parent_elem##.classList;
              let is_space = Js.to_bool (classList##contains (Js.string "space"));
              let is_indentation = Js.to_bool (classList##contains (Js.string "SIndentation"));
              let is_op = Js.to_bool (classList##contains (Js.string "seq-op"));
              if (is_space || is_indentation || is_op) {
                switch (Js.Opt.to_option parent##.nextSibling) {
                | Some sibling => (first_leaf sibling, 0)
                | None => (anchor, anchorOffset)
                }
              } else {
                (anchor, anchorOffset)
              }
            | _ => (anchor, anchorOffset)
            }
          | None => (anchor, anchorOffset)
          }
        } else if (
          anchorOffset == 0
        ) {
          switch (Js.Opt.to_option anchor##.parentNode) {
          | Some parent =>
            switch parent##.nodeType {
            | Dom.ELEMENT =>
              let parent_elem =
                Js.Opt.get (Dom_html.CoerceTo.element parent) (fun () => assert false);
              let classList = parent_elem##.classList;
              let is_space = Js.to_bool (classList##contains (Js.string "space"));
              let is_op = Js.to_bool (classList##contains (Js.string "seq-op"));
              if (is_space || is_op) {
                switch (Js.Opt.to_option parent##.previousSibling) {
                | Some sibling =>
                  let anchor' = last_leaf sibling;
                  (anchor', node_length anchor')
                | None => (anchor, anchorOffset)
                }
              } else {
                (anchor, anchorOffset)
              }
            | _ => (anchor, anchorOffset)
            }
          | None => (anchor, anchorOffset)
          }
        } else {
          (anchor, anchorOffset)
        }
      | Dom.ELEMENT =>
        let anchor_elem = Js.Opt.get (Dom_html.CoerceTo.element anchor) (fun () => assert false);
        let classList = anchor_elem##.classList;
        if (not (Js.to_bool (classList##contains (Js.string "SText")))) {
          let children = anchor##.childNodes;
          let child_opt = children##item anchorOffset;
          switch (Js.Opt.to_option child_opt) {
          | Some child =>
            switch (Js.Opt.to_option (Dom_html.CoerceTo.element child)) {
            | Some child_element =>
              let tagName = Js.to_string child_element##.tagName;
              if (String.equal tagName "BR") {
                switch (Js.Opt.to_option child_element##.previousSibling) {
                | Some sibling =>
                  let anchor' = last_leaf sibling;
                  (anchor', node_length anchor')
                | None => (first_leaf child, 0)
                }
              } else {
                (anchor, anchorOffset)
              }
            | None => (anchor, anchorOffset)
            }
          | None => (anchor, anchorOffset)
          }
        } else {
          (anchor, anchorOffset)
        }
      | _ => (anchor, anchorOffset)
      };
    anchor'
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
    JSUtil.log selection;
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
        JSUtil.log "BAD ANCHOR";
        None
      };
    switch parent_elem {
    | Some parent_elem =>
      let classList = parent_elem##.classList;
      let has_class = has_class classList; /* partially applied for convenience */
      JSUtil.log parent_elem##.className;
      if (has_class "hole-before-1") {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          /* came in from the left */
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.lastChild) {
            | Some lastChild => move_cursor_after lastChild
            | None => ()
            }
          | None => ()
          }
        } else if (
          anchorOffset == 2
        ) {
          /* came in from above or below */
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.firstChild) {
            | Some firstChild => move_cursor_before firstChild
            | None => ()
            }
          | None => ()
          }
        }
      } else if (
        has_class "op-before-1"
      ) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          /* came in from the left */
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.nextSibling) {
            | Some sibling => move_cursor_before sibling
            | None => ()
            }
          | None => ()
          }
        } else if (
          anchorOffset == 2
        ) {
          /* came in from above or below */
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.previousSibling) {
            | Some sibling => move_cursor_after sibling
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
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.firstChild) {
            | Some firstChild => move_cursor_before firstChild
            | None => ()
            }
          | None => ()
          }
        } else {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.lastChild) {
            | Some lastChild => move_cursor_after lastChild
            | None => ()
            }
          | None => ()
          }
        }
      } else if (
        has_class "op-center"
      ) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset <= 1) {
          /* clicked before the + */
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.previousSibling) {
            | Some sibling => move_cursor_after sibling
            | None => ()
            }
          | None => ()
          }
        } else {
          /* clicked after the + */
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.nextSibling) {
            | Some sibling => move_cursor_before sibling
            | None => ()
            }
          | None => ()
          }
        }
      } else if (
        has_class "hole-before-2" || has_class "hole-after-1"
      ) {
        switch (Js.Opt.to_option parent_elem##.parentNode) {
        | Some grandparent =>
          switch (Js.Opt.to_option grandparent##.firstChild) {
          | Some firstChild => move_cursor_before firstChild
          | None => ()
          }
        | None => ()
        }
      } else if (
        has_class "op-before-2" || has_class "op-after-1"
      ) {
        switch (Js.Opt.to_option parent_elem##.parentNode) {
        | Some grandparent =>
          switch (Js.Opt.to_option grandparent##.previousSibling) {
          | Some sibling => move_cursor_after sibling
          | None => ()
          }
        | None => ()
        }
      } else if (
        has_class "op-after-2"
      ) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 0) {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.previousSibling) {
            | Some sibling => move_cursor_after sibling
            | None => ()
            }
          | None => ()
          }
        } else {
          switch (Js.Opt.to_option parent_elem##.parentNode) {
          | Some grandparent =>
            switch (Js.Opt.to_option grandparent##.nextSibling) {
            | Some sibling => move_cursor_before sibling
            | None => ()
            }
          | None => ()
          }
        }
      } else if (
        has_class "seq-op"
      ) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          switch (Js.Opt.to_option parent_elem##.nextSibling) {
          | Some sibling => move_cursor_before sibling
          | None => ()
          }
        }
      } else if (
        has_class "op-no-margin"
      ) {
        switch (Js.Opt.to_option parent_elem##.parentNode) {
        | Some grandparent =>
          let anchorOffset = selection##.anchorOffset;
          if (anchorOffset == 0) {
            switch (Js.Opt.to_option grandparent##.previousSibling) {
            | Some sibling => move_cursor_after sibling
            | None => ()
            }
          } else {
            switch (Js.Opt.to_option grandparent##.nextSibling) {
            | Some sibling => move_cursor_before sibling
            | None => ()
            }
          }
        | None => ()
        }
      } else if (
        has_class "lambda-dot" || has_class "openParens" || has_class "space"
      ) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          switch (Js.Opt.to_option parent_elem##.nextSibling) {
          | Some sibling => move_cursor_before sibling
          | None => ()
          }
        }
      } else if (
        has_class "closeParens"
      ) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 0) {
          switch (Js.Opt.to_option parent_elem##.previousSibling) {
          | Some sibling => move_cursor_after sibling
          | None => ()
          }
        }
      }
    | None => ()
    }
  };
  let get_anchor_elem (anchor: Js.t Dom.node) =>
    switch anchor##.nodeType {
    | Dom.ELEMENT => Js.Opt.get (Dom_html.CoerceTo.element anchor) (fun () => assert false)
    | Dom.TEXT =>
      let parentNode = Js.Opt.get anchor##.parentNode (fun () => assert false);
      Js.Opt.get (Dom_html.CoerceTo.element parentNode) (fun () => assert false)
    | _ => assert false
    };
  /* TODO this should probably go in view.re */
  let determine_cursor_side
      (selection: Js.t Dom_html.selection)
      (anchor: Js.t Dom.node)
      (anchorOffset: int)
      (ast_elem: Js.t Dom_html.element) => {
    let classList = ast_elem##.classList;
    let ast_has_class = has_class classList;
    if (ast_has_class "Parenthesized") {
      let anchor_elem = get_anchor_elem anchor;
      let anchor_classList = anchor_elem##.classList;
      if (has_class anchor_classList "openParens") {
        Before
      } else {
        After
      }
    } else if (
      ast_has_class "Asc"
    ) {
      On
    } else if (
      ast_has_class "Let"
    ) {
      let anchor_elem = get_anchor_elem anchor;
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string anchor_elem##.innerHTML;
        if (String.equal innerHTML "let") {
          Before
        } else {
          On
        }
      } else {
        On
      }
    } else if (
      ast_has_class "Var" ||
      ast_has_class "NumLit" || ast_has_class "Num" || ast_has_class "number"
    ) {
      if (anchorOffset == 0) {
        Before
      } else {
        let anchor_elem = get_anchor_elem anchor;
        let innerText = Js.to_string anchor_elem##.innerHTML;
        let length = String.length innerText;
        if (anchorOffset == length) {
          After
        } else {
          On
        }
      }
    } else if (
      ast_has_class "Lam"
    ) {
      let anchor_elem = get_anchor_elem anchor;
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string anchor_elem##.innerHTML;
        if (String.equal innerHTML "\206\187") {
          Before
        } else {
          On
        }
      } else {
        On
      }
    } else if (
      ast_has_class "Ap"
    ) {
      After
    } else if (
      ast_has_class "Inj"
    ) {
      let anchor_elem = get_anchor_elem anchor;
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string anchor_elem##.innerHTML;
        if (String.equal innerHTML "inj") {
          Before
        } else {
          On
        }
      } else if (
        anchorOffset == 1
      ) {
        let innerHTML = Js.to_string anchor_elem##.innerHTML;
        if (String.equal innerHTML ")") {
          After
        } else {
          On
        }
      } else {
        On
      }
    } else if (
      ast_has_class "Case"
    ) {
      let anchor_elem = get_anchor_elem anchor;
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string anchor_elem##.innerHTML;
        if (String.equal innerHTML "case") {
          Before
        } else {
          On
        }
      } else {
        On
      }
    } else if (
      ast_has_class "EmptyHole" || ast_has_class "Hole"
    ) {
      let anchor_elem = get_anchor_elem anchor;
      let anchor_has_class = has_class anchor_elem##.classList;
      if (anchor_has_class "hole-before-1") {
        Before
      } else if (anchor_has_class "hole-after-2") {
        After
      } else {
        JSUtil.log "weird hole cursor position";
        After
      }
    } else if (
      ast_has_class "Arrow"
    ) {
      On
    } else if (
      ast_has_class "Sum"
    ) {
      On
    } else {
      JSUtil.log "Unknown ast element!";
      JSUtil.log classList;
      On
    }
  };
  let string_of_cursor_side cursor_side =>
    switch cursor_side {
    | On => "On"
    | Before => "Before"
    | After => "After"
    };
  let _ =
    JSUtil.listen_to_t
      (Dom.Event.make "selectionchange")
      Dom_html.document
      (
        fun evt => {
          /* transport */
          do_transport ();
          /* get effective anchor node (where selection began) */
          let selection = Dom_html.window##getSelection;
          let (anchor, anchorOffset) = fix_anchor selection selection##.anchorNode;
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
                let path = List.rev rev_path;
                let cursor_side = determine_cursor_side selection anchor anchorOffset cur_element;
                JSUtil.log (string_of_cursor_side cursor_side);
                do_action (Action.MoveTo (path, cursor_side));
                clear_cursors ();
                let elem = JSUtil.forceGetElementById cur_id;
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
          let pp_view = View.of_htype false "result-type" [] htype;
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
            let result = Dynamics.Evaluator.evaluate () d;
            switch result {
            | Dynamics.Evaluator.InvalidInput => [Html5.pcdata "(internal error: invalid input)"]
            | Dynamics.Evaluator.BoxedValue d_val
            | Dynamics.Evaluator.Indet d_val =>
              let pp_view = View.of_dhexp false "result-exp" NotInHole [] d_val;
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
    JSUtil.r_checkbox "show_hole_envs_checkbox" "Show hole environments" false;
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
  let the_cursor_inspector_panel = CursorInspector.cursor_inspector ms;
  /* let the_context_inspector_panel =
     Html5.(div a::[a_class ["context-inspector"]] [pcdata "TODO: Live Context Inspector"]); */
  let the_action_panel = ActionPanel.make (ms, es, do_action) set_cursor;
  let the_options_panel =
    Html5.(
      div
        a::[a_class ["panel", "options-panel"]]
        [PanelUtils.titlebar "Options", show_hole_envs_checkbox]
    );
  let the_sidebar =
    Html5.(
      div
        a::[a_class ["sidebar"]]
        [
          the_cursor_inspector_panel,
          /* the_context_inspector_panel, */
          the_action_panel,
          the_options_panel
          /* num_changes_counter */
        ]
    );
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
                the_sidebar,
                div
                  a::[a_class ["page-area"]]
                  [
                    div
                      a::[a_class ["page"]]
                      [
                        /* h1 [pcdata "Welcome to Hazel"], */
                        /* hr (), */
                        div [
                          pcdata "Hazel is an experiment in ",
                          strong [pcdata "hole-driven development"],
                          pcdata ". Use the actions on the right to construct a lambda term. Navigate using the standard text cursor."
                        ],
                        pp_view_parent,
                        div
                          a::[a_class ["cell-status"]]
                          [
                            div
                              a::[a_class ["type-indicator"]]
                              [
                                div a::[a_class ["type-label"]] [pcdata "Result of type: "],
                                div a::[a_class ["htype-view"]] [htype_view]
                              ]
                          ],
                        div a::[a_class ["result-view"]] [result_view]
                      ]
                  ]
              ]
          ]
      );
  (chrome, set_cursor)
};
