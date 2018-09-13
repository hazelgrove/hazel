open View;
open Tyxml_js;
open Semantics.Core;
open Model;
let view = (model: Model.t) => {
  let {
    edit_state_rs,
    cursor_info_rs,
    e_rs,
    result_rs,
    user_selected_instances_rs,
    user_selected_instances_rf,
    selected_instance_rs,
    selected_instance_rf,
    do_action,
  } = model;
  let kc = JSUtil.KeyCombo.key;
  let pp_view_width = 50;
  let prefix = "view";
  let view_rs =
    React.S.map(
      e => {
        let view = View.of_hexp(prefix, [], e);
        Pretty.PP.sdoc_of_doc(pp_view_width, view);
      },
      e_rs,
    );

  let pp_rs =
    React.S.map(
      ((sdoc, _)) => [Pretty.HTML_Of_SDoc.html_of_sdoc(sdoc)],
      view_rs,
    );

  let side_of_str_offset = (s, offset) =>
    if (offset == 0) {
      Before;
    } else if (offset == String.length(s)) {
      After;
    } else {
      In(offset);
    };

  let string_insert = (s1, offset, s2) => {
    let prefix = String.sub(s1, 0, offset);
    let length = String.length(s1);
    let suffix = String.sub(s1, offset, length - offset);
    prefix ++ s2 ++ suffix;
  };

  let string_backspace = (s, offset, ctrlKey) => {
    let prefix = ctrlKey ? "" : String.sub(s, 0, offset - 1);
    let length = String.length(s);
    let suffix = String.sub(s, offset, length - offset);
    let offset' = ctrlKey ? 0 : offset - 1;
    (prefix ++ suffix, offset');
  };

  let string_delete = (s, offset, ctrlKey) => {
    let prefix = String.sub(s, 0, offset);
    let length = String.length(s);
    let suffix =
      ctrlKey ? "" : String.sub(s, offset + 1, length - offset - 1);
    (prefix ++ suffix, offset);
  };

  let pp_view =
    R.Html5.div(
      ~a=
        Html5.[
          a_contenteditable(true),
          a_onkeypress(evt => {
            let charCode = Js.Optdef.get(evt##.charCode, () => assert(false));

            let key =
              Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));

            switch (int_of_string_opt(key)) {
            | Some(n) =>
              let cursor_info = React.S.value(cursor_info_rs);
              switch (cursor_info.form) {
              | ZExp.IsHole(_) =>
                Dom.preventDefault(evt);
                do_action(Action.Construct(Action.SLit(n, After)));
                true;
              | ZExp.IsNumLit =>
                let selection = Dom_html.window##getSelection;
                let anchorNode = selection##.anchorNode;
                let nodeValue =
                  Js.to_string(
                    Js.Opt.get(anchorNode##.nodeValue, () => assert(false)),
                  );
                let anchorOffset = selection##.anchorOffset;
                let newNodeValue =
                  string_insert(nodeValue, anchorOffset, key);
                Dom.preventDefault(evt);
                switch (int_of_string_opt(newNodeValue)) {
                | Some(new_n) =>
                  let new_side =
                    side_of_str_offset(newNodeValue, anchorOffset + 1);
                  do_action(Action.Construct(Action.SLit(new_n, new_side)));
                | None => ()
                };
                true;
              | _ =>
                Dom.preventDefault(evt);
                true;
              };
            | None =>
              if (charCode != 0
                  || String.equal(key, "Enter")
                  || String.equal(key, "Tab")) {
                Dom.preventDefault(evt);
                true;
              } else {
                true;
              }
            };
          }),
          a_onkeydown(evt => {
            let key = JSUtil.get_key(evt);

            let is_backspace = key == kc(JSUtil.KeyCombos.backspace);
            let is_del = key == kc(JSUtil.KeyCombos.del);
            if (is_backspace || is_del) {
              let cursor_info = React.S.value(cursor_info_rs);
              switch (cursor_info.form) {
              | ZExp.IsNumLit =>
                let side = cursor_info.side;
                let is_Before =
                  switch (side) {
                  | Before => true
                  | _ => false
                  };
                let is_After =
                  switch (side) {
                  | After => true
                  | _ => false
                  };
                if (is_backspace && is_Before || is_del && is_After) {
                  Dom.preventDefault(evt);
                  false;
                } else {
                  let selection = Dom_html.window##getSelection;
                  Dom_html.stopPropagation(evt);
                  Dom.preventDefault(evt);
                  let anchorNode = selection##.anchorNode;
                  let anchorOffset = selection##.anchorOffset;
                  let nodeValue =
                    Js.to_string(
                      Js.Opt.get(anchorNode##.nodeValue, () => assert(false)),
                    );
                  let ctrlKey = Js.to_bool(evt##.ctrlKey);
                  let (nodeValue', anchorOffset') =
                    is_backspace ?
                      string_backspace(nodeValue, anchorOffset, ctrlKey) :
                      string_delete(nodeValue, anchorOffset, ctrlKey);
                  if (String.equal(nodeValue', "")) {
                    if (is_Before) {
                      do_action(Action.Delete);
                    } else {
                      do_action(Action.Backspace);
                    };
                  } else {
                    let n = int_of_string(nodeValue');
                    let side = side_of_str_offset(nodeValue', anchorOffset');
                    do_action(Action.Construct(Action.SLit(n, side)));
                  };
                  false;
                };
              | _ =>
                Dom.preventDefault(evt);
                false;
              };
            } else {
              true;
            };
          }),
          a_ondrop(evt => {
            Dom.preventDefault(evt);
            false;
          }),
        ],
      ReactiveData.RList.from_signal(pp_rs),
    );

  let pp_view_dom = Tyxml_js.To_dom.of_div(pp_view);
  let preventDefault_handler = evt => {
    Dom.preventDefault(evt);
    false;
  };
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("paste"),
      pp_view_dom,
      preventDefault_handler,
    );

  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("cut"),
      pp_view_dom,
      preventDefault_handler,
    );

  let pp_view_parent =
    Html5.(div(~a=[a_id("pp_view"), a_class(["ModelExp"])], [pp_view]));

  let first_leaf = node => {
    let cur = ref(node);
    let found = ref(false);
    while (! found^) {
      let cur_node = cur^;
      switch (Js.Opt.to_option(cur_node##.firstChild)) {
      | Some(firstChild) => cur := firstChild
      | None => found := true
      };
    };
    cur^;
  };
  let last_leaf = node => {
    let cur = ref(node);
    let found = ref(false);
    while (! found^) {
      let cur_node = cur^;
      switch (Js.Opt.to_option(cur_node##.lastChild)) {
      | Some(lastChild) => cur := lastChild
      | None => found := true
      };
    };
    cur^;
  };
  let node_length = node => {
    let text_node = Js.Opt.get(Dom.CoerceTo.text(node), () => assert(false));
    text_node##.length;
  };
  let move_cursor_before = node => {
    let cursor_leaf = first_leaf(node);
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    range##setStart(cursor_leaf, 0);
    range##setEnd(cursor_leaf, 0);
    selection##removeAllRanges;
    selection##addRange(range);
  };
  let move_cursor_after = node => {
    let cursor_leaf = last_leaf(node);
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    let len = node_length(cursor_leaf);
    range##setStart(cursor_leaf, len);
    range##setEnd(cursor_leaf, len);
    selection##removeAllRanges;
    selection##addRange(range);
  };
  let move_cursor_to = (node, offset) => {
    let cursor_leaf = first_leaf(node);
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    range##setStart(cursor_leaf, offset);
    range##setEnd(cursor_leaf, offset);
    selection##removeAllRanges;
    selection##addRange(range);
  };
  let move_cursor_before_suppress = node => {
    move_cursor_before(node);
    true;
  };
  let move_cursor_after_suppress = node => {
    move_cursor_after(node);
    true;
  };
  let set_cursor_to = ((cursor_path, cursor_side)) => {
    let id = View.id_of_rev_path(prefix, List.rev(cursor_path));
    let cursor_elem = JSUtil.forceGetElementById(id);
    let cursor_node: Js.t(Dom.node) = (
      Js.Unsafe.coerce(cursor_elem): Js.t(Dom.node)
    );
    switch (cursor_side) {
    | In(offset) => move_cursor_to(cursor_node, offset)
    | Before => move_cursor_before(first_leaf(cursor_node))
    | After => move_cursor_after(last_leaf(cursor_node))
    };
  };
  let set_cursor = () => {
    let ((ze, _), _) = React.S.value(edit_state_rs);
    let (cursor_path, cursor_side) = Semantics.Core.Path.of_zexp(ze);
    set_cursor_to((cursor_path, cursor_side));
  };
  let rev_paths_rs = React.S.map(((_, rev_paths)) => rev_paths, view_rs);

  let fix_anchor = (selection, anchor) => {
    let anchorOffset = selection##.anchorOffset;
    let anchor' =
      switch (anchor##.nodeType) {
      | Dom.TEXT =>
        let anchor' =
          Js.Opt.get(Dom.CoerceTo.text(anchor), () => assert(false));

        let length = anchor'##.length;
        if (anchorOffset == length) {
          switch (Js.Opt.to_option(anchor##.parentNode)) {
          | Some(parent) =>
            switch (parent##.nodeType) {
            | Dom.ELEMENT =>
              let parent_elem =
                Js.Opt.get(Dom_html.CoerceTo.element(parent), () =>
                  assert(false)
                );

              let classList = parent_elem##.classList;
              let is_space =
                Js.to_bool(classList##contains(Js.string("space")));

              let is_indentation =
                Js.to_bool(classList##contains(Js.string("SIndentation")));

              let is_op =
                Js.to_bool(classList##contains(Js.string("seq-op")));

              let is_paren =
                Js.to_bool(classList##contains(Js.string("lparen")));

              if (is_space || is_indentation || is_op || is_paren) {
                switch (Js.Opt.to_option(parent##.nextSibling)) {
                | Some(sibling) => (first_leaf(sibling), 0)
                | None => (anchor, anchorOffset)
                };
              } else {
                (anchor, anchorOffset);
              };
            | _ => (anchor, anchorOffset)
            }
          | None => (anchor, anchorOffset)
          };
        } else if (anchorOffset == 0) {
          switch (Js.Opt.to_option(anchor##.parentNode)) {
          | Some(parent) =>
            switch (parent##.nodeType) {
            | Dom.ELEMENT =>
              let parent_elem =
                Js.Opt.get(Dom_html.CoerceTo.element(parent), () =>
                  assert(false)
                );

              let classList = parent_elem##.classList;
              let is_space =
                Js.to_bool(classList##contains(Js.string("space")));

              let is_op =
                Js.to_bool(classList##contains(Js.string("seq-op")));

              let is_paren =
                Js.to_bool(classList##contains(Js.string("rparen")));

              if (is_space || is_op || is_paren) {
                switch (Js.Opt.to_option(parent##.previousSibling)) {
                | Some(sibling) =>
                  let anchor' = last_leaf(sibling);
                  (anchor', node_length(anchor'));
                | None => (anchor, anchorOffset)
                };
              } else {
                (anchor, anchorOffset);
              };
            | _ => (anchor, anchorOffset)
            }
          | None => (anchor, anchorOffset)
          };
        } else {
          (anchor, anchorOffset);
        };
      | Dom.ELEMENT =>
        let anchor_elem =
          Js.Opt.get(Dom_html.CoerceTo.element(anchor), () => assert(false));

        let classList = anchor_elem##.classList;
        if (!Js.to_bool(classList##contains(Js.string("SText")))) {
          let children = anchor##.childNodes;
          let child_opt = children##item(anchorOffset);
          switch (Js.Opt.to_option(child_opt)) {
          | Some(child) =>
            switch (Js.Opt.to_option(Dom_html.CoerceTo.element(child))) {
            | Some(child_element) =>
              let tagName = Js.to_string(child_element##.tagName);

              if (String.equal(tagName, "BR")) {
                switch (Js.Opt.to_option(child_element##.previousSibling)) {
                | Some(sibling) =>
                  let anchor' = last_leaf(sibling);
                  (anchor', node_length(anchor'));
                | None => (first_leaf(child), 0)
                };
              } else {
                (anchor, anchorOffset);
              };
            | None => (anchor, anchorOffset)
            }
          | None => (anchor, anchorOffset)
          };
        } else {
          (anchor, anchorOffset);
        };
      | _ => (anchor, anchorOffset)
      };
    anchor';
  };
  let clear_cursors = () => {
    let cursors =
      Dom_html.document##getElementsByClassName(Js.string("cursor"));
    let num_cursors = cursors##.length;
    for (i in 0 to num_cursors - 1) {
      let cursor = Js.Opt.get(cursors##item(i), () => assert(false));

      cursor##.classList##remove(Js.string("cursor"));
    };
  };
  let has_class = (classList, cls) =>
    Js.to_bool(classList##contains(Js.string(cls)));
  let do_transport = (): bool => {
    let selection = Dom_html.window##getSelection;
    let anchor = selection##.anchorNode;
    let parent_elem =
      switch (anchor##.nodeType) {
      | Dom.TEXT =>
        switch (Js.Opt.to_option(anchor##.parentNode)) {
        | Some(parentNode) =>
          Js.Opt.to_option(Dom_html.CoerceTo.element(parentNode))
        | None => None
        }
      | Dom.ELEMENT => Js.Opt.to_option(Dom_html.CoerceTo.element(anchor))
      | _ =>
        JSUtil.log("BAD ANCHOR");
        None;
      };
    switch (parent_elem) {
    | Some(parent_elem) =>
      let classList = parent_elem##.classList;
      let has_class = has_class(classList);
      if (has_class("hole-before-1")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.lastChild)) {
            | Some(lastChild) => move_cursor_after_suppress(lastChild)
            | None => false
            }
          | None => false
          };
        } else if (anchorOffset == 2) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.firstChild)) {
            | Some(firstChild) => move_cursor_before_suppress(firstChild)
            | None => false
            }
          | None => false
          };
        } else {
          false;
        };
      } else if (has_class("op-before-1")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.nextSibling)) {
            | Some(sibling) => move_cursor_before_suppress(sibling)
            | None => false
            }
          | None => false
          };
        } else if (anchorOffset == 2) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.previousSibling)) {
            | Some(sibling) => move_cursor_after_suppress(sibling)
            | None => false
            }
          | None => false
          };
        } else {
          false;
        };
      } else if (has_class("holeName")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 0) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.firstChild)) {
            | Some(firstChild) => move_cursor_before_suppress(firstChild)
            | None => false
            }
          | None => false
          };
        } else {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.lastChild)) {
            | Some(lastChild) => move_cursor_after_suppress(lastChild)
            | None => false
            }
          | None => false
          };
        };
      } else if (has_class("op-center")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset <= 1) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.previousSibling)) {
            | Some(sibling) => move_cursor_after_suppress(sibling)
            | None => false
            }
          | None => false
          };
        } else {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.nextSibling)) {
            | Some(sibling) => move_cursor_before_suppress(sibling)
            | None => false
            }
          | None => false
          };
        };
      } else if (has_class("hole-before-2") || has_class("hole-after-1")) {
        switch (Js.Opt.to_option(parent_elem##.parentNode)) {
        | Some(grandparent) =>
          switch (Js.Opt.to_option(grandparent##.firstChild)) {
          | Some(firstChild) => move_cursor_before_suppress(firstChild)
          | None => false
          }
        | None => false
        };
      } else if (has_class("op-before-2") || has_class("op-after-1")) {
        switch (Js.Opt.to_option(parent_elem##.parentNode)) {
        | Some(grandparent) =>
          switch (Js.Opt.to_option(grandparent##.previousSibling)) {
          | Some(sibling) => move_cursor_after_suppress(sibling)
          | None => false
          }
        | None => false
        };
      } else if (has_class("op-after-2")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 0) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.previousSibling)) {
            | Some(sibling) => move_cursor_after_suppress(sibling)
            | None => false
            }
          | None => false
          };
        } else {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.nextSibling)) {
            | Some(sibling) => move_cursor_before_suppress(sibling)
            | None => false
            }
          | None => false
          };
        };
      } else if (has_class("seq-op")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          switch (Js.Opt.to_option(parent_elem##.nextSibling)) {
          | Some(sibling) => move_cursor_before_suppress(sibling)
          | None => false
          };
        } else {
          false;
        };
      } else if (has_class("op-no-margin")) {
        switch (Js.Opt.to_option(parent_elem##.parentNode)) {
        | Some(grandparent) =>
          let anchorOffset = selection##.anchorOffset;

          if (anchorOffset == 0) {
            switch (Js.Opt.to_option(grandparent##.previousSibling)) {
            | Some(sibling) => move_cursor_after_suppress(sibling)
            | None => false
            };
          } else {
            switch (Js.Opt.to_option(grandparent##.nextSibling)) {
            | Some(sibling) => move_cursor_before_suppress(sibling)
            | None => false
            };
          };
        | None => false
        };
      } else if (has_class("lambda-dot")
                 || has_class("openParens")
                 || has_class("space")) {
        let anchorOffset = selection##.anchorOffset;

        if (anchorOffset == 1) {
          switch (Js.Opt.to_option(parent_elem##.nextSibling)) {
          | Some(sibling) => move_cursor_before_suppress(sibling)
          | None => false
          };
        } else {
          false;
        };
      } else if (has_class("closeParens")) {
        let anchorOffset = selection##.anchorOffset;

        if (anchorOffset == 0) {
          switch (Js.Opt.to_option(parent_elem##.previousSibling)) {
          | Some(sibling) => move_cursor_after_suppress(sibling)
          | None => false
          };
        } else {
          false;
        };
      } else {
        false;
      };
    | None => false
    };
  };

  let get_anchor_elem = (anchor: Js.t(Dom.node)) =>
    switch (anchor##.nodeType) {
    | Dom.ELEMENT =>
      Js.Opt.get(Dom_html.CoerceTo.element(anchor), () => assert(false))
    | Dom.TEXT =>
      let parentNode = Js.Opt.get(anchor##.parentNode, () => assert(false));
      Js.Opt.get(Dom_html.CoerceTo.element(parentNode), () => assert(false));
    | _ => assert(false)
    };
  let determine_cursor_side =
      (
        selection: Js.t(Dom_html.selection),
        anchor: Js.t(Dom.node),
        anchorOffset: int,
        ast_elem: Js.t(Dom_html.element),
      ) => {
    let classList = ast_elem##.classList;
    let ast_has_class = has_class(classList);
    if (ast_has_class("Parenthesized")) {
      let anchor_elem = get_anchor_elem(anchor);
      let anchor_classList = anchor_elem##.classList;
      if (has_class(anchor_classList, "openParens")) {
        Before;
      } else {
        After;
      };
    } else if (ast_has_class("Asc")) {
      In(0);
    } else if (ast_has_class("Let")) {
      let anchor_elem = get_anchor_elem(anchor);
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string(anchor_elem##.innerHTML);
        if (String.equal(innerHTML, "let")) {
          Before;
        } else {
          After;
        };
      } else {
        After;
      };
    } else if (ast_has_class("Var")
               || ast_has_class("NumLit")
               || ast_has_class("Num")
               || ast_has_class("number")) {
      if (anchorOffset == 0) {
        Before;
      } else {
        let anchor_elem = get_anchor_elem(anchor);
        let innerText = Js.to_string(anchor_elem##.innerHTML);
        let length = String.length(innerText);
        if (anchorOffset == length) {
          After;
        } else {
          In(anchorOffset);
        };
      };
    } else if (ast_has_class("Lam")) {
      let anchor_elem = get_anchor_elem(anchor);
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string(anchor_elem##.innerHTML);
        if (String.equal(innerHTML, LangUtil.lamSym)) {
          Before;
        } else {
          After;
        };
      } else {
        After;
      };
    } else if (ast_has_class("Ap")) {
      After;
    } else if (ast_has_class("Inj")) {
      let anchor_elem = get_anchor_elem(anchor);
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string(anchor_elem##.innerHTML);

        if (String.equal(innerHTML, "inj")) {
          Before;
        } else {
          In(0);
        };
      } else if (anchorOffset == 1) {
        let innerHTML = Js.to_string(anchor_elem##.innerHTML);
        if (String.equal(innerHTML, ")")) {
          After;
        } else {
          In(0);
        };
      } else {
        In(0);
      };
    } else if (ast_has_class("Case")) {
      let anchor_elem = get_anchor_elem(anchor);
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string(anchor_elem##.innerHTML);
        if (String.equal(innerHTML, "case")) {
          Before;
        } else {
          In(0);
        };
      } else {
        In(0);
      };
    } else if (ast_has_class("EmptyHole") || ast_has_class("Hole")) {
      let anchor_elem = get_anchor_elem(anchor);
      let anchor_has_class = has_class(anchor_elem##.classList);
      if (anchor_has_class("hole-before-1")) {
        Before;
      } else if (anchor_has_class("hole-after-2")) {
        After;
      } else {
        After;
      };
    } else if (ast_has_class("Arrow")) {
      In(0);
    } else if (ast_has_class("Sum")) {
      In(0);
    } else if (ast_has_class("OpSeq")) {
      In(0);
    } else {
      JSUtil.log("Unknown ast element!");
      JSUtil.log(classList);
      In(0);
    };
  };

  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"),
      Dom_html.document,
      evt => {
        let selection = Dom_html.window##getSelection;
        let anchorNode = selection##.anchorNode;
        if (JSUtil.div_contains_node(pp_view_dom, anchorNode)) {
          let (anchor, anchorOffset) =
            fix_anchor(selection, selection##.anchorNode);
          let did_transport = do_transport();
          if (did_transport) {
            ();
          } else {
            let rev_paths = React.S.value(rev_paths_rs);
            let cur = ref(Js.some(anchor));
            let found = ref(false);
            while (Js.Opt.test(cur^) && ! found^) {
              let cur_node = Js.Opt.get(cur^, () => assert(false));

              switch (cur_node##.nodeType) {
              | Dom.ELEMENT =>
                let cur_element' = Dom_html.CoerceTo.element(cur_node);

                let cur_element =
                  Js.Opt.get(cur_element', () => assert(false));

                let cur_id = Js.to_string(cur_element##.id);
                switch (Hashtbl.find(rev_paths, cur_id)) {
                | rev_path =>
                  found := true;
                  let path = List.rev(rev_path);
                  let cursor_side =
                    determine_cursor_side(
                      selection,
                      anchor,
                      anchorOffset,
                      cur_element,
                    );

                  do_action(Action.MoveTo((path, cursor_side)));
                  clear_cursors();
                  let elem = JSUtil.forceGetElementById(cur_id);

                  elem##.classList##add(Js.string("cursor"));
                | exception Not_found => ()
                };
                ();
              | _ => ()
              };
              cur :=  cur_node##.parentNode;
              ();
            };
            ();
          };
        };
      },
    );

  let htype_rs =
    React.S.map(
      _ => {
        let ((_, ty), _) = React.S.value(edit_state_rs);
        let pp_view = View.of_htype(false, "result-type", [], ty);
        let (sdoc, _) = Pretty.PP.sdoc_of_doc(pp_view_width, pp_view);
        let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc(sdoc);
        [prettified];
      },
      e_rs,
    );

  let htype_view = R.Html5.div(ReactiveData.RList.from_signal(htype_rs));
  let move_to_hole = u =>
    switch (Path.path_to_hole((), React.S.value(e_rs), u)) {
    | Some(hole_path) =>
      do_action(Action.MoveTo(hole_path));
      set_cursor();
    | None => JSUtil.log("Path not found!!")
    };
  let instance_click_fn = ((u, i) as inst) => {
    let usi = React.S.value(user_selected_instances_rs);
    user_selected_instances_rf(UserSelectedInstances.update(usi, inst));
    move_to_hole(u);
    selected_instance_rf(Some(inst));
  };
  let result_view_rs =
    React.S.l1(
      ((d, hii, result)) =>
        switch (result) {
        | Dynamics.Evaluator.InvalidInput(n) => [
            Html5.pcdata(
              "(internal error: expansion or evaluation invariant violated)",
            ),
          ]
        | Dynamics.Evaluator.BoxedValue(d)
        | Dynamics.Evaluator.Indet(d) =>
          let pp_view = View.of_dhexp(instance_click_fn, "result-exp", d);
          let (sdoc, _) = Pretty.PP.sdoc_of_doc(pp_view_width, pp_view);
          let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc(sdoc);
          [prettified];
        },
      result_rs,
    );

  let result_view =
    R.Html5.div(ReactiveData.RList.from_signal(result_view_rs));
  let path_instance_cls = "path-instance";
  let selected_instance_cls = "selected-instance";
  let set_selected_instances = () => {
    let (_, hii, _) = React.S.value(result_rs);
    let selected_instance = React.S.value(selected_instance_rs);
    JSUtil.remove_cls_from_all(path_instance_cls, "hole-instance");
    JSUtil.remove_cls_from_all(selected_instance_cls, "hole-instance");
    switch (selected_instance) {
    | Some(inst) =>
      switch (Dynamics.DHExp.HoleInstanceInfo.lookup(hii, inst)) {
      | Some((_, path)) =>
        let cls_of_inst = View.cls_of_inst(inst);
        JSUtil.add_cls_to_all(path_instance_cls, cls_of_inst);
        JSUtil.add_cls_to_all(selected_instance_cls, cls_of_inst);
        List.iter(
          ((inst', _)) => {
            let cls_of_inst = View.cls_of_inst(inst');
            JSUtil.add_cls_to_all(path_instance_cls, cls_of_inst);
          },
          path,
        );
      | None => ()
      }
    | None => ()
    };
  };

  let num_changes_si = ref(0);
  let num_changes_si_str_rs =
    React.S.l4(
      (_, _, _, _) => {
        let num_changes' = num_changes_si^ + 1;
        num_changes_si := num_changes';
        string_of_int(num_changes');
      },
      result_rs,
      selected_instance_rs,
      cursor_info_rs,
      edit_state_rs,
    );

  let num_changes_counter =
    Html5.(
      div(
        ~a=[a_id("num_changes_counter_si")],
        [R.Html5.pcdata(num_changes_si_str_rs)],
      )
    );

  let num_changes_si_counter_dom =
    Tyxml_js.To_dom.of_div(num_changes_counter);

  let _ =
    MutationObserver.observe(
      ~child_list=false,
      ~attributes=false,
      ~node=num_changes_si_counter_dom,
      ~subtree=true,
      ~character_data=true,
      ~f=(_, _) => set_selected_instances(),
      (),
    );

  let num_changes = ref(0);
  let num_changes_str_rs =
    React.S.l1(
      _ => {
        let num_changes' = num_changes^ + 1;
        num_changes := num_changes';
        string_of_int(num_changes');
      },
      e_rs,
    );

  let num_changes_counter =
    Html5.(
      div(
        ~a=[a_id("num_changes_counter")],
        [R.Html5.pcdata(num_changes_str_rs)],
      )
    );

  let num_changes_counter_dom = Tyxml_js.To_dom.of_div(num_changes_counter);

  let _ =
    MutationObserver.observe(
      ~child_list=false,
      ~attributes=false,
      ~node=num_changes_counter_dom,
      ~subtree=true,
      ~character_data=true,
      ~f=(_, _) => set_cursor(),
      (),
    );

  let the_cursor_inspector_panel = CursorInspector.mk(cursor_info_rs);
  let the_context_inspector_panel =
    ContextInspector.mk(model, instance_click_fn);
  let the_action_panel = ActionPanel.make(model, set_cursor);
  let the_leftbar =
    Html5.(div(~a=[a_class(["sidebar", "leftbar"])], [the_action_panel]));

  let the_rightbar =
    Html5.(
      div(
        ~a=[a_class(["sidebar", "rightbar"])],
        [the_cursor_inspector_panel, the_context_inspector_panel],
      )
    );

  let chrome =
    Tyxml_js.To_dom.of_div(
      Html5.(
        div(
          ~a=[a_id("root")],
          [
            div(
              ~a=[a_class(["top-bar"])],
              [
                a(
                  ~a=[a_class(["logo-text"]), a_href("http://hazel.org/")],
                  [pcdata("Hazel")],
                ),
              ],
            ),
            div(
              ~a=[a_class(["main-area"])],
              [
                the_leftbar,
                div(
                  ~a=[a_class(["page-area"])],
                  [
                    div(
                      ~a=[a_class(["page"])],
                      [
                        div([
                          pcdata("Hazel is an experiment in "),
                          strong([pcdata("live functional programming")]),
                          pcdata(" with "),
                          strong([pcdata("typed holes")]),
                          pcdata(
                            ". Use the actions on the left to construct an expression. Navigate using the text cursor in the usual way.",
                          ),
                        ]),
                        pp_view_parent,
                        div(
                          ~a=[a_class(["cell-status"])],
                          [
                            div(
                              ~a=[a_class(["type-indicator"])],
                              [
                                div(
                                  ~a=[a_class(["type-label"])],
                                  [pcdata("Result of type: ")],
                                ),
                                div(
                                  ~a=[a_class(["htype-view"])],
                                  [htype_view],
                                ),
                              ],
                            ),
                          ],
                        ),
                        div(~a=[a_class(["result-view"])], [result_view]),
                      ],
                    ),
                  ],
                ),
                the_rightbar,
              ],
            ),
          ],
        )
      ),
    );

  (chrome, set_cursor);
};
