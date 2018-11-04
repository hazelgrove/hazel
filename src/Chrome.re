open Tyxml_js;
open SemanticsCore;
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
    _,
  } = model;
  let pp_view_width = 50;
  let prefix = "view";
  let view_rs =
    React.S.map(
      e => {
        let palette_stuff =
          View.{
            palette_view_ctx: Palettes.initial_palette_view_ctx,
            do_action,
          };
        let rec mk_html_cell = (prefix', rev_path, hexp) => {
          let view =
            View.of_hexp(
              (prefix', rev_path, hexp) =>
                fst(mk_html_cell(prefix', rev_path, hexp)),
              palette_stuff,
              prefix',
              rev_path,
              hexp,
            );
          let (sdoc, rev_paths) = Pretty.PP.sdoc_of_doc(pp_view_width, view);
          let html_result =
            EditorBox.view(model, Pretty.HTML_Of_SDoc.html_of_sdoc(sdoc));
          (html_result, rev_paths);
        };
        mk_html_cell(prefix, [], e);
      },
      e_rs,
    );

  let pp_view' = React.S.map(((view_html, _)) => [view_html], view_rs);
  /* TODO WTF */
  let pp_view = R.Html5.(div(ReactiveData.RList.from_signal(pp_view')));
  let pp_view_dom = Tyxml_js.To_dom.of_div(pp_view);
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
    let (cursor_path, cursor_side) = SemanticsCore.Path.of_zexp(ze);
    set_cursor_to((cursor_path, cursor_side));
  };
  /* TODO not sure if the rev_paths are complete anymore in light of palettes */
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
        if (! Js.to_bool(classList##contains(Js.string("SText")))) {
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
  let do_transport = () : bool => {
    let selection = Dom_html.window##getSelection;
    JSUtil.log(selection);
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
               || ast_has_class("number")
               || ast_has_class("ApPalette")) {
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
      _ => {
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
                    determine_cursor_side(anchor, anchorOffset, cur_element);

                  do_action(Action.MoveTo((path, cursor_side)));
                  clear_cursors();
                  let elem = JSUtil.forceGetElementById(cur_id);

                  elem##.classList##add(Js.string("cursor"));
                | exception Not_found => ()
                };
                ();
              | _ => ()
              };
              cur := cur_node##.parentNode;
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
  let instance_click_fn = ((u, _) as inst) => {
    let usi = React.S.value(user_selected_instances_rs);
    user_selected_instances_rf(UserSelectedInstances.update(usi, inst));
    move_to_hole(u);
    selected_instance_rf(Some(inst));
  };
  let result_view_rs =
    React.S.l1(
      ((_, _, result)) =>
        switch (result) {
        | Dynamics.Evaluator.InvalidInput(_) => [
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

  let color_picker_script = Html5.(script(pcdata("
    (function(s,t,u){var v=(s.SVGAngle||t.implementation.hasFeature(\"http://www.w3.org/TR/SVG11/feature#BasicStructure\",\"1.1\")?\"SVG\":\"VML\"),picker,slide,hueOffset=15,svgNS='http://www.w3.org/2000/svg';var w=['<div class=\"picker-wrapper\">','<div class=\"picker\"></div>','<div class=\"picker-indicator\"></div>','</div>','<div class=\"slide-wrapper\">','<div class=\"slide\"></div>','<div class=\"slide-indicator\"></div>','</div>'].join('');function mousePosition(a){if(s.event&&s.event.contentOverflow!==u){return{x:s.event.offsetX,y:s.event.offsetY}}if(a.offsetX!==u&&a.offsetY!==u){return{x:a.offsetX,y:a.offsetY}}var b=a.target.parentNode.parentNode;return{x:a.layerX-b.offsetLeft,y:a.layerY-b.offsetTop}}function $(a,b,c){a=t.createElementNS(svgNS,a);for(var d in b)a.setAttribute(d,b[d]);if(Object.prototype.toString.call(c)!='[object Array]')c=[c];var i=0,len=(c[0]&&c.length)||0;for(;i<len;i++)a.appendChild(c[i]);return a}if(v=='SVG'){slide=$('svg',{xmlns:'http://www.w3.org/2000/svg',version:'1.1',width:'100%',height:'100%'},[$('defs',{},$('linearGradient',{id:'gradient-hsv',x1:'0%',y1:'100%',x2:'0%',y2:'0%'},[$('stop',{offset:'0%','stop-color':'#FF0000','stop-opacity':'1'}),$('stop',{offset:'13%','stop-color':'#FF00FF','stop-opacity':'1'}),$('stop',{offset:'25%','stop-color':'#8000FF','stop-opacity':'1'}),$('stop',{offset:'38%','stop-color':'#0040FF','stop-opacity':'1'}),$('stop',{offset:'50%','stop-color':'#00FFFF','stop-opacity':'1'}),$('stop',{offset:'63%','stop-color':'#00FF40','stop-opacity':'1'}),$('stop',{offset:'75%','stop-color':'#0BED00','stop-opacity':'1'}),$('stop',{offset:'88%','stop-color':'#FFFF00','stop-opacity':'1'}),$('stop',{offset:'100%','stop-color':'#FF0000','stop-opacity':'1'})])),$('rect',{x:'0',y:'0',width:'100%',height:'100%',fill:'url(#gradient-hsv)'})]);picker=$('svg',{xmlns:'http://www.w3.org/2000/svg',version:'1.1',width:'100%',height:'100%'},[$('defs',{},[$('linearGradient',{id:'gradient-black',x1:'0%',y1:'100%',x2:'0%',y2:'0%'},[$('stop',{offset:'0%','stop-color':'#000000','stop-opacity':'1'}),$('stop',{offset:'100%','stop-color':'#CC9A81','stop-opacity':'0'})]),$('linearGradient',{id:'gradient-white',x1:'0%',y1:'100%',x2:'100%',y2:'100%'},[$('stop',{offset:'0%','stop-color':'#FFFFFF','stop-opacity':'1'}),$('stop',{offset:'100%','stop-color':'#CC9A81','stop-opacity':'0'})])]),$('rect',{x:'0',y:'0',width:'100%',height:'100%',fill:'url(#gradient-white)'}),$('rect',{x:'0',y:'0',width:'100%',height:'100%',fill:'url(#gradient-black)'})])}else if(v=='VML'){slide=['<DIV style=\"position: relative; width: 100%; height: 100%\">','<v:rect style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%\" stroked=\"f\" filled=\"t\">','<v:fill type=\"gradient\" method=\"none\" angle=\"0\" color=\"red\" color2=\"red\" colors=\"8519f fuchsia;.25 #8000ff;24903f #0040ff;.5 aqua;41287f #00ff40;.75 #0bed00;57671f yellow\"></v:fill>','</v:rect>','</DIV>'].join('');picker=['<DIV style=\"position: relative; width: 100%; height: 100%\">','<v:rect style=\"position: absolute; left: -1px; top: -1px; width: 101%; height: 101%\" stroked=\"f\" filled=\"t\">','<v:fill type=\"gradient\" method=\"none\" angle=\"270\" color=\"#FFFFFF\" opacity=\"100%\" color2=\"#CC9A81\" o:opacity2=\"0%\"></v:fill>','</v:rect>','<v:rect style=\"position: absolute; left: 0px; top: 0px; width: 100%; height: 101%\" stroked=\"f\" filled=\"t\">','<v:fill type=\"gradient\" method=\"none\" angle=\"0\" color=\"#000000\" opacity=\"100%\" color2=\"#CC9A81\" o:opacity2=\"0%\"></v:fill>','</v:rect>','</DIV>'].join('');if(!t.namespaces['v'])t.namespaces.add('v','urn:schemas-microsoft-com:vml','#default#VML')}function hsv2rgb(a){var R,G,B,X,C;var h=(a.h%360)/60;C=a.v*a.s;X=C*(1-Math.abs(h%2-1));R=G=B=a.v-C;h=~~h;R+=[C,X,0,0,X,C][h];G+=[X,C,C,X,0,0][h];B+=[0,0,X,C,C,X][h];var r=Math.floor(R*255);var g=Math.floor(G*255);var b=Math.floor(B*255);return{r:r,g:g,b:b,hex:\"#\"+(16777216|b|(g<<8)|(r<<16)).toString(16).slice(1)}}function rgb2hsv(a){var r=a.r;var g=a.g;var b=a.b;if(a.r>1||a.g>1||a.b>1){r/=255;g/=255;b/=255}var H,S,V,C;V=Math.max(r,g,b);C=V-Math.min(r,g,b);H=(C==0?null:V==r?(g-b)/C+(g<b?6:0):V==g?(b-r)/C+2:(r-g)/C+4);H=(H%6)*60;S=C==0?0:C/V;return{h:H,s:S,v:V}}function slideListener(d,e,f){return function(a){a=a||s.event;var b=mousePosition(a);d.h=b.y/e.offsetHeight*360+hueOffset;d.s=d.v=1;var c=hsv2rgb({h:d.h,s:1,v:1});f.style.backgroundColor=c.hex;d.callback&&d.callback(c.hex,{h:d.h-hueOffset,s:d.s,v:d.v},{r:c.r,g:c.g,b:c.b},u,b)}};function pickerListener(d,e){return function(a){a=a||s.event;var b=mousePosition(a),width=e.offsetWidth,height=e.offsetHeight;d.s=b.x/width;d.v=(height-b.y)/height;var c=hsv2rgb(d);d.callback&&d.callback(c.hex,{h:d.h-hueOffset,s:d.s,v:d.v},{r:c.r,g:c.g,b:c.b},b)}};var x=0;function ColorPicker(f,g,h){if(!(this instanceof ColorPicker))return new ColorPicker(f,g,h);this.h=0;this.s=1;this.v=1;if(!h){var i=f;i.innerHTML=w;this.slideElement=i.getElementsByClassName('slide')[0];this.pickerElement=i.getElementsByClassName('picker')[0];var j=i.getElementsByClassName('slide-indicator')[0];var k=i.getElementsByClassName('picker-indicator')[0];ColorPicker.fixIndicators(j,k);this.callback=function(a,b,c,d,e){ColorPicker.positionIndicators(j,k,e,d);g(a,b,c)}}else{this.callback=h;this.pickerElement=g;this.slideElement=f}if(v=='SVG'){var l=slide.cloneNode(true);var m=picker.cloneNode(true);var n=l.getElementsByTagName('linearGradient')[0];var o=l.getElementsByTagName('rect')[0];n.id='gradient-hsv-'+x;o.setAttribute('fill','url(#'+n.id+')');var p=[m.getElementsByTagName('linearGradient')[0],m.getElementsByTagName('linearGradient')[1]];var q=m.getElementsByTagName('rect');p[0].id='gradient-black-'+x;p[1].id='gradient-white-'+x;q[0].setAttribute('fill','url(#'+p[1].id+')');q[1].setAttribute('fill','url(#'+p[0].id+')');this.slideElement.appendChild(l);this.pickerElement.appendChild(m);x++}else{this.slideElement.innerHTML=slide;this.pickerElement.innerHTML=picker}addEventListener(this.slideElement,'click',slideListener(this,this.slideElement,this.pickerElement));addEventListener(this.pickerElement,'click',pickerListener(this,this.pickerElement));enableDragging(this,this.slideElement,slideListener(this,this.slideElement,this.pickerElement));enableDragging(this,this.pickerElement,pickerListener(this,this.pickerElement))};function addEventListener(a,b,c){if(a.attachEvent){a.attachEvent('on'+b,c)}else if(a.addEventListener){a.addEventListener(b,c,false)}}function enableDragging(b,c,d){var e=false;addEventListener(c,'mousedown',function(a){e=true});addEventListener(c,'mouseup',function(a){e=false});addEventListener(c,'mouseout',function(a){e=false});addEventListener(c,'mousemove',function(a){if(e){d(a)}})}ColorPicker.hsv2rgb=function(a){var b=hsv2rgb(a);delete b.hex;return b};ColorPicker.hsv2hex=function(a){return hsv2rgb(a).hex};ColorPicker.rgb2hsv=rgb2hsv;ColorPicker.rgb2hex=function(a){return hsv2rgb(rgb2hsv(a)).hex};ColorPicker.hex2hsv=function(a){return rgb2hsv(ColorPicker.hex2rgb(a))};ColorPicker.hex2rgb=function(a){return{r:parseInt(a.substr(1,2),16),g:parseInt(a.substr(3,2),16),b:parseInt(a.substr(5,2),16)}};function setColor(a,b,d,e){a.h=b.h%360;a.s=b.s;a.v=b.v;var c=hsv2rgb(a);var f={y:(a.h*a.slideElement.offsetHeight)/360,x:0};var g=a.pickerElement.offsetHeight;var h={x:a.s*a.pickerElement.offsetWidth,y:g-a.v*g};a.pickerElement.style.backgroundColor=hsv2rgb({h:a.h,s:1,v:1}).hex;a.callback&&a.callback(e||c.hex,{h:a.h,s:a.s,v:a.v},d||{r:c.r,g:c.g,b:c.b},h,f);return a};ColorPicker.prototype.setHsv=function(a){return setColor(this,a)};ColorPicker.prototype.setRgb=function(a){return setColor(this,rgb2hsv(a),a)};ColorPicker.prototype.setHex=function(a){return setColor(this,ColorPicker.hex2hsv(a),u,a)};ColorPicker.positionIndicators=function(a,b,c,d){if(c){b.style.left='auto';b.style.right='0px';b.style.top='0px';a.style.top=(c.y-a.offsetHeight/2)+'px'}if(d){b.style.top=(d.y-b.offsetHeight/2)+'px';b.style.left=(d.x-b.offsetWidth/2)+'px'}};ColorPicker.fixIndicators=function(a,b){b.style.pointerEvents='none';a.style.pointerEvents='none'};s.ColorPicker=ColorPicker})(window,window.document);
  ")));

  let chrome =
    Tyxml_js.To_dom.of_div(
      Html5.(
        div(
          ~a=[a_id("root")],
          [
            color_picker_script,
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
