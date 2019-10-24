open Js_of_ocaml_tyxml.Tyxml_js;
open Model;
open SemanticsCommon;
module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module StringMap = Map.Make(String);
type example_info = {
  serialized: string,
  desc: string,
};
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
    replace_e,
    _,
  } = model;
  let pp_view_width = 100;
  let prefix = "view";
  let rec mk_editor_box:
    (EditorBox.rev_path, EditorBox.rev_paths, UHExp.t) => EditorBox.t =
    (rev_path, rev_paths, e') =>
      EditorBox.mk(mk_editor_box, prefix, rev_path, rev_paths, model, e');
  let editor_box_rs: React.signal(EditorBox.t) =
    React.S.map(e => mk_editor_box([], EditorBox.mk_rev_paths(), e), e_rs);

  let pp_view' =
    React.S.map(({EditorBox.pp_view, _}) => [pp_view], editor_box_rs);
  let pp_view = R.Html5.(div(ReactiveData.RList.from_signal(pp_view')));
  let pp_view_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div(pp_view);
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
    let (ze, _, _) = React.S.value(edit_state_rs);
    let (cursor_path, cursor_side) = Path.of_zexp(ze);
    set_cursor_to((cursor_path, cursor_side));
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
  /* TODO not sure if the rev_paths are complete anymore in light of palettes */
  let has_class = JSUtil.has_class;
  let rev_paths_rs =
    React.S.map(({EditorBox.rev_paths, _}) => rev_paths, editor_box_rs);
  let do_transport = (): bool => {
    let selection = Dom_html.window##getSelection;
    /* JSUtil.log(selection); */
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
    | None => false
    | Some(parent_elem) =>
      let classList = parent_elem##.classList;
      let has_class = has_class(classList);
      if (has_class("SIndentation")) {
        switch (Js.Opt.to_option(parent_elem##.nextSibling)) {
        | Some(sibling) => move_cursor_before_suppress(sibling)
        | None => false
        };
      } else if (!has_class("SText")) {
        /* odd case in Firefox where cursor doesn't end up in text */
        let children = anchor##.childNodes;
        let anchorOffset = selection##.anchorOffset;
        switch (Js.Opt.to_option(children##item(anchorOffset))) {
        | Some(child) =>
          switch (Js.Opt.to_option(Dom_html.CoerceTo.element(child))) {
          | Some(child_element) =>
            let tagName = Js.to_string(child_element##.tagName);
            if (String.equal(tagName, "BR")) {
              switch (Js.Opt.to_option(child_element##.previousSibling)) {
              | Some(sibling) => move_cursor_after_suppress(sibling)
              | None => move_cursor_before_suppress(child)
              };
            } else {
              move_cursor_before_suppress(child);
            };
          | None => move_cursor_before_suppress(child)
          }
        | None => false
        };
      } else if (has_class("hole-before-1")) {
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
                 || has_class("lambda-sym")
                 || has_class("lparen")
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
      } else if (has_class("rparen")) {
        let anchorOffset = selection##.anchorOffset;

        if (anchorOffset == 0) {
          switch (Js.Opt.to_option(parent_elem##.previousSibling)) {
          | Some(sibling) =>
            switch (Js.Opt.to_option(Dom_html.CoerceTo.element(sibling))) {
            | None => false
            | Some(sibling_element) =>
              if (!
                    JSUtil.has_class(
                      sibling_element##.classList,
                      "SIndentation",
                    )) {
                move_cursor_after_suppress(sibling);
              } else {
                false;
              }
            }
          | None => false
          };
        } else {
          false;
        };
      } else {
        false;
      };
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
      if (has_class(anchor_classList, "lparen")) {
        Before;
      } else {
        After;
      };
    } else if (ast_has_class("Asc")) {
      In(0);
    } else if (ast_has_class("LetLine")) {
      let anchor_elem = get_anchor_elem(anchor);
      let innerHTML = Js.to_string(anchor_elem##.innerHTML);
      if (String.equal(innerHTML, "let")) {
        if (anchorOffset == 0) {
          Before;
        } else {
          In(0);
        };
      } else {
        In(1);
      };
    } else if (ast_has_class("Var")
               || ast_has_class("var_binding")
               || ast_has_class("Wild")
               || ast_has_class("ListNil")
               || ast_has_class("NumLit")
               || ast_has_class("BoolLit")
               || ast_has_class("Num")
               || ast_has_class("Bool")
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
    } else if (ast_has_class("List")) {
      let anchor_elem = get_anchor_elem(anchor);
      if (anchorOffset == 0) {
        let innerHTML = Js.to_string(anchor_elem##.innerHTML);

        if (String.equal(innerHTML, "List")) {
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
      let innerHTML = Js.to_string(anchor_elem##.innerHTML);
      if (String.equal(innerHTML, "case")) {
        if (anchorOffset == 0) {
          Before;
        } else {
          In(0);
        };
      } else if (String.equal(innerHTML, "end")) {
        if (anchorOffset <= 2) {
          In(1);
        } else {
          After;
        };
      } else {
        Before;
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
    } else if (ast_has_class("EmptyLine")) {
      Before;
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
        let anchor = selection##.anchorNode;
        let anchorOffset = selection##.anchorOffset;
        if (JSUtil.div_contains_node(pp_view_dom, anchor)) {
          /* let (anchor, anchorOffset) = */
          /*   fix_anchor(selection, selection##.anchorNode); */
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
        let (_, ty, _) = React.S.value(edit_state_rs);
        let prettified = View.html_of_ty(pp_view_width, "result-type", ty);
        [prettified];
      },
      e_rs,
    );

  let htype_view = R.Html5.div(ReactiveData.RList.from_signal(htype_rs));
  let move_to_hole = u =>
    switch (Path.path_to_hole(Path.holes_e(React.S.value(e_rs), [], []), u)) {
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
            Html5.txt(
              "(internal error: expansion or evaluation invariant violated)",
            ),
          ]
        | Dynamics.Evaluator.BoxedValue(d)
        | Dynamics.Evaluator.Indet(d) =>
          let prettified =
            View.html_of_dhexp(
              instance_click_fn,
              pp_view_width,
              "result-exp",
              d,
            );
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
        [R.Html5.txt(num_changes_si_str_rs)],
      )
    );

  let num_changes_si_counter_dom =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div(num_changes_counter);

  let _ =
    Js_of_ocaml.MutationObserver.observe(
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
        [R.Html5.txt(num_changes_str_rs)],
      )
    );

  let num_changes_counter_dom =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div(num_changes_counter);

  let _ =
    Js_of_ocaml.MutationObserver.observe(
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
  /*let the_history_panel = HistoryPanel.make(model.code_history_rs);*/
  let the_action_panel = ActionPanel.make(model, set_cursor);

  let serialize_onclick_handler = _ => {
    JSUtil.log(Js.string(Serialize.string_of_uhexp(React.S.value(e_rs))));
    true;
  };

  let deserialize_onclick_handler = (serialized, _) => {
    replace_e(Deserialize.uhexp_of_string(serialized));
  };

  let examples =
    StringMap.add(
      "qsort_example",
      {
        serialized: "(Tm NotInHole(LineItem(LetLine(Pat NotInHole(Var NotInVHole append))((OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(SeqOpExp(ExpOpExp(List Num)Arrow(List Num))Arrow(List Num))))(Tm NotInHole(Lam(Pat NotInHole(Var NotInVHole xs))()(Tm NotInHole(Lam(Pat NotInHole(Var NotInVHole ys))()(Tm NotInHole(Case(Tm NotInHole(Var NotInVHole xs))((Rule(Pat NotInHole ListNil)(Tm NotInHole(Var NotInVHole ys)))(Rule(Pat NotInHole(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(ExpOpExp(Pat NotInHole(Var NotInVHole z))Cons(Pat NotInHole(Var NotInVHole zs)))))(Tm NotInHole(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(ExpOpExp(Tm NotInHole(Var NotInVHole z))Cons(Parenthesized(Tm NotInHole(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(SeqOpExp(ExpOpExp(Tm NotInHole(Var NotInVHole append))Space(Tm NotInHole(Var NotInVHole zs)))Space(Tm NotInHole(Var NotInVHole ys)))))))))))())))))))(Tm NotInHole(LineItem EmptyLine(Tm NotInHole(LineItem(LetLine(Pat NotInHole(Var NotInVHole partition))((OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(SeqOpExp(ExpOpExp(Parenthesized(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(ExpOpExp Num Arrow Bool)))Arrow(List Num))Arrow(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(ExpOpExp(List Num)Prod(List Num)))))))(Tm NotInHole(Lam(Pat NotInHole(Var NotInVHole f))()(Tm NotInHole(Lam(Pat NotInHole(Var NotInVHole xs))()(Tm NotInHole(Case(Tm NotInHole(Var NotInVHole xs))((Rule(Pat NotInHole ListNil)(Parenthesized(Tm NotInHole(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(ExpOpExp(Tm NotInHole ListNil)Comma(Tm NotInHole ListNil))))))(Rule(Pat NotInHole(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(ExpOpExp(Pat NotInHole(Var NotInVHole y))Cons(Pat NotInHole(Var NotInVHole ys)))))(Tm NotInHole(LineItem EmptyLine(Tm NotInHole(LineItem(LetLine(Parenthesized(Pat NotInHole(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(ExpOpExp(Pat NotInHole(Var NotInVHole ys1))Comma(Pat NotInHole(Var NotInVHole ys2))))))()(Tm NotInHole(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(SeqOpExp(ExpOpExp(Tm NotInHole(Var NotInVHole partition))Space(Tm NotInHole(Var NotInVHole f)))Space(Tm NotInHole(Var NotInVHole ys))))))(Tm NotInHole(Case(Tm NotInHole(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(ExpOpExp(Tm NotInHole(Var NotInVHole f))Space(Tm NotInHole(Var NotInVHole y)))))((Rule(Pat NotInHole(BoolLit true))(Parenthesized(Tm NotInHole(OpSeq(BinOp NotInHole Comma(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(Placeholder 2))(SeqOpExp(ExpOpExp(Tm NotInHole(Var NotInVHole y))Cons(Tm NotInHole(Var NotInVHole ys1)))Comma(Tm NotInHole(Var NotInVHole ys2)))))))(Rule(Pat NotInHole(BoolLit false))(Parenthesized(Tm NotInHole(OpSeq(BinOp NotInHole Comma(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(Placeholder 2)))(SeqOpExp(ExpOpExp(Tm NotInHole(Var NotInVHole ys1))Comma(Tm NotInHole(Var NotInVHole y)))Cons(Tm NotInHole(Var NotInVHole ys2))))))))()))))))))())))))))(Tm NotInHole(LineItem EmptyLine(Tm NotInHole(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(ExpOpExp(Tm NotInHole(Var(InVHole Free 192)qsort))Space(Parenthesized(Tm NotInHole(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(BinOp NotInHole Cons(Placeholder 3)(BinOp NotInHole Cons(Placeholder 4)(BinOp NotInHole Cons(Placeholder 5)(BinOp NotInHole Cons(Placeholder 6)(Placeholder 7))))))))(SeqOpExp(SeqOpExp(SeqOpExp(SeqOpExp(SeqOpExp(SeqOpExp(ExpOpExp(Tm NotInHole(NumLit 4))Cons(Tm NotInHole(NumLit 2)))Cons(Tm NotInHole(NumLit 6)))Cons(Tm NotInHole(NumLit 5)))Cons(Tm NotInHole(NumLit 3)))Cons(Tm NotInHole(NumLit 1)))Cons(Tm NotInHole(NumLit 7)))Cons(Tm NotInHole ListNil))))))))))))))))",
        desc: "qsort prompt",
      },
      StringMap.add(
        "map_example",
        {
          serialized: "(Tm NotInHole(LineItem(LetLine(Pat NotInHole(Var NotInVHole map))((OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(SeqOpExp(ExpOpExp(Parenthesized(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(ExpOpExp Num Arrow Num)))Arrow(List Num))Arrow(List Num))))(Tm NotInHole(Lam(Pat NotInHole(Var NotInVHole f))()(Tm NotInHole(Lam(Pat NotInHole(Var NotInVHole xs))()(Tm NotInHole(Case(Tm NotInHole(Var NotInVHole xs))((Rule(Pat NotInHole ListNil)(Tm NotInHole ListNil))(Rule(Pat NotInHole(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(ExpOpExp(Pat NotInHole(Var NotInVHole y))Cons(Pat NotInHole(Var NotInVHole ys)))))(Tm NotInHole(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(ExpOpExp(Parenthesized(Tm NotInHole(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(ExpOpExp(Tm NotInHole(Var NotInVHole f))Space(Tm NotInHole(Var NotInVHole y))))))Cons(Parenthesized(Tm NotInHole(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(SeqOpExp(ExpOpExp(Tm NotInHole(Var NotInVHole map))Space(Tm NotInHole(Var NotInVHole f)))Space(Tm NotInHole(Var NotInVHole ys)))))))))))())))))))(Tm NotInHole(EmptyHole 46))))",
          desc: "map example",
        },
        StringMap.add(
          "let_line",
          {
            serialized: "(Tm NotInHole(LineItem(LetLine(Pat NotInHole(Var y))()(Tm NotInHole(EmptyHole 0)))(Tm NotInHole(LineItem EmptyLine(Tm NotInHole(LineItem(LetLine(Pat NotInHole(Var x))()(Tm NotInHole(EmptyHole 4)))(Tm NotInHole(LineItem(ExpLine(Tm NotInHole(Var NotInVHole x)))(Tm NotInHole(Var NotInVHole y))))))))))",
            desc: "Let with extra lines example",
          },
          StringMap.add(
            "basic_holey",
            {
              serialized: "(Tm NotInHole(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(ExpOpExp(Parenthesized(Tm NotInHole(Lam(Pat NotInHole(EmptyHole 1))(Hole)(Tm NotInHole(EmptyHole 0)))))Space(Tm NotInHole(EmptyHole 2)))))",
              desc: "Basic holey lambda example",
            },
            StringMap.add(
              "just_hole",
              {
                serialized: "(Tm NotInHole(EmptyHole 0))",
                desc: "Just a hole example",
              },
              StringMap.empty,
            ),
          ),
        ),
      ),
    );

  let examples_select =
    Html5.(
      select(
        List.map(
          ((name, info)) => option(~a=[a_value(name)], txt(info.desc)),
          StringMap.bindings(examples),
        ),
      )
    );
  let examples_select_dom = To_dom.of_select(examples_select);

  let _ =
    JSUtil.listen_to_t(Dom_html.Event.click, examples_select_dom, evt =>
      deserialize_onclick_handler(
        StringMap.find(Js.to_string(examples_select_dom##.value), examples).
          serialized,
        evt,
      )
    );

  let chrome =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div(
      Html5.(
        div(
          ~a=[a_id("root")],
          [
            div(
              ~a=[a_class(["top-bar"])],
              [
                div(
                  ~a=[a_id("logo")],
                  [
                    a(
                      ~a=[
                        a_class(["logo-text"]),
                        a_href("http://hazel.org/"),
                      ],
                      [txt("Hazel")],
                    ),
                  ],
                ),
                div(
                  ~a=[a_id("link-to-dev")],
                  [
                    a(
                      ~a=[
                        a_id("link-to-dev-text"),
                        a_href("http://hazel.org/hazel-build/dev/hazel.html"),
                      ],
                      [txt("Danger Zone")],
                    ),
                  ],
                ),
              ],
            ),
            div(
              ~a=[a_class(["main-area"])],
              [
                Sidebar.left([the_action_panel /*, the_history_panel*/]),
                div(
                  ~a=[a_class(["flex-wrapper"])],
                  [
                    div(
                      ~a=[a_class(["page-area"])],
                      [
                        div(
                          ~a=[a_class(["page"])],
                          [
                            div([
                              txt("Hazel is an experiment in "),
                              strong([txt("live functional programming")]),
                              txt(" with "),
                              strong([txt("typed holes")]),
                              txt(
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
                                      [txt("Result of type: ")],
                                    ),
                                    div(
                                      ~a=[a_class(["htype-view"])],
                                      [htype_view],
                                    ),
                                  ],
                                ),
                              ],
                            ),
                            div(
                              ~a=[a_class(["result-view"])],
                              [result_view],
                            ),
                          ],
                        ),
                        examples_select,
                        button(
                          ~a=[a_onclick(serialize_onclick_handler)],
                          [txt("Serialize to dev console")],
                        ),
                      ],
                    ),
                  ],
                ),
                Sidebar.right([
                  the_cursor_inspector_panel,
                  the_context_inspector_panel,
                ]),
              ],
            ),
          ],
        )
      ),
    );

  (chrome, set_cursor);
};
