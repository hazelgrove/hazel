open Tyxml_js;
open Model;
open GeneralUtil;
open SemanticsCommon;
module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module StringMap = Map.Make(String);

let has_class = JSUtil.has_class;

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
    (EditorBox.rev_path, EditorBox.rev_paths, UHExp.block) => EditorBox.t =
    (rev_path, rev_paths, e') =>
      EditorBox.mk(mk_editor_box, prefix, rev_path, rev_paths, model, e');
  let editor_box_rs: React.signal(EditorBox.t) =
    React.S.map(e => mk_editor_box([], EditorBox.mk_rev_paths(), e), e_rs);

  let pp_view' =
    React.S.map(({EditorBox.pp_view, _}) => [pp_view], editor_box_rs);
  let pp_view = R.Html5.(div(ReactiveData.RList.from_signal(pp_view')));
  let pp_view_dom = Tyxml_js.To_dom.of_div(pp_view);
  let pp_view_parent =
    Html5.(div(~a=[a_id("pp_view"), a_class(["ModelExp"])], [pp_view]));

  let operator_node =
      (k: int, node: Js.t(Dom.node)): option(Js.t(Dom.node)) =>
    JSUtil.get_descendant_node_satisfying(
      descendant => JSUtil.node_has_cls(descendant, View.operator_cls(k)),
      node,
    );

  let delimiter_node =
      (k: int, node: Js.t(Dom.node)): option(Js.t(Dom.node)) =>
    JSUtil.get_child_node_satisfying(
      child => JSUtil.node_has_cls(child, View.delimiter_cls(k)),
      node,
    );

  let node_length = node => {
    let text_node = Js.Opt.get(Dom.CoerceTo.text(node), () => assert(false));
    text_node##.length;
  };
  /* Assumes given node is a text node */
  let move_cursor_start = node => {
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    range##setStart(node, 0);
    range##setEnd(node, 0);
    selection##removeAllRanges;
    selection##addRange(range);
  };
  /* Moves cursor to start of first descendant text node within the given node. */
  let move_cursor_before = node =>
    move_cursor_start(JSUtil.first_leaf(node));
  /* Assumes given node is a text node */
  let move_cursor_end = node => {
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    let len = node_length(node);
    range##setStart(node, len);
    range##setEnd(node, len);
    selection##removeAllRanges;
    selection##addRange(range);
  };
  /* Moves cursor to end of last descendant text node within the given node. */
  let move_cursor_after = node => move_cursor_end(JSUtil.last_leaf(node));
  let move_cursor_to = (node, offset) => {
    let cursor_leaf = JSUtil.first_leaf(node);
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
  let set_cursor_to_opseq = (cursor_path, (k, side): cursor_pos) => {
    let id = View.id_of_rev_path(prefix, List.rev(cursor_path));
    let cursor_elem = JSUtil.forceGetElementById(id);
    let cursor_node: Js.t(Dom.node) = (
      Js.Unsafe.coerce(cursor_elem): Js.t(Dom.node)
    );
    switch (operator_node(k, cursor_node)) {
    | None => ()
    | Some(node) =>
      switch (side) {
      | Before => move_cursor_before(node)
      | After => move_cursor_after(node)
      }
    };
  };
  let set_cursor_to = (cursor_path, (cursor_pos, node_type)) => {
    let id = View.id_of_rev_path(prefix, List.rev(cursor_path));
    let cursor_elem = JSUtil.forceGetElementById(id);
    let cursor_node: Js.t(Dom.node) = (
      Js.Unsafe.coerce(cursor_elem): Js.t(Dom.node)
    );
    switch (node_type, cursor_pos) {
    | (Outer, (offset, _)) =>
      // If the cursor node is a keyword, then calling move_cursor_to
      // directly on it always moves the cursor the nondisplay character
      // cushion on the left side of it, which may not always be what we
      // want. Instead, call move_cursor_to on the underlying keyword
      // text and let do_transport figure out correct side of keyword to
      // place cursor.
      switch (
        JSUtil.get_child_node_satisfying(
          child => JSUtil.node_has_cls(child, View.kw_cls),
          cursor_node,
        )
      ) {
      | None => move_cursor_to(cursor_node, offset)
      | Some(kw_node) =>
        switch (
          JSUtil.get_child_node_satisfying(
            child => JSUtil.node_has_cls(child, "kw-txt"),
            kw_node,
          )
        ) {
        | None =>
          JSUtil.log("Could not find expected kw-txt");
          ();
        | Some(kw_txt_node) => move_cursor_to(kw_txt_node, offset)
        }
      }
    | (Inner, (k, side)) =>
      switch (delimiter_node(k, cursor_node)) {
      | None => ()
      | Some(node) =>
        switch (side) {
        | Before => move_cursor_before(node)
        | After => move_cursor_after(node)
        }
      }
    };
  };
  let next_linear_sibling = (node: Js.t(Dom.node)): option(Js.t(Dom.node)) => {
    let cur_node = ref(Some(node));
    let sibling_node = ref(None);
    while (cur_node^ != None) {
      switch (cur_node^) {
      | None =>
        /* should never happen given while loop guard */
        ()
      | Some(node) =>
        switch (Js.Opt.to_option(Dom_html.CoerceTo.element(node))) {
        | None =>
          /* not an element, try to go up a level */
          switch (Js.Opt.to_option(node##.parentNode)) {
          | None =>
            /* no more parents, stop loop */
            cur_node := None
          | Some(parent_node) => cur_node := Some(parent_node)
          }
        | Some(elem) =>
          switch (Js.Opt.to_option(elem##.nextSibling)) {
          | None =>
            /* no next sibling at this level, try to go up a level */
            switch (Js.Opt.to_option(node##.parentNode)) {
            | None =>
              /* no more parents, stop loop */
              cur_node := None
            | Some(parent_node) => cur_node := Some(parent_node)
            }
          | Some(sibling) =>
            /* found sibling, stop loop */
            cur_node := None;
            sibling_node := Some(sibling);
          }
        }
      };
    };
    sibling_node^;
  };
  let set_cursor = () => {
    let (zblock, _, _) = React.S.value(edit_state_rs);
    let (cursor_path, cursor_pos) = Path.of_zblock(zblock);
    let node_type = ZExp.cursor_node_type_zblock(zblock);
    ZExp.cursor_on_opseq_block(zblock)
      ? set_cursor_to_opseq(cursor_path, cursor_pos)
      : set_cursor_to(cursor_path, (cursor_pos, node_type));
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
  let rev_paths_rs =
    React.S.map(({EditorBox.rev_paths, _}) => rev_paths, editor_box_rs);
  let do_transport = (): bool => {
    let selection = Dom_html.window##getSelection;
    /* JSUtil.log(selection); */
    let anchor = selection##.anchorNode;
    let (
      parent_elem: option(Js.t(Dom_html.element)),
      parent_node: option(Js.t(Dom.node)),
    ) =
      switch (anchor##.nodeType) {
      | Dom.TEXT =>
        switch (Js.Opt.to_option(anchor##.parentNode)) {
        | None => (None, None)
        | Some(parentNode) => (
            Js.Opt.to_option(Dom_html.CoerceTo.element(parentNode)),
            Some(parentNode),
          )
        }
      | Dom.ELEMENT => (
          Js.Opt.to_option(Dom_html.CoerceTo.element(anchor)),
          Some(anchor),
        )
      | _ =>
        JSUtil.log("BAD ANCHOR");
        (None, None);
      };
    switch (parent_elem, parent_node) {
    | (None, _) => false
    | (_, None) => false
    | (Some(parent_elem), Some(parent_node)) =>
      let classList = parent_elem##.classList;
      let has_class = has_class(classList);
      if (has_class("SIndentation")) {
        switch (Js.Opt.to_option(parent_elem##.nextSibling)) {
        | None => false
        | Some(sibling) => move_cursor_before_suppress(sibling)
        };
      } else if (!has_class("SText")) {
        /* odd case in Firefox where cursor doesn't end up in text */
        let children = anchor##.childNodes;
        let anchorOffset = selection##.anchorOffset;
        switch (Js.Opt.to_option(children##item(anchorOffset))) {
        | None => false
        | Some(child) =>
          switch (Js.Opt.to_option(Dom_html.CoerceTo.element(child))) {
          | None => move_cursor_before_suppress(child)
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
          }
        };
      } else if (has_class("kw-txt")) {
        switch (Js.Opt.to_option(parent_node##.firstChild)) {
        | None => assert(false)
        | Some(txt_node) =>
          switch (Js.Opt.to_option(txt_node##.nodeValue)) {
          | None => assert(false)
          | Some(delim_txt) =>
            let delim_len = String.length(Js.to_string(delim_txt));
            let anchorOffset = selection##.anchorOffset;
            if (anchorOffset <= delim_len / 2) {
              switch (Js.Opt.to_option(parent_elem##.parentNode)) {
              | None => false
              | Some(grandparent) =>
                switch (Js.Opt.to_option(grandparent##.firstChild)) {
                | None => false
                | Some(firstChild) => move_cursor_before_suppress(firstChild)
                }
              };
            } else {
              switch (Js.Opt.to_option(parent_elem##.parentNode)) {
              | None => false
              | Some(grandparent) =>
                switch (Js.Opt.to_option(grandparent##.lastChild)) {
                | None => false
                | Some(lastChild) => move_cursor_after_suppress(lastChild)
                }
              };
            };
          }
        };
      } else if (has_class("delim-before")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | None => false
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.lastChild)) {
            | None => false
            | Some(lastChild) => move_cursor_after_suppress(lastChild)
            }
          };
        } else if (anchorOffset == 2) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | None => false
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.firstChild)) {
            | None => false
            | Some(firstChild) => move_cursor_before_suppress(firstChild)
            }
          };
        } else {
          false;
        };
      } else if (has_class("delim-after")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | None => false
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.firstChild)) {
            | None => false
            | Some(firstChild) => move_cursor_before_suppress(firstChild)
            }
          };
        } else if (anchorOffset == 0) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | None => false
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.lastChild)) {
            | None => false
            | Some(lastChild) => move_cursor_after_suppress(lastChild)
            }
          };
        } else {
          false;
        };
      } else if (has_class("hole-before-1")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 1) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | None => false
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.lastChild)) {
            | None => false
            | Some(lastChild) => move_cursor_after_suppress(lastChild)
            }
          };
        } else if (anchorOffset == 2) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | None => false
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.firstChild)) {
            | None => false
            | Some(firstChild) => move_cursor_before_suppress(firstChild)
            }
          };
        } else {
          false;
        };
      } else if (has_class("holeName")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset == 0) {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | None => false
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.firstChild)) {
            | None => false
            | Some(firstChild) => move_cursor_before_suppress(firstChild)
            }
          };
        } else {
          switch (Js.Opt.to_option(parent_elem##.parentNode)) {
          | None => false
          | Some(grandparent) =>
            switch (Js.Opt.to_option(grandparent##.lastChild)) {
            | None => false
            | Some(lastChild) => move_cursor_after_suppress(lastChild)
            }
          };
        };
      } else if (has_class("hole-before-2") || has_class("hole-after-1")) {
        switch (Js.Opt.to_option(parent_elem##.parentNode)) {
        | None => false
        | Some(grandparent) =>
          switch (Js.Opt.to_option(grandparent##.firstChild)) {
          | None => false
          | Some(firstChild) => move_cursor_before_suppress(firstChild)
          }
        };
      } else if (has_class("space") || has_class("small-space")) {
        let anchorOffset = selection##.anchorOffset;
        if (anchorOffset === 1) {
          switch (
            next_linear_sibling(
              (parent_elem: Js.t(Dom_html.element) :> Js.t(Dom.node)),
            )
          ) {
          | None => false
          | Some(sibling) => move_cursor_before_suppress(sibling)
          };
        } else {
          false;
        };
      } else {
        false;
      };
    };
  };

  exception SkelWithoutIndex(Js.t(Dom_html.element));
  exception UnknownAstElement(Js.t(Dom_html.element));

  let get_anchor_elem = (anchor: Js.t(Dom.node)) =>
    switch (anchor##.nodeType) {
    | Dom.ELEMENT =>
      Js.Opt.get(Dom_html.CoerceTo.element(anchor), () => assert(false))
    | Dom.TEXT =>
      let parentNode = Js.Opt.get(anchor##.parentNode, () => assert(false));
      Js.Opt.get(Dom_html.CoerceTo.element(parentNode), () => assert(false));
    | _ => assert(false)
    };
  let determine_cursor_pos =
      (
        anchor: Js.t(Dom.node),
        anchorOffset: int,
        ast_elem: Js.t(Dom_html.element),
      ) => {
    let anchor_elem = get_anchor_elem(anchor);
    let ast_has_class = has_class(ast_elem##.classList);
    let anchor_has_class = has_class(anchor_elem##.classList);
    let anchor_has_text =
      String.equal(Js.to_string(anchor_elem##.innerHTML));
    if (ast_has_class("Parenthesized")) {
      if (anchor_has_class(View.delim_endpoint_cls(0))) {
        inner_cursor(0, anchor_has_class("delim-before") ? Before : After);
      } else if (anchor_has_class(View.delim_endpoint_cls(1))) {
        inner_cursor(1, anchor_has_class("delim-before") ? Before : After);
      } else {
        inner_cursor(0, Before);
      };
    } else if (ast_has_class("LetLine")) {
      if (anchor_has_class(View.delim_endpoint_cls(0))) {
        inner_cursor(0, anchor_has_class("delim-before") ? Before : After);
      } else if (anchor_has_class(View.delim_endpoint_cls(1))) {
        inner_cursor(1, anchor_has_class("delim-before") ? Before : After);
      } else if (anchor_has_class(View.delim_endpoint_cls(2))) {
        inner_cursor(2, anchor_has_class("delim-before") ? Before : After);
      } else {
        inner_cursor(0, Before);
      };
    } else if (ast_has_class("ListNil")) {
      outer_cursor(anchor_has_class("delim-before") ? 0 : 2);
    } else if (ast_has_class("Num")) {
      outer_cursor(anchor_has_class("delim-before") ? 0 : 3);
    } else if (ast_has_class("Bool")) {
      outer_cursor(anchor_has_class("delim-before") ? 0 : 4);
    } else if (ast_has_class("Var")
               || ast_has_class("var_binding")
               || ast_has_class("Wild")
               || ast_has_class("NumLit")
               || ast_has_class("BoolLit")
               || ast_has_class("number")
               || ast_has_class("ApPalette")) {
      outer_cursor(anchorOffset);
    } else if (ast_has_class("Lam")) {
      if (anchor_has_class(View.delim_endpoint_cls(0))) {
        inner_cursor(0, anchor_has_class("delim-before") ? Before : After);
      } else if (anchor_has_class(View.delim_endpoint_cls(1))) {
        inner_cursor(1, anchor_has_class("delim-before") ? Before : After);
      } else if (anchor_has_class(View.delim_endpoint_cls(2))) {
        inner_cursor(2, anchor_has_class("delim-before") ? Before : After);
      } else {
        inner_cursor(0, Before);
      };
    } else if (ast_has_class("Inj")) {
      if (anchor_has_text("inj")) {
        inner_cursor(0, Before);
      } else if (anchor_has_text("[")
                 || anchor_has_text("L")
                 || anchor_has_text("R")
                 || anchor_has_text("]")) {
        inner_cursor(0, After);
      } else if (anchor_has_text(")")) {
        inner_cursor(1, anchor_has_class("delim-before") ? Before : After);
      } else {
        inner_cursor(0, Before);
      };
    } else if (ast_has_class("List")) {
      if (anchor_has_class(View.delim_endpoint_cls(0))) {
        inner_cursor(0, anchor_has_class("delim-before") ? Before : After);
      } else if (anchor_has_class(View.delim_endpoint_cls(1))) {
        inner_cursor(1, anchor_has_class("delim-before") ? Before : After);
      } else {
        inner_cursor(0, Before);
      };
    } else if (ast_has_class("Case")) {
      if (anchor_has_class(View.delim_endpoint_cls(0))) {
        inner_cursor(0, anchor_has_class("delim-before") ? Before : After);
      } else if (anchor_has_class(View.delim_endpoint_cls(1))) {
        inner_cursor(1, anchor_has_class("delim-before") ? Before : After);
      } else {
        /* TODO fix once cursor is redesigned */
        inner_cursor(0, Before);
      };
    } else if (ast_has_class("Case-rule")) {
      if (anchor_has_class(View.delim_endpoint_cls(0))) {
        inner_cursor(0, anchor_has_class("delim-before") ? Before : After);
      } else if (anchor_has_class(View.delim_endpoint_cls(1))) {
        inner_cursor(1, anchor_has_class("delim-before") ? Before : After);
      } else {
        inner_cursor(0, Before);
      };
    } else if (ast_has_class("EmptyHole") || ast_has_class("Hole")) {
      if (anchor_has_class("hole-before-1")) {
        outer_cursor(0);
      } else if (anchor_has_class("hole-after-2")) {
        outer_cursor(1);
      } else {
        outer_cursor(1);
      };
    } else if (ast_has_class("Arrow")
               || ast_has_class("Sum")
               || ast_has_class("Prod")
               || ast_has_class("Comma")
               || ast_has_class("Space")
               || ast_has_class("Cons")
               || ast_has_class("Plus")
               || ast_has_class("Times")
               || ast_has_class("LessThan")) {
      switch (
        JSUtil.has_class_satisfying(
          ast_elem##.classList,
          View.operator_index_of_cls,
        )
      ) {
      | None => raise(SkelWithoutIndex(ast_elem))
      | Some(k) =>
        inner_cursor(k, anchor_has_class("delim-before") ? Before : After)
      };
    } else if (ast_has_class("EmptyLine")) {
      outer_cursor(0);
    } else {
      JSUtil.log("Unknown ast element!");
      JSUtil.log(ast_elem##.classList);
      raise(UnknownAstElement(ast_elem));
    };
  };

  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"),
      Dom_html.document,
      _ => {
        /* look up on MDN
             assuming cursor is collapsed and that there's some anchor node
           */
        let selection = Dom_html.window##getSelection;
        /* almost always a text node, sometimes in weird cases (Firefox) */
        let anchor = selection##.anchorNode;
        let anchorOffset = selection##.anchorOffset;
        /* make sure cursor is actually in editorbox */
        if (JSUtil.div_contains_node(pp_view_dom, anchor)) {
          /* let (anchor, anchorOffset) = */
          /*   fix_anchor(selection, selection##.anchorNode); */
          let did_transport = do_transport();
          if (did_transport) {
            ();
              /* if there was a transport, new selectionchange event triggered, cancel this one */
          } else {
            /* find which path corresponds to the selection */
            let rev_paths = React.S.value(rev_paths_rs);
            let cur = ref(Js.some(anchor));
            let found = ref(false);
            /* traverse the dOM tree up until it finds a node with a path indentifier */
            /* initially just starting with a text node, not a AST node node */
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
                  /* usually cur_element is immediate parent of anchor, but
                   * some situations where it's a grandparent */
                  let cursor_pos =
                    determine_cursor_pos(anchor, anchorOffset, cur_element);

                  do_action(Action.MoveTo((path, cursor_pos)));
                  /* avoid rerendering view on cursor movement */
                  clear_cursors();

                  let (zblock, _, _) = React.S.value(edit_state_rs);
                  if (ZExp.cursor_on_opseq_block(zblock)) {
                    let (_, cursor_pos) = Path.of_zblock(zblock);
                    let (k, _) = cursor_pos;
                    let elem = View.forceGetSkelElement(cur_id, k);
                    elem##.classList##add(Js.string("cursor"));
                  } else {
                    let elem = JSUtil.forceGetElementById(cur_id);
                    elem##.classList##add(Js.string("cursor"));
                  };
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
    switch (
      Path.path_to_hole(Path.holes_block(React.S.value(e_rs), [], []), u)
    ) {
    | None => JSUtil.log("Path not found!!")
    | Some(hole_path) =>
      do_action(Action.MoveTo(hole_path));
      set_cursor();
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
    Tyxml_js.To_dom.of_div(num_changes_counter);

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

  let num_changes_counter_dom = Tyxml_js.To_dom.of_div(num_changes_counter);

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
    JSUtil.log(Js.string(Serialize.string_of_block(React.S.value(e_rs))));
    true;
  };

  let deserialize_onclick_handler = (serialized, _) => {
    replace_e(Deserialize.block_of_string(serialized));
  };

  let examples =
    StringMap.add(
      "qsort_example",
      {
        serialized:
          string_of_sexp(UHExp.sexp_of_block(Examples.qsort_example)),
        desc: "qsort prompt",
      },
      StringMap.add(
        "map_example",
        {
          serialized:
            string_of_sexp(UHExp.sexp_of_block(Examples.map_example)),
          desc: "map example",
        },
        StringMap.add(
          "let_line",
          {
            serialized:
              string_of_sexp(UHExp.sexp_of_block(Examples.let_line)),
            desc: "Let with extra lines example",
          },
          StringMap.add(
            "basic_holey",
            {
              serialized:
                string_of_sexp(UHExp.sexp_of_block(Examples.holey_lambda)),
              desc: "Basic holey lambda example",
            },
            StringMap.add(
              "just_hole",
              {
                serialized:
                  string_of_sexp(UHExp.sexp_of_block(Examples.just_hole)),
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
                  [txt("Hazel")],
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
