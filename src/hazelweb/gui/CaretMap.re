module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;

module RowCol = {
  type t = (int, int);
  let compare = ((row1, col1), (row2, col2)) =>
    if (row1 < row2) {
      (-1);
    } else if (row1 == row2) {
      Int.compare(col1, col2);
    } else {
      1;
    };
};
module RowColMap = Map.Make(RowCol);

let revpath_to_rowcol: Hashtbl.t(CursorPath.rev_t, (int, int)) =
  Hashtbl.create(300);
let rowcol_to_revpath: ref(RowColMap.t(CursorPath.rev_t)) =
  ref(RowColMap.empty);

let add = (row_col, rev_path) => {
  Hashtbl.add(revpath_to_rowcol, rev_path, row_col);
  rowcol_to_revpath := rowcol_to_revpath^ |> RowColMap.add(row_col, rev_path);
};

let lookup_rowcol = row_col =>
  rowcol_to_revpath^ |> RowColMap.find_opt(row_col);

let lookup_rowcol_before = row_col =>
  rowcol_to_revpath^
  |> RowColMap.find_first_opt(rc => RowCol.compare(rc, row_col) >= 0);

let lookup_rowcol_after = row_col =>
  rowcol_to_revpath^
  |> RowColMap.find_last_opt(rc => RowCol.compare(rc, row_col) <= 0);

let lookup_revpath = rev_path => Hashtbl.find(revpath_to_rowcol, rev_path);

/*
 let set_caret_row_text = (~state: State.t, (row, col)) => {
   let row_elem = JSUtil.force_get_elem_by_id(ViewUtil.row_text_id(row));
   let row_text =
     Js.Opt.get(
       (row_elem: Js.t(Dom_html.element) :> Js.t(Dom.node))##.firstChild, () =>
       assert(false)
     );
   state.setting_caret := true;
   JSUtil.set_caret(row_text, col);
 };

 let set_caret_row_eol = (~state: State.t, row) => {
   let contenteditable = (
     JSUtil.force_get_elem_by_id("contenteditable"): Js.t(Dom_html.element) :>
       Js.t(Dom.node)
   );
   let anchor_offset = 2 * row + 1;
   state.setting_caret := true;
   JSUtil.set_caret(contenteditable, anchor_offset);
 };

  let set_caret_rowcol = (~state, (row, col) as row_col) => {
    let set_caret_row_text = set_caret_row_text(~state);
    let set_caret_row_eol = set_caret_row_eol(~state);
    switch (lookup_rowcol_before(row_col), lookup_rowcol_after(row_col)) {
    | (None, None) =>
      failwith(
        Printf.sprintf("No caret position found near (%d, %d)", row, col),
      )
    | (Some(((next_row, next_col), rev_path)), None)
    | (None, Some(((next_row, next_col), rev_path))) =>
      set_caret_row_text((next_row, next_col));
      rev_path;
    | (Some(((next_row, next_col), rev_path)), Some(((row_after, _), _)))
        when row_after != row =>
      switch (rev_path) {
      | (OnText(0), _) => set_caret_row_eol(next_row)
      | _ => set_caret_row_text((next_row, next_col))
      };
      rev_path;
    | (Some(((row_before, _), _)), Some(((next_row, next_col), rev_path)))
        when row_before != row =>
      switch (rev_path) {
      | (OnText(0), _) => set_caret_row_eol(next_row)
      | _ => set_caret_row_text((next_row, next_col))
      };
      rev_path;
    // row_before == row_after
    | (
        Some(((_, col_before), (cursor_before, rev_steps) as rev_path_before)),
        Some(((_, col_after), (cursor_after, _) as rev_path_after)),
      ) =>
      switch (cursor_before, cursor_after) {
      | (OnText(_), OnText(_)) =>
        set_caret_row_text(row_col);
        (OnText(col - col_before), rev_steps);
      | _ =>
        if (col - col_before >= col_after - col) {
          set_caret_row_text((row, col_before));
          rev_path_before;
        } else {
          set_caret_row_text((row, col_after));
          rev_path_after;
        }
      }
    };
  };
  */

let is_empty_line = ((row, _) as row_col, (cursor, _)) =>
  switch ((cursor: CursorPosition.t)) {
  | OnText(0) =>
    switch (lookup_rowcol_after(row_col)) {
    | Some(((row_after, _), _)) when row_after != row => true
    | _ => false
    }
  | _ => false
  };

/*
 let set_caret_revpath = (~state, rev_path) => {
   let (row, _) as row_col = lookup_revpath(rev_path);
   if (is_empty_line(row_col, rev_path)) {
     set_caret_row_eol(~state, row);
   } else {
     set_caret_row_text(~state, row_col);
   };
 };
 */

let anchor_of_revpath = rev_path => {
  let (row, col) as row_col = lookup_revpath(rev_path);
  if (is_empty_line(row_col, rev_path)) {
    let anchor_node = (
      JSUtil.force_get_elem_by_id("contenteditable"): Js.t(Dom_html.element) :>
        Js.t(Dom.node)
    );
    (anchor_node, 2 * row + 1);
  } else {
    let anchor_node =
      JSUtil.force_get_elem_by_id(ViewUtil.row_text_id(row))
      |> JSUtil.force_get_first_child_node;
    (anchor_node, col);
  };
};

let revpath_of_anchor = () => {
  let anchor_node = Dom_html.window##getSelection##.anchorNode;
  let (row, col) as row_col =
    switch (anchor_node |> Dom_html.CoerceTo.element |> Js.Opt.to_option) {
    | None =>
      // anchor_node is text node in a row-text element
      let row =
        anchor_node
        |> JSUtil.force_get_closest_elem
        |> JSUtil.get_id
        |> ViewUtil.row_of_row_text_id
        |> Option.get;
      let col = Dom_html.window##getSelection##.anchorOffset;
      (row, col);
    | Some(elem) =>
      // anchor_node is contenteditable element.
      // anchor_offset = n means is on nth child
      // of contenteditable, in particular on a <br>.
      let row =
        elem |> JSUtil.get_id |> ViewUtil.row_of_row_eol_id |> Option.get;
      (row, 0);
    };
  switch (lookup_rowcol(row_col)) {
  | Some(rev_path) => rev_path
  | None =>
    switch (lookup_rowcol_before(row_col), lookup_rowcol_after(row_col)) {
    | (None, None) =>
      failwith(
        Printf.sprintf("No caret position found near (%d, %d)", row, col),
      )
    | (Some((_, rev_path)), None)
    | (None, Some((_, rev_path))) => rev_path
    | (Some((_, rev_path)), Some(((row_after, _), _)))
        when row_after != row => rev_path
    | (Some(((row_before, _), _)), Some((_, rev_path)))
        when row_before != row => rev_path
    // row_before == row_after
    | (
        Some((
          (_, col_before),
          (cursor_before, rev_steps) as rev_path_before,
        )),
        Some(((_, col_after), (cursor_after, _) as rev_path_after)),
      ) =>
      switch (cursor_before, cursor_after) {
      | (OnText(_), OnText(_)) => (OnText(col - col_before), rev_steps)
      | _ =>
        col - col_before >= col_after - col ? rev_path_before : rev_path_after
      }
    }
  };
};
