open Util;
open Util.OptUtil.Syntax;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* Vertical list projector.
 *
 * Renders the items of a Hazel list literal [e0, ..., eN-1] as
 * splice editors stacked vertically, with [+]/[-] buttons to add
 * or remove items. Because splices are transparent at term-construction
 * time, the underlying term is a normal ListLit(...), so statics and
 * dynamics see a real list. */

/* An "empty" splice contains a single Convex grout (an empty hole),
 * matching how the rest of the zipper represents empty term positions.
 * A literally-empty content segment would be rejected downstream
 * (e.g. [Segment.skel] raises [Nonconvex_segment] on [[]]). */
let empty_splice = (): Base.piece =>
  Piece.mk_splice([Piece.mk_grout(Convex)]);

let comma_piece = (): Base.piece => Piece.mk_tile(Form.get(CommaExp), []);

let empty_list_piece = (): Base.piece =>
  Base.Tile({
    id: Id.mk(),
    label: ["[]"],
    mold: Mold.mk_op(Sort.Exp, []),
    shards: [0],
    children: [],
  });

let list_piece = (inner: Base.segment): Base.piece =>
  Piece.mk_tile(Form.get(ListLitExp), [inner]);

let is_comma_tile = (p: Base.piece): bool =>
  switch (p) {
  | Tile({label: [","], _}) => true
  | _ => false
  };

/* Given a list of splice pieces, interleave with commas to form the
 * list tile's child segment:
 *   [s0; s1; s2]  ~>  [s0, comma, s1, comma, s2] */
let interleave_with_commas = (splices: list(Base.piece)): Base.segment =>
  switch (splices) {
  | [] => []
  | [s, ...rest] => [s] @ List.concat_map(s => [comma_piece(), s], rest)
  };

let build_list_syntax = (splices: list(Base.piece)): Base.segment =>
  switch (splices) {
  | [] => [empty_list_piece()]
  | splices => [list_piece(interleave_with_commas(splices))]
  };

/* Split a segment at comma tiles (at the top level), returning the
 * segment groups between commas. An empty segment yields [[]]. */
let split_at_commas = (seg: Base.segment): list(Base.segment) => {
  let (groups, last) =
    List.fold_left(
      ((groups, current), p: Base.piece) =>
        if (is_comma_tile(p)) {
          (groups @ [current], []);
        } else {
          (groups, current @ [p]);
        },
      ([], []),
      seg,
    );
  groups @ [last];
};

/* Drop leading and trailing secondary (whitespace/comment) pieces so
 * that items extracted from between commas start clean. */
let trim_secondary = (seg: Base.segment): Base.segment => {
  let drop_while = (p: Base.piece => bool, xs) =>
    List.fold_left(
      (acc, x) =>
        switch (acc) {
        | [] when p(x) => []
        | _ => acc @ [x]
        },
      [],
      xs,
    );
  let is_secondary = (p: Base.piece) =>
    switch (p) {
    | Secondary(_) => true
    | _ => false
    };
  seg
  |> drop_while(is_secondary)
  |> List.rev
  |> drop_while(is_secondary)
  |> List.rev;
};

/* Extract the splice pieces from the VList's stored syntax. Returns
 * [None] if the syntax doesn't have the expected shape (e.g. the user
 * has edited it via a SetSyntax into something unexpected). */
let get_splices_from_syntax =
    (syntax: Base.segment): option(list(Base.splice)) => {
  let* items_child =
    switch (syntax) {
    | [Tile({label: [t], children: [], _})] when Token.is_empty_list(t) =>
      Some([])
    | [Tile({children: [child], _})] => Some(child)
    | _ => None
    };
  let extract_group = (pieces: Base.segment): option(Base.splice) =>
    switch (trim_secondary(pieces)) {
    | [Splice(s)] => Some(s)
    | _ => None
    };
  let rec extract_groups = (groups: list(Base.segment)) =>
    switch (groups) {
    | [] => Some([])
    | [group, ...rest] =>
      let* splice = extract_group(group);
      let+ tail = extract_groups(rest);
      [splice, ...tail];
    };
  switch (items_child) {
  | [] => Some([])
  | _ => extract_groups(split_at_commas(items_child))
  };
};

let splices_to_syntax = (splices: list(Base.splice)): Base.segment => {
  let pieces = List.map((s: Base.splice) => Base.Splice(s), splices);
  build_list_syntax(pieces);
};

/* Build the initial projector syntax by walking the selected segment:
 * locate the [ ... ] tile, split its inner child at commas, and wrap
 * each item in a splice. Returns [None] if the shape doesn't match. */
let transform_selected = (seg: Base.segment): option(Base.segment) => {
  let* items_child =
    switch (seg) {
    | [Tile({label: [t], children: [], _})] when Token.is_empty_list(t) =>
      Some([])
    | [Tile({label: ["[", "]"], children: [child], _})] => Some(child)
    | _ => None
    };
  let splices =
    switch (split_at_commas(items_child)) {
    | [[]] => [empty_splice()] /* empty input: start with one empty splice */
    | groups =>
      List.map(
        (g: Base.segment) => Piece.mk_splice(trim_secondary(g)),
        groups,
      )
    };
  Some(build_list_syntax(splices));
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;

  let init = (any: Any.t, seg: Base.segment) =>
    switch (any) {
    | Exp({term: ListLit(_), _}) =>
      switch (transform_selected(seg)) {
      | Some(syntax) => Some(((), Some(syntax)))
      | None =>
        /* Fallback: synthesise a single-item list if we can't parse the
         * selected segment's shape. */
        let syntax = build_list_syntax([empty_splice()]);
        Some(((), Some(syntax)));
      }
    | _ => None
    };

  /* The projector occupies vertical rows totalling the stacked splice
   * content plus one row for the button bar. */
  let placeholder = ((), info: info, splice_size: View.splice_size) => {
    let splices =
      Option.value(get_splices_from_syntax(info.syntax), ~default=[]);
    let splice_rows =
      List.fold_left(
        (acc, s: Base.splice) => acc + max(1, splice_size(s.id).row + 1),
        0,
        splices,
      );
    let rows =
      /* Button row + top/bottom padding + stacked splice rows/gaps. */
      splice_rows + 2;
    let cols =
      List.fold_left(
        (acc, s: Base.splice) =>
          /* Add room for the inline-editor hex padding on both sides. */
          max(acc, splice_size(s.id).col + 3),
        7, /* minimum width so the buttons and padding fit */
        splices,
      );
    ProjectorCore.Shape.{
      horizontal: cols,
      vertical: Block(rows),
    };
  };

  let update = ((), _, ()) => ();
  let error = (_, _): option(ProjectorBase.error) => None;

  let splices_to_term = (info: info, splices: list(Base.splice)): Any.t => {
    let splice_to_exp = (splice: Base.splice): Exp.t =>
      switch (info.utility.seg_to_term(trim_secondary(splice.content))) {
      | Some(Exp(exp)) =>
        IdTagged.fast_copy(splice.id, (Splice(exp): Exp.term) |> Exp.fresh)
      | _ =>
        /* The syntax is preserved by splice id on SetTerm; use a semantic
         * hole rather than crashing if a splice is temporarily unparsable. */
        IdTagged.fast_copy(
          splice.id,
          (Splice(Exp.fresh(EmptyHole)): Exp.term) |> Exp.fresh,
        )
      };
    let list_term =
      IdTagged.fast_copy(
        info.id,
        (ListLit(List.map(splice_to_exp, splices)): Exp.term) |> Exp.fresh,
      );
    Exp(list_term);
  };

  let add_item = (info: info, parent) =>
    switch (get_splices_from_syntax(info.syntax)) {
    | None => Ui_effect.Ignore
    | Some(splices) =>
      let new_splice =
        switch (empty_splice()) {
        | Splice(s) => s
        | _ => failwith("VListProj.add_item: empty_splice is not a splice")
        };
      let new_splices = splices @ [new_splice];
      parent(SetTerm(splices_to_term(info, new_splices), true));
    };

  let remove_item = (info: info, parent) =>
    switch (get_splices_from_syntax(info.syntax)) {
    | None
    | Some([]) => Ui_effect.Ignore
    | Some(splices) =>
      let n = List.length(splices);
      let new_splices = List.filteri((i, _) => i < n - 1, splices);
      parent(SetTerm(splices_to_term(info, new_splices), true));
    };

  let button = (~label: string, ~disabled: bool, ~on_click) => {
    let base = [
      Attr.classes(["vlist-btn"]),
      Attr.on_pointerdown(_ =>
        disabled
          ? Effect.Stop_propagation
          : Effect.Many([Effect.Stop_propagation, on_click()])
      ),
      Attr.on_mousedown(_ => Effect.Stop_propagation),
      Attr.on_click(_ => Effect.Stop_propagation),
    ];
    Node.button(
      ~attrs=disabled ? [Attr.disabled, ...base] : base,
      [Node.text(label)],
    );
  };

  let view =
      ({info, parent, view_seg, splice_view, _}: View.args(model, action)) => {
    switch (get_splices_from_syntax(info.syntax)) {
    | None =>
      View.mk(
        Node.div(
          ~attrs=[Attr.classes(["vlist", "fallback"])],
          [view_seg(~background=true, Sort.Exp, info.syntax)],
        ),
      )
    | Some(splices) =>
      let empty = splices == [];
      let buttons =
        Node.div(
          ~attrs=[Attr.classes(["vlist-buttons"])],
          [
            button(~label="+", ~disabled=false, ~on_click=() =>
              add_item(info, parent)
            ),
            button(~label="-", ~disabled=empty, ~on_click=() =>
              remove_item(info, parent)
            ),
          ],
        );
      let rows =
        List.map(
          (s: Base.splice) =>
            Node.div(
              ~attrs=[Attr.classes(["vlist-row"])],
              [splice_view(s.id)],
            ),
          splices,
        );
      View.mk(
        Node.div(~attrs=[Attr.classes(["vlist"])], [buttons, ...rows]),
      );
    };
  };
};
