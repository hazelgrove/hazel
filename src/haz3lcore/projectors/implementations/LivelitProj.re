open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Node;

let of_id = (id: Id.t) =>
  "id" ++ (id |> Id.to_string |> String.sub(_, 0, 8));

/* Define a function to collect all leaf pieces from a tile */
let rec leaf_pieces_from_node =
        (node: Base.piece, ~ignored_labels: list(list(string)))
        : list(Piece.t) =>
  switch (node) {
  | Tile(tile) =>
    /* Check if this tile's label is in the ignored labels */
    let should_ignore =
      List.exists(label => label == tile.label, ignored_labels);
    if (should_ignore) {
      [];
        /* Ignore this tile */
    } else if (tile.children == []) {
      [
        /* It's a leaf piece */
        Piece.Tile(tile),
      ];
    } else {
      /* Recurse into the children */
      tile.children
      |> List.concat_map(segment =>
           segment
           |> List.concat_map(child_node =>
                leaf_pieces_from_node(child_node, ~ignored_labels)
              )
         );
    };
  | _ => []
  };

let leaf_pieces_from_segment =
    (segment: Base.segment, ~ignored_labels: list(list(string)))
    : list(Piece.t) => {
  segment |> List.concat_map(leaf_pieces_from_node(_, ~ignored_labels));
};

let rec replace_piece_node =
        (node: Base.piece, piece_to_replace: Piece.t): Base.piece =>
  switch (node) {
  | Tile(tile) =>
    if (tile.id == Piece.id(piece_to_replace)) {
      /* Replace this tile with the input piece */
      piece_to_replace;
    } else if (tile.children == []) {
      /* Leaf tile, return as is */
      Tile(tile);
    } else {
      /* Recurse into the children */
      let new_children =
        tile.children
        |> List.map(segment =>
             segment
             |> List.map(child_node =>
                  replace_piece_node(child_node, piece_to_replace)
                )
           );
      /* Return a new Tile with updated children */
      Tile({
        ...tile,
        children: new_children,
      });
    }
  | _ => node
  };

let replace_piece_in_segment =
    (segment: Base.segment, piece_to_replace: Piece.t): Base.segment => {
  /* Replace the piece in the segment */
  segment |> List.map(node => replace_piece_node(node, piece_to_replace));
};

let extract_livelit_name_from_exp = (exp: Exp.t): string => {
  let (term, _) = Exp.unwrap(exp);
  switch (term) {
  | Ap(_dir, ll_exp, _args) =>
    let (ll_term, _) = Exp.unwrap(ll_exp);
    switch (ll_term) {
    | LivelitName(name) => name
    | _ =>
      failwith("LivelitProj: Not a LivelitName term -- " ++ Exp.show(ll_exp))
    };
  | _ => failwith("LivelitProj: Not an Ap term -- " ++ Exp.show(exp))
  };
};

let extract_args_from_exp = (exp: Exp.t): list(Exp.t) => {
  let (term, _) = Exp.unwrap(exp);
  switch (term) {
  | Ap(_dir, _ll_exp, args) =>
    let (term, _) = Exp.unwrap(args);
    switch (term) {
    | Tuple(lst) => lst
    | _ => [args]
    };
  | _ => failwith("LivelitProj: Not an Ap term -- " ++ Exp.show(exp))
  };
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (_any: Term.Any.t) => Some();
  let can_focus = false;
  let placeholder = (_model, info) => {
    let llname =
      switch (info.statics) {
      | Some(InfoExp(exp)) => extract_livelit_name_from_exp(exp.term)
      | _ =>
        print_endline(
          "Warning - LivelitProj.placeholder: Not an InfoExp term",
        );
        "error";
      };

    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => []
      };
    switch (Ctx.lookup_livelit(ctx, llname)) {
    | Some(ll) => ll.size
    | None => ProjectorCore.Shape.inline(32)
    };
  };
  let update = (model, _, _) => model;

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: None,
    };

  let dynamics = false;

  let view =
      (
        _,
        info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) => {
    let (ll_name, args): (string, list(Exp.t)) =
      switch (info.statics) {
      | Some(InfoExp(exp)) => (
          extract_livelit_name_from_exp(exp.term),
          extract_args_from_exp(exp.term),
        )
      | _ =>
        print_endline("Warning - LivelitProj.view: Not an InfoExp term");
        ("error", []);
      };

    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => []
      };
    let ll = Ctx.lookup_livelit(ctx, ll_name);

    switch (ll) {
    | None =>
      ProjectorBase.View.mk(
        div([text("LivelitProj: No livelit found for " ++ ll_name)]),
      )
    | Some(ll) =>
      /* Ignore the first piece, which is the livelit invocation */
      let pieces =
        List.tl(
          leaf_pieces_from_segment(info.syntax, ~ignored_labels=[[","]]),
        );

      /* Combine args and pieces into model_piece records */
      let model_pieces =
        List.map2(
          (arg, piece): Ctx.model_piece => {
            {
              model: arg,
              piece,
            }
          },
          args,
          pieces,
        );

      let replace = (piece: Base.piece) => {
        let new_syntax = replace_piece_in_segment(info.syntax, piece);
        parent(SetSyntax(new_syntax));
      };

      /* Call the projector function */
      View.mk(
        Node.div(
          ~attrs=[Attr.class_(ll_name), Attr.id(Id.cls(info.id))],
          switch (ll.projector(model_pieces, replace, info.id)) {
          | Node(node) => [node]
          | List(nodes) => nodes
          },
        ),
      );
    };
  };
};
