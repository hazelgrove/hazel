open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Node;

let of_id = (id: Id.t) =>
  "id" ++ (id |> Id.to_string |> String.sub(_, 0, 8));

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

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = LivelitCtx.action_exp;

  let get = (info: info) =>
    switch (info.statics) {
    | Some(
        InfoExp({
          term: {term: Ap(_dir, {term: LivelitName(llname), _}, model), _},
          _,
        }),
      ) =>
      Some((llname, model))
    | _ =>
      print_endline("Warning - LivelitProj.get: Not an InfoExp term");
      None;
    };

  let init = (_any: Term.Any.t) => Some();
  let can_focus = false;
  let placeholder = (_model, info) => {
    switch (get(info), info.statics) {
    | (Some((llname, _)), Some(InfoExp(exp))) =>
      /* Get the livelit size */
      switch (Ctx.lookup_livelit(exp.ctx, llname)) {
      | Some(ll) => ll.size
      | None =>
        /* Default size */
        ProjectorCore.Shape.inline(32)
      }
    | _ =>
      /* Default size */
      ProjectorCore.Shape.inline(32)
    };
  };
  let update = (_, info, action: LivelitCtx.action_exp) => {
    print_endline("LivelitProj.update " ++ Exp.show(action));

    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => []
      };

    switch (get(info)) {
    | Some((llname, model)) =>
      let ll = Ctx.lookup_livelit(ctx, llname);
      switch (ll) {
      | Some(ll) =>
        let new_model = ll.update(action, model);
        let seg = info.utility.term_to_seg(Exp(new_model));
        print_endline("new segment update: " ++ Segment.show(seg));
        ();
      | None =>
        print_endline("Warning - LivelitProj.update: not found in context")
      };
    | None => print_endline("Warning - LivelitProj.update: get is empty")
    };
    ();
  };

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
        ~local: action => Ui_effect.t(unit),
        ~parent as _,
        ~view_seg as _,
      ) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => []
      };

    let node =
      switch (get(info)) {
      | Some((ll_name, args)) =>
        let ll = Ctx.lookup_livelit(ctx, ll_name);

        switch (ll) {
        | Some(ll) =>
          let list_contents =
            switch (ll.view(args, local)) {
            | Node(node) => [node]
            | List(nodes) => nodes
            };
          Node.div(
            ~attrs=[Attr.class_(ll_name), Attr.id(Id.cls(info.id))],
            list_contents,
          );
        | None =>
          print_endline("Warning - LivelitProj.view: not found in context");
          Node.text("No livelit found");
        };
      | None =>
        print_endline("Warning - LivelitProj.view: get is empty");
        Node.text("No livelit found");
      };

    View.mk(node);
  };
};
