open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let of_id = (id: Id.t) =>
  "id" ++ (id |> Id.to_string |> String.sub(_, 0, 8));

/* Define a function to collect all leaf pieces from a tile */
let rec getLeafPieces =
        (segment: Base.segment, ~ignored_labels: list(list(string)))
        : list(Piece.t) => {
  let getLeaves = (node: Base.piece): list(Piece.t) =>
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
        tile.children |> List.concat_map(getLeafPieces(~ignored_labels));
      };
    | _ => []
    };

  segment |> List.concat_map(getLeaves);
};

let rec replacePieceInSegment =
        (segment: Base.segment, pieceToReplace: Piece.t): Base.segment => {
  let rec replacePieceNode = (node: Base.piece): Base.piece =>
    switch (node) {
    | Tile(tile) =>
      if (tile.id == Piece.id(pieceToReplace)) {
        /* Replace this tile with the input piece */
        pieceToReplace;
      } else if (tile.children == []) {
        /* Leaf tile, return as is */
        Tile(tile);
      } else {
        /* Recurse into the children */
        let newChildren =
          tile.children
          |> List.map(segment =>
               segment |> List.map(childNode => replacePieceNode(childNode))
             );
        /* Return a new Tile with updated children */
        Tile({
          ...tile,
          children: newChildren,
        });
      }
    | _ => node
    };
  /* Replace the piece in the segment */
  segment |> List.map(replacePieceNode);
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (_any: Term.Any.t) => None;
  let can_project = _p => true;
  let can_focus = false;
  let placeholder = (_model, info) => {
    let llname =
      switch (info.statics) {
      | Some(InfoExp(exp)) =>
        let (term, _) = Exp.unwrap(exp.term);
        switch (term) {
        | Parens(args) =>
          switch (args.term) {
          | Ap(_dir, ll_Exp, _args) =>
            let (ll_term, _) = Exp.unwrap(ll_Exp);
            switch (ll_term) {
            | LivelitName(name) => name
            | _ =>
              failwith(
                "LivelitProj: Not a LivelitName term -- " ++ Exp.show(ll_Exp),
              )
            };
          | _ =>
            failwith("LivelitProj: Not an Ap term -- " ++ Exp.show(args))
          }
        | _ =>
          failwith(
            "LivelitProj: Not a Parens term -- " ++ Exp.show(exp.term),
          )
        };
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

  let focusable = Focusable.non;

  let dynamics = false;

  let view =
      (
        _,
        info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) => {
    let (ll, args): (string, list(Exp.t)) =
      switch (info.statics) {
      | Some(InfoExp(exp)) =>
        let (term, _) = Exp.unwrap(exp.term);
        switch (term) {
        | Parens(args) =>
          switch (args.term) {
          | Ap(_dir, ll_Exp, args) =>
            let (ll_term, _) = Exp.unwrap(ll_Exp);
            let ll =
              switch (ll_term) {
              | LivelitName(name) => name
              | _ =>
                failwith(
                  "LivelitProj: Not a LivelitName term -- "
                  ++ Exp.show(ll_Exp),
                )
              };
            let (term, _) = Exp.unwrap(args);
            switch (term) {
            | Tuple(lst) => (ll, lst)
            | _ => (ll, [args])
            };
          | _ =>
            failwith("LivelitProj: Not an Ap term -- " ++ Exp.show(args))
          }
        | _ =>
          failwith(
            "LivelitProj: Not a Parens term -- " ++ Exp.show(exp.term),
          )
        };
      | _ =>
        print_endline("Warning - LivelitProj.view: Not an InfoExp term");
        ("error", []);
      };

    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => []
      };
    let ll = Ctx.lookup_livelit(ctx, ll);

    switch (ll) {
    | None => failwith("LivelitProj: Not a Parens term")
    | Some(ll) =>
      /* Ignore the first piece, which is the livelit invocation */
      let pieces =
        List.tl(getLeafPieces(info.syntax, ~ignored_labels=[[","]]));

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
        let newSyntax = replacePieceInSegment(info.syntax, piece);
        parent(SetSyntax(newSyntax));
      };

      /* Call the projector function */
      View.mk(ll.projector(model_pieces, replace));
    };
  };

  let focus = _ => ();
};
