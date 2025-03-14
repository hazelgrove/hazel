open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let of_id = (id: Id.t) =>
  "id" ++ (id |> Id.to_string |> String.sub(_, 0, 8));

/* Define a function to collect all leaf pieces from a tile */
let rec getLeafPieces =
        (
          syntaxNode: ProjectorBase.syntax,
          ~ignored_labels: list(list(string)),
        )
        : list(Piece.t) =>
  switch (syntaxNode) {
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
        Tile(tile),
      ];
    } else {
      /* Recurse into the children */
      tile.children
      |> List.concat_map(segment =>
           segment |> List.concat_map(getLeafPieces(~ignored_labels))
         );
    };
  | _ => []
  };

let rec replacePieceInSyntax =
        (syntaxNode: ProjectorBase.syntax, pieceToReplace: Piece.t)
        : ProjectorBase.syntax =>
  switch (syntaxNode) {
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
             segment
             |> List.map(childNode =>
                  replacePieceInSyntax(childNode, pieceToReplace)
                )
           );
      /* Return a new Tile with updated children */
      Tile({...tile, children: newChildren});
    }
  | _ => syntaxNode
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = _p => true;
  let can_focus = false;
  let placeholder = (_model, info) => {
    let llname =
      switch (info.ci) {
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
      switch (info.ci) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => []
      };
    switch (Ctx.lookup_livelit(ctx, llname)) {
    | Some(ll) => ll.size
    | None => ProjectorCore.Inline(32)
    };
  };
  let update = (model, _) => model;

  let view =
      (_, ~info, ~local as _, ~parent: external_action => Ui_effect.t(unit)) => {
    let (ll, args): (string, list(Exp.t)) =
      switch (info.ci) {
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
      switch (info.ci) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => []
      };
    let ll = Ctx.lookup_livelit(ctx, ll);

    switch (ll) {
    | None =>
      Node.div(
        ~attrs=[Attr.class_("livelit")],
        [Node.text("Cannot display livelit -- are statics enabled?")],
      )
    | Some(ll) =>
      /* Ignore the first piece, which is the livelit invocation */
      let pieces =
        List.tl(getLeafPieces(info.syntax, ~ignored_labels=[[","]]));

      /* Combine args and pieces into model_piece records */
      let model_pieces =
        List.map2(
          (arg, piece): Ctx.model_piece => {{model: arg, piece}},
          args,
          pieces,
        );

      let replace = (piece: Base.piece) => {
        let newSyntax = replacePieceInSyntax(info.syntax, piece);
        parent(SetSyntax(newSyntax));
      };

      /* Call the projector function */
      ll.projector(model_pieces, replace);
    };
  };
  //   let focus = ((id: Id.t, d: option(Direction.t))) => {
  //     JsUtil.get_elem_by_id(of_id(id))##focus;
  //     switch (d) {
  //     | _ => ()
  //     };
  //   };
  let focus = _ => ();
};
