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
        : list(Piece.tile) =>
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
        tile,
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
        (syntaxNode: ProjectorBase.syntax, pieceToReplace: Piece.tile)
        : ProjectorBase.syntax =>
  switch (syntaxNode) {
  | Tile(tile) =>
    if (tile.id == pieceToReplace.id) {
      /* Replace this tile with the input piece */
      Tile(pieceToReplace);
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
  let can_project = p => true;
  let can_focus = false;
  let placeholder = (model, info) => {
    let llname =
      switch (info.ci) {
      | Some(InfoExp(exp)) =>
        let (term, _) = UExp.unwrap(exp.term);
        switch (term) {
        | Parens(args) =>
          switch (args.term) {
          | Ap(_dir, ll_uexp, args) =>
            let (ll_term, _) = UExp.unwrap(ll_uexp);
            switch (ll_term) {
            | LivelitInvocation(name) => name
            | _ =>
              failwith(
                "LivelitProj: Not a LivelitInvocation term -- "
                ++ UExp.show(ll_uexp),
              )
            };
          | _ =>
            failwith("LivelitProj: Not an Ap term -- " ++ UExp.show(args))
          }
        | _ =>
          failwith(
            "LivelitProj: Not a Parens term -- " ++ UExp.show(exp.term),
          )
        };
      | _ => failwith("LivelitProj: Not an InfoExp term")
      };
    let ll = Livelit.find_livelit(llname);
    Inline(ll.size);
  };
  let update = (model, _) => model;

  let view =
      (_, ~info, ~local as _, ~parent: external_action => Ui_effect.t(unit)) => {
    let (ll, args): (string, list(UExp.t)) =
      switch (info.ci) {
      | Some(InfoExp(exp)) =>
        let (term, _) = UExp.unwrap(exp.term);
        switch (term) {
        | Parens(args) =>
          switch (args.term) {
          | Ap(_dir, ll_uexp, args) =>
            let (ll_term, _) = UExp.unwrap(ll_uexp);
            let ll =
              switch (ll_term) {
              | LivelitInvocation(name) => name
              | _ =>
                failwith(
                  "LivelitProj: Not a LivelitInvocation term -- "
                  ++ UExp.show(ll_uexp),
                )
              };
            let (term, _) = UExp.unwrap(args);
            switch (term) {
            | Tuple(lst) => (ll, lst)
            | _ => (ll, [args])
            };
          | _ =>
            failwith("LivelitProj: Not an Ap term -- " ++ UExp.show(args))
          }
        | _ =>
          failwith(
            "LivelitProj: Not a Parens term -- " ++ UExp.show(exp.term),
          )
        };
      | _ => failwith("LivelitProj: Not an InfoExp term")
      };

    /* Ignore the first piece, which is the livelit invocation */
    let pieces =
      List.tl(getLeafPieces(info.syntax, ~ignored_labels=[[","]]));
    let ll = Livelit.find_livelit(ll);

    // print_endline("size of args: " ++ string_of_int(List.length(args)));
    // print_endline("size of pieces: " ++ string_of_int(List.length(pieces)));
    // print_endline(
    //   "types of pieces: "
    //   ++ (List.map(Tile.show, pieces) |> String.concat(", ")),
    // );

    /* Combine args and pieces into model_piece records */
    let model_pieces =
      List.map2(
        (arg, piece): Livelit.model_piece => {{model: arg, piece}},
        args,
        pieces,
      );

    let replace = (piece: Piece.tile) => {
      let newSyntax = replacePieceInSyntax(info.syntax, piece);
      parent(SetSyntax(newSyntax));
    };

    /* Call the projector function */
    ll.projector(model_pieces, replace);
  };
  let focus = ((id: Id.t, d: option(Direction.t))) => {
    JsUtil.get_elem_by_id(of_id(id))##focus;
    switch (d) {
    | _ => ()
    };
  };
};
