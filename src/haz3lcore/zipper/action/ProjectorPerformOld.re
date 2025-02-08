// open Projector;
  // open ProjectorBase;
  // /* Updates the underlying piece of syntax for a projector */
  // module Update = {
  //   let update_piece =
  //       (f: Base.projector => Base.projector, id: Id.t, syntax: syntax) =>
  //     switch (syntax) {
  //     | Projector(pr) when pr.id == id => Base.Projector(f(pr))
  //     | x => x
  //     };
  //   let init = (kind: t, syntax: syntax, projector_id): syntax => {
  //     /* We set the projector id equal to the Piece id for convienence
  //      * including cursor-info association. We maintain this invariant
  //      * when we update a projector's contained syntax */
  //     let (module P) = to_module(kind);
  //     switch (P.can_project(syntax) && minimum_projection_condition(syntax)) {
  //     | false => syntax
  //     | true => Projector({id: projector_id, kind, model: P.init, syntax})
  //     };
  //   };
  //   let init_from_str = (kind: t, syntax: syntax, model_str: string): syntax => {
  //     let (module P) = to_module(kind);
  //     switch (P.can_project(syntax) && minimum_projection_condition(syntax)) {
  //     | false => syntax
  //     | true =>
  //       Projector({id: Piece.id(syntax), kind, model: model_str, syntax})
  //     };
  //   };
  //   let add_projector =
  //       (kind: Base.kind, projector_id, piece_id: Id.t, syntax: syntax) =>
  //     switch (syntax) {
  //     | Projector(pr) when Piece.id(syntax) == piece_id =>
  //       init(kind, pr.syntax, projector_id)
  //     | syntax when Piece.id(syntax) == piece_id =>
  //       init(kind, syntax, projector_id)
  //     | x => x
  //     };
  //   let remove_projector = (id: Id.t, syntax: syntax) =>
  //     switch (syntax) {
  //     | Projector(pr) when pr.id == id => pr.syntax
  //     | x => x
  //     };
  //   let add_or_remove_projector =
  //       (kind: Base.kind, piece_id: Id.t, projector_id, syntax: syntax) =>
  //     switch (syntax) {
  //     | Projector(pr) when Piece.id(syntax) == piece_id => pr.syntax
  //     | syntax when Piece.id(syntax) == piece_id =>
  //       init(kind, syntax, projector_id)
  //     | x => x
  //     };
  //   let remove_any_projector = (syntax: syntax) =>
  //     switch (syntax) {
  //     | Projector(pr) => pr.syntax
  //     | x => x
  //     };
  //   let update =
  //       (f: Base.projector => Base.projector, id: Id.t, z: ZipperBase.t)
  //       : ZipperBase.t =>
  //     ZipperBase.MapPiece.fast_local(update_piece(f, id), id, z);
  //   let add =
  //       (k: Base.kind, projector_id, piece_id: Id.t, z: ZipperBase.t)
  //       : ZipperBase.t =>
  //     ZipperBase.MapPiece.fast_local(
  //       add_projector(k, projector_id, piece_id),
  //       piece_id,
  //       z,
  //     );
  //   let add_or_remove =
  //       (k: Base.kind, piece_id: Id.t, projector_id, z: ZipperBase.t)
  //       : ZipperBase.t =>
  //     ZipperBase.MapPiece.fast_local(
  //       add_or_remove_projector(k, piece_id, projector_id),
  //       piece_id,
  //       z,
  //     );
  //   let remove = (id: Id.t, z: ZipperBase.t): ZipperBase.t =>
  //     ZipperBase.MapPiece.fast_local(remove_projector(id), id, z);
  //   let remove_all = (z: ZipperBase.t): ZipperBase.t =>
  //     ZipperBase.MapPiece.go(remove_any_projector, z);
  // };
  // /* If the caret is inside the indicated piece, move it out
  //  * NOTE: Might need to be updated to support pieces with more than 2 delims */
  // let move_out_of_piece =
  //     (d: Util.Direction.t, rel: Indicated.relation, z: Zipper.t): Zipper.t =>
  //   switch (rel) {
  //   | Sibling => {...z, caret: Outer}
  //   | Parent =>
  //     switch (Zipper.move(d, {...z, caret: Outer})) {
  //     | Some(z) => z
  //     | None => z
  //     }
  //   };
  // let go =
  //     (jump_to_id_indicated, jump_to_side_of_id, a: Action.project, z: Zipper.t)
  //     : result(ZipperBase.t, Action.Failure.t) => {
  //   switch (a) {
  //   | SetIndicated(p, id) =>
  //     print_endline("SetIndicated");
  //     switch (Indicated.for_index(z)) {
  //     | None => Error(Cant_project)
  //     | Some((piece, d, rel)) =>
  //       print_endline("oldz:" ++ Zipper.show(z));
  //       print_endline("piece_id:" ++ Id.to_string(Piece.id(piece)));
  //       print_endline("id:" ++ Id.to_string(id));
  //       let new_z =
  //         move_out_of_piece(d, rel, z) |> Update.add(p, id, Piece.id(piece));
  //       print_endline("newz:" ++ Zipper.show(new_z));
  //       Ok(new_z);
  //     };
  //   | ToggleIndicated(p, id) =>
  //     switch (Indicated.for_index(z)) {
  //     | None => Error(Cant_project)
  //     | Some((_, d, rel)) =>
  //       //TODO: projector id?
  //       Ok(move_out_of_piece(d, rel, z) |> Update.add_or_remove(p, id, id))
  //     }
  //   | Remove(id) => Ok(Update.remove(id, z))
  //   | SetSyntax(id, syntax) =>
  //     /* Note we update piece id to keep in sync with projector id;
  //      * See intial id setting in Update.init */
  //     Ok(
  //       Update.update(
  //         p => {...p, syntax: Piece.replace_id(id, syntax)},
  //         id,
  //         z,
  //       ),
  //     )
  //   | SetModel(id, model) => Ok(Update.update(pr => {...pr, model}, id, z))
  //   | Focus(id, d) =>
  //     let z =
  //       switch (d) {
  //       | None =>
  //         /* d == None means focus by mouse click */
  //         jump_to_id_indicated(z, id) |> Option.value(~default=z)
  //       | Some(_) => z
  //       };
  //     switch (Projector.indicated(z)) {
  //     | Some((_, p)) =>
  //       let (module P) = to_module(p.kind);
  //       P.focus((id, d));
  //       Ok(z);
  //     | None => Error(Cant_project)
  //     };
  //   | Escape(id, d) => Ok(jump_to_side_of_id(d, z, id))
  //   };
  // };
