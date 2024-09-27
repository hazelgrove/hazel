open Projector;
open ProjectorBase;
open Util.OptUtil.Syntax;

/* Updates the underlying piece of syntax for a projector */
module Update = {
  let init = (id, kind: t, syntax: syntax): option(Base.projector) => {
    let (module P) = to_module(kind);
    switch (P.can_project(syntax) && minimum_projection_condition(syntax)) {
    | false => None
    | true =>
      /* We set the projector id equal to the root piece id for convienence
       * including cursor-info association. We maintain this invariant
       * when we update a projector's contained syntax */
      Some({id, kind, model: P.init, syntax})
    };
  };

  let rem_p_if = (id: Id.t, piece: Piece.t): Segment.t =>
    switch (piece) {
    | Projector(pr) when pr.id == id => pr.syntax
    | x => [x]
    };

  let remove_if = (id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapSegment.fast_local(List.concat_map(rem_p_if(id)), id, z);

  let remove_p = (piece: Piece.t): Segment.t =>
    switch (piece) {
    | Projector(pr) => pr.syntax
    | x => [x]
    };

  let remove_all = (z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapSegment.go(List.concat_map(p => remove_p(p)), z);

  let update_piece =
      (f: Base.projector => Base.projector, id: Id.t, piece: Piece.t) =>
    switch (piece) {
    | Projector(pr) when pr.id == id => Base.Projector(f(pr))
    | x => x
    };

  let update =
      (f: Base.projector => Base.projector, id: Id.t, z: ZipperBase.t)
      : ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(update_piece(f, id), id, z);
};

/* If the caret is inside the indicated piece, move it out
 * NOTE: Might need to be updated to support pieces with more than 2 delims */
let move_out_of_piece =
    (d: Util.Direction.t, rel: Indicated.relation, z: Zipper.t): Zipper.t =>
  switch (rel) {
  | Sibling => {...z, caret: Outer}
  | Parent =>
    switch (Zipper.move(d, {...z, caret: Outer})) {
    | Some(z) => z
    | None => z
    }
  };

let proj_rep_id = (syntax: Segment.t): option(Id.t) =>
  //TODO(andrew): currently permissive in that it permits truncating forms
  //in some cases eg "if true then" can be folded without the "else"
  switch (syntax) {
  | [] => failwith("ProjectorPerform: Expected non-empty syntax")
  | [p] when Piece.is_complete(p) =>
    /* Single complete piece case (e.g. let) */
    Some(Piece.id(p))
  //TODO(andrew): multi-tile non-term case e.g. `: Int`
  | [p, ..._] when List.for_all(Piece.is_secondary, syntax) =>
    Some(Piece.id(p))
  | _ =>
    switch (Segment.root_rep_id(syntax)) {
    | exception _ =>
      print_endline("ProjectorPerform: Not complete term");
      None;
    | id => Some(id)
    }
  };

let go =
    (
      jump_to_id_indicated,
      jump_to_side_of_id,
      move_primary,
      do_until:
        (
          ZipperBase.t => option(ZipperBase.t),
          Piece.t => bool,
          ZipperBase.t
        ) =>
        option(ZipperBase.t),
      select_term,
      indicated_token,
      a: Action.project,
      z: Zipper.t,
    )
    : result(ZipperBase.t, Action.Failure.t) => {
  let move_to_id = (d: Util.Direction.t, id: Id.t, z: ZipperBase.t) => {
    //TODO(andrew):
    /* This approach is suboptimal in that it sometimes
       eg for apps doesnt leave cursor at an indication position */
    /* NOTE: using current_term_fancy here has unintended consequences for
       lets and case rules (former seems to work okay, latter is broken though) */
    //TODO(andrew): cleanup or clairfy
    let z_init =
      switch (
        do_until(
          move_primary(Zipper.ByToken, d),
          (p: Piece.t) => Piece.id(p) == id,
          z,
        )
      ) {
      | None =>
        switch (Indicated.piece'(~no_ws=false, ~ign=_ => false, z)) {
        | None => z
        | Some((piece, _, _)) when Piece.id(piece) == id =>
          //already there
          z
        | Some(_) =>
          let z =
            do_until(
              move_primary(Zipper.ByToken, Util.Direction.toggle(d)),
              (p: Piece.t) => Piece.id(p) == id,
              z,
            )
            |> Option.value(~default=z);
          z
          |> move_primary(
               Zipper.ByToken,
               d == Right ? Util.Direction.Right : Left,
             )
          |> Option.value(~default=z);
        }
      | Some(z) =>
        move_primary(
          Zipper.ByToken,
          d == Right ? Util.Direction.Left : Right,
          z,
        )
        |> Option.value(~default=z)
      };
    if (Indicated.index(z) == Some(id)) {
      z;
    } else if (Indicated.index(z_init) == Some(id)) {
      z_init;
    } else {
      z_init
      |> move_primary(
           Zipper.ByToken,
           d == Right ? Util.Direction.Right : Left,
         )
      |> Option.value(~default=z);
    };
  };
  let remove_indicated = (id, d, z) =>
    Update.remove_if(id, z) |> move_to_id(d, id);
  switch (a) {
  | ToggleIndicated(kind) =>
    print_endline("ToggleIndicated");
    switch (z.selection.content) {
    | [] =>
      switch (Indicated.for_index(z)) {
      | None => Error(Cant_project)
      | Some((piece, d, _)) =>
        let id = Piece.id(piece);
        switch (Piece.is_projector(piece)) {
        | None =>
          switch (
            {
              let* z: ZipperBase.t =
                if (Piece.is_secondary(piece)) {
                  indicated_token(z);
                } else {
                  select_term(z);
                };
              let+ p = Update.init(id, kind, z.selection.content);
              let z =
                Zipper.put_selection(
                  {...z.selection, content: [Piece.Projector(p)], focus: d},
                  z,
                );
              Zipper.unselect(z);
            }
          ) {
          | None => Error(Cant_project)
          | Some(z) => Ok(z)
          }
        | Some(current) when current.kind == kind =>
          Ok(remove_indicated(id, d, z))
        | Some(current) =>
          switch (Update.init(id, kind, current.syntax)) {
          | None => Error(Cant_project)
          | Some(p) => Ok(Update.update(_ => p, id, z))
          }
        };
      }
    | _ =>
      /* If there's a selection, try to project that */
      switch (
        {
          let* id = proj_rep_id(z.selection.content);
          let+ p = Update.init(id, kind, z.selection.content);
          let z =
            Zipper.put_selection(
              {...z.selection, content: [Piece.Projector(p)]},
              z,
            );
          Zipper.unselect(z);
        }
      ) {
      | None => Error(Cant_project)
      | Some(z) => Ok(z)
      }
    };
  | RemoveIndicated =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, _)) => Ok(remove_indicated(Piece.id(piece), d, z))
    }
  | SetSyntax(id, syntax) =>
    /* Note we update piece id to keep in sync with projector id;
     * See intial id setting in Update.init */
    /* all secondary? fine. complete term? fine
       otherwise: check if either side has tile which is concave
       at that side. if adding a hole to any such sides results
       in a complete term, then we're good */
    switch (proj_rep_id(syntax)) {
    | None => failwith("ProjectorPerform: Bad syntax")
    | Some(proj_rep_id) =>
      Ok(
        Update.update(
          pr =>
            {
              ...pr,
              syntax:
                List.map(
                  (p: Piece.t) =>
                    proj_rep_id == Piece.id(p) ? Piece.replace_id(id, p) : p,
                  syntax,
                ),
            },
          id,
          z,
        ),
      )
    }

  | SetModel(id, model) => Ok(Update.update(pr => {...pr, model}, id, z))
  | Focus(id, d) =>
    let z =
      switch (d) {
      | None =>
        /* d == None means focus by mouse click */
        jump_to_id_indicated(z, id) |> Option.value(~default=z)
      | Some(_) => z
      };
    switch (Projector.indicated(z)) {
    | Some((_, p)) =>
      let (module P) = to_module(p.kind);
      P.focus((id, d));
      Ok(z);
    | None => Error(Cant_project)
    };
  | Escape(id, d) => Ok(jump_to_side_of_id(d, z, id))
  };
};
