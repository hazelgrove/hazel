open Projector;
open ProjectorBase;
open Util.OptUtil.Syntax;

/* Updates the underlying piece of syntax for a projector */
module Update = {
  let init = (id, kind: t, syntax: syntax): option(syntax) => {
    let (module P) = to_module(kind);
    switch (P.can_project(syntax) && minimum_projection_condition(syntax)) {
    | false => None
    | true =>
      /* We set the projector id equal to the root piece id for convienence
       * including cursor-info association. We maintain this invariant
       * when we update a projector's contained syntax */
      Some([Piece.Projector({id, kind, model: P.init, syntax})])
    };
  };

  let from_selection =
      (id, kind, focus: Util.Direction.t, z: Zipper.t): option(Zipper.t) => {
    let+ content = init(id, kind, z.selection.content);
    let z = Zipper.put_selection({...z.selection, content, focus}, z);
    Zipper.unselect(z);
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
    /* NOTE: using nice_term here has unintended consequences for
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
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, _)) =>
      let id = Piece.id(piece);
      switch (Piece.is_projector(piece)) {
      | None =>
        let z =
          if (Piece.is_secondary(piece)) {
            indicated_token(z);
          } else {
            select_term(z);
          };
        switch (Util.OptUtil.and_then(Update.from_selection(id, kind, d), z)) {
        | None => Error(Cant_project)
        | Some(z) => Ok(z)
        };
      | Some(current) when current.kind == kind =>
        Ok(remove_indicated(id, d, z))
      | Some(_old_proj) =>
        //TODO(andrew): change action
        // will need to take into account the projected syntax
        Ok(remove_indicated(id, d, z))
      };
    }
  | RemoveIndicated(_id) =>
    //TODO(andrew): remove param
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, _rel)) =>
      let id = Piece.id(piece);
      Ok(remove_indicated(id, d, z));
    }
  | SetSyntax(id, syntax) =>
    /* Note we update piece id to keep in sync with projector id;
     * See intial id setting in Update.init */
    /* all secondary? fine. complete term? fine
       otherwise: check if either side has tile which is concave
       at that side. if adding a hole to any such sides results
       in a complete term, then we're good */
    let top_id =
      switch (syntax) {
      | [] => failwith("ProjectorPerform: Expected non-empty syntax")
      | [p] =>
        /* Single piece case (e.g. let) */
        Piece.id(p)
      //TODO(andrew): multi-tile non-term case e.g. `: Int`
      | [p, ..._] when List.for_all(Piece.is_secondary, syntax) =>
        Piece.id(p)
      | _ =>
        switch (Segment.root_rep_id(syntax)) {
        | exception _ => failwith("ProjectorPerform: Not complete term")
        | id => id
        }
      };
    Ok(
      Update.update(
        pr =>
          {
            ...pr,
            syntax:
              List.map(
                (p: Piece.t) =>
                  top_id == Piece.id(p) ? Piece.replace_id(id, p) : p,
                syntax,
              ),
          },
        id,
        z,
      ),
    );
  //TODO(andrew): more principled approach?
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
