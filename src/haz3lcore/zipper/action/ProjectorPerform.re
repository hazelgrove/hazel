open Projector;
open ProjectorBase;

/* Updates the underlying piece of syntax for a projector */
module Update = {
  let init = (id, kind: t, syntax: syntax): syntax => {
    /* We set the projector id equal to the root piece id for convienence
     * including cursor-info association. We maintain this invariant
     * when we update a projector's contained syntax */
    let (module P) = to_module(kind);
    switch (P.can_project(syntax) && minimum_projection_condition(syntax)) {
    | false => syntax
    | true => [Projector({id, kind, model: P.init, syntax})]
    };
  };

  let add_projector = (kind: Base.kind, id: Id.t, seg: syntax) => {
    print_endline("term_partitions add_projector");
    switch (Segment.term_partitions(seg, id)) {
    | None => seg
    | Some((l, m, r)) => l @ init(id, kind, m) @ r
    };
  };

  // subseg => piece
  // let add = (k: Base.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
  //   ZipperBase.MapSegment.fast_local(add_projector(k, id), id, z);

  let add =
      (indicated_token, select_term, k: Base.kind, id: Id.t, z: ZipperBase.t)
      : ZipperBase.t => {
    //move_out_of_piece(d, rel, z)
    switch (Indicated.for_index(z)) {
    | None => z
    | Some((piece, _d, _rel)) when Piece.is_secondary(piece) =>
      //TODO(andrew): reinstate direction
      let z = indicated_token(z) |> Option.value(~default=z); //TODO(andrew): errors
      let content = init(id, k, z.selection.content);
      let z = {
        ...z,
        selection: {
          ...z.selection,
          content,
        },
      };
      Zipper.unselect(z);
    | Some((_piece, _d, _rel)) =>
      //TODO(andrew): reinstate direction
      let z = select_term(z) |> Option.value(~default=z); //TODO(andrew): errors
      let content = init(id, k, z.selection.content);
      let z = {
        ...z,
        selection: {
          ...z.selection,
          content,
        },
      };
      Zipper.unselect(z);
    };
  };

  let remove_p_if = (id: Id.t, piece: Piece.t): Segment.t =>
    switch (piece) {
    | Projector(pr) when pr.id == id => pr.syntax
    | x => [x]
    };

  let remove_s_if = (id: Id.t, syntax: syntax) =>
    List.concat(List.map(p => remove_p_if(id, p), syntax));

  // TODO(andrew): piece => subseg
  let remove_if = (id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapSegment.fast_local(remove_s_if(id), id, z);

  let remove_p = (piece: Piece.t): Segment.t =>
    switch (piece) {
    | Projector(pr) => pr.syntax
    | x => [x]
    };

  let remove_s = (seg: syntax) =>
    List.concat(List.map(p => remove_p(p), seg));

  let remove_all = (z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapSegment.go(remove_s, z);

  // let is_projector_of = (id: Id.t, seg: Segment.t): bool =>
  //   List.exists(
  //     (p: Piece.t) =>
  //       switch (p) {
  //       | Projector(pr) => pr.id == id
  //       | _ => false
  //       },
  //     seg,
  //   );

  // let add_or_remove_projector = (kind: Base.kind, id: Id.t, seg: Segment.t) => {
  //   is_projector_of(id, seg)
  //     ? remove_projector(id, seg) : add_projector(kind, id, seg);
  // };

  // let add_or_remove = (k: Base.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
  //   ZipperBase.MapSegment.fast_local(add_or_remove_projector(k, id), id, z);

  let update_piece =
      (f: Base.projector => Base.projector, id: Id.t, piece: Piece.t) =>
    switch (piece) {
    | Projector(pr) when pr.id == id => Base.Projector(f(pr))
    | x => x
    };

  // piece => piece
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
    //TODO(andrew): unhardcode this direction Left
    //TODO(andrew): cleanup or clairfy
    print_endline("AAA");
    let z_init =
      switch (
        do_until(
          move_primary(Zipper.ByToken, d),
          (p: Piece.t) => Piece.id(p) == id,
          z,
        )
      ) {
      | None =>
        print_endline("BBB");
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
          print_endline("QQQ");
          z
          |> move_primary(
               Zipper.ByToken,
               d == Right ? Util.Direction.Right : Left,
             )
          |> Option.value(~default=z);
        };
      | Some(z) =>
        print_endline("CCC");
        move_primary(
          Zipper.ByToken,
          d == Right ? Util.Direction.Left : Right,
          z,
        )
        |> Option.value(~default=z);
      };
    if (Indicated.index(z_init) == Some(id)) {
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
  | SetIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, _d, _rel)) =>
      //TODO(andrew): reinstate direction
      Ok(Update.add(indicated_token, select_term, p, Piece.id(piece), z))
    }
  | ToggleIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, _rel)) =>
      let id = Piece.id(piece);
      /* new strategy:
         1. call select term
         2. add_or_remove only acts on the selection,
            which will be only the projector piece (if projected)
            or a complete term segment (if not)
            We'll have guaranteed locality, so no need to map thru
            the zipper for this one.
          */
      Ok(
        if (Piece.is_projector(piece) != None) {
          print_endline("ZZZ");
          let x = remove_indicated(id, d, z);
          print_endline("WWW");
          x;
        } else {
          Update.add(indicated_token, select_term, p, id, z);
        },
      );
    }
  | RemoveIndicated(_id) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, _rel)) =>
      let id = Piece.id(piece);
      Ok(remove_indicated(id, d, z));
    }
  | SetSyntax(id, syntax) =>
    /* Note we update piece id to keep in sync with projector id;
     * See intial id setting in Update.init */
    //TODO(andrew): verify new replacement logic works
    Ok(
      Update.update(
        pr =>
          {
            ...pr,
            syntax:
              List.map(
                (p: Piece.t) =>
                  pr.id == Piece.id(p) ? Piece.replace_id(id, p) : p,
                syntax,
              ),
          },
        id,
        z,
      ),
    )
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
