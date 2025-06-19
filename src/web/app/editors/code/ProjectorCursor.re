open Util;
open OptUtil.Syntax;
open Haz3lcorep;

/* This is the logic for determining contextual editor actions involving
 * projectors, including projecting and unprojecting syntax, as used in
 * ninjakeys and the projectors panel. */

/* Determines what term to target for projection. This logic
 * should be kept in sync with the projector add/remove logic
 * in ProjectorPerform */
let target_segment =
    (
      selection: option(Segment.t('a)),
      indicated_piece: option(Piece.t('a)),
    )
    : option(Segment.t('a)) => {
  //TODO(andrew): if targeted seg is projector, need to unproject
  // to figure out if syntax can be reprojected
  let* seg =
    switch (selection) {
    | None => None
    | Some([]) =>
      switch (indicated_piece) {
      | Some(Tile(_) as p)
      | Some(Projector(_) as p) => Some([p])
      | Some(Grout(_))
      | Some(Secondary(_))
      | None => None
      }
    | Some(seg) => Some(seg)
    };
  let* () = Segment.deep_tile_complete(seg) ? Some() : None;
  let* () = Segment.is_padded(seg) ? None : Some();
  let* skel =
    switch (Segment.skel(seg)) {
    | exception _ => None
    | skel => Some(skel)
    };
  let* () =
    switch (Segment.sort_of(skel, seg)) {
    | Exp
    | Pat
    | Typ
    | TPat => Some()
    | Rul
    | Any => None
    };
  Some(seg);
};

// TODO(matt|andrew): make this work more generally for different sorts
let target_term = (make_term_prj, seg: Segment.t('a)) =>
  seg
  |> Zipper.unzip
  |> Editor.Model.mk
  |> Editor.Update.make_term(~make_term_prj, ~sort=Exp)
  |> snd
  |> Calc.get_value;

let target_ed =
    (seg: Segment.t('a), ())
    : option(Editor.Model.t(ProjectorCore.Kind.t, 'p_m, 'p_a)) =>
  switch (seg) {
  | []
  | [Projector(_)] => None
  | s => Some(s |> Zipper.unzip |> Editor.Model.mk)
  };

let is_applicable =
    (~selection, ~indicated_piece, ~make_term_prj, ~mk_projector, kind) => {
  /* Is a projector of `kind` applicable to the target term? */
  let* target_seg = target_segment(selection, indicated_piece);
  let term = target_term(make_term_prj, target_seg);
  let ed = target_ed(target_seg);
  let+ _ = mk_projector(kind, term, ed);
  kind;
};

/* If the current indicated term is a projector, return its kind */
let indicated_kind =
    (
      indicated_piece: option(Piece.t('a)),
      get_kind: 'a => ProjectorCore.Kind.t,
    )
    : option(ProjectorCore.Kind.t) => {
  switch (indicated_piece) {
  | Some(Projector(p)) => Some(get_kind(p.model))
  | _ => None
  };
};

/* The string names of all projectors applicable to the currently
 * indicated syntax, with the currently applied projection (if any)
 * lifted to the top of the list */
let lift_active_projector =
    (indicated_kind, applicable_projectors: list(ProjectorCore.Kind.t))
    : list(ProjectorCore.Kind.t) =>
  switch (indicated_kind) {
  | Some(kind) => ListUtil.lift(kind, applicable_projectors)
  | None => applicable_projectors
  };

let mk =
    (
      ~indicated_piece,
      ~selection,
      ~read_only,
      ~get_kind,
      ~mk_projector,
      ~make_term_prj,
      ~inject:
         Editor.Update.t(ProjectorCore.Kind.t, 'p_m, 'p_a) =>
         Ui_effect.t(unit),
    ) => {
  let indicated_kind = indicated_kind(indicated_piece, get_kind);
  let applicable_projectors =
    if (read_only) {
      [];
    } else {
      ProjectorCore.Kind.projectors
      |> List.filter_map(
           is_applicable(
             ~selection,
             ~indicated_piece,
             ~make_term_prj,
             ~mk_projector,
           ),
         )
      |> lift_active_projector(indicated_kind);
    };
  let mk_action = kind =>
    ContextualAction.mk(
      ~section="Projection",
      ProjectorCore.Kind.name(kind),
      ~hotkey=?ProjectorKind.shortcut_of(kind),
      inject(Project(SetIndicated(Specific(kind)))),
    );
  let unproject =
    ContextualAction.mk(
      ~section="Projection",
      "Unproject",
      inject(Project(RemoveIndicated)),
    );
  List.map(mk_action, applicable_projectors)
  @ (indicated_kind != None ? [unproject] : []);
};
