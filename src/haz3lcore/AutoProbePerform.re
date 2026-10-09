/* Auto-probe placement (AutoProbe.t): picks the anchor(s) for the current
 * mode — Caret: the enclosing top-level def (+ function-sugar params), All:
 * the program root — and reconciles them onto the zipper as multi-probes.
 * Driven by Editor.calculate. */
open Util;
open Language;

/* Walk ancestors outermost-to-innermost, picking the def of the
 * enclosing Let / first component of a Seq / the bare expression at the cursor
 * (types aren't probeable); fall back to the cursor's own piece. A Test target
 * is rewritten to its body (unwrap_test) to probe the boolean condition. */
let toplevel_def_body_id = (~statics: Statics.Map.t, ~id: Id.t): option(Id.t) => {
  open Language;

  let unwrap_test = (id: Id.t): Id.t =>
    switch (Statics.Map.lookup(id, statics)) {
    | Some(
        InfoExp({
          user_term: {term: Test(body) | HintedTest(body, _), _},
          _,
        }),
      ) =>
      IdTagged.rep_id(body)
    | _ => id
    };

  let probe_for_piece = (id: Id.t): option(Id.t) =>
    switch (Statics.Map.lookup(id, statics)) {
    | Some(InfoExp({user_term: {term: Let(_, def, _), _}, _})) =>
      Some(IdTagged.rep_id(def))
    | Some(InfoExp({user_term: {term: Seq(e1, _), _}, _})) =>
      Some(IdTagged.rep_id(e1))
    | Some(InfoExp({user_term: {term: TyAlias(_), _}, _})) => None
    | Some(InfoExp({user_term, _})) => Some(IdTagged.rep_id(user_term))
    | _ => None
    };

  let find_target = (starting_id: Id.t, ancestors: list(Id.t)): option(Id.t) => {
    let len = List.length(ancestors);
    let rec walk = (idx: int): option(Id.t) =>
      if (idx < 0) {
        None;
      } else {
        let anc_id = List.nth_exn(ancestors, idx);
        let child_id =
          if (idx == 0) {
            starting_id;
          } else {
            List.nth_exn(ancestors, idx - 1);
          };
        switch (Statics.Map.lookup(anc_id, statics)) {
        | Some(InfoExp({user_term: {term: Let(_, def, body), _}, _})) =>
          if (Id.equal(child_id, IdTagged.rep_id(body))) {
            walk(idx - 1);
          } else {
            Some(IdTagged.rep_id(def));
          }
        | Some(InfoExp({user_term: {term: Seq(e1, e2), _}, _})) =>
          let e1_id = IdTagged.rep_id(e1);
          let e2_id = IdTagged.rep_id(e2);
          if (Id.equal(child_id, e1_id) || Id.equal(child_id, e2_id)) {
            walk(idx - 1);
          } else {
            Some(e1_id);
          };
        | Some(InfoExp({user_term: {term: TyAlias(_, _, body), _}, _})) =>
          if (Id.equal(child_id, IdTagged.rep_id(body))) {
            walk(idx - 1);
          } else {
            None;
          }
        | _ => Some(anc_id)
        };
      };
    walk(len - 1);
  };

  /* WORKAROUND: function-sugar reuses the surface Let's id, so it appears twice
   * in `ancestors`; the positional walk would then misread a cursor in the let
   * body as being in the def. Dedup adjacent ids. Remove once fixed in statics. */
  let rec dedup_adjacent = (ids: list(Id.t)): list(Id.t) =>
    switch (ids) {
    | []
    | [_] => ids
    | [x, y, ...rest] =>
      Id.equal(x, y)
        ? dedup_adjacent([y, ...rest])
        : [x, ...dedup_adjacent([y, ...rest])]
    };

  switch (Statics.Map.lookup(id, statics)) {
  | Some(info) =>
    let ancestors = dedup_adjacent(Info.ancestors_of(info));
    let target =
      switch (find_target(id, ancestors)) {
      | Some(_) as result => result
      | None => probe_for_piece(id)
      };
    Option.map(~f=unwrap_test, target);
  | None => None
  };
};

let clear_autoprobe =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.autoprobe_target) {
  | [] => z
  | old_ids =>
    /* Skip the cursor reset: this isn't an edit, so the stale probes render one
     * more frame; resetting now would flash them a reset color. editor_effects
     * resets once they're gone from the syntax cache. */
    List.fold_left(
      ~f=
        (z, old_id) =>
          ProbePerform.rm_multi(
            ~drill=false,
            ~reset=false,
            ~syntax,
            ~info_map,
            old_id,
            z,
          ),
      ~init=z,
      old_ids,
    )
    |> Zipper.update_refractors(_, r =>
         {
           ...r,
           autoprobe_target: [],
         }
       )
  };

/* Pick the id to base autoprobe placement on: (1) Indicated.index for the usual
 * cursor-on-tile case; (2) left-bias fallback past secondaries (keeps the probe
 * sticky after a trailing space); (3) the enclosing tile (cursor on a blank line). */
let current_toplevel_def =
    (info_map: Statics.Map.t, z: Zipper.t): option(Id.t) => {
  let try_id = id => toplevel_def_body_id(~statics=info_map, ~id);

  let from_indicated = () =>
    switch (Indicated.index(z)) {
    | None => None
    | Some(cursor_id) => try_id(cursor_id)
    };

  let from_left = () => {
    let (l_sibs, _) = ZipperBase.sibs_with_sel(z);
    /* trim right-end secondaries, then take the last piece (nearest non-secondary on the left). */
    let trimmed = Segment.trim_secondary(Right, l_sibs);
    switch (ListUtil.split_last_opt(trimmed)) {
    | Some((_, last)) => try_id(Piece.id(last))
    | None => None
    };
  };

  let from_right = () => {
    let (_, r_sibs) = ZipperBase.sibs_with_sel(z);
    let trimmed = Segment.trim_secondary(Left, r_sibs);
    switch (trimmed) {
    | [first, ..._] => try_id(Piece.id(first))
    | [] => None
    };
  };

  let from_ancestor = () =>
    switch (z.relatives.ancestors) {
    | [] => None
    | [(ancestor, _), ..._] => try_id(ancestor.id)
    };

  [from_indicated, from_right, from_left, from_ancestor]
  |> List.fold_left(
       ~f=(acc, f) => Option.is_none(acc) ? f() : acc,
       ~init=None,
     );
};

/* Program root id: the single `All`-mode anchor (expands to one probe per row).
 * Memoized on physical identity of `syntax.segment`, since Segment.skel parses
 * the whole program and update_autoprobe calls this every All-mode frame. */
let root_id_segment: ref(option(Segment.t)) = ref(None);
let root_id_result: ref(option(Id.t)) = ref(None);

let program_root_id = (syntax: CachedSyntax.t): option(Id.t) => {
  let stable =
    switch (root_id_segment^) {
    | Some(seg) => phys_equal(seg, syntax.segment)
    | None => false
    };
  if (stable) {
    root_id_result^;
  } else {
    let result =
      switch (syntax.segment) {
      | [] => None
      | seg =>
        switch (Segment.root_id(Segment.skel(seg), seg)) {
        | id => Some(id)
        | exception _ => None
        }
      };
    root_id_segment := Some(syntax.segment);
    root_id_result := result;
    result;
  };
};

/* Caret: anchor on the top-level def the cursor is in (reconstitutes on crossing
 * into another def). All: anchor on the program root (constant). Off: unreached
 * (Editor.calculate clears instead); empty anchors here for totality. */
let update_autoprobe =
    (
      ~mode: AutoProbe.t,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* drill=false so the anchor itself is multi-probed (expanded by row), not drilled into subterms. */
  let (current_anchors, add_new) =
    switch (mode) {
    | Off => ([], (z => z))
    | All =>
      switch (program_root_id(syntax)) {
      | None => ([], (z => z))
      | Some(root_id) => (
          [root_id],
          (
            z =>
              ProbePerform.add_multi(
                root_id,
                ~drill=false,
                ~set_pending_cursor=ProbePerform.auto_focus(z),
                ~syntax,
                ~info_map,
                z,
              )
          ),
        )
      }
    | Caret =>
      let current_def = current_toplevel_def(info_map, z);
      /* Function-sugar: also anchor the param pattern so params are probed on the header line(s). */
      let current_param =
        switch (current_def) {
        | Some(def_id) =>
          ProbePerform.function_sugar_param_anchor(info_map, def_id)
        | None => None
        };
      let anchors =
        Option.to_list(current_def) @ Option.to_list(current_param);
      /* def body carries cursor following (gated on auto_focus); the param anchor is added without it, keeping focus on the body's first sample. */
      let add = z =>
        switch (current_def) {
        | None => z
        | Some(def_id) =>
          let z =
            ProbePerform.add_multi(
              def_id,
              ~drill=false,
              ~set_pending_cursor=ProbePerform.auto_focus(z),
              ~syntax,
              ~info_map,
              z,
            );
          switch (current_param) {
          | Some(param_id) =>
            ProbePerform.add_multi(
              param_id,
              ~drill=false,
              ~set_pending_cursor=false,
              ~syntax,
              ~info_map,
              z,
            )
          | None => z
          };
        };
      (anchors, add);
    };
  let prev_anchors = z.refractors.autoprobe_target;
  /* Anchors can be removed from multis.ids while autoprobe_target still lists
     them (RemoveAll, or Cmd+E on a term that is an anchor); without this check
     the same-anchors short-circuit would be a permanent no-op. Self-heal. */
  let anchors_intact =
    List.for_all(
      ~f=id => Id.Map.mem(id, z.refractors.multis.ids),
      current_anchors,
    );
  if (List.equal(Id.equal, current_anchors, prev_anchors) && anchors_intact) {
    z;
  } else {
    /* drill=false to match how they were added. */
    let z =
      List.fold_left(
        ~f=
          (z, old_id) =>
            ProbePerform.rm_multi(
              ~drill=false,
              ~syntax,
              ~info_map,
              old_id,
              z,
            ),
        ~init=z,
        prev_anchors,
      );

    /* Regenerate ephemerals: rm_multi(~drill=false) drops only the anchor id, not its expanded ephemerals; without this they'd persist a frame when transitioning to no-def. */
    let z = ProbePerform.add_ids_from_multi_term(~syntax, ~info_map, z);

    let z = add_new(z);
    Zipper.update_refractors(z, r =>
      {
        ...r,
        autoprobe_target: current_anchors,
      }
    );
  };
};
