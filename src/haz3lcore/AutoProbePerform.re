/* Auto-probe placement (AutoProbe.t): picks the anchor(s) for the current
 * mode — Caret: the enclosing top-level def (+ function-sugar params), All:
 * the program root — and reconciles them onto the zipper as multi-probes.
 * Driven by Editor.calculate. */
open Util;
open Language;

/* Walk ancestors outermost-to-innermost, picking the def of the
 * enclosing Let / first component of a Seq / the bare expression at the cursor
 * (types aren't probeable); fall back to the cursor's own piece. Modules are
 * walked into, so a member's def is picked as a top-level one is. A Test
 * target is rewritten to its body (unwrap_test) to probe the boolean
 * condition. */
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

  /* a definition with items of its own, which the walk goes into: a
     module literal, or a module statement's let chain */
  let holds_items = (def: Exp.t): bool =>
    switch (def.term) {
    | Module(_) => true
    | Let(_)
    | TyAlias(_)
    | ModuleExp(_)
    | Seq(_) =>
      switch (Statics.Map.lookup(IdTagged.rep_id(def), statics)) {
      | Some(InfoExp({cls: Mod(ModExp), _})) => true
      | _ => false
      }
    | _ => false
    };

  let find_target = (starting_id: Id.t, ancestors: list(Id.t)): option(Id.t) => {
    let ancestors = Array.of_list(ancestors);
    let rec walk = (idx: int): option(Id.t) =>
      if (idx < 0) {
        None;
      } else {
        let anc_id = ancestors[idx];
        let child_id = idx == 0 ? starting_id : ancestors[idx - 1];
        /* the item after this one, or the items a definition holds */
        let into = (def: Exp.t, body: Exp.t) =>
          Id.equal(child_id, IdTagged.rep_id(body))
          || Id.equal(child_id, IdTagged.rep_id(def))
          && holds_items(def);
        switch (Statics.Map.lookup(anc_id, statics)) {
        | Some(InfoExp({user_term: {term: Let(_, def, body), _}, _})) =>
          into(def, body) ? walk(idx - 1) : Some(IdTagged.rep_id(def))
        | Some(InfoExp({user_term: {term: ModuleExp(_, def, body), _}, _}))
            when into(def, body) =>
          walk(idx - 1)
        | Some(InfoExp({user_term: {term: Module(_), _}, _})) =>
          walk(idx - 1)
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
    walk(Array.length(ancestors) - 1);
  };

  switch (Statics.Map.lookup(id, statics)) {
  | Some(info) =>
    /* all of them, a function definition's id once */
    let ancestors = Info.ancestors(~info_map=statics, info);
    let target =
      switch (find_target(id, ancestors)) {
      | Some(_) as result => result
      | None => probe_for_piece(id)
      };
    Option.map(unwrap_test, target);
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
      (z, old_id) =>
        ProbePerform.rm_multi(
          ~drill=false,
          ~reset=false,
          ~syntax,
          ~info_map,
          old_id,
          z,
        ),
      z,
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
    | [(ancestor, _), ..._] => try_id(Ancestor.id(ancestor))
    };

  [from_indicated, from_right, from_left, from_ancestor]
  |> List.fold_left((acc, f) => acc == None ? f() : acc, None);
};

/* Program root id: the single `All`-mode anchor (expands to one probe per row).
 * Memoized on physical identity of `CachedSyntax.segment(syntax)`, since Segment.skel parses
 * the whole program and update_autoprobe calls this every All-mode frame. */
let root_id_segment: ref(option(Segment.t)) = ref(None);
let root_id_result: ref(option(Id.t)) = ref(None);

let program_root_id = (syntax: CachedSyntax.t): option(Id.t) => {
  let stable =
    switch (root_id_segment^) {
    | Some(seg) => seg === CachedSyntax.segment(syntax)
    | None => false
    };
  if (stable) {
    root_id_result^;
  } else {
    let result =
      switch (CachedSyntax.segment(syntax)) {
      | [] => None
      | seg =>
        switch (Segment.root_id(Segment.skel(seg), seg)) {
        | id => Some(id)
        | exception _ => None
        }
      };
    root_id_segment := Some(CachedSyntax.segment(syntax));
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
      id => Id.Map.mem(id, z.refractors.multis.ids),
      current_anchors,
    );
  if (List.equal(Id.equal, current_anchors, prev_anchors) && anchors_intact) {
    z;
  } else {
    /* drill=false to match how they were added. */
    let z =
      List.fold_left(
        (z, old_id) =>
          ProbePerform.rm_multi(~drill=false, ~syntax, ~info_map, old_id, z),
        z,
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

let program_exp = (syntax: CachedSyntax.t): option(Exp.t) =>
  switch (
    Option.bind(program_root_id(syntax), id =>
      Id.Map.find_opt(id, syntax.terms)
    )
  ) {
  | Some(Exp(e)) => Some(e)
  | _ => None
  };

/* the rest of the program's top level after a let, type alias, module
   let, use, theorem or `;` statement */
let top_level_rest = (e: Exp.t): option(Exp.t) =>
  switch (e.term) {
  | Let(_, _, body)
  | TyAlias(_, _, body)
  | ModuleExp(_, _, body)
  | Use(_, body)
  | Theorem(_, _, body)
  | Seq(_, body) => Some(body)
  | _ => None
  };

/* the program's last expression (the outline's ⇒ row) */
let tail_id = (syntax: CachedSyntax.t): option(Id.t) => {
  let rec tail_of = (e: Exp.t): Exp.t =>
    switch (top_level_rest(e)) {
    | Some(body) => tail_of(body)
    | None => e
    };
  Option.map(e => Exp.rep_id(tail_of(e)), program_exp(syntax));
};

/* the ⇓ toggle: one probe on the last expression, an open drawer with no
   marks on the code. it follows the expression as it changes; a previous
   anchor the auto-probe still holds goes back to a plain probe */
let update_tail =
    (
      ~on: bool,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  let target = on ? tail_id(syntax) : None;
  let prev = z.refractors.tail_target;
  /* its entry keeps the drawer's own state (a menu, a rich view) */
  let placed =
    switch (target) {
    | Some(id) =>
      Id.Map.mem(id, z.refractors.multis.ephemerals)
      || List.mem_assoc(id, z.refractors.manuals)
    | None => true
    };
  if (target == prev && placed) {
    z;
  } else {
    let z =
      Zipper.update_refractors(z, r =>
        {
          ...r,
          tail_target: target,
        }
      )
      |> ProbePerform.add_ids_from_multi_term(~syntax, ~info_map);
    Zipper.update_ephemerals(
      eph => {
        let eph =
          switch (prev) {
          | Some(old) when Some(old) != target && Id.Map.mem(old, eph) =>
            Id.Map.add(old, Refractors.mk_entry(Probe), eph)
          | _ => eph
          };
        switch (target) {
        | Some(id) when Id.Map.mem(id, eph) =>
          Id.Map.add(
            id,
            Refractors.mk_entry(~model=ProbeProj.tail_model, Probe),
            eph,
          )
        | _ => eph
        };
      },
      z,
    );
  };
};

/* the theorems along a program's top level, in order */
let theorems_of_exp = (e: Exp.t): list(Id.t) => {
  let rec go = (acc, e: Exp.t) => {
    let acc =
      switch (e.term) {
      | Theorem(_) => [Exp.rep_id(e), ...acc]
      | _ => acc
      };
    switch (top_level_rest(e)) {
    | Some(body) => go(acc, body)
    | None => List.rev(acc)
    };
  };
  go([], e);
};

let theorem_ids = (syntax: CachedSyntax.t): list(Id.t) =>
  Option.fold(~none=[], ~some=theorems_of_exp, program_exp(syntax));

/* where an editor shows proofs: none, under each of its theorems, or
   (a theorem's cell, which holds its statement) under its root */
type proofs =
  | NoProofs
  | Theorems
  | ProofOf(Id.t);

/* a proof drawer per theorem; an entry stays while its theorem does */
let update_proofs =
    (~proofs: proofs, ~syntax: CachedSyntax.t, z: Zipper.t): Zipper.t => {
  /* (anchor, model) */
  let wanted =
    switch (proofs) {
    | NoProofs => []
    | Theorems =>
      List.map(id => (id, ProofProj.model_string()), theorem_ids(syntax))
    | ProofOf(thm) =>
      switch (program_root_id(syntax)) {
      | Some(root) => [(root, ProofProj.model_string(~theorem=thm, ()))]
      | None => []
      }
    };
  let proofs =
    List.fold_left(
      (m, (id, model)) =>
        Id.Map.add(id, Refractors.mk_entry(~model, Proof), m),
      Id.Map.empty,
      wanted,
    );
  Id.Map.equal(
    (a: Refractors.entry, b: Refractors.entry) => a.model == b.model,
    proofs,
    z.refractors.proofs,
  )
    ? z
    : Zipper.update_refractors(z, r =>
        {
          ...r,
          proofs,
        }
      );
};
