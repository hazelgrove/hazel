open Language;

/* PROMISE RENDER (stage 1, projection + parity only).

   The display as a PROJECTION of the E-side promise artifact rather
   than a reconstruction. mk has the SAME output type as
   DisplayFork.mk and shares its assist stream (extend_t2), its
   view-policy layer (ghost_selection: caret-relative suppress/slide),
   its splice ordering (splice_sort) and its well-formedness gate
   (tiles_well_formed) — one home, so the two implementations cannot
   drift on those axes. The ONE difference stage 1 introduces:

   SYSTEM material comes from the KEPT completed_seg (for_editor's
   E-side artifact, frame-fresh) instead of being reconstructed by
   ghost_pieces. for_editor now returns, per engine insertion, the
   run's REAL pieces from completed_seg (CanonicalCompletion.for_editor'
   / projection_for); PromiseRender splices those real pieces at the
   insertion's splice ref, marks them as ghost material, and applies
   the padding oracle ONCE — exactly the current fork's tail.

   STAGE-1 SCOPE (per spec):
   - T1 obligation commas and T2 lookahead tails are NOT in
     completed_seg (for_editor runs syntactic completion only, no type
     reification). Their material still comes from
     TypeObligations.ghost_pieces — the documented stage-1 fallback.
     Stage 2 reifies T1 into the artifact and ports T2, at which point
     projection_for covers them too.
   - Witnesses render as the REAL completed token (Tile.shard_of),
     marked. Sub-token styling (typed prefix normal, remainder ghost)
     is stage 2/3; for parity the completed token renders in full.
     This is a parity waiver (witness text differs from the current
     fork's comment-remainder) — enumerated in Test_CompletionDisplay.

   Total fail-open like the current fork: any exception shows the raw
   segment with no assist. */

let mk_inner =
    (
      ~info_map: Statics.Map.t,
      ~obligations: list(TypeObligations.t),
      ~armed: bool,
      z: Zipper.t,
    )
    : DisplayFork.t => {
  let raw = Zipper.unselect_and_zip(z);
  /* run for_editor ONCE and KEEP the artifact (completed_seg + run
     pieces) — the assist stream reads this same result instead of
     recomputing it (the E-side completed_seg is no longer discarded) */
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let projection = CanonicalCompletion.for_editor'(seg);
  let assist =
    TypeObligations.assist_stream_with(
      ~result=projection.result,
      z,
      ~info_map,
      obligations,
    )
    |> DisplayFork.extend_t2(~info_map, ~armed, z);
  /* view policy: the same caret-relative selection the current fork
     uses — pre-caret runs suppressed, slide applied, multi-ghost */
  let selected = DisplayFork.ghost_selection(~armed, z, assist);
  /* material per selected ghost. A PURE-ENGINE run (syntactic
     completion only — closers, keywords; every delimiter carries an
     of_shard and none is a witness or T1 comma) is PROJECTED from
     the kept completed_seg: its real pieces. That is the stage-1
     projection proper — the display segment's system leaves are the
     real E-side artifact pieces, spliced at the run's true ref.

     The remaining classes fall back to ghost_pieces reconstruction,
     each a documented stage-1 boundary (spec MIGRATION step 1):
     - WITNESSES: completed_seg holds the FULL token; sub-token
       styling (typed prefix + ghost remainder) is stage 2/3. Until
       Code.re splits the span, the reconstruction's comment
       remainder is the faithful render.
     - T1 obligation commas / T2 lookahead tails: not in
       completed_seg (for_editor runs no type reification). Stage 2
       reifies T1 into the artifact and ports T2. */
  let pure_engine = (ins: CanonicalCompletion.insertion): bool =>
    ins.delimiters != []
    && List.for_all(
         (d: CanonicalCompletion.delimiter_info) =>
           d.of_shard != None && d.typed_len == None,
         ins.delimiters,
       );
  /* trim a LEADING synthesized hole from a projected run: `let`'s
     empty pattern slot completion adds a grout BEFORE `=`, a position
     hole the reconstruction path treats as the (user-side) pattern
     hole, not run material. Trailing/interior holes are real promise
     material (a `fun x -> ?` body hole) and stay. Only drop when a
     real shard follows, so a pure-grout run is untouched. */
  let trim_leading_grout = (pieces: Segment.t): Segment.t =>
    switch (pieces) {
    | [Grout(_), Tile(_), ..._] as ps =>
      switch (ps) {
      | [_, ...tl] => tl
      | [] => ps
      }
    | ps => ps
    };
  let ghosts =
    selected
    |> List.filter_map(((orig, ins: CanonicalCompletion.insertion)) => {
         /* project via the PRE-slide `orig` (its physical identity is
            the run-pieces key); splice via the slid `ins` */
         let pieces =
           if (pure_engine(orig)) {
             switch (
               CanonicalCompletion.projection_for(projection.run_pieces, orig)
             ) {
             | Some(real) => Some(trim_leading_grout(real))
             | None => TypeObligations.ghost_pieces(z, ins)
             };
           } else {
             TypeObligations.ghost_pieces(z, ins);
           };
         pieces |> Option.map(pieces => (orig, ins, pieces));
       });
  let forked = () => {
    let (segment, ghost_marks) =
      DisplayFork.splice_sort(raw, ghosts)
      |> List.fold_left(
           ((seg, marks), (_, ins, pieces)) =>
             switch (CanonicalCompletion.splice_ghost(seg, ~ins, ~pieces)) {
             | Some((seg, more)) => (seg, marks @ more)
             | None => (seg, marks)
             },
           (raw, []),
         );
    let segment =
      ghost_marks == []
        ? segment
        : segment
          |> CanonicalCompletion.normalize_display
          |> DisplayFork.restore_ghost_holes(
               ~marks=ghost_marks,
               ~pre=segment,
             )
          |> CanonicalCompletion.finish_display(
               ~marks=ghost_marks,
               ~raw,
               ~caret_after=CanonicalCompletion.caret_left_atom(z),
             );
    if (ghost_marks != [] && !DisplayFork.tiles_well_formed(segment)) {
      failwith("PromiseRender: malformed splice");
    };
    (segment, ghost_marks, MakeTerm.go(segment));
  };
  let (segment, ghost_marks, parsed) =
    switch (forked()) {
    | r => r
    | exception _ => (raw, [], MakeTerm.go(raw))
    };
  DisplayFork.{
    segment,
    ghost_marks,
    assist,
    ghosted: ghost_marks == [] ? [] : List.map(((o, _, _)) => o, ghosts),
    parsed,
  };
};

/* same contract as DisplayFork.mk: never crash the editor */
let mk =
    (
      ~info_map: Statics.Map.t,
      ~obligations: list(TypeObligations.t),
      ~armed: bool,
      z: Zipper.t,
    )
    : DisplayFork.t =>
  switch (mk_inner(~info_map, ~obligations, ~armed, z)) {
  | fork => fork
  | exception _ => DisplayFork.plain(z)
  };
