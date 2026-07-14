open Language;

/* THE display fork: everything between a zipper and the displayed
   segment lives here, and ONLY here. The live editor (CachedSyntax)
   and the test harness (Test_CompletionDisplay) both call `mk`, so
   what the tests pin is what the user sees — the pipelines cannot
   drift. */

type t = {
  segment: Segment.t,
  /* (id, shard) marks of display-only ghost pieces spliced into
     `segment` — the zipper never contains them */
  ghost_marks: list((Id.t, option(int))),
  /* THE assist stream (A1 single source): chips, the inline ghost,
     and Tab all read this one list */
  assist: list(CanonicalCompletion.insertion),
  /* the insertions actually ghosted this frame (physical members of
     `assist`) — chip suppression matches against exactly these */
  ghosted: list(CanonicalCompletion.insertion),
  parsed: MakeTerm.t,
};

/* normalize_display's regrout DELETES a ghost hole the parser deems
   unnecessary: display comments are invisible to nib logic, so the
   promised hole in `st ⟨(⟩ ? ⟨)⟩, ?, ?)` sits between an operand and
   a comma and gets dropped. The promise still owes it — restore any
   ghost-marked grout that normalization removed, right after its
   pre-normalization neighbor. */
let restore_ghost_holes =
    (~marks: list((Id.t, option(int))), ~pre: Segment.t, post: Segment.t)
    : Segment.t => {
  let marked = (id: Id.t) =>
    List.exists(
      ((mid, sh): (Id.t, option(int))) => sh == None && Id.equal(mid, id),
      marks,
    );
  let rec ids = (acc, sg: Segment.t) =>
    List.fold_left(
      (acc, p: Piece.t) =>
        switch (p) {
        | Tile(t) => List.fold_left(ids, [t.id, ...acc], t.children)
        | p => [Piece.id(p), ...acc]
        },
      acc,
      sg,
    );
  let post_ids = ids([], post);
  let present = (id: Id.t) => List.exists(Id.equal(id), post_ids);
  /* (preceding sibling id, grout) for each dropped marked hole whose
     neighbor is a display comment — where a real delimiter precedes,
     regrout's verdict is structural truth and stands */
  let is_comment = (p: Piece.t) =>
    switch (p) {
    | Secondary({content: Comment(_), _}) => true
    | _ => false
    };
  let rec dropped = (acc, prev: option(Piece.t), sg: Segment.t) =>
    switch (sg) {
    | [] => acc
    | [p, ...tl] =>
      let acc =
        switch (p, prev) {
        | (Piece.Grout(g), Some(prev))
            when
              marked(g.id)
              && !present(g.id)
              && is_comment(prev)
              && present(Piece.id(prev)) => [
            (Piece.id(prev), g),
            ...acc,
          ]
        | (Piece.Tile(t), _) =>
          List.fold_left((acc, c) => dropped(acc, None, c), acc, t.children)
        | _ => acc
        };
      dropped(acc, Some(p), tl);
    };
  let rec insert_after = (sg: Segment.t, pid: Id.t, g: Grout.t): Segment.t =>
    switch (sg) {
    | [] => []
    | [p, ...tl] =>
      Id.equal(Piece.id(p), pid)
        ? [p, Grout(g), ...tl]
        : [
          switch (p) {
          | Tile(t) =>
            Piece.Tile({
              ...t,
              children: List.map(c => insert_after(c, pid, g), t.children),
            })
          | p => p
          },
          ...insert_after(tl, pid, g),
        ]
    };
  dropped([], None, pre)
  |> List.fold_left((sg, (pid, g)) => insert_after(sg, pid, g), post);
};

/* degenerate fork: no assist machinery (settings off, init paths) */
let plain = (z: Zipper.t): t => {
  let segment = Zipper.unselect_and_zip(z);
  {
    segment,
    ghost_marks: [],
    assist: [],
    ghosted: [],
    parsed: MakeTerm.go(segment),
  };
};

let mk_inner =
    (
      ~info_map: Statics.Map.t,
      ~obligations: list(TypeObligations.t),
      ~armed: bool,
      z: Zipper.t,
    )
    : t => {
  let assist = TypeObligations.assist_stream(z, ~info_map, obligations);
  /* T2: the top TyDi suggestion joins the SAME stream, shaped as a
     witness insertion at the caret's left token — one display path
     (ghost splice), one acceptance path (Tab via tab_text). At most
     one witness per anchor: an engine witness (earlier in the
     stream) IS the recognition of that token, so T2 defers to it.
     Armed-only: unarmed frames carry no T2, so Tab falls through
     exactly as it did when the retired buffer was cleared. */
  let assist =
    if (armed) {
      let engine_witness_at = (id: Id.t) =>
        List.exists(
          (ins: CanonicalCompletion.insertion) =>
            CanonicalCompletion.is_pure_witness(ins)
            && Id.equal(ins.adjacent_id, id),
          assist,
        );
      switch (TyDi.anchor_to_left(z)) {
      | Some((anchor_id, _)) when !engine_witness_at(anchor_id) =>
        switch (
          TyDi.suggestion(~ci=Indicated.ci_for_completion(z, info_map), z)
        ) {
        | Some((head, typed_len, tail)) =>
          /* T1 owns separator commas at a deficient site: when an
             insertion at this anchor already promises them, the
             lookahead's trailing commas would double-count — drop
             them (the ap part of the tail stays) */
          let t1_commas_here =
            List.exists(
              (ins: CanonicalCompletion.insertion) =>
                Id.equal(ins.adjacent_id, anchor_id)
                && ins.side == Util.Direction.Right
                && List.exists(
                     (d: CanonicalCompletion.delimiter_info) =>
                       d.text == ","
                       && d.of_shard == None
                       && d.typed_len == None,
                     ins.delimiters,
                   ),
              assist,
            );
          let rec drop_comma_suffix =
                  (ds: list(TyDiSuggestion.tail_delim))
                  : list(TyDiSuggestion.tail_delim) =>
            switch (ds) {
            | [] => []
            | [d, ...rest] =>
              switch (drop_comma_suffix(rest)) {
              | [] when d.text == "," => []
              | rest => [d, ...rest]
              }
            };
          let tail = t1_commas_here ? drop_comma_suffix(tail) : tail;
          /* tail hole-BEFORE flags become hole-AFTER (needs_hole) on
             the preceding delimiter — ghost_pieces' convention */
          let hole_after = (i: int) =>
            switch (List.nth_opt(tail, i + 1)) {
            | Some(d: TyDiSuggestion.tail_delim) => d.hole_before
            | None => false
            };
          assist
          @ [
            CanonicalCompletion.{
              adjacent_id: anchor_id,
              side: Util.Direction.Right,
              splice: Some((anchor_id, None, Util.Direction.Right)),
              delimiters: [
                {
                  text: head,
                  needs_hole: hole_after(-1),
                  typed_len: Some(typed_len),
                  of_shard: None,
                },
                ...List.mapi(
                     (i, d: TyDiSuggestion.tail_delim) =>
                       CanonicalCompletion.{
                         text: d.text,
                         needs_hole: hole_after(i),
                         typed_len: None,
                         of_shard: None,
                       },
                     tail,
                   ),
              ],
            },
          ];
        | None => assist
        }
      | _ => assist
      };
    } else {
      assist;
    };
  /* inline chip ghost: when the caret sits in a chip's zone while
     ARMED (an edit arms, any other action disarms — movement never
     conjures), the chip's pending content splices into the display
     at its anchor. The splice_precedes_caret guard runs AFTER the
     slide — a ghost hugging the caret through whitespace is
     at-caret, not pre-caret. */
  /* MULTI-GHOST: every insertion whose zone holds the caret ghosts
     (a linebreak can split one merged promise into several
     insertions all valid at the caret — one used to ghost and the
     rest fell back to chips). Each is slid/guarded independently;
     each splices at its own ref. `ghosted` keeps the ORIGINAL
     (unslid) insertions for suppression identity. */
  let ghosts =
    if (armed) {
      /* at most ONE witness ghost per anchor id (stream order wins:
         engine witnesses precede T2) */
      let zone =
        CanonicalCompletion.chip_zone_all(z, assist)
        |> List.fold_left(
             (acc, ins: CanonicalCompletion.insertion) =>
               CanonicalCompletion.is_pure_witness(ins)
               && List.exists(
                    (w: CanonicalCompletion.insertion) =>
                      CanonicalCompletion.is_pure_witness(w)
                      && Id.equal(w.adjacent_id, ins.adjacent_id),
                    acc,
                  )
                 ? acc : acc @ [ins],
             [],
           );
      zone
      |> List.filter_map(orig => {
           let ins = CanonicalCompletion.slide_to_caret(z, orig);
           CanonicalCompletion.splice_precedes_caret(z, ins)
             ? None
             : TypeObligations.ghost_pieces(z, ins)
               |> Option.map(pieces => (orig, ins, pieces));
         });
    } else {
      [];
    };
  let raw = Zipper.unselect_and_zip(z);
  /* FAIL OPEN around the WHOLE fork, not just the parse: the fork is
     display-only, so ANY exception in splice/normalize/pads (e.g. a
     shards/children mismatch the fuzzer found via `case fun |`)
     means no ghost this frame — never a crash */
  let forked = () => {
    /* splice in DESCENDING (slid ref, original ref) order: each
       splice inserts directly after its ref, so the LAST-spliced
       lands closest — later refs must go first, and same-slid-ref
       ties (several insertions slid to the caret) resolve by
       ORIGINAL material order (the `_ ¦` arm case: `=>` slid across
       the typed space must land before `end in`) */
    let rank = CanonicalCompletion.rank_map(raw);
    let rank_of = (ins: CanonicalCompletion.insertion) =>
      switch (ins.splice) {
      | Some((id, sh, _)) =>
        switch (
          Hashtbl.find_opt(
            rank,
            (
              id,
              switch (sh) {
              | Some(i) => i
              | None => (-1)
              },
            ),
          )
        ) {
        | Some(r) => r
        | None => max_int
        }
      | None => max_int
      };
    let key = ((orig, ins, _): (CanonicalCompletion.insertion, _, _)) => (
      rank_of(ins),
      rank_of(orig),
    );
    let (segment, ghost_marks) =
      ghosts
      |> List.sort((a, b) => compare(key(b), key(a)))
      |> List.fold_left(
           ((seg, marks), (_, ins, pieces)) =>
             switch (CanonicalCompletion.splice_ghost(seg, ~ins, ~pieces)) {
             | Some((seg, more)) => (seg, marks @ more)
             | None => (seg, marks)
             },
           (raw, []),
         );
    /* ghost shards may complete a tile whose shards were split
       across the segment — reassemble or the parser (Skel) sees an
       impossible all-present-unassembled run. Then the padding
       oracle: F1 spacing around system material, applied LAST so
       nothing can reorder it. */
    let segment =
      ghost_marks == []
        ? segment
        : segment
          |> CanonicalCompletion.normalize_display
          |> restore_ghost_holes(~marks=ghost_marks, ~pre=segment)
          |> CanonicalCompletion.finish_display(
               ~marks=ghost_marks,
               ~raw,
               ~caret_after=CanonicalCompletion.caret_left_atom(z),
             );
    /* a splice can produce a tile violating the shards/children
       arity invariant (Base.re) yet still PARSE — the renderer's
       Aba walk crashes on it (fuzzer: `case fun in |`). Validate
       before accepting the fork. */
    let rec tiles_well_formed = (sg: Segment.t): bool =>
      List.for_all(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) =>
            List.length(t.children) == List.length(t.shards)
            - 1
            && List.for_all(
                 i => i >= 0 && i < List.length(t.label),
                 t.shards,
               )
            && List.for_all(tiles_well_formed, t.children)
          | _ => true
          },
        sg,
      );
    if (ghost_marks != [] && !tiles_well_formed(segment)) {
      failwith("DisplayFork: malformed splice");
    };
    (segment, ghost_marks, MakeTerm.go(segment));
  };
  let (segment, ghost_marks, parsed) =
    switch (forked()) {
    | r => r
    | exception _ => (raw, [], MakeTerm.go(raw))
    };
  {
    segment,
    ghost_marks,
    assist,
    ghosted: ghost_marks == [] ? [] : List.map(((o, _, _)) => o, ghosts),
    parsed,
  };
};

/* The fork's contract: it can NEVER crash the editor. An exception
   anywhere — assist derivation included (the fuzzer found an
   out-of-range shard index from a case/fun/in interleave) — means
   this frame shows the raw segment with no assist at all. */
let mk =
    (
      ~info_map: Statics.Map.t,
      ~obligations: list(TypeObligations.t),
      ~armed: bool,
      z: Zipper.t,
    )
    : t =>
  switch (mk_inner(~info_map, ~obligations, ~armed, z)) {
  | fork => fork
  | exception _ => plain(z)
  };
