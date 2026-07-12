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
  parsed: MakeTerm.t,
};

/* degenerate fork: no assist machinery (settings off, init paths) */
let plain = (z: Zipper.t): t => {
  let segment = Zipper.unselect_and_zip(z);
  {
    segment,
    ghost_marks: [],
    assist: [],
    parsed: MakeTerm.go(segment),
  };
};

let mk =
    (
      ~info_map: Statics.Map.t,
      ~obligations: list(TypeObligations.t),
      ~armed: bool,
      z: Zipper.t,
    )
    : t => {
  let assist = TypeObligations.assist_stream(z, ~info_map, obligations);
  /* inline chip ghost: when the caret sits in a chip's zone while
     ARMED (an edit arms, any other action disarms — movement never
     conjures), the chip's pending content splices into the display
     at its anchor. Pure-witness insertions are TyDi's (they live in
     the buffer); the splice_precedes_caret guard runs AFTER the
     slide — a ghost hugging the caret through whitespace is
     at-caret, not pre-caret. */
  let ghost =
    if (armed) {
      let ghostable =
        List.filter(
          ins => !CanonicalCompletion.is_pure_witness(ins),
          assist,
        );
      switch (
        CanonicalCompletion.chip_among(z, ghostable)
        |> Option.map(CanonicalCompletion.slide_to_caret(z))
      ) {
      | Some(ins) when !CanonicalCompletion.splice_precedes_caret(z, ins) =>
        TypeObligations.ghost_pieces(z, ins)
        |> Option.map(pieces => (ins, pieces))
      | _ => None
      };
    } else {
      None;
    };
  let raw = Zipper.unselect_and_zip(z);
  let (segment, ghost_marks) =
    switch (ghost) {
    | Some((ins, pieces)) =>
      switch (CanonicalCompletion.splice_ghost(raw, ~ins, ~pieces)) {
      | Some((segment, marks)) => (segment, marks)
      | None => (raw, [])
      }
    | None => (raw, [])
    };
  /* ghost shards may complete a tile whose shards were split across
     the segment — reassemble or the parser (Skel) sees an impossible
     all-present-unassembled run. Then the padding oracle: F1 spacing
     around system material, applied LAST so nothing can reorder it. */
  let segment =
    ghost_marks == []
      ? segment
      : segment
        |> CanonicalCompletion.normalize_display
        |> CanonicalCompletion.finish_display(
             ~marks=ghost_marks,
             ~raw,
             ~caret_after=CanonicalCompletion.caret_left_atom(z),
           );
  /* FAIL OPEN: the fork is display-only — a splice the parser can't
     take means no ghost this frame, never a crash */
  let (segment, ghost_marks, parsed) =
    switch (MakeTerm.go(segment)) {
    | parsed => (segment, ghost_marks, parsed)
    | exception _ when ghost_marks != [] => (raw, [], MakeTerm.go(raw))
    };
  {
    segment,
    ghost_marks,
    assist,
    parsed,
  };
};
