open Language;

/* PROMISE RENDER (stage 2): the display is a PROJECTION of the single
   unified artifact (PromiseArtifact) — one material, zero
   reconstruction of system pieces.

   mk has the SAME output type as DisplayFork.mk and shares its assist
   stream (extend_t2), its view-policy layer (ghost_selection:
   caret-relative suppress/slide), its splice ordering (splice_sort)
   and its well-formedness gate (tiles_well_formed) — one home, so the
   two implementations cannot drift on those axes.

   The display's SYSTEM material comes from the reified artifact:
   - engine closers/keywords are REAL completed shards;
   - T1 obligation commas + their holes are the REAL reified comma
     tiles + convex grout, carrying the deterministic ids statics
     analyzed (so an inspector/error on a presumed hole finds it in the
     info_map by construction);
   - WITNESS delimiters (in / => / -> / then) are the REAL completed
     shards, styled SUB-TOKEN: the typed prefix normal, the remainder
     ghost (typed_lens threads (tile, shard) -> typed_len to Code.re).

   The ONE class still rendered as a display comment is a genuine T2
   LOOKAHEAD tail — a token the user has not typed and the engine has
   not synthesized (a TyDi variable/keyword head remainder, or its
   speculative `::` / `)`). These live only in the suggestion channel;
   they have no real artifact piece to project. When the port of the
   TyDi channel lands they join the artifact too.

   Total fail-open like the current fork: any exception shows the raw
   segment with no assist. */

let is_engine_witness = (d: CanonicalCompletion.delimiter_info): bool =>
  d.typed_len != None && d.of_shard != None;

/* A pure engine-witness run: a single real delimiter (in / => / -> /
   then) the user began typing. In the reified artifact it is a REAL
   completed shard standing where the user's partial token sits — the
   display swaps the partial token for that full shard (sub-token
   styled), so a witness is ONE real token, not a partial + comment. */
let is_witness_replace = (ins: CanonicalCompletion.insertion): bool =>
  switch (ins.delimiters) {
  | [d] => is_engine_witness(d)
  | _ => false
  };

/* an insertion projects wholly from the artifact iff every delimiter
   is either a real completed shard (of_shard set — closers, keywords,
   engine witnesses) or a T1 comma (of_shard=None, text=","). A
   genuine T2 head/tail (of_shard=None, non-comma) is lookahead with
   no artifact piece and stays on the comment channel. */
let projectable = (ins: CanonicalCompletion.insertion): bool =>
  ins.delimiters
  |> List.for_all((d: CanonicalCompletion.delimiter_info) =>
       (d.of_shard != None || d.text == ",")
       /* a WITNESS delimiter (typed_len set) can only be shown by
          REPLACING the user's partial token — projecting its full
          shard alongside the typed prefix duplicates it (`= ? =>`).
          Single-witness insertions take the replace path before this
          predicate is consulted; anything else degrades to the
          remainder-ghost channel. */
       && d.typed_len == None
     );

let is_grout = (p: Piece.t): bool =>
  switch (p) {
  | Grout(_) => true
  | _ => false
  };

/* The real reified hole a concave-right delimiter owns. For an
   INTERIOR delimiter the hole opens the child in the slot after it.
   For the tile's LAST shard (a prefix form's `in` / `->` opening its
   body to the RIGHT) the hole is the grout immediately following the
   tile at its level. Either way we return the REAL reified grout so
   the display hole is the one statics analyzed. */
let art_hole_after =
    (art: PromiseArtifact.t, tid: Id.t, i: int): option(Piece.t) => {
  let interior =
    switch (PromiseArtifact.find_reified(art, tid)) {
    | None => None
    | Some(t) =>
      let rec pos = (j, sh) =>
        switch (sh) {
        | [] => (-1)
        | [x, ..._] when x == i => j
        | [_, ...tl] => pos(j + 1, tl)
        };
      let k = pos(0, t.shards);
      k >= 0 && k < List.length(t.children)
        ? List.find_opt(is_grout, List.nth(t.children, k)) : None;
    };
  switch (interior) {
  | Some(_) as g => g
  | None =>
    /* last-shard / prefix body: the grout following the tile at its
       level, before the next non-secondary piece (placement may put
       it past a typed space) */
    let rec grout_before_content = (sg: Segment.t): option(Piece.t) =>
      switch (sg) {
      | [Piece.Secondary(_), ...tl] => grout_before_content(tl)
      | [g, ..._] when is_grout(g) => Some(g)
      | _ => None
      };
    let rec after = (sg: Segment.t): option(Piece.t) =>
      switch (sg) {
      | [] => None
      | [Tile(t), ...tl] =>
        Id.equal(t.id, tid)
          ? grout_before_content(tl)
          : (
            switch (after(List.concat(t.children))) {
            | Some(_) as r => r
            | None => after(tl)
            }
          )
      | [_, ...tl] => after(tl)
      };
    after(art.reified);
  };
};

/* Project a PROJECTABLE insertion's real pieces from the artifact.
   Returns (pieces, typed_lens): each engine delimiter is its real
   completed shard (+ a real following hole when it owes one); each T1
   comma expands to the site's next owed comma (+ hole) from the
   reified child — the deterministic tiles statics analyzed. */
let project_pieces =
    (art: PromiseArtifact.t, ins: CanonicalCompletion.insertion)
    : option((Segment.t, list(((Id.t, int), int)))) => {
  let typed_lens = ref([]);
  /* per-site owed comma cursors: a merged insertion (`f(g(`) lists
     inner-then-outer closers with the OUTER's commas between them, so
     a comma belongs to the site whose closer FOLLOWS it in the list.
     Each site draws its owed commas in order. */
  let cursors: Hashtbl.t(Id.t, list(Piece.t)) = Hashtbl.create(4);
  let owed_of = (site: Id.t): list(Piece.t) =>
    switch (Hashtbl.find_opt(cursors, site)) {
    | Some(ps) => ps
    | None =>
      let ps = PromiseArtifact.site_owed_pieces(art, site);
      Hashtbl.replace(cursors, site, ps);
      ps;
    };
  /* pop the next (comma, following-hole) from a site's cursor */
  let next_comma = (site: Id.t): list(Piece.t) => {
    let rec go = (ps: list(Piece.t)) =>
      switch (ps) {
      | [] => ([], [])
      | [Piece.Tile({label: [","], _}) as c, ...tl] =>
        switch (tl) {
        | [Grout(_) as g, ...rest] => ([c, g], rest)
        | _ => ([c], tl)
        }
      | [_, ...tl] => go(tl)
      };
    let (out, rest) = go(owed_of(site));
    Hashtbl.replace(cursors, site, rest);
    out;
  };
  let rec build =
          (ds: list(CanonicalCompletion.delimiter_info)): option(Segment.t) =>
    switch (ds) {
    | [] => Some([])
    | [d, ...rest] =>
      let this: option(Segment.t) =
        switch (d.of_shard) {
        | Some((tid, i)) =>
          switch (PromiseArtifact.find_reified(art, tid)) {
          | Some(t) =>
            switch (d.typed_len) {
            | Some(n) => typed_lens := [((tid, i), n), ...typed_lens^]
            | None => ()
            };
            let shard = Piece.Tile(Tile.shard_of(t, i));
            /* a closer (`)` `]` `}`) is self-terminating: never pull a
               following hole for it (its sibling grout is the real
               junction to the next statement, not a promised hole) */
            let hole =
              d.needs_hole && !CanonicalCompletion.f1_closes(d.text)
                ? Option.to_list(art_hole_after(art, tid, i)) : [];
            Some([shard, ...hole]);
          | None => None
          }
        | None when d.text == "," =>
          /* the site is the tile of the next closer delimiter (its
             owed commas sit before that closer); if none follows, the
             insertion's own trailing site (last of_shard) */
          let next_site =
            rest
            |> List.find_map((d': CanonicalCompletion.delimiter_info) =>
                 switch (d'.of_shard) {
                 | Some((tid, _)) => Some(tid)
                 | None => None
                 }
               );
          switch (next_site) {
          | Some(site) => Some(next_comma(site))
          | None => Some([])
          };
        | None => None
        };
      switch (this, build(rest)) {
      | (Some(own), Some(tl)) => Some(own @ tl)
      | _ => None
      };
    };
  build(ins.delimiters) |> Option.map(pieces => (pieces, typed_lens^));
};

/* a witness insertion's real reified shard, the id of the user's
   partial token it replaces, its typed_len and a following hole (when
   the delimiter owes one). None when the shard isn't in the artifact
   or the record has no absorbed token (nothing to replace). */
let witness_shard =
    (art: PromiseArtifact.t, ins: CanonicalCompletion.insertion)
    : option(
        (Id.t, Segment.t, ((Id.t, int), int), (Id.t, (Id.t, int, int))),
      ) =>
  switch (ins.delimiters) {
  | [{of_shard: Some((tid, i)), typed_len: Some(n), needs_hole, _}] =>
    switch (
      PromiseArtifact.find_reified(art, tid),
      PromiseArtifact.prefix_of(art, tid, i),
    ) {
    | (Some(t), Some(sp)) =>
      /* just the shard: reassembly folds it into its tile, and the
         tile's OWN interior holes (the reified body hole) come with
         the reassembled structure — attaching one here would double
         it against the raw trailing hole */
      ignore(needs_hole);
      let shard = Piece.Tile(Tile.shard_of(t, i));
      Some((
        sp.token_id,
        [shard],
        ((tid, i), n),
        (sp.token_id, (tid, i, n)),
      ));
    | _ => None
    }
  | _ => None
  };

/* replace the tile whose id is `pid` (the user's partial witness
   token) with `repl` in a display segment, recursing into children */
let rec replace_tile =
        (seg: Segment.t, ~pid: Id.t, ~repl: Segment.t): Segment.t =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when Id.equal(t.id, pid) => repl
      | Tile(t) => [
          Piece.Tile({
            ...t,
            children:
              List.map(c => replace_tile(c, ~pid, ~repl), t.children),
          }),
        ]
      | p => [p]
      },
    seg,
  );

let mk_inner =
    (
      ~info_map: Statics.Map.t,
      ~obligations: list(TypeObligations.t),
      ~armed: bool,
      z: Zipper.t,
    )
    : DisplayFork.t => {
  let raw = Zipper.unselect_and_zip(z);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  /* reify with the FRAME-FRESH obligations (statics records + sites
     synthesized this frame) so the artifact — and the display — carry
     a just-typed `f(`'s commas one debounce before statics catches
     up. At the settled frame this equals the analyzed set, so the
     display holes' ids coincide with the info_map's. */
  let frame_obs =
    TypeObligations.frame_obligations(z, ~info_map, obligations);
  let art = PromiseArtifact.mk(~obligations=frame_obs, seg);
  let assist =
    TypeObligations.assist_stream_with(
      ~result={
        completed_seg: art.completed,
        shard_records: art.shard_records,
        insertions:
          CanonicalCompletion.derive_insertions(
            ~original=seg,
            ~records=art.shard_records,
            art.completed,
          ),
      },
      z,
      ~info_map,
      obligations,
    )
    |> DisplayFork.extend_t2(~info_map, ~armed, z);
  let selected = DisplayFork.ghost_selection(~armed, z, assist);
  let caret_after = CanonicalCompletion.caret_left_atom(z);
  /* Build the fork. `use_replaces` chooses whether engine witnesses
     (in / => / -> / then) become REAL reified shards in place
     (sub-token styled) or stay on the ghost-splice path. A witness
     replace reassembles the tile; in a degenerate interleave that
     reassembly can shift a PRE-CARET junction — the sacred no-change-
     before-the-caret invariant. So we build WITH replaces, verify the
     pre-caret text is unchanged, and fall back WITHOUT replaces if it
     regressed. (The vast majority of frames pass the first build.) */
  let build = (~use_replaces: bool) => {
    let typed_lens = ref([]);
    let caret_witnesses = ref([]);
    let replaces =
      use_replaces
        ? selected
          |> List.filter_map(((orig, _)) =>
               is_witness_replace(orig) ? witness_shard(art, orig) : None
             )
        : [];
    let replaced_ghosted =
      replaces == []
        ? []
        : selected
          |> List.filter_map(((orig, _)) =>
               is_witness_replace(orig) && witness_shard(art, orig) != None
                 ? Some(orig) : None
             );
    let raw =
      List.fold_left(
        (raw, (pid, repl, tl, cw)) => {
          typed_lens := [tl, ...typed_lens^];
          caret_witnesses := [cw, ...caret_witnesses^];
          replace_tile(raw, ~pid, ~repl);
        },
        raw,
        replaces,
      );
    let replace_marks =
      replaces
      |> List.concat_map(((_, repl, _, _)) =>
           CanonicalCompletion.ghost_marks(repl)
         );
    /* SPLICE runs (everything not a witness replace): real reified
       shards + T1 comma pieces from the artifact; genuine T2 lookahead
       falls back to the suggestion channel's comment pieces */
    let ghosts =
      selected
      |> List.filter_map(((orig, ins: CanonicalCompletion.insertion)) =>
           if (use_replaces && is_witness_replace(orig)) {
             None;
           } else {
             let pieces =
               if (projectable(orig)) {
                 switch (project_pieces(art, orig)) {
                 /* an empty projection (the artifact carries no owed
                    material for this insertion this frame) is NOT a
                    ghost — keep it a chip, don't suppress it */
                 | Some(([], _)) => None
                 | Some((real, tls)) =>
                   typed_lens := tls @ typed_lens^;
                   Some(real);
                 | None => TypeObligations.ghost_pieces(z, ins)
                 };
               } else {
                 TypeObligations.ghost_pieces(z, ins);
               };
             pieces |> Option.map(pieces => (orig, ins, pieces));
           }
         );
    /* FAIL OPEN around the WHOLE splice/normalize/parse (display-only):
       any exception means no ghost this frame — the raw segment, NO
       marks — but the assist stream (chips/Tab) is preserved, computed
       above outside this closure. */
    let forked = () => {
      let (segment, ghost_marks) =
        DisplayFork.splice_sort(raw, ghosts)
        |> List.fold_left(
             ((seg, marks), (_, ins, pieces)) =>
               switch (CanonicalCompletion.splice_ghost(seg, ~ins, ~pieces)) {
               | Some((seg, more)) => (seg, marks @ more)
               | None => (seg, marks)
               },
             (raw, replace_marks),
           );
      let transparent = (w: Secondary.t) =>
        (
          switch (w.content) {
          | Comment(_) => true
          | Whitespace(_) => false
          }
        )
        && List.exists(
             ((mid, msh): (Id.t, option(int))) =>
               msh == None && Id.equal(mid, w.id),
             ghost_marks,
           );
      let segment =
        ghost_marks == []
          ? GroutPlace.place(segment)
          : segment
            |> CanonicalCompletion.normalize_display(~transparent)
            |> DisplayFork.restore_ghost_holes(
                 ~marks=ghost_marks,
                 ~pre=segment,
               )
            |> CanonicalCompletion.finish_display(
                 ~marks=ghost_marks,
                 ~raw,
                 ~caret_after,
               );
      let ghost_marks =
        ghost_marks
        @ DisplayFork.inherit_ghost_marks(~marks=ghost_marks, segment);
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
      typed_lens: ghost_marks == [] ? [] : typed_lens^,
      caret_witnesses: ghost_marks == [] ? [] : caret_witnesses^,
      assist,
      ghosted:
        ghost_marks == []
          ? [] : List.map(((o, _, _)) => o, ghosts) @ replaced_ghosted,
      parsed,
    };
  };
  /* the display text strictly before the caret (markers-free) — must
     equal raw's, whichever build we use */
  let pre_caret = (~caret_witnesses, seg: Segment.t): string => {
    let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
    let caret = DisplayCaret.point(~caret_witnesses, measured, z);
    /* grout is zero-width in measured but prints as a ?/~ char: the
       slice column shifts by the holes printed before the caret on
       its row */
    let shift = {
      let rec count = (sg: Segment.t): int =>
        List.fold_left(
          (acc, pc: Piece.t) =>
            switch (pc) {
            | Grout(g) =>
              switch (Measured.find_g(g, measured)) {
              | m when m.origin.row == caret.row && m.origin.col < caret.col =>
                acc + 1
              | _ => acc
              | exception _ => acc
              }
            | Tile(t) =>
              List.fold_left((a, k) => a + count(k), acc, t.children)
            | _ => acc
            },
          0,
          sg,
        );
      count(seg);
    };
    let col = caret.col + shift;
    let rows =
      Printer.of_segment(
        ~holes="?",
        ~concave_holes="~",
        ~indent=" ",
        ~measured,
        seg,
      )
      |> String.split_on_char('\n');
    let before = List.filteri((i, _) => i < caret.row, rows);
    let at = List.nth_opt(rows, caret.row) |> Option.value(~default="");
    let prefix = col <= String.length(at) ? String.sub(at, 0, col) : at;
    String.concat("\n", before @ [prefix]);
  };
  let has_witness_replace =
    selected
    |> List.exists(((orig, _)) =>
         is_witness_replace(orig) && witness_shard(art, orig) != None
       );
  let full = build(~use_replaces=true);
  /* only pay the pre-caret check when a witness replace could have
     shifted structure; otherwise the projection can't move pre-caret.
     If the replace moved pre-caret text (a degenerate interleave where
     reassembling the witness shard shifts a junction), fall back to
     the ghost-splice path, which never touches pre-caret material. */
  /* the raw baseline goes through the ONE derivation too: the
     display legitimately shows placed holes before the caret, and by
     layout invisibility the placed raw agrees with it exactly */
  if (has_witness_replace
      && pre_caret(~caret_witnesses=full.caret_witnesses, full.segment)
      != pre_caret(~caret_witnesses=[], GroutPlace.place(raw))) {
    build(~use_replaces=false);
  } else {
    full;
  };
};

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
