/* The zipper-facing completion queries: which records of an insertion
 * stream the caret OWNS, and what Tab does for them. Kept apart from
 * CanonicalCompletion so the engine stays segment-in/segment-out.
 *
 * ONE ownership list (chips_among) feeds every interactive surface —
 * the quiver bubble drawn at the caret, the inline ghost (DisplayFork),
 * and Tab — so the bubble's first delimiter is what Tab types by
 * construction. Deriving ownership twice (measured zones for the
 * display, a sibling walk for Tab) is how they drifted: a bubble
 * reading "else ? end in" while Tab typed end. */

open Util;

type insertion = CanonicalCompletion.insertion;

/* a witness record: its head delimiter carries a typed prefix */
let is_pure_witness = (ins: insertion): bool =>
  switch (ins.delimiters) {
  | [{typed_len: Some(_), _}, ..._] => true
  | _ => false
  };

/* a witness IN PROGRESS (prefix shorter than the delimiter): its Tab
   text is the token REMAINDER, which only extends the prefix when the
   caret abuts the typed token — past the hole after it (`e ?|`) the
   remainder would land as a stray `lse`. Owned only when adjacent. */
let is_partial_witness = (ins: insertion): bool =>
  switch (ins.delimiters) {
  | [{typed_len: Some(n), text, _}, ..._] => n < String.length(text)
  | _ => false
  };

/* Every record of the stream whose zone holds the caret, in WALK order
   (left walk before right, nearer pieces first; records on one piece
   keep stream order). This is the ghost fork's input — splice_sort
   decides the visual order there and breaks ties by list order, so the
   walk order is load-bearing for ghosts. Tab and the bubble read the
   same set in Tab order (chips_owned).

   Zone = the caret's inter-content run: whitespace/grout siblings on
   either side match records anchored on them from either side; the
   bounding content pieces match only records on their caret-facing
   side. One stack can split into several records sharing the zone
   (`else` glued to `4`, `end in` after the next line's indentation),
   so the nearest anchor is NOT the answer.

   PARTITION GATE: the caret's segment partitioned as the engine
   partitions it — a record is owned only from its anchor's partition
   or from contentless partitions after it (an empty trailing line
   changes nothing; `if ⏎ ⏎ |1` does: then typed at the 1 absorbs it).

   Inner caret (inside a token, e.g. a string literal): the promise
   anchored on the host token still applies for DISPLAY — match the
   immediate neighbors only; Tab declines (tab_action). */
let zone_matches =
    (z: Zipper.t, insertions: list(insertion)): list((int, insertion)) => {
  let indexed = List.mapi((i, ins) => (i, ins), insertions);
  let find_all = (~adjacent: bool, id: Id.t, sides: list(Direction.t)) =>
    indexed
    |> List.filter(((_, ins: insertion)) =>
         Id.equal(ins.adjacent_id, id)
         && List.mem(ins.side, sides)
         && (adjacent || !is_partial_witness(ins))
       );
  let is_content = (p: Piece.t): bool =>
    switch (p) {
    | Secondary(_)
    | Grout(_) => false
    | _ => true
    };
  let matches =
    switch (z.caret) {
    | Inner(_) =>
      let both = [Direction.Left, Direction.Right];
      let try_head = (ps: list(Piece.t)) =>
        switch (ps) {
        | [p, ..._] => find_all(~adjacent=true, Piece.id(p), both)
        | [] => []
        };
      let (l, r) = z.relatives.siblings;
      switch (try_head(List.rev(l))) {
      | [] => try_head(r)
      | hits => hits
      };
    | Outer =>
      let (l, r) = Zipper.unselect(z).relatives.siblings;
      /* adjacent = this piece touches the caret (first step of the
         left walk only: witnesses anchor on their token's right) */
      let rec probe = (ps: list(Piece.t), ~facing: Direction.t, ~adjacent) =>
        switch (ps) {
        | [] => []
        | [p, ...rest] =>
          is_content(p)
            ? find_all(~adjacent, Piece.id(p), [facing])
            : find_all(
                ~adjacent,
                Piece.id(p),
                [Direction.Left, Direction.Right],
              )
              @ probe(rest, ~facing, ~adjacent=false)
        };
      let parts =
        CanonicalCompletion.partition_segment(l @ r)
        |> List.map(fst)
        |> Array.of_list;
      let part_of = (id: Id.t): option(int) => {
        let rec go = k =>
          k >= Array.length(parts)
            ? None
            : List.exists(
                (q: Piece.t) => Id.equal(Piece.id(q), id),
                parts[k],
              )
                ? Some(k) : go(k + 1);
        go(0);
      };
      let caret_part =
        switch (List.rev(l), r) {
        | ([p, ..._], _)
        | ([], [p, ..._]) => part_of(Piece.id(p))
        | ([], []) => None
        };
      let same_reading = (ins: insertion) =>
        switch (part_of(ins.adjacent_id), caret_part) {
        | (Some(j), Some(k)) =>
          j == k
          || j < k
          && List.for_all(
               i => !List.exists(is_content, parts[i]),
               List.init(k - j, i => j + 1 + i),
             )
        | _ => false
        };
      probe(List.rev(l), ~facing=Direction.Right, ~adjacent=true)
      @ probe(r, ~facing=Direction.Left, ~adjacent=false)
      |> List.filter(((_, ins)) => same_reading(ins));
    };
  /* dedupe: a record can match both walks */
  List.fold_left(
    (acc, (i, ins)) => List.mem_assoc(i, acc) ? acc : acc @ [(i, ins)],
    [],
    matches,
  );
};

let chips_among =
    (z: Zipper.t, insertions: list(insertion)): list(insertion) =>
  zone_matches(z, insertions) |> List.map(snd);

/* The same set in TAB ORDER — what the bubble at the caret shows and
   what Tab applies: partial witnesses first (they anchor at the
   caret's own token — the NEAREST promise; a T2 suggestion sits last
   in the stream but completes what the user is typing), then STREAM
   order (landing-site order in the completed program). Never the walk
   order: nearest-anchor flips with the caret's column inside the
   indentation (`else` after `4` vs `end in` after the indent). */
let chips_owned =
    (z: Zipper.t, insertions: list(insertion)): list(insertion) => {
  let (witnesses, rest) =
    zone_matches(z, insertions)
    |> List.partition(((_, ins)) => is_partial_witness(ins));
  let by_idx = List.sort(((i, _), (j, _)) => Int.compare(i, j));
  List.map(snd, by_idx(witnesses) @ by_idx(rest));
};

/* The record Tab dispatches: the first the caret owns */
let chip_among =
    (z: Zipper.t, insertions: list(insertion)): option(insertion) =>
  List.nth_opt(chips_owned(z, insertions), 0);

/* legacy name: Tab's chip IS the first owned record (witness first) */
let tab_chip = chip_among;

/* The chip stream as DISPLAYED: a chip whose content is ghosted
   inline never also shows as a chip. ONE home for this policy:
   the live deco and the test harness both call it. */
let chips_displayed =
    (~ghosted: list(insertion), assist: list(insertion)): list(insertion) =>
  assist |> List.filter(ins => !List.memq(ins, ghosted));

/* whether the caret's left neighborhood already provides separation
   (space, linebreak, line start, or an opener's inside edge) — a
   non-hugging delimiter accepted here needs no leading space */
let left_separated = (z: Zipper.t): bool =>
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [] => true
  | [Secondary(_), ..._] => true
  | [Tile(t), ..._] =>
    switch (Util.ListUtil.last_opt(t.shards)) {
    | Some(i) => CanonicalCompletion.f1_opens(List.nth(t.label, i))
    | None => false
    }
  | _ => false
  };

/* TAB SLICES THE PROMISE (2026-07-28, andrew): the paste text for a
   plain-delimiter chunk is read off the DISPLAYED completion itself —
   the display pieces from the caret through the accepted delimiter's
   shard, plus its trailing spaces — so acceptance is byte-preserving
   by construction: post-accept, placement re-derives the holes into
   the pasted spacing and the display is unchanged. One spacing
   authority (the display; the lead/trail synthesis below was a third
   one and produced the ragged `?,? )` artifacts). Holes contribute
   nothing (they stay derived, never typed). Slicing from the CACHED
   display means Tab accepts exactly what is on screen, whatever the
   statics cadence. Fails open to the synthesis path.

   FLAGGED CHOICE (design doc): pads around a still-unfilled hole
   paste as real spaces — the happy path fills the hole where the pad
   is wanted anyway; the residue is one space in states that look
   hand-typed-then-deleted. Revisit on feel. */
let tab_slice =
    (
      ~display: Segment.t,
      ~marks: list((Id.t, option(int))),
      z: Zipper.t,
      d: CanonicalCompletion.delimiter_info,
    )
    : option(string) => {
  let is_marked = (id: Id.t, sh: int) =>
    List.exists(
      ((mid, msh): (Id.t, option(int))) =>
        Id.equal(mid, id) && (msh == Some(sh) || msh == None),
      marks,
    );
  /* flat atom stream over the display, shard-granular, ids kept so
     the caret's left atom can be located */
  let rec atoms =
          (sg: Segment.t)
          : list(
              (
                Id.t,
                int,
                [
                  | `Tok(string, bool)
                  | `Sp
                  | `Brk
                  | `Hole
                  | `Other
                ],
              ),
            ) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Grout(g) => [(g.id, (-1), `Hole)]
        | Secondary(w) when Secondary.is_space(w) => [(w.id, (-1), `Sp)]
        | Secondary(w) when Secondary.is_linebreak(w) => [
            (w.id, (-1), `Brk),
          ]
        | Secondary(w) => [(w.id, (-1), `Other)]
        | Projector(_) => [(Piece.id(p), (-1), `Other)]
        | Tile(t) =>
          Aba.mk(t.shards, t.children)
          |> Aba.join(
               i =>
                 [
                   (
                     t.id,
                     i,
                     `Tok((
                       switch (List.nth_opt(t.label, i)) {
                       | Some(tok) => tok
                       | None => ""
                       },
                       is_marked(t.id, i),
                     )),
                   ),
                 ],
               atoms,
             )
          |> List.concat
        },
      sg,
    );
  switch (CanonicalCompletion.caret_left_atom(z)) {
  | None => None
  | Some((cid, csh)) =>
    /* hole-HOSTING cells are not text: a space consumed as a hole's
       cell (borrowed or P16 backing) contributes nothing to the
       paste — the re-derived hole re-hosts in the pasted FORMATTING
       spaces only */
    let cells = GroutCells.classify(display);
    let visible_sp = (id: Id.t) => !GroutCells.is_consumed(cells, id);
    let ats = atoms(display);
    let start =
      ats
      |> List.mapi((i, a) => (i, a))
      |> List.find_opt(((_, (id, sh, _))) =>
           Id.equal(id, cid) && (sh == csh || csh == (-1) && sh != (-1))
         )
      |> Option.map(fst);
    /* walk right of the caret: spaces and holes accumulate, the first
       GHOST token must be the chunk's delimiter; anything else bails
       to the synthesis path */
    let rec collect = (i: int, acc: string): option(string) =>
      switch (List.nth_opt(ats, i)) {
      | None => None
      | Some((sid, _, `Sp)) =>
        collect(i + 1, visible_sp(sid) ? acc ++ " " : acc)
      | Some((_, _, `Hole)) => collect(i + 1, acc)
      | Some((_, _, `Tok(tok, true))) when tok == d.text =>
        Some(trail(i + 1, acc ++ tok))
      | Some(_) => None
      }
    /* trailing spaces travel with the chunk (the display pads after
       the delimiter re-host the re-derived hole), unless the line
       ends — a line-end hole's pad is a render class, not text */
    and trail = (i: int, acc: string): string => {
      let rec spaces = (i, n) =>
        switch (List.nth_opt(ats, i)) {
        | Some((sid, _, `Sp)) => spaces(i + 1, visible_sp(sid) ? n + 1 : n)
        | Some((_, _, `Hole)) => spaces(i + 1, n)
        | Some((_, _, `Brk))
        | None => (0, false)
        | Some(_) => (n, true)
        };
      let (n, keep) = spaces(i, 0);
      keep ? acc ++ String.make(n, ' ') : acc;
    };
    switch (start) {
    | None => None
    | Some(start) => collect(start + 1, "")
    };
  };
};

/* Tab = "type it for me": the paste text for the chip's next chunk.
   A witness chip pastes the token REMAINDER (no spaces — it merges
   into the typed prefix exactly as typing would); a plain delimiter
   is sliced from the display when one is given, else synthesized: a
   leading space when it would jam against an alphanumeric left
   neighbor and a trailing space when wordish. */
let tab_text =
    (~display: option(Segment.t)=?, ~marks=[], z: Zipper.t, ins: insertion)
    : option(string) => {
  let rec go = (ds: list(CanonicalCompletion.delimiter_info)) =>
    switch (ds) {
    | [] => None
    | [d, ...rest] =>
      switch (d.typed_len) {
      | Some(n) when n < String.length(d.text) =>
        Some(String.sub(d.text, n, String.length(d.text) - n))
      | Some(_) => go(rest) /* fully-typed witness: next chunk */
      | None when Option.is_some(display) =>
        switch (tab_slice(~display=Option.get(display), ~marks, z, d)) {
        | Some(_) as s => s
        | None => synth(d)
        }
      | None => synth(d)
      }
    }
  and synth = (d: CanonicalCompletion.delimiter_info) => {
    let lead =
      !CanonicalCompletion.f1_hugs_left(d.text) && !left_separated(z);
    /* no trailing pad when the accepted delimiter ends its line —
       the next material lives on a later line already */
    let next_is_break =
      switch (snd(z.relatives.siblings)) {
      | [Secondary(w), ..._] => Secondary.is_linebreak(w)
      | _ => false
      };
    let trail =
      !CanonicalCompletion.f1_closes(d.text)
      && !CanonicalCompletion.f1_opens(d.text)
      && !next_is_break;
    Some((lead ? " " : "") ++ d.text ++ (trail ? " " : ""));
  };
  go(ins.delimiters);
};

/* An opener: the tile's LEADING shard, not a witness in progress */
let is_opener = (ins: insertion): bool =>
  switch (ins.delimiters) {
  | [{of_shard: Some((_, 0)), typed_len: None, _}, ..._] => true
  | _ => false
  };

/* What Tab does at this caret — THE dispatch, shared by the editor and
   the tests. A witness remainder or a trailing/middle delimiter is
   TYPED at the caret (Paste through the normal pipeline: spacing and
   caret land as if typed, and the material lands where the user is,
   e.g. on the fresh line after `then 4`). An OPENER is materialized by
   the engine instead: typing `(` pairs it with the most recently
   stranded `)` (backpack order), not with the closer the bubble shows
   it paired with. An Inner caret declines: the zone matches for
   display, but Paste would land INSIDE the token. */
let tab_action =
    (
      ~display: option(Segment.t)=?,
      ~marks=[],
      z: Zipper.t,
      assist: list(insertion),
    )
    : option(Action.t) =>
  switch (z.caret) {
  | Inner(_) => None
  | Outer =>
    switch (chip_among(z, assist)) {
    | None => None
    | Some(ins) when is_opener(ins) =>
      switch (ins.delimiters) {
      | [{of_shard: Some((tid, _)), _}, ..._] =>
        Some(Action.ApplyCompletion(One(tid)))
      | _ => None
      }
    | Some(ins) =>
      tab_text(~display?, ~marks, z, ins)
      |> Option.map(text => Action.Paste(text))
    }
  };

/* Engine-only conveniences (tests, tooling): ownership over the bare
   completion of the caret's program */
let chip_at_caret = (z: Zipper.t): option(insertion) => {
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  chip_among(z, CanonicalCompletion.for_editor(seg).insertions);
};

let obligation_at_caret = (z: Zipper.t): option(Id.t) =>
  chip_at_caret(z)
  |> Option.map((ins: insertion) =>
       switch (ins.delimiters) {
       | [{of_shard: Some((tid, _)), _}, ..._] => Some(tid)
       | _ => None
       }
     )
  |> Option.join;
