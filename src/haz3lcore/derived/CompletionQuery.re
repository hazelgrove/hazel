/* The zipper-facing completion queries: what obligation is the caret
 * pinned to, and what would Tab type for it. Kept apart from
 * CanonicalCompletion so the engine stays segment-in/segment-out
 * (this is the only completion code that reads a Zipper.t). */

open Util;

/* These gapless pairs are single atomic tokens, with no child hole. */
let fuses_empty = (left: Segment.t, text: string): bool =>
  switch (List.rev(left)) {
  | [p, ..._] =>
    switch (SpaceNormalize.last_token(p)) {
    | Some(token) =>
      List.mem(
        token ++ text,
        [
          Token.empty_tuple,
          Token.empty_list,
          Token.empty_module,
          Token.empty_string,
        ],
      )
    | None => false
    }
  | [] => false
  };

/* Every obligation whose insertion zone contains the caret, in ENGINE
   order — landing-site order in the completed program, which is both
   the order the quiver bubble draws them and the order Tab applies.
   The zone is the caret's inter-content run: whitespace/grout
   siblings on either side match insertions anchored on them from
   either side; the bounding content pieces match only insertions on
   their caret-facing side. One stack can split into several records
   sharing the zone (a single-line form glues its closer back to
   content while multiline forms append past the linebreak: `else`
   after `4`, `end in` after the next line's indentation), so the
   nearest anchor is NOT the answer — it flips with the caret's
   column inside the indentation and disagrees with the bubble. */
let chips_at_caret =
    (~seg: option(Segment.t)=?, z: Zipper.t)
    : list(CanonicalCompletion.insertion) =>
  switch (z.caret) {
  | Inner(_) => []
  | Outer =>
    /* ~seg: the caller's already-zipped engine segment (the view
       zips once per frame); the memoized completion is shared */
    let seg =
      switch (seg) {
      | Some(seg) => seg
      | None => Zipper.unselect_and_zip(~erase_buffer=true, z)
      };
    let result = CanonicalCompletion.for_editor(seg);
    let indexed = List.mapi((i, ins) => (i, ins), result.insertions);
    /* a witness in progress (typed prefix of a delimiter): its Tab
       text is the token REMAINDER, which only extends the prefix when
       the caret abuts the typed token — past the hole after it
       (`e ?|`) the remainder would land as a stray `lse`. Owned only
       when adjacent. */
    let is_witness = (ins: CanonicalCompletion.insertion) =>
      switch (ins.delimiters) {
      | [{typed_len: Some(n), text, _}, ..._] => n < String.length(text)
      | _ => false
      };
    let matching = (~adjacent: bool, id: Id.t, sides: list(Direction.t)) =>
      indexed
      |> List.filter(((_, ins: CanonicalCompletion.insertion)) =>
           Id.equal(ins.adjacent_id, id)
           && List.mem(ins.side, sides)
           && (adjacent || !is_witness(ins))
         );
    let is_content = (p: Piece.t): bool =>
      switch (p) {
      | Secondary(_)
      | Grout(_) => false
      | _ => true
      };
    /* adjacent = this piece touches the caret (first step of the left
       walk only: witnesses anchor on their token's right side) */
    let rec probe = (ps: list(Piece.t), ~facing: Direction.t, ~adjacent) =>
      switch (ps) {
      | [] => []
      | [p, ...rest] =>
        is_content(p)
          ? matching(~adjacent, Piece.id(p), [facing])
          : matching(
              ~adjacent,
              Piece.id(p),
              [Direction.Left, Direction.Right],
            )
            @ probe(rest, ~facing, ~adjacent=false)
      };
    let (l, r) = {
      let z = z |> Zipper.clear_unparsed_buffer |> Zipper.unselect;
      z.relatives.siblings;
    };
    /* PARTITION GATE: the caret's segment, partitioned as the engine
       partitions it. A linebreak that starts a new partition is where
       the completion's reading changes, so a delimiter typed past it
       lands in a different program when that side has content
       (`if ⏎ ¦1`: then typed at the 1 absorbs it as the branch, which
       the bubble never promised). Contentless partitions after the
       anchor (an empty trailing line) change nothing and stay owned. */
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
    let same_reading = (ins: CanonicalCompletion.insertion) =>
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
    |> List.filter(((_, ins)) => same_reading(ins))
    |> List.sort(((i, _), (j, _)) => Int.compare(i, j))
    |> List.map(snd)
    |> List.mapi((k, ins: CanonicalCompletion.insertion) =>
         k != 0
           ? ins
           : (
             switch (ins.delimiters) {
             | [] => ins
             | [d, ...rest] =>
               let left =
                 l |> List.rev |> List.find_opt(p => !Piece.is_secondary(p));
               let leading_hole =
                 switch (left, d.of_shard, d.typed_len) {
                 | (Some(p), Some((id, shard)), None) =>
                   switch (
                     Piece.nibs(p),
                     Segment.find_ctx(result.completed_seg, id),
                   ) {
                   | (Some((_, rn)), Some((_, _, Piece.Tile(t)))) =>
                     let (ln, _) = Mold.nibs(~index=shard, t.mold);
                     switch (rn.shape, ln.shape) {
                     | (Concave(_), Concave(_)) => !fuses_empty(l, d.text)
                     | _ => false
                     };
                   | _ => d.leading_hole
                   }
                 | _ => d.leading_hole
                 };
               {
                 ...ins,
                 delimiters: [
                   {
                     ...d,
                     leading_hole,
                   },
                   ...rest,
                 ],
               };
             }
           )
       )
    |> (
      owned => {
        /* The rightmost preview boundary meets the existing buffer even
           across spaces/newlines. A hole already there is not an insertion.
           Keep holes BETWEEN delimiters: they belong to the completion. */
        let right_hole =
          switch (List.find_opt(p => !Piece.is_secondary(p), r)) {
          | Some(Grout({shape, _})) => Some(shape)
          | Some(Tile({label: ["?"], _})) => Some(Grout.Convex)
          | _ => None
          };
        List.mapi(
          (i, ins: CanonicalCompletion.insertion) =>
            Option.is_none(right_hole) || i != List.length(owned) - 1
              ? ins
              : {
                ...ins,
                delimiters:
                  List.mapi(
                    (j, d: CanonicalCompletion.delimiter_info) =>
                      j == List.length(ins.delimiters)
                      - 1
                      && d.trailing_hole == right_hole
                        ? {
                          ...d,
                          trailing_hole: None,
                        }
                        : d,
                    ins.delimiters,
                  ),
              },
          owned,
        );
      }
    );
  };

/* The record Tab dispatches: the first of the caret's chips. The
   quiver draws the same list as the bubble at the caret (QuiverDec
   takes it as ~owned), so the bubble's first delimiter is Tab's by
   construction. */
let chip_at_caret =
    (~seg: option(Segment.t)=?, z: Zipper.t)
    : option(CanonicalCompletion.insertion) =>
  List.nth_opt(chips_at_caret(~seg?, z), 0);

/* Whether an existing operand hole can remain after this delimiter.
   Closers with convex right nibs consume trailing grout instead. */
let accepts_right_hole = (z: Zipper.t): bool =>
  switch (chip_at_caret(z)) {
  | Some({delimiters: [{of_shard: Some((id, shard)), _}, ..._], _}) =>
    let result =
      CanonicalCompletion.for_editor(
        Zipper.unselect_and_zip(~erase_buffer=true, z),
      );
    switch (Segment.find_ctx(result.completed_seg, id)) {
    | Some((_, _, Tile(t))) =>
      let (_, rn) = Mold.nibs(~index=shard, t.mold);
      switch (rn.shape) {
      | Concave(_) => true
      | Convex => false
      };
    | _ => false
    };
  | _ => false
  };

let obligation_at_caret = (z: Zipper.t): option(Id.t) =>
  chip_at_caret(z)
  |> Option.map((ins: CanonicalCompletion.insertion) =>
       switch (ins.delimiters) {
       | [{of_shard: Some((tid, _)), _}, ..._] => Some(tid)
       | _ => None
       }
     )
  |> Option.join;

/* Padding is shared by Tab and the caret's preview. Existing whitespace
   belongs to the buffer; only missing padding belongs to the chip. The
   implicit-hole marker is converted to Grout by ApplyCompletion(Next). */
let padding =
    (z: Zipper.t, d: CanonicalCompletion.delimiter_info): (string, string) => {
  let (l, r) = z.relatives.siblings;
  let word = SpaceNormalize.spaced(d.text) || List.mem(d.text, ["=", "->"]);
  let hole = d.leading_hole ? Token.implicit_hole_marker ++ " " : "";
  let before =
    switch (List.rev(l)) {
    | [Piece.Secondary(_), ..._]
    | [] => ""
    | [p, ..._] =>
      switch (SpaceNormalize.last_token(p)) {
      | Some(t) =>
        !List.mem(t, SpaceNormalize.tight_after)
        && (word || SpaceNormalize.needs_space(t, d.text))
          ? " " : ""
      | None => word ? " " : ""
      }
    };
  let after =
    switch (r) {
    | [Piece.Secondary(_), ..._] =>
      Option.is_some(d.trailing_hole) && word ? " " : ""
    | [p, ..._] =>
      switch (SpaceNormalize.first_token(p)) {
      | Some(t) => SpaceNormalize.needs_space(d.text, t) || word ? " " : ""
      | None => word ? " " : ""
      }
    | [] => word ? " " : ""
    };
  (before ++ hole, after);
};

let tab_text =
    (z: Zipper.t, ins: CanonicalCompletion.insertion): option(string) =>
  switch (ins.delimiters) {
  | [] => None
  | [d, ..._] =>
    switch (d.typed_len) {
    | Some(n) when n < String.length(d.text) =>
      Some(String.sub(d.text, n, String.length(d.text) - n))
    | Some(_) => None
    | None =>
      let (before, after) = padding(z, d);
      Some(before ++ d.text ++ after);
    }
  };

/* An opener: the tile's LEADING shard, not a witness in progress */
let is_opener = (ins: CanonicalCompletion.insertion): bool =>
  switch (ins.delimiters) {
  | [{of_shard: Some((_, 0)), typed_len: None, _}, ..._] => true
  | _ => false
  };

/* What Tab does at this caret — THE dispatch, shared by the editor
   and the tests. Witness remainders use Paste; whole trailing/middle
   delimiters use Next to preserve hole and whitespace positions while
   parsing the shared payload at the caret. Atomic empty forms use Paste
   so their placeholder hole can disappear. An OPENER is
   materialized by the engine instead: typing `(` pairs it with the
   most recently stranded `)` (backpack order), not with the closer
   the bubble shows it paired with — at `¦?) x ⏎ a)` a typed ( closed
   the outer ), the bubble promised the inner. ApplyCompletion(One)
   lands the engine's own placement. */
let tab_action = (~seg: option(Segment.t)=?, z: Zipper.t): option(Action.t) =>
  switch (chip_at_caret(~seg?, z)) {
  | None => None
  | Some(ins) when is_opener(ins) =>
    switch (ins.delimiters) {
    | [{of_shard: Some((tid, _)), _}, ..._] =>
      Some(Action.ApplyCompletion(One(tid)))
    | _ => None
    }
  | Some({delimiters: [{text, typed_len: None, _}, ..._], _})
      when !fuses_empty(fst(z.relatives.siblings), text) =>
    Some(Action.ApplyCompletion(Next))
  | Some(ins) => tab_text(z, ins) |> Option.map(text => Action.Paste(text))
  };
