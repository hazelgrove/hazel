/* The zipper-facing completion queries: what obligation is the caret
 * pinned to, and what would Tab type for it. Kept apart from
 * CanonicalCompletion so the engine stays segment-in/segment-out
 * (this is the only completion code that reads a Zipper.t). */

open Util;


/* The obligation whose insertion zone contains the caret — the chip
   the caret is visually pinned to (chips pin coincidence-first, so a
   caret anywhere in the inter-content whitespace around an anchor
   coincides with its chip). Tab dispatches this. Zone matching:
   whitespace/grout siblings around the caret match an insertion
   anchored on them from either side; the bounding content pieces
   match only insertions on their caret-facing side. */
let chip_at_caret = (z: Zipper.t): option(CanonicalCompletion.insertion) =>
  switch (z.caret) {
  | Inner(_) => None
  | Outer =>
    let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
    let result = CanonicalCompletion.for_editor(seg);
    let find = (id: Id.t, sides: list(Direction.t)): option(CanonicalCompletion.insertion) =>
      result.insertions
      |> List.find_opt((ins: CanonicalCompletion.insertion) =>
           Id.equal(ins.adjacent_id, id) && List.mem(ins.side, sides)
         );
    let is_content = (p: Piece.t): bool =>
      switch (p) {
      | Secondary(_)
      | Grout(_) => false
      | _ => true
      };
    let rec probe = (ps: list(Piece.t), ~facing: Direction.t) =>
      switch (ps) {
      | [] => None
      | [p, ...rest] =>
        if (is_content(p)) {
          find(Piece.id(p), [facing]);
        } else {
          switch (find(Piece.id(p), [Direction.Left, Direction.Right])) {
          | Some(_) as r => r
          | None => probe(rest, ~facing)
          };
        }
      };
    let (l, r) = z.relatives.siblings;
    switch (probe(List.rev(l), ~facing=Direction.Right)) {
    | Some(_) as hit => hit
    | None => probe(r, ~facing=Direction.Left)
    };
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

/* Tab = "type it for me": the paste text for the chip's next chunk.
   A witness chip pastes the token REMAINDER (no spaces — it merges
   into the typed prefix exactly as typing would); a plain delimiter
   gets a leading space when it would jam against an alphanumeric
   left neighbor and a trailing space when wordish. */
let tab_text = (z: Zipper.t, ins: CanonicalCompletion.insertion): option(string) => {
  let alnum = Token.is_wordish_char;
  switch (ins.delimiters) {
  | [] => None
  | [d, ..._] =>
    switch (d.typed_len) {
    | Some(n) when n < String.length(d.text) =>
      Some(String.sub(d.text, n, String.length(d.text) - n))
    | Some(_) => None
    | None =>
      let jam_left =
        switch (z.relatives.siblings |> fst |> List.rev) {
        | [Tile({label: [tok], _}), ..._] when Token.length(tok) > 0 =>
          alnum(tok.[Token.length(tok) - 1]) && alnum(d.text.[0])
        | _ => false
        };
      let wordish_last = alnum(d.text.[String.length(d.text) - 1]);
      Some((jam_left ? " " : "") ++ d.text ++ (wordish_last ? " " : ""));
    }
  };
};

