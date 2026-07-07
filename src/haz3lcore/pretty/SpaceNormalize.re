/* Insert spacing at bare token junctions in a printed segment.
 *
 * In a PARSED buffer, two adjacent tokens with nothing between them
 * cannot occur — the lexer would have merged them at typing time. So
 * any junction with no secondary in roundtrip-printed output is
 * provably a boundary of SYNTHESIZED syntax (refactorings, agent
 * edits), and this pass can be generous there without ever touching
 * user-authored spacing. */

let tight_after = ["(", "[", "^^", "@<", "."];
let tight_before = [")", "]", ",", ";", ">", "."];

let is_wordish = (t: Token.t): bool =>
  Token.length(t) > 0
  && (
    switch (t.[0]) {
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_' => true
    | _ => false
    }
  );

/* Self-delimiting punctuation: adjacency without spacing is normal in
 * user code (`(a)(b)`, `f(x)`), so never treat it as synthesized */
let self_delim = ["(", ")", "[", "]", ",", ";", "{", "}"];

let is_symbolic = (t: Token.t): bool =>
  !is_wordish(t) && !List.mem(t, self_delim);

/* Tokens that always deserve surrounding space when synthesized next
 * to something (keyword forms, rule delimiters) */
let spaced = (t: Token.t): bool =>
  Token.is_keyword(t) || List.mem(t, ["|", "=>"]);

let needs_space = (prev: Token.t, next: Token.t): bool =>
  if (List.mem(prev, tight_after) || List.mem(next, tight_before)) {
    false;
  } else if (spaced(prev) || spaced(next)) {
    true;
  } else if (is_wordish(prev) && is_wordish(next)) {
    true;
        /* would lex as one token */
  } else if (is_symbolic(prev) && is_symbolic(next)) {
    true;
        /* could lex as a longer operator */
  } else {
    false;
  };

let space = () => Piece.secondary(Secondary.mk_space(Id.mk()));

/* Last/first token of a piece, textually */
let last_token = (p: Piece.t): option(Token.t) =>
  switch (p) {
  | Tile(t) =>
    let l = Tile.effective_label(t);
    l == [] ? None : Some(List.nth(l, List.length(l) - 1));
  | _ => None
  };
let first_token = (p: Piece.t): option(Token.t) =>
  switch (p) {
  | Tile(t) =>
    switch (Tile.effective_label(t)) {
    | [tok, ..._] => Some(tok)
    | [] => None
    }
  | _ => None
  };

/* Normalize one level: insert a space between adjacent tile pieces
 * whose junction has no secondary; recurse into children, including
 * the shard<->child junctions (a tile's child segment sits between
 * two shard tokens) */
let rec go = (seg: Segment.t): Segment.t =>
  switch (seg) {
  | [] => []
  | [p] => [normalize_piece(p)]
  | [p1, p2, ...rest] =>
    let p1 = normalize_piece(p1);
    switch (last_token(p1), first_token(p2)) {
    | (Some(a), Some(b)) when needs_space(a, b) => [
        p1,
        space(),
        ...go([p2, ...rest]),
      ]
    | _ => [p1, ...go([p2, ...rest])]
    };
  }
and normalize_piece = (p: Piece.t): Piece.t =>
  switch (p) {
  | Tile(t) =>
    let shards_tokens = Tile.effective_label(t);
    let children =
      t.children
      |> List.mapi((i, child) => {
           let child = go(child);
           /* pad the child against its surrounding shards */
           let left = List.nth_opt(shards_tokens, i);
           let right = List.nth_opt(shards_tokens, i + 1);
           let child =
             switch (child, left) {
             | ([Piece.Tile(_), ..._], Some(l)) =>
               switch (first_token(List.hd(child))) {
               | Some(b) when needs_space(l, b) => [space(), ...child]
               | _ => child
               }
             | _ => child
             };
           switch (List.rev(child), right) {
           | ([Piece.Tile(_) as lastp, ...revrest], Some(r)) =>
             switch (last_token(lastp)) {
             | Some(a) when needs_space(a, r) =>
               List.rev([space(), lastp, ...revrest])
             | _ => child
             }
           | _ => child
           };
         });
    Tile({
      ...t,
      children,
    });
  | p => p
  };
