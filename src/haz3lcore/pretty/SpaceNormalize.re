/* Insert spacing at bare token junctions in a printed segment.
 *
 * In a PARSED buffer, two adjacent tokens with nothing between them
 * cannot occur — the lexer would have merged them at typing time. So
 * any junction with no secondary in roundtrip-printed output is
 * provably a boundary of SYNTHESIZED syntax (refactorings, agent
 * edits), and this pass can be generous there without ever touching
 * user-authored spacing.
 *
 * With ~canonicalize=true the pass additionally rewrites pure-space
 * runs between two tokens to policy width (one space or none). Runs
 * touching linebreaks (indentation, line-trailing spaces), comments,
 * or grout are left alone, so line structure and comments are never
 * affected. Canonicalize is opt-in (explicit Format action only). */

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

/* Junctions where canonicalization deletes spacing entirely */
let tight_junction = (prev: Token.t, next: Token.t): bool =>
  List.mem(prev, tight_after) || List.mem(next, tight_before);

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

let is_space_piece = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_space(s)
  | _ => false
  };

/* Maximal prefix of space pieces (no linebreaks, no comments) */
let split_space_run = (seg: Segment.t): (Segment.t, Segment.t) => {
  let rec loop = (acc, seg) =>
    switch (seg) {
    | [p, ...rest] when is_space_piece(p) => loop([p, ...acc], rest)
    | _ => (List.rev(acc), seg)
    };
  loop([], seg);
};

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
 * whose junction has no secondary (and, canonicalizing, collapse
 * token-to-token space runs to policy width); recurse into children,
 * including the shard<->child junctions (a tile's child segment sits
 * between two shard tokens) */
let rec go = (~canonicalize=false, seg: Segment.t): Segment.t =>
  switch (seg) {
  | [] => []
  | [p] => [normalize_piece(~canonicalize, p)]
  | [p1, ...rest] =>
    let p1 = normalize_piece(~canonicalize, p1);
    let (run, rest) = canonicalize ? split_space_run(rest) : ([], rest);
    switch (last_token(p1), rest) {
    | (Some(a), [p2, ..._]) =>
      switch (first_token(p2)) {
      | Some(b) =>
        /* an existing run collapses to one space (zero only at tight
           junctions); a bare junction gains a space only where the
           tokens would glom */
        let sep =
          switch (run) {
          | [] => needs_space(a, b) ? [space()] : []
          | [first, ..._] => tight_junction(a, b) ? [] : [first]
          };
        [p1] @ sep @ go(~canonicalize, rest);
      | None => [p1] @ run @ go(~canonicalize, rest)
      }
    | _ => [p1] @ run @ go(~canonicalize, rest)
    };
  }
and normalize_piece = (~canonicalize=false, p: Piece.t): Piece.t =>
  switch (p) {
  | Tile(t) =>
    let shards_tokens = Tile.effective_label(t);
    let children =
      t.children
      |> List.mapi((i, child) => {
           let child = go(~canonicalize, child);
           /* pad the child against its surrounding shards */
           let left = List.nth_opt(shards_tokens, i);
           let right = List.nth_opt(shards_tokens, i + 1);
           let child =
             if (canonicalize) {
               /* collapse a leading space run against the left shard */
               switch (split_space_run(child), left) {
               | (([_, ..._], [Piece.Tile(_) as hd, ...tl]), Some(l)) =>
                 switch (first_token(hd)) {
                 | Some(b) =>
                   (tight_junction(l, b) ? [] : [space()]) @ [hd, ...tl]
                 | None => child
                 }
               | _ => child
               };
             } else {
               child;
             };
           let child =
             switch (child, left) {
             | ([Piece.Tile(_), ..._], Some(l)) =>
               switch (first_token(List.hd(child))) {
               | Some(b) when needs_space(l, b) => [space(), ...child]
               | _ => child
               }
             | _ => child
             };
           let child =
             if (canonicalize) {
               /* collapse a trailing space run against the right shard */
               switch (split_space_run(List.rev(child)), right) {
               | (
                   ([_, ..._], [Piece.Tile(_) as lastp, ...revrest]),
                   Some(r),
                 ) =>
                 switch (last_token(lastp)) {
                 | Some(a) =>
                   List.rev(
                     (tight_junction(a, r) ? [] : [space()])
                     @ [lastp, ...revrest],
                   )
                 | None => child
                 }
               | _ => child
               };
             } else {
               child;
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
