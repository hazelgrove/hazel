/* STYLE padding for system material: how formatted code LOOKS — pad
 * every junction except where a token opens (`f(`) or hugs left
 * (`,` `)` `]`). THE one home: the display padding oracle
 * (CanonicalCompletion, which re-exports these as f1_*) and hole-cell
 * classification (GroutCells, for the trailing pad) both consume it.
 * It lives here in tiles/ because GroutCells cannot reach derived/ —
 * Measured carries a GroutCells.t, so the dependency would cycle.
 *
 * Deliberately NOT SpaceNormalize.needs_space, which was tried and
 * rejected 2026-07-26: that answers a different question. It is a
 * LEXICAL-SAFETY rule (would these two tokens glom into one? is
 * separation required?), intentionally conservative so the printer
 * never disturbs user spacing. It answers false for `,` `?` and for
 * `x` `=`, so reusing it under-pads system material into `f(a,?,?)`
 * and `let x= ? in ?`. Style padding is a superset.
 *
 * They must never CONTRADICT, though: anything the printer deems
 * lexically required must also be padded here, or the display would
 * show two tokens glommed. Test_GroutPlace "padding soundness" pins
 * that implication over a token-pair corpus (P12). */

let hugs_left = (t: string): bool =>
  String.length(t) > 0
  && (
    switch (t.[0]) {
    | ','
    | ')'
    | ']'
    | '}' => true
    | _ => false
    }
  );

let closes = (t: string): bool =>
  String.length(t) > 0
  && (
    switch (t.[String.length(t) - 1]) {
    | ')'
    | ']'
    | '}' => true
    | _ => false
    }
  );

let opens = (t: string): bool =>
  String.length(t) > 0
  && (
    switch (t.[String.length(t) - 1]) {
    | '('
    | '[' => true
    | _ => false
    }
  );

/* the hole token, as the padding rule sees it (a hole is an operand:
 * it never hugs left, so a pad depends only on the token before it) */
let hole_token = "?";

/* P15: junction personality is MOLD-derived where the characters
 * can't carry it. "Prefix operator" (takes an operand right, none
 * left) is a mold fact — `!` and unary `-` reveal nothing at the
 * character level, and the char rule wrongly padded `!` from its
 * operand hole (`! ?`). Callers that hold the piece pass
 * Mold.is_prefix_op; string-only callers get the old rule.
 *
 * The hug is scoped to the HOLE: in display, a prefix operator's
 * operand junction is always hole-mediated (placement mints the
 * operand hole — `!?`), and a REAL operand beside it (`!t`) is an
 * all-user junction the oracle never re-spaces (P5b). Symbolic
 * pairs like `!` `-` keep their pad, so the P12 implication
 * (lexically required => style-padded) is untouched. */
let pad = (~l_prefix: bool=false, lt: string, rt: string): bool =>
  l_prefix && rt == hole_token ? false : !opens(lt) && !hugs_left(rt);
