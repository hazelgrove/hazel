open Util;

/* Linear-time text→segment for agent chunk inserts.

   The simulated-typing parser (Parser.to_segment) is quadratic in chunk
   size — each char pays a remold/regrout over the chunk-so-far — which
   reads as a hung editor on multi-KB chunks. This fast path:

     1. lexes the source once, keeping every token's text and the gap
        (whitespace/comments) before it;
     2. parses with the Menhir batch parser (linear);
     3. renders the term through ExpToSegment in PreserveExact +
        Structural mode, which with empty annotations yields a segment
        whose leaves are EXACTLY the program's tokens, no synthesized
        whitespace or parens;
     4. WEAVES the source back into that scaffold: token texts must
        match 1:1, and the source gaps are re-inserted as Secondary
        pieces at the position where the next token is emitted.

   The weave is the safety argument: any token mismatch (unsupported
   form, printer divergence, exotic lexeme) returns None and the caller
   falls back to the typing parser. On success the landed tokens are
   the source's own; MakeTerm re-reads the segment as usual, and the
   differential suites (corpus, fuzz, roundtrip) pin menhir-parse ≡
   editor-parse. Source formatting survives verbatim. */

exception Mismatch;

/* Why the last of_text call bailed — fallback telemetry and debugging. */
let bail_note: ref(option(string)) = ref(None);
let note = msg => bail_note := Some(msg);

/* Refractor triggers (^^probe/^^statics, optionally ^^kind@opt)
   consumed by the last of_text call when ~collect_refractors was set:
   (target term id, verbatim trigger token). The caller re-applies them
   as manual refractor entries on the unzipped zipper (the token is
   parsed by Triggers.refractor_of_invoke_token — FastParse stays below
   the action layer). */
let collected_refractors: ref(list((Id.t, string))) = ref([]);

type tok = {
  gap: string, /* whitespace/comments between previous token and this */
  text: string,
};

/* Tokenize with the SAME lexer the Menhir parse uses, so tokenization
   can not disagree with the parse. */
let lex_with_gaps = (src: string): option((list(tok), string)) => {
  MenhirParser.Lexer.reset_delims();
  let lexbuf = Lexing.from_string(src);
  let toks = ref([]);
  let prev_end = ref(0);
  let rec go = () => {
    switch (MenhirParser.Lexer.token(lexbuf)) {
    | MenhirParser.Parser.EOF =>
      Some((
        List.rev(toks^),
        String.sub(src, prev_end^, String.length(src) - prev_end^),
      ))
    | _ =>
      let start = Lexing.lexeme_start(lexbuf);
      let stop = Lexing.lexeme_end(lexbuf);
      let gap = String.sub(src, prev_end^, start - prev_end^);
      toks :=
        [
          {
            gap,
            text: Lexing.lexeme(lexbuf),
          },
          ...toks^,
        ];
      prev_end := stop;
      go();
    | exception e =>
      note("lex: " ++ Printexc.to_string(e));
      None;
    };
  };
  go();
};

/* A gap string becomes Secondary pieces: comments whole, whitespace
   char-by-char (matching what typing produces). */
let gap_pieces = (gap: string): list(Piece.t) => {
  let n = String.length(gap);
  let rec go = (i: int, acc: list(Piece.t)) =>
    if (i >= n) {
      List.rev(acc);
    } else {
      switch (gap.[i]) {
      | ' '
      | '\t' =>
        go(i + 1, [Piece.Secondary(Secondary.mk_space(Id.mk())), ...acc])
      | '\n' =>
        go(i + 1, [Piece.Secondary(Secondary.mk_newline(Id.mk())), ...acc])
      | '\r' => go(i + 1, acc)
      | '#' =>
        /* single-line comment: consume through the closing # */
        let j = ref(i + 1);
        while (j^ < n && gap.[j^] != '#' && gap.[j^] != '\n') {
          incr(j);
        };
        if (j^ < n && gap.[j^] == '#') {
          let comment = String.sub(gap, i, j^ - i + 1);
          go(
            j^ + 1,
            [Piece.Secondary(Secondary.mk(Id.mk(), comment)), ...acc],
          );
        } else {
          raise(Mismatch);
        };
      | _ => raise(Mismatch)
      };
    };
  go(0, []);
};

/* Materializes a projector trigger: gets the full trigger token
   ("^^livelit") and the zipped syntax it wraps, returns the projector
   piece — or None to bail. Passed in by callers (Triggers.invoked_projector)
   to keep FastParse below the action layer. */
type materialize = (string, Segment.t) => option(Piece.t);

/* Weave the source's tokens and gaps into the printed scaffold.
   Whitespace is deliberately NEVER attached to terms on this path:
   gaps land positionally (before the next token; inside the child when
   that token is a closing shard), so MakeTerm's secondary-attachment
   policy remains the codebase's single implementation of "which term
   owns which whitespace" — a term-side copy here would have to be kept
   in agreement with it forever. (Not the zipper's zip/unzip.) */
let weave =
    (
      ~materialize: materialize,
      ~collect_refractors: bool,
      tokens: list(tok),
      seg: Segment.t,
    )
    : Segment.t => {
  let toks = Array.of_list(tokens);
  let idx = ref(0);
  /* PROVENANCE RETIREMENT LIST — these token equivalences (and the
     hole_tiles printer setting) exist only because the menhir AST loses
     concrete syntax: atom spellings, optional tokens, display-flag ids.
     When completion-provenance (lexeme/shard retention) lands, each one
     deletes; if this list grows past a handful, flip priorities and
     land provenance first. Members: float spellings, label quoting,
     optional leading sum +, nullary f() vs f(()). */
  /* Float literals lose their source spelling through the menhir AST
     (the printer emits e.g. "400.000000" for source "400.0"). Accept
     value-equal float spellings — the SOURCE token is what lands, so
     MakeTerm re-reads the source spelling and meaning is preserved.
     Both sides must be float-syntax (dot/exponent): an int/float pair
     is a genuine sort difference and must still mismatch. */
  let float_syntax = (s: string): bool =>
    String.exists(c => c == '.' || c == 'e' || c == 'E', s)
    && Option.is_some(float_of_string_opt(s));
  let float_equal_toks = (a: string, b: string): bool =>
    float_syntax(a)
    && float_syntax(b)
    && float_of_string(a) == float_of_string(b);
  /* The printer quotes labels only when necessary; a source label may
     carry backticks the reprint drops. Source spelling lands. */
  let unquote = (s: string): option(string) =>
    String.length(s) >= 2 && s.[0] == '`' && s.[String.length(s) - 1] == '`'
      ? Some(String.sub(s, 1, String.length(s) - 2)) : None;
  let label_equal_toks = (src: string, printed: string): bool =>
    switch (unquote(src), unquote(printed)) {
    | (Some(s), None) => s == printed
    | (None, Some(p)) => src == p
    | _ => false
    };
  /* Returns (gap before the token, the SOURCE spelling that matched).
     The source spelling is what must land in the zipped piece: it equals
     the segment's token except through the float-equivalence accept. */
  let expect = (text: string): (string, string) => {
    if (idx^ >= Array.length(toks)) {
      note(
        "segment expects '" ++ text ++ "' but source tokens are exhausted",
      );
      raise(Mismatch);
    };
    let t = toks[idx^];
    if (t.text == text
        || float_equal_toks(t.text, text)
        || label_equal_toks(t.text, text)) {
      incr(idx);
      (t.gap, t.text);
    } else if (idx^
               + 1 < Array.length(toks)
               && toks[idx^ + 1].gap == ""
               && t.text
               ++ toks[idx^ + 1].text == text) {
      /* the segment fuses adjacent source tokens into one (e.g. the
         empty list "[]" vs lexed "[", "]") — accept when gapless */
      idx := idx^ + 2;
      (t.gap, text);
    } else {
      note(
        "token "
        ++ string_of_int(idx^)
        ++ ": segment has '"
        ++ text
        ++ "', source has '"
        ++ t.text
        ++ "'",
      );
      raise(Mismatch);
    };
  };
  /* Fast-path half of the `¿` convention (MarkerParse is the
     recovering-parser half): markers become Grout directly in the weave. */
  let implicit_hole = Token.implicit_hole_marker;
  let expect_hole = (): (string, string) => {
    if (idx^ >= Array.length(toks)) {
      note("segment expects a hole but source tokens are exhausted");
      raise(Mismatch);
    };
    let t = toks[idx^];
    if (t.text == "?" || t.text == implicit_hole) {
      incr(idx);
      (t.gap, t.text);
    } else {
      note(
        "token "
        ++ string_of_int(idx^)
        ++ ": segment has a hole, source has '"
        ++ t.text
        ++ "'",
      );
      raise(Mismatch);
    };
  };
  let peek = (k: int): option(string) =>
    idx^ + k < Array.length(toks) ? Some(toks[idx^ + k].text) : None;
  /* Projector trigger in the source (^^kind( ... )): the menhir parse
     dropped it, so the segment holds the bare wrapped term. Consume the
     trigger tokens around the corresponding pieces and materialize the
     projector piece. The first unmatched `)` closes the trigger: inner
     parens are consumed symmetrically by the wrapped pieces. */
  let is_trigger_next = (): bool =>
    switch (peek(0), peek(1)) {
    | (Some(t), Some("(")) =>
      String.length(t) > 2 && String.sub(t, 0, 2) == "^^"
    | _ => false
    };
  /* Refractor triggers (^^probe / ^^statics) are decorations added
     through the zipper's refractor path, not projector pieces — bail so
     the typing parser's trigger machinery handles them. Unknown kinds
     bail too (of_name raises). */
  let trigger_kind = (trigger: string): option(Language.ProjectorKind.t) =>
    switch (
      {
        let name =
          switch (Token.of_projector_invoke_base(trigger)) {
          | Some(name) => name
          | None => raise(Mismatch)
          };
        Language.ProjectorKind.of_name(name);
      }
    ) {
    | kind => Some(kind)
    | exception _ => None
    };
  let trigger_is_refractor = (trigger: string): bool =>
    switch (trigger_kind(trigger)) {
    | Some(kind) => Language.ProjectorKind.is_refractor(kind)
    | None => false
    };
  let trigger_is_projector = (trigger: string): bool =>
    /* Token splits the ^^ prefix and the _sidebar placement suffix;
       any @-suffix (argument syntax) comes off the remaining name. */
    switch (
      {
        let name =
          switch (Token.of_projector_invoke_base(trigger)) {
          | Some(name) => name
          | None => raise(Mismatch)
          };
        Language.ProjectorKind.of_name(name);
      }
    ) {
    | kind => !Language.ProjectorKind.is_refractor(kind)
    | exception _ => false
    };
  let rec weave_seg = (seg: Segment.t): Segment.t => {
    switch (seg) {
    | [] => []
    | [p, ...rest]
        when
          collect_refractors
          && is_trigger_next()
          && trigger_is_refractor(toks[idx^].text) =>
      /* refractor decoration: consume the trigger, splice the wrapped
         pieces bare, and record the target for the caller to re-pin */
      let trigger = toks[idx^].text;
      let (trig_gap, _) = expect(trigger);
      let (paren_gap, _) = expect("(");
      if (paren_gap != "") {
        raise(Mismatch);
      };
      let rec grab = (ps: Segment.t, acc: Segment.t) =>
        if (peek(0) == Some(")")) {
          (List.rev(acc), ps);
        } else {
          switch (ps) {
          | [] => raise(Mismatch)
          | [q, ...qs] => grab(qs, List.rev(weave_piece(q)) @ acc)
          };
        };
      let (wrapped, rest') = grab([p, ...rest], []);
      let (close_gap, _) = expect(")");
      let inner = wrapped @ gap_pieces(close_gap);
      collected_refractors :=
        [
          (Segment.root_id(Segment.skel(inner), inner), trigger),
          ...collected_refractors^,
        ];
      /* bind before recursing: token consumption must follow order */
      let tail = weave_seg(rest');
      gap_pieces(trig_gap) @ inner @ tail;
    | [p, ...rest]
        when is_trigger_next() && trigger_is_projector(toks[idx^].text) =>
      let trigger = toks[idx^].text;
      let (trig_gap, _) = expect(trigger);
      let (paren_gap, _) = expect("(");
      if (paren_gap != "") {
        raise(
          Mismatch /* triggers are written ^^kind( adjacent */
        );
      };
      let rec grab = (ps: Segment.t, acc: Segment.t) =>
        if (peek(0) == Some(")")) {
          (List.rev(acc), ps);
        } else {
          switch (ps) {
          | [] => raise(Mismatch)
          | [q, ...qs] => grab(qs, List.rev(weave_piece(q)) @ acc)
          };
        };
      let (wrapped, rest') = grab([p, ...rest], []);
      let (close_gap, _) = expect(")");
      let inner = wrapped @ gap_pieces(close_gap);
      switch (materialize(trigger, inner)) {
      | Some(proj) =>
        /* bind before recursing: @ and cons evaluate right-to-left, and
           token consumption must follow piece order */
        let tail = weave_seg(rest');
        gap_pieces(trig_gap) @ [proj, ...tail];
      | None =>
        note("trigger '" ++ trigger ++ "' could not materialize");
        raise(Mismatch);
      };
    | [Tile({label: ["+"], mold, _}), ...rest]
        when mold.out == Sort.Typ && peek(0) != Some("+") =>
      /* the printer emits a leading + on sums; the source may not have
         one (both spellings read as the same Sum) — skip the piece */
      weave_seg(rest)
    | [
        Tile({
          label: ["(", ")"],
          children: [[Tile({label: ["()"], _}) as unit_tile]],
          _,
        }),
        ...rest,
      ]
        when peek(0) == Some("()") =>
      /* nullary ap: the term prints f(()) (the nullary flag id is lost
         to fresh annotations); the source spells f() — land the POSTFIX
         empty-ap tile (operand-molded unit would read as juxtaposition) */
      let (gap, _) = expect("()");
      let form =
        switch (unit_tile) {
        | Tile({mold, _}) when mold.out == Sort.Pat => Form.get(ApPatEmpty)
        | _ => Form.get(ApExpEmpty)
        };
      let tail = weave_seg(rest);
      gap_pieces(gap) @ [Piece.mk_tile(form, []), ...tail];
    | [p, ...rest] =>
      let head = weave_piece(p);
      head @ weave_seg(rest);
    };
  }
  and weave_piece = (p: Piece.t): list(Piece.t) =>
    switch (p) {
    /* PreserveExact with empty annotations emits no secondaries; drop
       defensively if any appear. */
    | Secondary(_) => []
    | Grout(g) =>
      /* residual structural grout: either hole spelling is acceptable */
      let (gap, _) = expect_hole();
      gap_pieces(gap) @ [Piece.Grout(g)];
    | Tile({label: ["?"], _} as t) =>
      /* source `?` keeps the explicit tile; `¿` means implicit Grout */
      let (gap, tok) = expect_hole();
      gap_pieces(gap)
      @ [
        tok == implicit_hole
          ? Piece.Grout({
              id: t.id,
              shape: Convex,
            })
          : Piece.Tile(t),
      ];
    | Projector(_) => raise(Mismatch)
    | Tile(t) =>
      if (List.length(t.shards) != List.length(t.label)) {
        raise(Mismatch);
      };
      let (gap0, tok0) = expect(List.hd(t.label));
      let (children, label_rev, _) =
        List.fold_left(
          ((children_acc, label_acc, shard_i), label_tok) => {
            /* child shard_i-1 sits between shard_i-1 and shard_i; the
               gap before the closing token belongs inside the child */
            let child = weave_seg(List.nth(t.children, shard_i - 1));
            let (gap, tok) = expect(label_tok);
            (
              children_acc @ [child @ gap_pieces(gap)],
              [tok, ...label_acc],
              shard_i + 1,
            );
          },
          ([], [tok0], 1),
          List.tl(t.label),
        );
      gap_pieces(gap0)
      @ [
        Piece.Tile({
          ...t,
          label: List.rev(label_rev),
          children,
        }),
      ];
    };
  let woven = weave_seg(seg);
  if (idx^ != Array.length(toks)) {
    note(
      "segment ended with "
      ++ string_of_int(Array.length(toks) - idx^)
      ++ " source tokens unconsumed (next: '"
      ++ toks[idx^].text
      ++ "')",
    );
    raise(Mismatch);
  };
  woven;
};

let attempt =
    (~materialize: materialize, ~collect_refractors: bool, text: string)
    : option(Segment.t) =>
  switch (lex_with_gaps(text)) {
  | None => None
  | Some((tokens, trailing_gap)) =>
    switch (MenhirParser.Interface.parse_program(text)) {
    | exception e =>
      note("menhir: " ++ Printexc.to_string(e));
      None;
    | ast =>
      let term =
        Language.Grammar.map_exp_annotation(
          _ => Language.IdTagged.IdTag.fresh(),
          MenhirParser.Conversion.Exp.of_menhir_ast(ast),
        );
      let settings =
        ExpToSegment.Settings.{
          ...ExpToSegment.Settings.editable(~inline=true),
          secondary: PreserveExact,
          parenthesization: Structural,
          /* a source `?` lands as the explicit hole TILE; `¿` (the
             TextRoundtrip marker, lexed like ?) becomes Grout in zip */
          hole_tiles: true,
        };
      switch (ExpToSegment.exp_to_segment(~settings, term)) {
      | exception _ => None
      | seg =>
        switch (weave(~materialize, ~collect_refractors, tokens, seg)) {
        | exception _ => None
        | woven => Some(woven @ gap_pieces(trailing_gap))
        }
      };
    }
  };

/* Binding-chain fragments (agent insert convention: "let y = 2 in") are
   not complete programs; complete with a hole, then strip exactly the
   space+grout we appended. */
let strip_appended_hole = (seg: Segment.t): option(Segment.t) =>
  switch (List.rev(seg)) {
  | [Piece.Tile({label: ["?"], _}), Piece.Secondary(sp), ...rest]
      when Secondary.is_space(sp) =>
    Some(List.rev(rest))
  | _ => None
  };

let of_text =
    (
      ~materialize: materialize=(_, _) => None,
      ~collect_refractors: bool=false,
      ~root: Sort.t,
      text: string,
    )
    : option(Segment.t) => {
  bail_note := None;
  collected_refractors := [];
  let attempt = attempt(~collect_refractors);
  if (root == Sort.Mod) {
    /* Module-member chunks (update_binding_clause on a member): parse
       as a braced module body, then unwrap the brace tile. Chunks with
       spliced separators (leading/trailing ;) fail the wrap parse and
       fall back — they are small. */
    switch (attempt(~materialize, "{" ++ String.trim(text) ++ "}")) {
    | Some([Tile({label: ["{", "}"], children: [inner], _})]) =>
      Some(inner)
    | _ =>
      note("mod-root wrap did not yield a single module body");
      None;
    };
  } else if (root != Sort.Exp) {
    None;
  } else {
    switch (attempt(~materialize, text)) {
    | Some(seg) => Some(seg)
    | None =>
      /* keep the first attempt's bail note: the retry's failure mode
         (usually "menhir: parse error on text + ?") is less telling */
      let first_note = bail_note^;
      switch (attempt(~materialize, text ++ " ?")) {
      | Some(seg) => strip_appended_hole(seg)
      | None =>
        bail_note := first_note;
        None;
      };
    };
  };
};
