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
     4. ZIPS that segment against the source token stream: token texts
        must match 1:1, and the source gaps are re-inserted as Secondary
        pieces at the position where the next token is emitted.

   The zip is the safety argument: Menhir never defines meaning — any
   token mismatch (unsupported form, printer divergence, exotic lexeme)
   returns None and the caller falls back to the typing parser. On
   success the spliced tokens are the source's own, molds come from
   ExpToSegment and the splice-time remold, and the next MakeTerm pass
   re-reads the segment as usual — so an accept can not corrupt meaning,
   only preserve it. Source formatting survives verbatim. */

exception Mismatch;

/* Why the last of_text call bailed — fallback telemetry and debugging. */
let bail_note: ref(option(string)) = ref(None);
let note = msg => bail_note := Some(msg);

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
    | exception _ => None
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

let zip =
    (~materialize: materialize, tokens: list(tok), seg: Segment.t): Segment.t => {
  let toks = Array.of_list(tokens);
  let idx = ref(0);
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
    if (t.text == text || float_equal_toks(t.text, text)) {
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
  let trigger_is_projector = (trigger: string): bool =>
    /* Token splits the ^^ prefix and the _sidebar placement suffix;
       any @-suffix (argument syntax) comes off the remaining name. */
    switch (
      {
        let (name, _placement) =
          switch (Token.of_projector_invoke_parts(trigger)) {
          | Some(parts) => parts
          | None => raise(Mismatch)
          };
        let name =
          switch (String.index_opt(name, '@')) {
          | Some(i) => String.sub(name, 0, i)
          | None => name
          };
        Language.ProjectorKind.of_name(name);
      }
    ) {
    | kind => !Language.ProjectorKind.is_refractor(kind)
    | exception _ => false
    };
  let rec zip_seg = (seg: Segment.t): Segment.t => {
    switch (seg) {
    | [] => []
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
          | [q, ...qs] => grab(qs, List.rev(zip_piece(q)) @ acc)
          };
        };
      let (wrapped, rest') = grab([p, ...rest], []);
      let (close_gap, _) = expect(")");
      let inner = wrapped @ gap_pieces(close_gap);
      switch (materialize(trigger, inner)) {
      | Some(proj) =>
        /* bind before recursing: @ and cons evaluate right-to-left, and
           token consumption must follow piece order */
        let tail = zip_seg(rest');
        gap_pieces(trig_gap) @ [proj, ...tail];
      | None =>
        note("trigger '" ++ trigger ++ "' could not materialize");
        raise(Mismatch);
      };
    | [p, ...rest] =>
      let head = zip_piece(p);
      head @ zip_seg(rest);
    };
  }
  and zip_piece = (p: Piece.t): list(Piece.t) =>
    switch (p) {
    /* PreserveExact with empty annotations emits no secondaries; drop
       defensively if any appear. */
    | Secondary(_) => []
    | Grout(g) =>
      let (gap, _) = expect("?");
      gap_pieces(gap) @ [Piece.Grout(g)];
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
            let child = zip_seg(List.nth(t.children, shard_i - 1));
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
  let zipped = zip_seg(seg);
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
  zipped;
};

let attempt = (~materialize: materialize, text: string): option(Segment.t) =>
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
        };
      switch (ExpToSegment.exp_to_segment(~settings, term)) {
      | exception _ => None
      | seg =>
        switch (zip(~materialize, tokens, seg)) {
        | exception _ => None
        | zipped => Some(zipped @ gap_pieces(trailing_gap))
        }
      };
    }
  };

/* Binding-chain fragments (agent insert convention: "let y = 2 in") are
   not complete programs; complete with a hole, then strip exactly the
   space+grout we appended. */
let strip_appended_hole = (seg: Segment.t): option(Segment.t) =>
  switch (List.rev(seg)) {
  | [Piece.Grout({shape: Convex, _}), Piece.Secondary(sp), ...rest]
      when Secondary.is_space(sp) =>
    Some(List.rev(rest))
  | _ => None
  };

let of_text =
    (~materialize: materialize=(_, _) => None, ~root: Sort.t, text: string)
    : option(Segment.t) => {
  bail_note := None;
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
