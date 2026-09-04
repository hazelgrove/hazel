open Util_web;

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
    : (Segment.t, list((Id.t, string))) => {
  let toks = Array.of_list(tokens);
  let idx = ref(0);
  /* Refractor triggers (^^probe/^^statics, optionally ^^kind_opt) consumed
     from the source: (target term id, verbatim trigger token). Accumulated
     alongside idx because both are written deep in weave_seg's recursion,
     and returned so callers never read parser state after the fact — the
     token is parsed back into a kind by Triggers.refractor_of_invoke_token,
     keeping FastParse below the action layer. */
  let refractors = ref([]);
  /* RETIREMENT LIST for the planned completion-provenance work (tiles
     retaining their source lexemes/shards, making print∘parse the
     identity on tokens). These token equivalences (and the hole_tiles
     printer setting) exist only because the menhir AST loses concrete
     syntax: atom spellings, optional tokens, display-flag ids. When
     provenance lands, each one deletes; if this list grows past a
     handful, flip priorities and land provenance first. Members: float
     spellings, label quoting, optional leading sum +, nullary f() vs
     f(()). */
  /* Float literals lose their source spelling through the menhir AST
     (the printer emits e.g. "400.000000" for source "400.0"). Accept
     value-equal float spellings — the SOURCE token is what lands, so
     MakeTerm re-reads the source spelling and meaning is preserved.
     Both sides must be float-syntax (dot/exponent): an int/float pair
     is a genuine sort difference and must still mismatch. */
  let float_value = (s: string): option(float) =>
    String.exists(c => c == '.' || c == 'e' || c == 'E', s)
      ? float_of_string_opt(s) : None;
  let float_equal_toks = (a: string, b: string): bool =>
    switch (float_value(a), float_value(b)) {
    | (Some(x), Some(y)) => x == y
    | _ => false
    };
  /* The printer quotes labels only when necessary; a source label may
     carry backticks the reprint drops. Source spelling lands. (The
     both-plain and both-quoted arms only see unequal spellings — equal
     tokens hit expect's primary equality — but comparing keeps this
     helper total on its own terms.) */
  let unquote = (s: string): option(string) =>
    String.length(s) >= 2 && s.[0] == '`' && s.[String.length(s) - 1] == '`'
      ? Some(String.sub(s, 1, String.length(s) - 2)) : None;
  let label_equal_toks = (src: string, printed: string): bool =>
    switch (unquote(src), unquote(printed)) {
    | (Some(s), None) => s == printed
    | (None, Some(p)) => src == p
    | (None, None)
    | (Some(_), Some(_)) => src == printed
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
    | (Some(t), Some("(")) => Option.is_some(Token.of_projector_invoke(t))
    | _ => false
    };
  /* Refractor triggers (^^probe / ^^statics) are decorations added
     through the zipper's refractor path, not projector pieces — bail so
     the typing parser's trigger machinery handles them. Unknown kinds
     answer None on both predicates. Token splits the ^^ prefix and the
     _opt suffix. */
  let trigger_kind = (trigger: string): option(Language.ProjectorKind.t) =>
    Option.bind(
      Token.of_projector_invoke_base(trigger),
      Language.ProjectorKind.of_name_opt,
    );
  let trigger_is_refractor = (trigger: string): bool =>
    switch (trigger_kind(trigger)) {
    | Some(kind) => Language.ProjectorKind.is_refractor(kind)
    | None => false
    };
  let trigger_is_projector = (trigger: string): bool =>
    switch (trigger_kind(trigger)) {
    | Some(kind) => !Language.ProjectorKind.is_refractor(kind)
    | None => false
    };
  let rec weave_seg = (seg: Segment.t): Segment.t => {
    switch (weave_trigger(seg)) {
    | Some((head, rest)) =>
      /* bind before recursing: token consumption must follow order */
      let tail = weave_seg(rest);
      head @ tail;
    | None =>
      switch (seg) {
      | [] => []
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
      }
    };
  }
  /* Trigger at the head of a segment, shared by weave_seg and by the
     grab inside consume_trigger — so triggers nest (^^probe(^^slider(1))).
     Returns the pieces the trigger contributes and the segment left over;
     None when the next source token is not a trigger this parse handles. */
  and weave_trigger = (seg: Segment.t): option((list(Piece.t), Segment.t)) =>
    switch (seg) {
    | [] => None
    | [_, ..._] when !is_trigger_next() => None
    | [_, ..._]
        when collect_refractors && trigger_is_refractor(toks[idx^].text) =>
      /* refractor decoration: consume the trigger, splice the wrapped
         pieces bare, and record the target for the caller to re-pin */
      let (trigger, trig_gap, inner, rest) = consume_trigger(seg);
      refractors :=
        [
          (Segment.root_id(Segment.skel(inner), inner), trigger),
          ...refractors^,
        ];
      Some((gap_pieces(trig_gap) @ inner, rest));
    | [_, ..._] when trigger_is_projector(toks[idx^].text) =>
      let (trigger, trig_gap, inner, rest) = consume_trigger(seg);
      switch (materialize(trigger, inner)) {
      | Some(proj) => Some((gap_pieces(trig_gap) @ [proj], rest))
      | None =>
        note("trigger '" ++ trigger ++ "' could not materialize");
        raise(Mismatch);
      };
    | [_, ..._] => None
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
      let (children, label_rev) =
        List.fold_left2(
          ((children_acc, label_acc), label_tok, child_seg) => {
            /* each child sits between consecutive shards; the gap
               before the closing token belongs inside the child */
            let child = weave_seg(child_seg);
            let (gap, tok) = expect(label_tok);
            (
              children_acc @ [child @ gap_pieces(gap)],
              [tok, ...label_acc],
            );
          },
          ([], [tok0]),
          List.tl(t.label),
          t.children,
        );
      gap_pieces(gap0)
      @ [
        Piece.Tile({
          ...t,
          label: List.rev(label_rev),
          children,
        }),
      ];
    }
  /* Trigger syntax ^^kind( ... ): consume the trigger token and its
     parens around the woven wrapped pieces. Shared by the refractor and
     projector arms of weave_seg; returns (trigger, gap before it, the
     woven pieces inside the parens incl. the closing gap, remaining
     segment). The first unmatched `)` closes the trigger: inner parens
     are consumed symmetrically by the wrapped pieces. */
  and consume_trigger =
      (seg: Segment.t): (string, string, Segment.t, Segment.t) => {
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
        switch (weave_trigger(ps)) {
        | Some((head, rest)) => grab(rest, List.rev(head) @ acc)
        | None =>
          switch (ps) {
          | [] => raise(Mismatch)
          | [q, ...qs] => grab(qs, List.rev(weave_piece(q)) @ acc)
          }
        };
      };
    let (wrapped, rest) = grab(seg, []);
    let (close_gap, _) = expect(")");
    (trigger, trig_gap, wrapped @ gap_pieces(close_gap), rest);
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
  (woven, refractors^);
};

/* A successful fast parse: the woven segment plus the refractor triggers
   the weave consumed from the source. Each attempt builds its own, so a
   bailed attempt leaves nothing behind for the next one to inherit. */
type parsed = {
  segment: Segment.t,
  refractors: list((Id.t, string)),
};

let attempt =
    (~materialize: materialize, ~collect_refractors: bool, text: string)
    : option(parsed) =>
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
             MarkerParse marker, lexed like ?) becomes Grout in zip */
          hole_tiles: true,
        };
      switch (ExpToSegment.exp_to_segment(~settings, term)) {
      | exception _ => None
      | seg =>
        switch (weave(~materialize, ~collect_refractors, tokens, seg)) {
        | exception _ => None
        | (woven, refractors) =>
          Some({
            segment: woven @ gap_pieces(trailing_gap),
            refractors,
          })
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

/* Full result: the segment plus the refractor triggers the weave consumed.
   Only callers that re-pin those triggers (text-slide loading, paste) need
   this; everything else takes of_text below. Error carries the bail
   reason (the most specific note the attempt recorded), so result-based
   callers never read the bail_note ref. */
let parsed_of_text =
    (
      ~materialize: materialize=(_, _) => None,
      ~collect_refractors: bool=false,
      ~root: Sort.t,
      text: string,
    )
    : result(parsed, string) => {
  bail_note := None;
  let attempt = attempt(~collect_refractors);
  let fail = (): result(parsed, string) =>
    Error(Option.value(bail_note^, ~default="no note"));
  if (root == Sort.Mod) {
    /* Module-member chunks (update_binding_clause on a member): parse
       as a braced module body, then unwrap the brace tile. Chunks with
       spliced separators (leading/trailing ;) fail the wrap parse and
       fall back — they are small. */
    switch (attempt(~materialize, "{" ++ String.trim(text) ++ "}")) {
    | Some({
        segment: [Tile({label: ["{", "}"], children: [inner], _})],
        refractors,
      }) =>
      Ok({
        segment: inner,
        refractors,
      })
    | _ =>
      /* keep the attempt's own note when it bailed; the generic wrap
         message only describes a successful parse of the wrong shape */
      if (bail_note^ == None) {
        note("mod-root wrap did not yield a single module body");
      };
      fail();
    };
  } else if (root != Sort.Exp) {
    note("root sort is not fast-parseable (Exp and Mod only)");
    fail();
  } else {
    switch (attempt(~materialize, text)) {
    | Some(p) => Ok(p)
    | None =>
      /* keep the first attempt's bail note: the retry's failure mode
         (usually "menhir: parse error on text + ?") is less telling */
      let first_note = bail_note^;
      switch (attempt(~materialize, text ++ " ?")) {
      | Some(p) =>
        switch (strip_appended_hole(p.segment)) {
        | Some(segment) =>
          Ok({
            ...p,
            segment,
          })
        | None =>
          bail_note := first_note;
          fail();
        }
      | None =>
        bail_note := first_note;
        fail();
      };
    };
  };
};

let of_text =
    (
      ~materialize: materialize=(_, _) => None,
      ~collect_refractors: bool=false,
      ~root: Sort.t,
      text: string,
    )
    : option(Segment.t) =>
  switch (parsed_of_text(~materialize, ~collect_refractors, ~root, text)) {
  | Ok(p) => Some(p.segment)
  | Error(_) => None
  };
