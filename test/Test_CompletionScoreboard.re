open Util;
open Alcotest;
open Haz3lcore;

/* === Deletion-inverse scoreboard ===
   (plans/completion-heuristics.md, "Scoreboard v2 spec")

   For each corpus program: delete each delimiter shard through the
   real edit pipeline — fully (repair states) and, for multi-char
   delimiters, its last char only (prefix-witness states) — then
   canonically complete and check whether the completion restores the
   original token stream (whitespace/grout-insensitive: restoration
   means the delimiter lands back at its original position among the
   tokens).

   Scores are pinned EXACTLY per program and class: any heuristic
   change that moves a score must update the pin deliberately, and
   the per-failure log (in the alcotest output file) shows which
   states changed. 100% is not the goal — the pins are a ratchet and
   a map of where the heuristics stop. */

let settings = Test_Editing.default_settings; /* ux off: deterministic */

let corpus: list((string, string)) = [
  ("let-chain", "let a = 1 in\nlet b = a + 2 in\na + b"),
  ("fun-ap", "let f = fun x -> x * 2 in\nf(3) + f(4)"),
  ("if-else-inline", "let a = 1 in\nif a < 2 then a else a + 1"),
  ("if-else-multiline", "let a = 1 in\nif a < 2 then a\nelse a + 1"),
  ("case-multiline", "let t = 1 in\ncase t\n| 1 => 2\n| _ => 3\nend"),
  (
    "type-adt",
    "type Shape = Circle + Square(Int) in\nlet s = Circle in\ncase s\n| Circle => 0\n| Square(n) => n\nend",
  ),
  ("tuple-list", "let p = (1, 2 + 3) in\nlet l = [4, 5, 6] in\np"),
  /* case in the let's DEFINITION slot (andrew's end+in repro shape;
     the body-slot case-multiline never exercises it) */
  ("case-def-inline", "let f = case x | 1 => 2 | 3 => 4 end in f"),
  (
    "case-def-multiline",
    "let f =\n  case x\n  | 1 => 2\n  | 3 => 4\n  end in\nf",
  ),
];

let build = (text: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Sort.Exp, text)) {
  | Some(z) => z
  | None => fail("corpus does not parse: " ++ text)
  };

/* Token stream, whitespace/grout-insensitive, shards in place */
let rec tokens = (seg: Segment.t): list(string) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Secondary(_)
      | Grout(_)
      | Projector(_) => []
      | Tile(t) =>
        let rec interleave = (ls, chs) =>
          switch (ls, chs) {
          | ([], _) => []
          | ([l, ...ls], []) => [l, ...interleave(ls, [])]
          | ([l, ...ls], [c, ...chs]) =>
            [l, ...tokens(c)] @ interleave(ls, chs)
          };
        interleave(Tile.effective_label(t), t.children);
      },
    seg,
  );

/* Delimiter-shard targets: every shard of every multi-token tile,
   with the caret point just after its last char */
let targets = (z: Zipper.t): list((Token.t, Point.t)) => {
  let measured = CachedSyntax.init(z).measured;
  let acc = ref([]);
  let rec walk = (sg: Segment.t) =>
    List.iter(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          List.iter(walk, t.children);
          if (List.length(t.label) > 1) {
            switch (Measured.find_shards(~msg="scoreboard", t, measured)) {
            | shards =>
              List.iter(
                ((i, m: Measured.measurement)) =>
                  acc := [(List.nth(t.label, i), m.last), ...acc^],
                shards,
              )
            | exception _ => ()
            };
          };
        | _ => ()
        },
      sg,
    );
  walk(Zipper.unselect_and_zip(z));
  List.rev(acc^);
};

/* Error-tolerant perform: a failed action makes the mutation
   inapplicable rather than failing the suite */
let perform_soft = (z: Zipper.t, acts: list(Action.t)): option(Zipper.t) => {
  let step = (z: Zipper.t, a: Action.t) => {
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let statics =
      CachedStatics.init_from_term(~settings, ~is_dynamic_term=true, term);
    switch (
      Perform.go(
        ~settings,
        ~statics,
        ~syntax=CachedSyntax.init(z),
        a,
        {
          zipper: z,
          col_target: None,
        },
        ~root=Sort.Exp,
      )
    ) {
    | Ok(z) => Some(z)
    | Error(_) => None
    | exception _ => None
    };
  };
  List.fold_left((z, a) => Option.bind(z, z => step(z, a)), Some(z), acts);
};

let completed_tokens = (z: Zipper.t): list(string) => {
  let seg =
    z
    |> Zipper.clear_unparsed_buffer
    |> Zipper.unselect_and_zip(~erase_buffer=true);
  let result = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
  tokens(result.completed_seg);
};

type outcome = {
  restored: int,
  destroyed: int, /* the EDIT destroyed the evidence (token merge) */
  total: int,
};

/* Did the deletion leave the token stream = original minus (or with a
   prefix of) the deleted shard? If not, the edit itself merged or
   reshaped tokens — no visible-state completion can restore, and the
   miss belongs to the editor's delete semantics, not the heuristics. */
let evidence_intact =
    (~replacement: option(string), original, mutated, tok): bool => {
  let rec go = (pre, rest) =>
    switch (rest) {
    | [] => false
    | [x, ...tl] =>
      x == tok
      && List.rev_append(
           pre,
           switch (replacement) {
           | None => tl
           | Some(r) => [r, ...tl]
           },
         )
      == mutated
      || go([x, ...pre], tl)
    };
  go([], original);
};

let run_class = (~prefix_only: bool, name: string, text: string): outcome => {
  let z0 = build(text);
  let original = tokens(Zipper.unselect_and_zip(z0));
  let shards = targets(z0);
  let shards =
    prefix_only
      ? List.filter(((tok, _)) => Token.length(tok) > 1, shards) : shards;
  List.fold_left(
    (acc, (tok, pt: Point.t)) => {
      let k = prefix_only ? 1 : Token.length(tok);
      let acts =
        [Action.Move(Point(pt, None))]
        @ List.init(k, _ => Action.Destruct(Local(Left, ByChar)));
      switch (perform_soft(z0, acts)) {
      | None =>
        print_endline(
          Printf.sprintf(
            "[%s/%s] INAPPLICABLE %s at %d:%d",
            name,
            prefix_only ? "prefix" : "full",
            tok,
            pt.row,
            pt.col,
          ),
        );
        {
          ...acc,
          total: acc.total + 1,
        };
      | Some(z') =>
        let mutated =
          tokens(Zipper.unselect_and_zip(~erase_buffer=true, z'));
        let replacement =
          prefix_only
            ? Some(String.sub(tok, 0, Token.length(tok) - 1)) : None;
        if (!evidence_intact(~replacement, original, mutated, tok)) {
          print_endline(
            Printf.sprintf(
              "[%s/%s] DESTROYED %s at %d:%d -> %s",
              name,
              prefix_only ? "prefix" : "full",
              tok,
              pt.row,
              pt.col,
              String.concat(" ", mutated),
            ),
          );
          {
            ...acc,
            destroyed: acc.destroyed + 1,
            total: acc.total + 1,
          };
        } else {
          let got = completed_tokens(z');
          let ok = got == original;
          if (!ok) {
            print_endline(
              Printf.sprintf(
                "[%s/%s] MISS %s at %d:%d -> %s",
                name,
                prefix_only ? "prefix" : "full",
                tok,
                pt.row,
                pt.col,
                String.concat(" ", got),
              ),
            );
          };
          {
            ...acc,
            restored: acc.restored + (ok ? 1 : 0),
            total: acc.total + 1,
          };
        };
      };
    },
    {
      restored: 0,
      destroyed: 0,
      total: 0,
    },
    shards,
  );
};

/* === Pair-deletion class (joint satisfiability instrument) ===
   Delete the CLOSING shard of two different tiles (rightmost first,
   so the left target's coordinates stay valid), complete jointly,
   and check (a) restoration and (b) andrew's property: the joint
   result is delimiter-complete (no incomplete tiles survive).
   Measures the compatibility of co-broken completions — the class
   the single-deletion scoreboard can't see. */

let closer_targets = (z: Zipper.t): list((Token.t, Point.t)) => {
  let measured = CachedSyntax.init(z).measured;
  let acc = ref([]);
  let rec walk = (sg: Segment.t) =>
    List.iter(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          List.iter(walk, t.children);
          let n = List.length(t.label);
          if (n > 1) {
            switch (Measured.find_shards(~msg="scoreboard", t, measured)) {
            | shards =>
              List.iter(
                ((i, m: Measured.measurement)) =>
                  if (i == n - 1) {
                    acc := [(List.nth(t.label, i), m.last), ...acc^];
                  },
                shards,
              )
            | exception _ => ()
            };
          };
        | _ => ()
        },
      sg,
    );
  walk(Zipper.unselect_and_zip(z));
  List.rev(acc^);
};

type joint_outcome = {
  j_restored: int,
  j_incomplete: int, /* joint result NOT delimiter-complete */
  j_total: int,
};

let run_pairs = (name: string, text: string): joint_outcome => {
  let z0 = build(text);
  let original = tokens(Zipper.unselect_and_zip(z0));
  let closers = closer_targets(z0);
  let lt = (a: Point.t, b: Point.t) =>
    a.row < b.row || a.row == b.row && a.col < b.col;
  let pairs =
    closers
    |> List.concat_map(((t1, p1)) =>
         closers
         |> List.filter_map(((t2, p2)) =>
              lt(p1, p2) ? Some(((t1, p1), (t2, p2))) : None
            )
       );
  List.fold_left(
    (acc, ((tok_l, pt_l: Point.t), (tok_r, pt_r: Point.t))) => {
      let del = tok =>
        List.init(Token.length(tok), _ =>
          Action.Destruct(Action.Local(Left, ByChar))
        );
      let acts =
        [Action.Move(Point(pt_r, None))]
        @ del(tok_r)
        @ [Action.Move(Point(pt_l, None))]
        @ del(tok_l);
      let where =
        Printf.sprintf(
          "%s@%d:%d + %s@%d:%d",
          tok_l,
          pt_l.row,
          pt_l.col,
          tok_r,
          pt_r.row,
          pt_r.col,
        );
      switch (perform_soft(z0, acts)) {
      | None =>
        print_endline(
          Printf.sprintf("[%s/pair] INAPPLICABLE %s", name, where),
        );
        {
          ...acc,
          j_total: acc.j_total + 1,
        };
      | Some(z') =>
        let seg =
          z'
          |> Zipper.clear_unparsed_buffer
          |> Zipper.unselect_and_zip(~erase_buffer=true);
        let result =
          CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
        let got = tokens(result.completed_seg);
        let ok = got == original;
        let inc =
          Segment.incomplete_tiles_deep(result.completed_seg) |> List.length;
        if (!ok || inc > 0) {
          print_endline(
            Printf.sprintf(
              "[%s/pair] %s %s -> %s",
              name,
              inc > 0 ? "INCOMPLETE" : "MISS",
              where,
              String.concat(" ", got),
            ),
          );
        };
        {
          j_restored: acc.j_restored + (ok ? 1 : 0),
          j_incomplete: acc.j_incomplete + (inc > 0 ? 1 : 0),
          j_total: acc.j_total + 1,
        };
      };
    },
    {
      j_restored: 0,
      j_incomplete: 0,
      j_total: 0,
    },
    pairs,
  );
};

/* (program, "restored/incomplete/total") — PINNED like the others */
let pair_pins = [
  ("let-chain", "1/0/1"),
  ("fun-ap", "3/0/6"), /* all 3 inherit the known )@f(3 single miss */
  ("if-else-inline", "1/0/1"),
  ("if-else-multiline", "1/0/1"),
  ("case-multiline", "6/0/6"),
  ("type-adt", "20/1/21"), /* the 1: => drops inside Square(n's parens */
  ("tuple-list", "6/0/6"),
  ("case-def-inline", "5/1/6"), /* the 1: end+in, the andrew repro */
  ("case-def-multiline", "5/1/6"),
];

let pair_tests =
  pair_pins
  |> List.map(((name, pin)) =>
       test_case(
         name ++ " pairs",
         `Slow,
         () => {
           let text = List.assoc(name, corpus);
           let o = run_pairs(name, text);
           let shown =
             Printf.sprintf(
               "%d/%d/%d",
               o.j_restored,
               o.j_incomplete,
               o.j_total,
             );
           print_endline(Printf.sprintf("SCORE %s pairs: %s", name, shown));
           check(string, name ++ " pairs", pin, shown);
         },
       )
     );

/* (program, full "restored/destroyed/total", prefix same) — PINNED:
   update deliberately when heuristics (or delete semantics) change.
   `destroyed` = the edit merged tokens; not a completion miss. */
let pins = [
  ("let-chain", "5/0/6", "4/0/4"),
  ("fun-ap", "6/2/9", "3/0/4"),
  ("if-else-inline", "5/0/6", "5/0/5"),
  ("if-else-multiline", "5/0/6", "5/0/5"),
  ("case-multiline", "8/0/9", "4/0/6"),
  ("type-adt", "12/2/16", "6/0/8"),
  ("tuple-list", "8/0/10", "4/0/4"),
  ("case-def-inline", "9/0/9", "4/0/6"),
  ("case-def-multiline", "9/0/9", "4/0/6"),
];

let scoreboard_tests =
  pins
  |> List.map(((name, full_pin, prefix_pin)) =>
       test_case(
         name,
         `Slow,
         () => {
           let text = List.assoc(name, corpus);
           let full = run_class(~prefix_only=false, name, text);
           let prefix = run_class(~prefix_only=true, name, text);
           let show = o =>
             Printf.sprintf("%d/%d/%d", o.restored, o.destroyed, o.total);
           print_endline(
             Printf.sprintf(
               "SCORE %s: full %s, prefix %s",
               name,
               show(full),
               show(prefix),
             ),
           );
           check(string, name ++ " full", full_pin, show(full));
           check(string, name ++ " prefix", prefix_pin, show(prefix));
         },
       )
     );

let tests = [("CompletionScoreboard", scoreboard_tests @ pair_tests)];
