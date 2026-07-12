open Alcotest;
open Haz3lcore;
open Language;

/* Completion-display harness: render what the user actually sees —
   the DISPLAY segment with ghosts spliced at their anchors, caret
   as ¦, ghost runs in ⟪⟫ — plus the chip stream. The string IS the
   test: display_case("string_replace(a,¦ ?⟪, ?)⟫") types the text
   before ¦ and asserts the whole rendering.

   REVIEW STANDARD (not just pass/fail): a pinned trajectory is a
   user experience. Before pinning, read it in the user's frame —
   what moved that didn't need to? what was promised and then
   retracted? The load-bearing property is CONSTANCY: typing a
   promised character changes provenance styling only, never the
   rendered text (the promise defines a trajectory; along it the
   display must be still). constancy_audit checks this mechanically,
   but it's an ALARM, not a judge — when it fires, either the
   display regressed or the property mis-specifies the ergonomics;
   judge before pinning either way. Formatting compromises (promised
   spacing that vanishes on materialization) get flagged to andrew,
   not silently pinned. */

let string_testable = testable(Fmt.string, String.equal);

/* replicate the live pipeline (Editor.calculate → CachedSyntax):
   TyDi buffer first, chip ghost splice fallback. Statics derive from
   the SAME zipper we render — a re-typed program mints fresh ids and
   every id-keyed merge silently misses. */
let display_parts =
    (z: Zipper.t)
    : (
        Segment.t,
        Zipper.t,
        list((Id.t, option(int))),
        list(CanonicalCompletion.insertion),
      ) => {
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let obs = TypeObligations.derive(info_map);
  let assist = TypeObligations.assist_stream(z, obs);
  let z_tydi =
    switch (TyDi.set_buffer(~ci=Indicated.ci_for_completion(z, info_map), z)) {
    | Some(z) => z
    | None => z
    };
  if (Selection.is_buffer(z_tydi.selection)) {
    (
      Zipper.unselect_and_zip(z_tydi),
      z_tydi,
      CanonicalCompletion.ghost_marks(z_tydi.selection.content),
      assist,
    );
  } else {
    let seg = Zipper.unselect_and_zip(z);
    let (seg, marks) =
      switch (CanonicalCompletion.chip_among(z, assist)) {
      | Some(ins) =>
        let ins = CanonicalCompletion.slide_to_caret(z, ins);
        switch (TypeObligations.ghost_pieces(z, ins)) {
        | Some(pieces) =>
          switch (CanonicalCompletion.splice_ghost(seg, ~ins, ~pieces)) {
          | Some(r) => r
          | None => (seg, [])
          }
        | None => (seg, [])
        };
      | None => (seg, [])
      };
    /* system-material formatting rides with the ghost (CachedSyntax) */
    let seg =
      switch (marks, CanonicalCompletion.format_space_target(z)) {
      | ([_, ..._], Some(gid)) =>
        switch (CanonicalCompletion.splice_space_before(seg, gid)) {
        | Some(seg) => seg
        | None => seg
        }
      | _ => seg
      };
    let seg = marks == [] ? seg : CanonicalCompletion.deep_reassemble(seg);
    /* FAIL OPEN like live (CachedSyntax): a splice the parser can't
       take means no ghost this frame, never a crash — but surface it
       in the render so the jank is visible, not silent */
    switch (MakeTerm.go(seg)) {
    | _ => (seg, z, marks, assist)
    | exception _ when marks != [] => (
        Zipper.unselect_and_zip(z),
        z,
        [],
        assist,
      )
    };
  };
};

let display_state = (~chips as show_chips=true, z: Zipper.t): string => {
  let (seg, zc, marks, assist) = display_parts(z);
  /* live runs MakeTerm on the spliced display segment (CachedSyntax
     term_data) — the splice must keep the segment parseable */
  let _ = MakeTerm.go(seg);
  let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  let is_marked = (id: Id.t, sh: option(int)) =>
    List.exists(
      ((mid, msh): (Id.t, option(int))) => Id.equal(mid, id) && msh == sh,
      marks,
    );
  /* reading-order (marked, measurement) atoms of the display segment */
  let rec atoms = (sg: Segment.t): list((bool, Measured.measurement)) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          let ms = Measured.find_shards(~msg="DisplayState", t, measured);
          Util.Aba.mk(t.shards, t.children)
          |> Util.Aba.join(
               i => [(is_marked(t.id, Some(i)), List.assoc(i, ms))],
               atoms,
             )
          |> List.concat;
        | Grout(g) => [
            (is_marked(g.id, None), Measured.find_g(g, measured)),
          ]
        | Secondary(w) => [
            (is_marked(w.id, None), Measured.find_w(w, measured)),
          ]
        | Projector(_) => []
        },
      sg,
    );
  /* contiguous marked runs → ⟪ at first origin, ⟫ at last end */
  let runs = {
    let (closed, open_) =
      List.fold_left(
        ((rs, cur), (m, meas: Measured.measurement)) =>
          switch (m, cur) {
          | (false, None) => (rs, None)
          | (false, Some(r)) => ([r, ...rs], None)
          | (true, None) => (rs, Some((meas.origin, meas.last)))
          | (true, Some((o, _))) => (rs, Some((o, meas.last)))
          },
        ([], None),
        atoms(seg),
      );
    List.rev(Option.to_list(open_) @ closed);
  };
  let caret = Zipper.Caret.point(measured, zc);
  let text =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      ~indent=" ",
      ~measured,
      seg,
    );
  /* insert markers back-to-front so points stay valid; at a shared
     point later inserts land left, so descending priority yields
     ⟫¦ at a run end and ¦⟪ at a run start */
  let mark_list =
    [(caret, 1, "¦")]
    @ List.concat_map(
        ((o, l): (Util.Point.t, Util.Point.t)) =>
          [(o, 2, "⟪"), (l, 0, "⟫")],
        runs,
      );
  let disp =
    mark_list
    |> List.sort(((p1, pr1, _), (p2, pr2, _)) => {
         let c = Util.Point.compare(p1: Util.Point.t, p2: Util.Point.t);
         c != 0 ? - c : - compare(pr1: int, pr2: int);
       })
    |> List.fold_left(
         (rows, (pt, _, s)) => Printer.insert_string(s, pt, rows),
         String.split_on_char('\n', text),
       )
    |> String.concat("\n");
  /* live suppression: a ghosted chip never also shows as a chip
     (chip ghosts only — a TyDi buffer ghost isn't a chip) */
  let chips_shown =
    marks != [] && !Selection.is_buffer(zc.selection)
      ? switch (CanonicalCompletion.chip_among(zc, assist)) {
        | Some(ghosted) =>
          List.filter(
            (i: CanonicalCompletion.insertion) => i !== ghosted,
            assist,
          )
        | None => assist
        }
      : assist;
  let chips_str =
    chips_shown
    |> List.map((i: CanonicalCompletion.insertion) =>
         i.delimiters
         |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
         |> String.concat("+")
       )
    |> String.concat(" | ");
  show_chips ? disp ++ "   CHIPS[" ++ chips_str ++ "]" : disp;
};

/* Per-keystroke trajectory of typing `text` at the ¦ in `ctx` — the
   SCENARIO MATRIX axis: same form entered on a blank editor, above
   existing content, between content, or into a hole mid-program.
   Contexts are typed first (real ids), then each step renders. */
let trajectory_in = (~ctx="¦", text: string): string => {
  let base = Test_Editing.mk(ctx);
  let ins = Token.to_list(text) |> List.map(c => Action.Insert(c));
  let rec steps = (k, acc) =>
    if (k > List.length(ins)) {
      List.rev(acc);
    } else {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          base @ List.filteri((i, _) => i < k, ins),
        );
      steps(k + 1, [display_state(z), ...acc]);
    };
  steps(1, []) |> String.concat("\n");
};

let trajectory = (text: string): string => trajectory_in(~ctx="¦", text);

/* One-liner form: the string is both input and expectation. Text
   before ¦ (ghosts stripped) is typed left-to-right; the rendered
   display state must equal the whole string. */
let split_first = (needle: string, s: string): option((string, string)) => {
  let nl = String.length(needle);
  let sl = String.length(s);
  let rec go = i =>
    if (i + nl > sl) {
      None;
    } else if (String.sub(s, i, nl) == needle) {
      Some((String.sub(s, 0, i), String.sub(s, i + nl, sl - i - nl)));
    } else {
      go(i + 1);
    };
  go(0);
};

let strip_ghosts = (s: string): string => {
  let rec go = s =>
    switch (split_first("⟪", s)) {
    | None => s
    | Some((pre, rest)) =>
      switch (split_first("⟫", rest)) {
      | None => pre ++ rest
      | Some((_, post)) => go(pre ++ post)
      }
    };
  go(s);
};

/* CONSTANCY: for each keystroke that matches the first character
   right of the caret in the prior display (= typing the promised
   material, ghost or promised formatting), the marker-stripped
   renders before and after must be equal. Returns violation lines;
   trajectories pin the empty string. */
let constancy_audit = (text: string): string => {
  let acts = Test_Editing.mk(text ++ "¦");
  let n_acts = List.length(acts);
  let state = k =>
    display_state(
      ~chips=false,
      Test_Editing.perform(
        Zipper.init(),
        List.filteri((i, _) => i < k, acts),
      ),
    );
  let remove_all = (needle: string, s: string): string => {
    let rec go = s =>
      switch (split_first(needle, s)) {
      | None => s
      | Some((pre, post)) => pre ++ go(post)
      };
    go(s);
  };
  let strip = (s: string): string =>
    s |> remove_all("¦") |> remove_all("⟪") |> remove_all("⟫");
  /* first char right of the caret, markers skipped */
  let promised = (s: string): option(char) =>
    switch (split_first("¦", s)) {
    | None => None
    | Some((_, r)) =>
      let r = r |> remove_all("⟪") |> remove_all("⟫");
      String.length(r) > 0 ? Some(r.[0]) : None;
    };
  let violations = ref([]);
  let prev = ref(state(1));
  for (k in 2 to n_acts) {
    let cur = state(k);
    let key = String.length(text) >= k ? Some(text.[k - 1]) : None;
    switch (key, promised(prev^)) {
    | (Some(c), Some(p)) when c == p && strip(prev^) != strip(cur) =>
      violations :=
        [Printf.sprintf("'%c': %s -> %s", c, prev^, cur), ...violations^]
    | _ => ()
    };
    prev := cur;
  };
  List.rev(violations^) |> String.concat("\n");
};

let display_case = (spec: string) =>
  test_case(
    spec,
    `Quick,
    () => {
      let typed =
        switch (split_first("¦", strip_ghosts(spec))) {
        | Some((pre, _)) => pre
        | None => Alcotest.fail("display_case: no caret in: " ++ spec)
        };
      let z =
        Test_Editing.perform(Zipper.init(), Test_Editing.mk(typed ++ "¦"));
      check(string_testable, spec, spec, display_state(~chips=false, z));
    },
  );

let tests = [
  (
    "CompletionDisplay: target-0",
    [
      /* SNAPSHOT: left-to-right entry of the string_replace call.
         v2 anchored splice — ghosts land at their run's true
         position, after any real hole (v1's caret-locked buffer
         showed `a,¦⟪, ?)⟫?`: material past the visible hole). */
      test_case("string_replace left-to-right snapshot", `Quick, () =>
        check(
          string_testable,
          "t0",
          {|s¦   CHIPS[]
st¦⟪ring_capitalize⟫   CHIPS[]
str¦⟪ing_capitalize⟫   CHIPS[]
stri¦⟪ng_capitalize⟫   CHIPS[]
strin¦⟪g_capitalize⟫   CHIPS[]
string¦⟪_capitalize⟫   CHIPS[]
string_¦⟪capitalize⟫   CHIPS[]
string_r¦⟪eplace⟫   CHIPS[]
string_re¦⟪place⟫   CHIPS[]
string_rep¦⟪lace⟫   CHIPS[]
string_repl¦⟪ace⟫   CHIPS[]
string_repla¦⟪ce⟫   CHIPS[]
string_replac¦⟪e⟫   CHIPS[]
string_replace¦   CHIPS[]
string_replace(¦?⟪, ?, ?)⟫   CHIPS[]
string_replace(a¦⟪, ?, ?)⟫   CHIPS[]
string_replace(a,¦ ?⟪, ?)⟫   CHIPS[]
string_replace(a, ¦?⟪, ?)⟫   CHIPS[]
string_replace(a, b¦⟪, ?)⟫   CHIPS[]
string_replace(a, b,¦ ?⟪)⟫   CHIPS[]
string_replace(a, b, ¦?⟪)⟫   CHIPS[]
string_replace(a, b, c¦⟪)⟫   CHIPS[]
string_replace(a, b, c)¦   CHIPS[]|},
          trajectory("string_replace(a, b, c)"),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: one-liners",
    /* the string IS the test: text before ¦ is typed, the whole
       string is the expected rendering (ghost order = true landing
       order — the two middle cases were v1's buffer-jank states) */
    [
      /* empty parens presume: the full promise from `(` on */
      display_case("string_replace(¦?⟪, ?, ?)⟫"),
      display_case("string_replace(a¦⟪, ?, ?)⟫"),
      display_case("string_replace(a,¦ ?⟪, ?)⟫"),
      display_case("string_replace(a, b,¦ ?⟪)⟫"),
      display_case("string_replace(a, b, c¦⟪)⟫"),
      display_case("let x = 4 i¦⟪n⟫?"),
      /* trailing space: the ghost hugs the caret (slide_to_caret) —
         a closer drawn left of the caret would portray typing
         outside the completed call */
      display_case("string_replace(a, b, c ¦⟪)⟫"),
      display_case("string_replace(a, b, c  ¦⟪)⟫"),
      /* caret INSIDE the auto-closed string literal: the promise is
         anchored on the host token, so Inner carets still ghost */
      display_case("string_replace(\"¦\"⟪, ?, ?)⟫"),
    ],
  ),
  (
    "CompletionDisplay: matrix",
    /* forms x contexts: the same entry must read sensibly on a blank
       editor, above existing content, and into a hole mid-program.
       PROBE first, felt-read, then pin. */
    [
      /* KNOWN JANK snapshot (fixes show as diffs): real holes miss
         their system pad against tokens (`let¦?`, `?=`, `=?`, `in¦?`)
         — the fmt-space rule is comma-only; and the ghost's lead pad
         reads the CARET's left context instead of the SPLICE point's
         (`¦?⟪= ...` should read `¦? ⟪= ...`). */
      test_case("let entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "let-blank",
          {|l¦   CHIPS[]
le¦⟪t ⟫   CHIPS[]
let¦?⟪ = ? in ?⟫   CHIPS[]
let ¦?⟪= ? in ?⟫   CHIPS[]
let x¦⟪ = ? in ?⟫   CHIPS[]
let x ¦⟪= ? in ?⟫   CHIPS[]
let x =¦?⟪ in ?⟫   CHIPS[]
let x = ¦?⟪in ?⟫   CHIPS[]
let x = 1¦⟪ in ?⟫   CHIPS[]
let x = 1 ¦⟪in ?⟫   CHIPS[]
let x = 1 i¦⟪n⟫?   CHIPS[in]
let x = 1 in¦?   CHIPS[]|},
          trajectory("let x = 1 in"),
        )
      ),
      /* KNOWN JANK: with content below, the splice fails to parse
         exactly when a let slot holds a REAL token (x, 1) — those
         steps FAIL OPEN to chips (`let x¦~ ... CHIPS[=+in]`), no
         ghost. Grout-slot steps splice fine. Root cause open. Also:
         `let? ¦` — regrout puts the typed space AFTER the hole, so
         the caret visually jumps past it (zipper behavior, not
         display). */
      test_case("let entry above existing content", `Quick, () =>
        check(
          string_testable,
          "let-above",
          {|l¦~
string_replace(a, b, c)   CHIPS[]
le¦⟪t ⟫~
string_replace(a, b, c)   CHIPS[]
let¦?⟪ = ? in⟫
string_replace(a, b, c)   CHIPS[]
let? ¦⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let x¦~
string_replace(a, b, c)   CHIPS[=+in]
let x ¦~
string_replace(a, b, c)   CHIPS[=+in]
let x =¦?⟪ in⟫
string_replace(a, b, c)   CHIPS[]
let x =? ¦⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = 1¦~
string_replace(a, b, c)   CHIPS[in]
let x = 1 ¦~
string_replace(a, b, c)   CHIPS[in]
let x = 1 i¦⟪n⟫
string_replace(a, b, c)   CHIPS[in]
let x = 1 in¦
string_replace(a, b, c)   CHIPS[]|},
          trajectory_in(~ctx="¦\nstring_replace(a, b, c)", "let x = 1 in"),
        )
      ),
      /* CLEAN: mid-program insertion into a hole reads right
         throughout */
      test_case("ap entry into a hole mid-program", `Quick, () =>
        check(
          string_testable,
          "ap-in-hole",
          {|let a = s¦ in a + 1   CHIPS[]
let a = st¦⟪ring_capitalize⟫ in a + 1   CHIPS[]
let a = str¦⟪ing_capitalize⟫ in a + 1   CHIPS[]
let a = stri¦⟪ng_capitalize⟫ in a + 1   CHIPS[]
let a = strin¦⟪g_capitalize⟫ in a + 1   CHIPS[]
let a = string¦⟪_capitalize⟫ in a + 1   CHIPS[]
let a = string_¦⟪capitalize⟫ in a + 1   CHIPS[]
let a = string_r¦⟪eplace⟫ in a + 1   CHIPS[]
let a = string_re¦⟪place⟫ in a + 1   CHIPS[]
let a = string_rep¦⟪lace⟫ in a + 1   CHIPS[]
let a = string_repl¦⟪ace⟫ in a + 1   CHIPS[]
let a = string_repla¦⟪ce⟫ in a + 1   CHIPS[]
let a = string_replac¦⟪e⟫ in a + 1   CHIPS[]
let a = string_replace¦ in a + 1   CHIPS[]
let a = string_replace(¦?⟪, ?, ?)⟫ in a + 1   CHIPS[]
let a = string_replace(x¦⟪, ?, ?)⟫ in a + 1   CHIPS[]|},
          trajectory_in(~ctx="let a = ¦ in a + 1", "string_replace(x"),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: crash-repro",
    [
      test_case(
        "keyword completion above later content",
        `Quick,
        () => {
          /* le¦ on line 1, string_replace("") on line 3; typing t
             completes the let keyword — the spliced display segment
             must stay parseable (Skel.push_output crash) */
          let z =
            Test_Editing.perform(
              Zipper.init(),
              Test_Editing.mk("le\n\nstring_replace(\"\")¦"),
            );
          let z =
            Test_Editing.perform(
              z,
              [
                Test_Editing.move_point(~row=0, ~col=2, ()),
                Action.Insert("t"),
              ],
            );
          check(
            string_testable,
            "no crash",
            "let¦?⟪ = ? in⟫\n\nstring_replace(\"\")   CHIPS[,+,]",
            display_state(z),
          );
        },
      ),
    ],
  ),
  (
    "CompletionDisplay: constancy",
    [
      test_case("string_replace entry types through its promise", `Quick, () =>
        check(
          string_testable,
          "no constancy violations",
          "",
          constancy_audit("string_replace(a, b, c)"),
        )
      ),
    ],
  ),
];
