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
  let assist = TypeObligations.assist_stream(z, ~info_map, obs);
  let z_tydi =
    switch (TyDi.set_buffer(~ci=Indicated.ci_for_completion(z, info_map), z)) {
    | Some(z) => z
    | None => z
    };
  /* COEXISTENCE: TyDi's witness ghost lives in the buffer (at the
     caret); chip ghosts splice at their anchors — both render */
  let has_buffer =
    Selection.is_buffer(z_tydi.selection) && z_tydi.selection.content != [];
  let base_z = has_buffer ? z_tydi : z;
  let seg = Zipper.unselect_and_zip(base_z);
  let tydi_marks =
    has_buffer
      ? CanonicalCompletion.ghost_marks(z_tydi.selection.content) : [];
  let ghostable =
    assist
    |> List.filter((ins: CanonicalCompletion.insertion) =>
         switch (ins.delimiters) {
         | [{typed_len: Some(_), _}, ..._] => false
         | _ => true
         }
       );
  let (seg, chip_marks) =
    switch (
      CanonicalCompletion.chip_among(z, ghostable)
      |> Option.map(CanonicalCompletion.slide_to_caret(z))
    ) {
    | Some(ins) when !CanonicalCompletion.splice_precedes_caret(z, ins) =>
      switch (TypeObligations.ghost_pieces(z, ins)) {
      | Some(pieces) =>
        switch (CanonicalCompletion.splice_ghost(seg, ~ins, ~pieces)) {
        | Some(r) => r
        | None => (seg, [])
        }
      | None => (seg, [])
      }
    | _ => (seg, [])
    };
  /* normalize + padding oracle, exactly like live (CachedSyntax) */
  let seg =
    chip_marks == []
      ? seg
      : seg
        |> CanonicalCompletion.normalize_display
        |> CanonicalCompletion.finish_display(
             ~marks=chip_marks,
             ~raw=Zipper.unselect_and_zip(base_z),
             ~caret_after=CanonicalCompletion.caret_left_atom(base_z),
           );
  /* FAIL OPEN like live: unparseable splice = no chip ghost */
  switch (MakeTerm.go(seg)) {
  | _ => (seg, base_z, tydi_marks @ chip_marks, assist)
  | exception _ when chip_marks != [] => (
      Zipper.unselect_and_zip(base_z),
      base_z,
      tydi_marks,
      assist,
    )
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
  /* reading-order (marked, is_ws, measurement) atoms */
  let rec atoms = (sg: Segment.t): list((bool, bool, Measured.measurement)) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          let ms = Measured.find_shards(~msg="DisplayState", t, measured);
          Util.Aba.mk(t.shards, t.children)
          |> Util.Aba.join(
               i =>
                 [(is_marked(t.id, Some(i)), false, List.assoc(i, ms))],
               atoms,
             )
          |> List.concat;
        | Grout(g) => [
            (is_marked(g.id, None), false, Measured.find_g(g, measured)),
          ]
        | Secondary(w) => [
            (is_marked(w.id, None), true, Measured.find_w(w, measured)),
          ]
        | Projector(_) => []
        },
      sg,
    );
  /* marked runs → ⟪ at first origin, ⟫ at last end; unmarked
     whitespace BRIDGES a run (oracle pads carry no provenance) but
     never extends its end */
  let runs = {
    let (closed, open_) =
      List.fold_left(
        ((rs, cur), (m, ws, meas: Measured.measurement)) =>
          switch (m, cur) {
          | (false, None) => (rs, None)
          | (false, Some(r)) => ws ? (rs, Some(r)) : ([r, ...rs], None)
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
  /* suppression comes from THE one policy home; chip-ghost marks =
     marks not belonging to the TyDi buffer content */
  let chips_shown = {
    let sel_ids = Selection.selection_ids(zc.selection);
    let chip_ghost_active =
      marks
      |> List.exists(((id, _): (Id.t, option(int))) =>
           !List.exists(Id.equal(id), sel_ids)
         );
    CanonicalCompletion.chips_displayed(zc, ~chip_ghost_active, assist);
  };
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

/* per-backspace trajectory from the ¦ in ctx — DELETION flows jank
   differently than entry (promises grow instead of shrinking) */
let trajectory_bk = (~ctx: string, n: int): string => {
  let base = Test_Editing.mk(ctx);
  let rec steps = (k, acc) =>
    if (k > n) {
      List.rev(acc);
    } else {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          base @ List.init(k, _ => Action.Destruct(Local(Left, ByChar))),
        );
      steps(k + 1, [display_state(z), ...acc]);
    };
  steps(1, []) |> String.concat("\n");
};

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
      /* padding oracle: holes padded, typed spaces consume pads —
         text constant through every promised keystroke. Residual:
         the TyDi witness line's trailing `in?` (buffer path, no
         marks — matches plain dev rendering). */
      test_case("let entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "let-blank",
          {|l¦   CHIPS[]
le¦⟪t ⟫   CHIPS[]
let¦ ? ⟪= ? in ?⟫   CHIPS[]
let ¦? ⟪= ? in ?⟫   CHIPS[]
let x¦ ⟪= ? in ?⟫   CHIPS[]
let x ¦⟪= ? in ?⟫   CHIPS[]
let x =¦ ? ⟪in ?⟫   CHIPS[]
let x = ¦? ⟪in ?⟫   CHIPS[]
let x = 1¦ ⟪in ?⟫   CHIPS[]
let x = 1 ¦⟪in ?⟫   CHIPS[]
let x = 1 i¦⟪n⟫?   CHIPS[]
let x = 1 in¦?   CHIPS[]|},
          trajectory("let x = 1 in"),
        )
      ),
      /* Full shape normalization (regrout/reassemble/remold) keeps
         every step parseable — real-token slots included. KNOWN
         JANK remaining: missing hole pads (as above); and `let? ¦`
         — regrout puts the typed space AFTER the hole, so the caret
         visually jumps past it (zipper behavior, predates the
         fork — investigate vs dev). */
      test_case("let entry above existing content", `Quick, () =>
        check(
          string_testable,
          "let-above",
          {|l¦~
string_replace(a, b, c)   CHIPS[]
le¦⟪t ⟫~
string_replace(a, b, c)   CHIPS[]
let¦ ? ⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let ¦? ⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let x¦ ⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let x ¦⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let x =¦ ? ⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = ¦? ⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = 1¦ ⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = 1 ¦⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = 1 i¦⟪n⟫
string_replace(a, b, c)   CHIPS[]
let x = 1 in¦
string_replace(a, b, c)   CHIPS[]|},
          trajectory_in(~ctx="¦\nstring_replace(a, b, c)", "let x = 1 in"),
        )
      ),
      /* DELETION (andrew's backspace repro): back through
         `let  =  in` — watch for pre-caret reflow (policy: display
         never changes strictly before the cursor) and the prefix
         behavior when `in` decays to `i`. Pre-caret text is
         byte-stable throughout (policy holds). KNOWN JANK, first
         line: witness absorption doesn't fire for a decayed keyword
         shard, so `i` gets a synthesized `in` beside it instead of
         prefix-joining (`i¦⟪n⟫`) — needs middle-shard witness in the
         completion engine. `let?` snug: the pre-caret pad is
         correctly WITHHELD (policy) — matches the raw zipper and
         dev; pads appear only once the caret returns there. */
      test_case("backspace through let = in", `Quick, () =>
        check(
          string_testable,
          "let-bk",
          {|let?  =  i¦ ⟪in ?⟫   CHIPS[]
let?  =  ¦? ⟪in ?⟫   CHIPS[]
let?  = ¦? ⟪in ?⟫   CHIPS[]
let?  =¦ ? ⟪in ?⟫   CHIPS[]
let  ¦? ⟪= ? in ?⟫   CHIPS[]
let ¦? ⟪= ? in ?⟫   CHIPS[]|},
          trajectory_bk(~ctx="let  =  in¦", 6),
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
    "CompletionDisplay: inventory",
    /* exploratory sweep (andrew): new forms, leaving things
       incomplete, refactors that break existing form delimiters */
    [
      /* `=¦` line: the end-ghost survives mid-arrow typing (witness
         boundaries); the `=>` witness itself still shows as a chip
         because TyDi doesn't fire on the case arrow (the known
         tydi-backpack-case-arrow issue) — would ghost once that's
         fixed */
      test_case("case entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "case-entry",
          {|c¦   CHIPS[]
ca¦⟪se ⟫   CHIPS[]
cas¦⟪e ⟫   CHIPS[]
case¦ ? ⟪end⟫   CHIPS[]
case ¦? ⟪end⟫   CHIPS[]
case 1¦ ⟪end⟫   CHIPS[]
case 1 ¦⟪end⟫   CHIPS[]
case 1 |¦ ? ⟪=> ? end⟫   CHIPS[]
case 1 | ¦? ⟪=> ? end⟫   CHIPS[]
case 1 | 2¦ ⟪=> ? end⟫   CHIPS[]
case 1 | 2 ¦⟪=> ? end⟫   CHIPS[]
case 1 | 2 =¦ ? ⟪end⟫   CHIPS[=>]
case 1 | 2 =>¦ ? ⟪end⟫   CHIPS[]
case 1 | 2 => ¦? ⟪end⟫   CHIPS[]
case 1 | 2 => 3¦ ⟪end⟫   CHIPS[]|},
          trajectory("case 1 | 2 => 3"),
        )
      ),
      test_case("if entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "if-entry",
          {|i¦   CHIPS[]
if¦ ? ⟪then ? else ?⟫   CHIPS[]
if ¦? ⟪then ? else ?⟫   CHIPS[]
if 1¦ ⟪then ? else ?⟫   CHIPS[]
if 1 ¦⟪then ? else ?⟫   CHIPS[]
if 1 <¦ ? ⟪then ? else ?⟫   CHIPS[]|},
          trajectory("if 1 <"),
        )
      ),
      /* COEXISTENCE: TyDi's witness completes the token, chip
         promise follows — one continuous ghost (was: witness
         suppressed the chip ghosts to chips) */
      test_case("annotation tuple: TyDi x chip ghosts", `Quick, () =>
        check(
          string_testable,
          "annot-tydi",
          {|let a : (¦?⟪) = ? in ?⟫   CHIPS[]
let a : (S¦⟪) = ? in ?⟫   CHIPS[]
let a : (St¦⟪ring) = ? in ?⟫   CHIPS[]|},
          trajectory_in(~ctx="let a : ¦", "(St"),
        )
      ),
      /* CLEAN: deleting `)` inside a complete let recovers in place —
         ghost closer immediately, `in y` unmoved, pads correct */
      test_case("break the closer of a complete call", `Quick, () =>
        check(
          string_testable,
          "break-closer",
          {|let y = string_replace(a, b, c¦⟪)⟫ in y   CHIPS[]
let y = string_replace(a, b, ¦?⟪)⟫ in y   CHIPS[]
let y = string_replace(a, b,¦ ?⟪)⟫ in y   CHIPS[]|},
          trajectory_bk(~ctx="let y = string_replace(a, b, c)¦ in y", 3),
        )
      ),
      /* deleting the `(` of a complete call: the completion proposes
         re-opening from the LEFT (side-Left leading run) — as a
         ghost that would sit strictly before the caret, so it is
         SUPPRESSED to a chip (splice_precedes_caret; the caret is
         Inner in the preceding name after this deletion) */
      test_case("break the opener of a complete call", `Quick, () =>
        check(
          string_testable,
          "break-opener",
          "string_replace\xc2\xa6a, b, c)   CHIPS[(]",
          trajectory_bk(~ctx="string_replace(\xc2\xa6a, b, c)", 1),
        )
      ),
      /* CLEAN: moving away dismisses the ghost, chips persist for
         the abandoned site, typing below never re-conjures it, and
         line 1 stays byte-stable (the ~ is the real junction hole
         joining the incomplete call to the next line) */
      test_case("abandon incomplete call, work below", `Quick, () =>
        check(
          string_testable,
          "abandon",
          {|string_replace(a¦⟪, ?, ?)⟫ ~
1 + 1   CHIPS[]
---
string_replace(a~
1 + 1¦   CHIPS[,+,+)]
---
string_replace(a~
1 + 1 + 2¦   CHIPS[,+,+)]|},
          {
            let z =
              Test_Editing.perform(
                Zipper.init(),
                Test_Editing.mk("¦\n1 + 1")
                @ (
                  Token.to_list("string_replace(a")
                  |> List.map(c => Action.Insert(c))
                ),
              );
            let after_typing = display_state(z);
            let z =
              Test_Editing.perform(
                z,
                [Test_Editing.move_point(~row=1, ~col=5, ())],
              );
            let after_move = display_state(z);
            let z =
              Test_Editing.perform(
                z,
                Token.to_list(" + 2") |> List.map(c => Action.Insert(c)),
              );
            let after_more = display_state(z);
            after_typing ++ "\n---\n" ++ after_move ++ "\n---\n" ++ after_more;
          },
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: matrix-2",
    /* second sweep: fun, list literal, nested call, breaking an
       inner delimiter inside an outer complete form */
    [
      /* last line: mid-arrow witness (`-` of `->`) — same class
         as the case arrow: symbolic witness not TyDi-ghosted, chip
         holds the promise */
      test_case("fun entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "fun-entry",
          {|f¦   CHIPS[]
fu¦⟪n ⟫   CHIPS[]
fun¦ ? ⟪-> ?⟫   CHIPS[]
fun ¦? ⟪-> ?⟫   CHIPS[]
fun x¦ ⟪-> ?⟫   CHIPS[]
fun x ¦⟪-> ?⟫   CHIPS[]
fun x -¦?   CHIPS[->]|},
          trajectory("fun x -"),
        )
      ),
      test_case("list literal entry", `Quick, () =>
        check(
          string_testable,
          "list-entry",
          {|[¦?⟪]⟫   CHIPS[]
[1¦⟪]⟫   CHIPS[]
[1,¦ ?⟪]⟫   CHIPS[]
[1, ¦?⟪]⟫   CHIPS[]
[1, 2¦⟪]⟫   CHIPS[]|},
          trajectory("[1, 2"),
        )
      ),
      /* KNOWN JANK, mid-witness states: the merged
         witness+opener+closer+comma insertion assembles garbled
         ghost content — `( ), ,,` where the promise should read
         `(?), ?, ?)` (empty padded parens, doubled commas).
         Recovers fully once `(` is typed. Ghost-piece assembly for
         multi-delimiter witness merges needs the same order/holes
         treatment the diff gives plain runs. */
      test_case("nested call entry", `Quick, () =>
        check(
          string_testable,
          "nested-call",
          {|let s = s¦ in s   CHIPS[]
let s = st¦⟪ring_capitalize⟫ in s   CHIPS[]
let s = str¦⟪ing_capitalize⟫ in s   CHIPS[]
let s = stri¦⟪ng_capitalize⟫ in s   CHIPS[]
let s = strin¦⟪g_capitalize⟫ in s   CHIPS[]
let s = string¦⟪_capitalize⟫ in s   CHIPS[]
let s = string_¦⟪capitalize⟫ in s   CHIPS[]
let s = string_r¦⟪eplace⟫ in s   CHIPS[]
let s = string_re¦⟪place⟫ in s   CHIPS[]
let s = string_rep¦⟪lace⟫ in s   CHIPS[]
let s = string_repl¦⟪ace⟫ in s   CHIPS[]
let s = string_repla¦⟪ce⟫ in s   CHIPS[]
let s = string_replac¦⟪e⟫ in s   CHIPS[]
let s = string_replace¦ in s   CHIPS[]
let s = string_replace(¦?⟪, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(s¦⟪, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(st¦⟪ring_capitalize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(str¦⟪ing_capitalize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(stri¦⟪ng_capitalize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(strin¦⟪g_capitalize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string¦⟪_capitalize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_¦⟪capitalize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_c¦⟪apitalize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_ca¦⟪pitalize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_cap¦⟪italize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capi¦⟪talize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capit¦⟪alize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capita¦⟪lize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capital¦⟪ize( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitali¦⟪ze( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitaliz¦⟪e( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitalize¦⟪( ), ,, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitalize(¦?⟪, ?, ?))⟫ in s   CHIPS[]
let s = string_replace(string_capitalize(x¦⟪, ?, ?))⟫ in s   CHIPS[]|},
          trajectory_in(
            ~ctx="let s = ¦ in s",
            "string_replace(string_capitalize(x",
          ),
        )
      ),
      /* CLEAN: inner closer deletion recovers in place under the
         outer complete let */
      test_case("break inner paren inside complete let", `Quick, () =>
        check(
          string_testable,
          "break-inner",
          {|let y = (1 + 2¦⟪)⟫ in y   CHIPS[]
let y = (1 + ¦?⟪)⟫ in y   CHIPS[]|},
          trajectory_bk(~ctx="let y = (1 + 2)¦ in y", 2),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: probes-andrew-3",
    /* andrew's live reports 2026-07-12, all fixed: (1) witness chip
       suppressed while TyDi ghosts it — suppression now has ONE home
       (chips_displayed) shared by deco and harness; (2) `ue then`
       spaced — comments are content-width in the oracle, not
       separators; (3) else-promise anchors AFTER the visible
       witnessed `t(hen)` — witnessed shards are walk boundaries
       anchoring at their absorbed token */
    [
      test_case("let a = 1 i: ghost + chip both?", `Quick, () =>
        check(
          string_testable,
          "p1",
          {|let a = 1 i¦⟪n⟫?   CHIPS[]|},
          trajectory_in(~ctx="let a = 1 ¦", "i"),
        )
      ),
      test_case("if tr: TyDi + then-ghost spacing", `Quick, () =>
        check(
          string_testable,
          "p2",
          {|if t¦ ⟪then ? else ?⟫   CHIPS[]
if tr¦⟪ue then ? else ?⟫   CHIPS[]|},
          trajectory_in(~ctx="if ¦", "tr"),
        )
      ),
      test_case("if true t: else chip before then?", `Quick, () =>
        check(
          string_testable,
          "p3",
          {|if true t¦⟪hen⟫ ? ⟪else ?⟫   CHIPS[]|},
          trajectory_in(~ctx="if true ¦", "t"),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: raw-vs-display",
    [
      test_case(
        "space after let: zipper order vs display order",
        `Quick,
        () => {
          /* raw zipper rendering, no display fork — localizes the
             caret-jump: if raw is `let ¦?` and display is `let? ¦`,
             the DISPLAY regrout (normalize_display) reordered it */
          let raw_state = (code: string): string => {
            let z =
              Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
            let seg = Zipper.unselect_and_zip(z);
            let measured =
              Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
            let caret = Zipper.Caret.point(measured, z);
            Printer.of_segment(
              ~holes="?",
              ~concave_holes="~",
              ~indent=" ",
              ~measured,
              seg,
            )
            |> String.split_on_char('\n')
            |> Printer.insert_string("¦", caret)
            |> String.concat("\n");
          };
          let disp = (code: string): string => {
            let z =
              Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
            display_state(~chips=false, z);
          };
          check(
            string_testable,
            "raw vs display",
            /* FIXED: minted display grout hops after typed spaces
               (finish_display reorder) — the rendered caret matches
               the zipper; the display only ADDS material right of it */
            {|raw[let ]: let ¦?
disp[let ]: let ¦? ⟪= ? in ?⟫
raw[let x ]: let x ¦
disp[let x ]: let x ¦⟪= ? in ?⟫
raw[above]: let ¦
string_replace(a, b, c)|},
            "raw[let ]: "
            ++ raw_state("let ¦")
            ++ "\ndisp[let ]: "
            ++ disp("let ¦")
            ++ "\nraw[let x ]: "
            ++ raw_state("let x ¦")
            ++ "\ndisp[let x ]: "
            ++ disp("let x ¦")
            ++ "\nraw[above]: "
            ++ {
              let z =
                Test_Editing.perform(
                  Zipper.init(),
                  Test_Editing.mk("¦\nstring_replace(a, b, c)")
                  @ (
                    Token.to_list("let ") |> List.map(c => Action.Insert(c))
                  ),
                );
              let seg = Zipper.unselect_and_zip(z);
              let measured =
                Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
              let caret = Zipper.Caret.point(measured, z);
              Printer.of_segment(
                ~holes="?",
                ~concave_holes="~",
                ~indent=" ",
                ~measured,
                seg,
              )
              |> String.split_on_char('\n')
              |> Printer.insert_string("¦", caret)
              |> String.concat("\n");
            },
          );
        },
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
            "let¦ ? ⟪= ? in⟫\n\nstring_replace(\"\")   CHIPS[,+,]",
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
