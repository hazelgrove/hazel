open Alcotest;
open Language;

module CC = Web.ColorConfiguration;

/* The Colors config slide is committed Hazel text
   (hazel-programs/config/colors.hz) with an OCaml contract around it
   (ColorConfiguration.re): the CSS custom properties the app expects it to
   define, and the type it is analyzed against. Because those live apart from
   the program, they can drift from it, and nothing at runtime would say so —
   a colour the slide stops defining just keeps its stylesheet default, which
   is invisible in light mode and broken in dark.

   So the tests pin the join. The slide must satisfy its analyzed type with no
   static errors, that analysis must actually be engaged, and evaluating the
   slide must yield exactly the contract's variables — the applier reads the
   EVALUATED value, so type-checking alone would not catch a slide that checks
   but produces nothing. */

let source_satisfies_expected_type = () =>
  check(
    int,
    "built-in Colors source has no static errors under its expected type",
    0,
    ConfigSlideCheck.error_count(~ana=CC.expected_type, CC.source),
  );

/* Negative control: a vacuous ~ana would pass the check above on its own. */
let analysis_is_engaged = () =>
  check(
    bool,
    "analyzing the Colors source against String reports an error",
    true,
    ConfigSlideCheck.error_count(
      ~ana=IdTagged.FreshGrammar.Typ.string(),
      CC.source,
    )
    > 0,
  );

/* Evaluated once: statics is memoized but evaluation is not, and the slide is
   a 400-line program. */
let evaluated = lazy(CC.vars_of_source(CC.source));
let evaluated_vars = () => Lazy.force(evaluated);

let declared_names = CC.all_targets;

/* The join between the slide and its contract. A name in the contract that
   the slide does not produce is a variable that silently keeps its stylesheet
   default; a name the slide produces that the contract does not declare is a
   variable nothing analyzes. */
let slide_matches_contract = () => {
  let produced = List.map(fst, evaluated_vars());
  check(
    list(string),
    "every declared colour is produced by the slide",
    [],
    List.filter(n => !List.mem(n, produced), declared_names),
  );
  check(
    list(string),
    "the slide produces nothing the contract does not declare",
    [],
    List.filter(n => !List.mem(n, declared_names), produced),
  );
  check(
    int,
    "no variable is emitted twice",
    0,
    List.length(produced) - List.length(List.sort_uniq(compare, produced)),
  );
};

let starts_with = (p, s) =>
  String.length(s) >= String.length(p)
  && String.sub(s, 0, String.length(p)) == p;

let contains = (needle, s) => {
  let (n, h) = (String.length(needle), String.length(s));
  let rec go = i =>
    i + n <= h && (String.sub(s, i, n) == needle || go(i + 1));
  go(0);
};

/* OCaml float printing is the hazard: `%g` renders 1e-05 or nan, both of
   which a browser silently drops rather than reporting. */
let plausible = s =>
  (
    starts_with("oklch(", s)
    || starts_with("color-mix(", s)
    /* A seed written in sRGB bytes that nothing adjusted stays `Rgb`, and
       renders as rgb() -- valid CSS, and not to be read as unparseable. */
    || starts_with("rgb(", s)
    || starts_with("#", s)
  )
  && !List.exists(bad => contains(bad, s), ["nan", "inf", "e-", "e+"]);

/* Two emitted properties carry flags rather than colours, and are the only
   ones this check does not apply to. */
let flags = [CC.polarity_target, CC.contrast_target];

let unparseable = vars =>
  vars
  |> List.filter(((n, v)) => !List.mem(n, flags) && !plausible(v))
  |> List.map(((n, v)) => n ++ " = " ++ v);

/* The applier writes these straight into a CSS custom property, where an
   unparseable value is silently ignored by the browser. */
let every_value_is_css = () => {
  check(
    list(string),
    "no colour renders to something CSS cannot parse",
    [],
    unparseable(evaluated_vars()),
  );
};

/* The shape the slide is written for. The scheme flags are read at the very
   bottom and NOWHERE else: everything above is written once and reused by all
   four schemes. This is the property the slide was restructured to get, and
   it is what makes adding a scheme cost one line instead of one conditional
   per colour — the previous shape tested `dark_mode` at all 50-odd places a
   colour differed. Comments are stripped first, since the header prose names
   the flags. */
let strip_comments = (text: string) => {
  let buf = Buffer.create(String.length(text));
  let inside = ref(false);
  String.iter(
    c =>
      if (c == '#') {
        inside := ! inside^;
      } else if (! inside^) {
        Buffer.add_char(buf, c);
      },
    text,
  );
  Buffer.contents(buf);
};

/* Identifier tokens, split on anything that cannot appear in a name. The
   flags are read inside a tuple scrutinee — `case (dark_mode, ...` — so
   splitting on spaces alone would miss them. */
let words = text =>
  String.to_seq(text)
  |> Seq.map(c =>
       switch (c) {
       | 'a' .. 'z'
       | 'A' .. 'Z'
       | '0' .. '9'
       | '_' => c
       | _ => ' '
       }
     )
  |> String.of_seq
  |> String.split_on_char(' ')
  |> List.filter(w => w != "");

let flags_are_read_once = () => {
  let ws = words(strip_comments(CC.source.backup_text));
  let occurrences = w => List.length(List.filter(x => x == w, ws));
  /* one binding plus one use */
  check(
    int,
    "`dark_mode` is bound once and read once",
    2,
    occurrences("dark_mode"),
  );
  check(
    int,
    "`high_contrast` is bound once and read once",
    2,
    occurrences("high_contrast"),
  );
  /* One branch total: a single `case` on the flag pair covers all four
     schemes, so each flag is read once and no `if` survives. Nested `if`s
     would read one of the two flags twice. */
  check(int, "the slide branches exactly once", 1, occurrences("case"));
  check(int, "the slide uses no `if`", 0, occurrences("if"));
};

/* The slide MUST take the fast (menhir) parse path. When it does not, nothing
   fails and nothing is logged to the user — the recovering parser is quadratic,
   so the only symptom is that loading the Colors slide takes ~25 seconds
   instead of ~10ms. The trap is easy to spring: a labelled field written
   `BR2 = ...` instead of `` `BR2` = ... `` lexes as a CONSTRUCTOR, which
   menhir rejects. */
let slide_takes_the_fast_parse_path = () =>
  switch (
    Haz3lcore.FastParse.parsed_of_text(
      /* the same hooks PersistentZipper.parse_text uses, so this asks the
         real question: does the LOAD path take the fast route? */
      ~materialize=Haz3lcore.Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      CC.source.backup_text,
    )
  ) {
  | Ok(_) => ()
  | Error(note) =>
    fail(
      "colors.hz does not parse with FastParse, so loading it falls back to "
      ++ "the quadratic parser: "
      ++ note,
    )
  };

/* Everything above evaluates the slide as committed — both flags false — so
   only `light` is ever exercised. Statics does cover all four schemes (the
   whole program is analyzed), but evaluation does not, and it is evaluation
   the applier reads.

   The two flags are closed `^^check` literals, so rewriting them in the
   source text is the only lever the program offers. Splitting on the literal
   also pins that there are exactly two of them. */
let split_on_string = (~needle, s) => {
  let (n, h) = (String.length(needle), String.length(s));
  let rec go = (start, i, acc) =>
    if (i + n > h) {
      List.rev([String.sub(s, start, h - start), ...acc]);
    } else if (String.sub(s, i, n) == needle) {
      go(i + n, i + n, [String.sub(s, start, i - start), ...acc]);
    } else {
      go(start, i + 1, acc);
    };
  go(0, 0, []);
};

let flag_literal = "^^check(false)";

/* dark_mode is bound first, high_contrast second. */
let source_with_flags = (~dark_mode: bool, ~high_contrast: bool) =>
  switch (split_on_string(~needle=flag_literal, CC.source.backup_text)) {
  | [before, between, after] =>
    let lit = v => "^^check(" ++ string_of_bool(v) ++ ")";
    before
    ++ lit(dark_mode)
    ++ between
    ++ lit(high_contrast)
    ++ after
    |> Haz3lcore.PersistentZipper.of_slide_text;
  | parts =>
    failf(
      "colors.hz: expected exactly two %s flag literals, found %d",
      flag_literal,
      List.length(parts) - 1,
    )
  };

let schemes = [
  ("light", false, false),
  ("contrast_light", false, true),
  ("dark", true, false),
  ("contrast_dark", true, true),
];

let scheme_vars = ((_, dark_mode, high_contrast)) =>
  CC.vars_of_source(source_with_flags(~dark_mode, ~high_contrast));

let evaluated_schemes = lazy(List.map(s => (s, scheme_vars(s)), schemes));

/* Each scheme must define the whole contract and render to real CSS — a
   scheme nothing evaluates can go bad silently, and only `light` is on the
   default path. */
let every_scheme_is_complete_and_css = () =>
  List.iter(
    (((label, _, _), vars)) => {
      let produced = List.map(fst, vars);
      check(
        list(string),
        label ++ ": every declared colour is produced",
        [],
        List.filter(n => !List.mem(n, produced), declared_names),
      );
      check(
        list(string),
        label ++ ": no colour renders to something CSS cannot parse",
        [],
        unparseable(vars),
      );
    },
    Lazy.force(evaluated_schemes),
  );

/* The flags must COMPOSE. This is the regression: the switch used to read
   `high_contrast` first and return, so `dark_mode` was dead whenever high
   contrast was on and (true, true) gave back the light high-contrast scheme.
   Any scheme pair that renders identically means a flag is being ignored. */
let schemes_are_pairwise_distinct = () => {
  let evaluated = Lazy.force(evaluated_schemes);
  let collisions =
    List.concat_map(
      (((a, _, _), va)) =>
        List.filter_map(
          (((b, _, _), vb)) =>
            a < b && va == vb ? Some(a ++ " == " ++ b) : None,
          evaluated,
        ),
      evaluated,
    );
  check(list(string), "no two schemes render identically", [], collisions);
};

/* The startup path rests on this: a slide that does not satisfy the contract
   must yield NO colours -- not a partial theme, and not an exception.
   `apply_theme_at_startup` reads `[]` as "change nothing", which keeps the
   last theme up while the user repairs the slide. A partial theme would
   half-apply instead, and an exception would take the app down before Bonsai
   starts. */
let a_non_theme_yields_no_colours = () =>
  List.iter(
    ((label, text)) =>
      check(
        list(pair(string, string)),
        label ++ ": produces no colours",
        [],
        CC.vars_of_source(Haz3lcore.PersistentZipper.of_slide_text(text)),
      ),
    [
      ("an int", "1 + 1"),
      ("a string", "\"not a theme\""),
      ("empty", ""),
      ("the wrong record", "(palette = 1, roles = 2)"),
      /* The shape the slide had before the palette/roles rewrite. */
      ("the old flat list", "[(\"T1\", \"oklch(97% 0.025 90)\")]"),
    ],
  );

/* ── Golden: every variable, every scheme ──────────────────────────────

      The slide is 165 variables across four schemes, and the interesting
      failure is not "it errored" but "a colour moved". Nothing above would
      notice: the contract check only asks WHICH names appear, and the CSS check
      only asks whether each value parses. So the values themselves are pinned
      here, and a diff is the review.

      Regenerate deliberately, never reflexively -- the diff IS the thing to
      read, and an unexplained line in it is the bug:

          UPDATE_COLOR_GOLDEN=1 ./run_tests test 'ColorConfiguration'
   */
let golden_candidates = [
  "test/goldens/colors.tsv",
  "../test/goldens/colors.tsv",
  "../../test/goldens/colors.tsv",
  "../../../test/goldens/colors.tsv",
  "../../../../test/goldens/colors.tsv",
];

let golden_path = () =>
  switch (List.find_opt(Sys.file_exists, golden_candidates)) {
  | Some(p) => p
  | None =>
    /* First run, or the file was deleted: write where the tree root is. */
    switch (
      List.find_opt(
        d => Sys.file_exists(d),
        [
          "test/goldens",
          "../test/goldens",
          "../../test/goldens",
          "../../../test/goldens",
          "../../../../test/goldens",
        ],
      )
    ) {
    | Some(d) => Filename.concat(d, "colors.tsv")
    | None => failwith("Colors golden: cannot locate test/goldens")
    }
  };

let render_golden = (): string => {
  let buf = Buffer.create(64 * 1024);
  List.iter(
    (((label, _, _), vars)) =>
      List.iter(
        ((n, v)) =>
          Buffer.add_string(
            buf,
            Printf.sprintf("%s\t%s\t%s\n", label, n, v),
          ),
        List.sort(compare, vars),
      ),
    Lazy.force(evaluated_schemes),
  );
  Buffer.contents(buf);
};

let read_file = (path: string): string => {
  let ic = open_in_bin(path);
  let n = in_channel_length(ic);
  let s = really_input_string(ic, n);
  close_in(ic);
  s;
};

let write_file = (path: string, s: string): unit => {
  let oc = open_out_bin(path);
  output_string(oc, s);
  close_out(oc);
};

let colours_match_golden = () => {
  let path = golden_path();
  let actual = render_golden();
  if (Sys.getenv_opt("UPDATE_COLOR_GOLDEN") != None) {
    write_file(path, actual);
    check(bool, "golden rewritten (" ++ path ++ ")", true, true);
  } else if (!Sys.file_exists(path)) {
    failwith(
      "Colors golden missing at "
      ++ path
      ++ " -- run UPDATE_COLOR_GOLDEN=1 ./run_tests test 'ColorConfiguration'",
    );
  } else {
    let expected = read_file(path);
    /* Report the differing LINES, not a 40KB blob: alcotest would print both
       whole files and the actual change would be unfindable. */
    let split = t =>
      String.split_on_char('\n', t) |> List.filter(l => l != "");
    let (e, a) = (split(expected), split(actual));
    let missing = List.filter(l => !List.mem(l, a), e);
    let extra = List.filter(l => !List.mem(l, e), a);
    check(
      list(string),
      "no colour changed value (was, per the golden)",
      [],
      missing,
    );
    check(list(string), "no colour appeared or moved (is, now)", [], extra);
  };
};

/* ── The fan-out table ─────────────────────────────────────────────────

   These need no evaluation: they are properties of the table alone, so they
   fail in milliseconds and point straight at the row that is wrong. The
   table is the join between what a themer writes and what the stylesheets
   read, and a mistake in it is invisible from either side. */
let table_is_well_formed = () => {
  check(
    list(string),
    "no field expands to an empty target list",
    [],
    List.filter_map(
      (((group, name), targets)) =>
        targets == [] ? Some(group ++ "." ++ name) : None,
      CC.aliases,
    ),
  );
  /* Two fields writing one property means whichever evaluates last silently
     wins, which is a coin toss dressed up as a theme. */
  let dupes =
    List.filter(
      t => List.length(List.filter((==)(t), CC.all_targets)) > 1,
      List.sort_uniq(compare, CC.all_targets),
    );
  check(list(string), "no property is written by two fields", [], dupes);
  /* An alias for a field the slide does not have is a silent no-op. */
  check(
    list(string),
    "every aliased field exists",
    [],
    List.filter(((key, _)) => !List.mem(key, CC.field_names), CC.aliases)
    |> List.map((((group, name), _)) => group ++ "." ++ name),
  );
};

/* The gate: a slide missing even one property yields nothing rather than a
   half-applied theme. */
let a_partial_theme_yields_nothing = () => {
  let text = CC.source.backup_text;
  /* Drop one leaf by breaking its colour, leaving the shape intact. */
  let broken =
    /* Break one leaf while leaving the shape intact. Anchored on the value,
       not on a field name, so a rename does not silently turn this test into
       a no-op that always passes. */
    Str.replace_first(Str.regexp_string("= Transparent,"), "= 1,", text);
  check(bool, "the edit applied", true, broken != text);
  check(
    list(pair(string, string)),
    "one undecodable colour yields no theme at all",
    [],
    CC.vars_of_source(Haz3lcore.PersistentZipper.of_slide_text(broken)),
  );
};

/* ── The generated stylesheet ──────────────────────────────────────────

     Every colour the theme writes also needs a default, for the window before
     the theme is applied -- a first-ever load, or one whose slide fails to
     evaluate. Those defaults used to be hand-written, in a second idiom
     (`oklch(from var(--frame-1) 70% c h)`) that restated the slide's
     derivations in CSS, and some had drifted into projector stylesheets as
     `hsl()` colours from a palette that no longer existed.

     `style/theme-generated.css` is emitted from the light scheme instead, so a
     default is byte-for-byte the value the theme will set and nothing shifts
     when the theme lands. The WHOLE file is generated, header included, which
     is why this compares it entire rather than splicing a marked block: a
     generated file that also holds hand-written lines invites edits to the
     generated half. Regenerate with:

         make update-css-defaults
   */
let theme_css_rel = "src/web/www/style/theme-generated.css";

let theme_css_path = () =>
  switch (
    List.find_opt(
      Sys.file_exists,
      List.map(
        p => p ++ theme_css_rel,
        ["", "../", "../../", "../../../", "../../../../"],
      ),
    )
  ) {
  | Some(p) => p
  | None => failwith("cannot find " ++ theme_css_rel)
  };

let render_theme_css = (): string => {
  let light = List.assoc(List.hd(schemes), Lazy.force(evaluated_schemes));
  let decls =
    List.sort(compare, light)
    |> List.map(((n, v)) => Printf.sprintf("  --%s: %s;\n", n, v))
    |> String.concat("");
  {|/* GENERATED FILE -- DO NOT EDIT.

   Emitted from the Colors configuration slide (hazel-programs/config/colors.hz)
   by `make update-css-defaults`, which is checked by Test_ColorConfiguration.
   Any edit here is overwritten by the next run.

   These are DEFAULTS. At startup the app evaluates the slide and writes every
   one of these properties onto the document, so what a running editor shows
   comes from the slide, not from this file. This is what the first frame uses,
   before that happens, and what remains if the slide does not evaluate --
   which is why it is the light scheme, byte-for-byte, rather than anything
   hand-chosen.

   What each name means is documented where it is decided: the role groups in
   colors.hz, and the fan-out table in src/web/util/ColorConfiguration.re.
   See src/web/www/style/README.md for the whole dataflow. */
:root {
|}
  ++ decls
  ++ "}\n";
};

let theme_css_is_current = () => {
  let path = theme_css_path();
  let wanted = render_theme_css();
  if (Sys.getenv_opt("UPDATE_CSS_DEFAULTS") != None) {
    write_file(path, wanted);
    check(bool, "theme stylesheet rewritten (" ++ path ++ ")", true, true);
  } else {
    let split = t => String.split_on_char('\n', t);
    let (e, a) = (split(wanted), split(read_file(path)));
    check(
      list(string),
      theme_css_rel ++ " is stale (missing; run `make update-css-defaults`)",
      [],
      List.filter(l => !List.mem(l, a), e),
    );
    check(
      list(string),
      theme_css_rel ++ " is stale (extra)",
      [],
      List.filter(l => !List.mem(l, e), a),
    );
  };
};

let tests = [
  (
    "ColorConfiguration",
    [
      /* First on purpose: every case below parses the slide, so if the fast
         path is lost they all take the quadratic one and the suite runs for
         minutes before anything says why. */
      test_case(
        "takes the fast parse path",
        `Quick,
        slide_takes_the_fast_parse_path,
      ),
      test_case(
        "built-in source satisfies expected type",
        `Quick,
        source_satisfies_expected_type,
      ),
      test_case("analysis is engaged", `Quick, analysis_is_engaged),
      test_case("slide matches its contract", `Quick, slide_matches_contract),
      test_case("every value is valid CSS", `Quick, every_value_is_css),
      test_case("scheme flags read once", `Quick, flags_are_read_once),
      test_case("fan-out table is well formed", `Quick, table_is_well_formed),
      test_case(
        "a partial theme yields nothing",
        `Quick,
        a_partial_theme_yields_nothing,
      ),
      test_case("colours match golden", `Quick, colours_match_golden),
      test_case(
        "generated stylesheet is current",
        `Quick,
        theme_css_is_current,
      ),
      test_case(
        "a non-theme yields no colours",
        `Quick,
        a_non_theme_yields_no_colours,
      ),
      test_case(
        "every scheme is complete and valid CSS",
        `Quick,
        every_scheme_is_complete_and_css,
      ),
      test_case(
        "schemes are pairwise distinct",
        `Quick,
        schemes_are_pairwise_distinct,
      ),
    ],
  ),
];
