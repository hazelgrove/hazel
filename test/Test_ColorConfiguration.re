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
let evaluated =
  lazy(
    CC.css_vars_of_value(
      ConfigSlideCheck.evaluate(~ana=CC.expected_type, CC.source),
    )
  );
let evaluated_vars = () => Lazy.force(evaluated);

let declared_names = CC.palette @ List.concat_map(snd, CC.role_groups);

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

/* The applier writes these straight into a CSS custom property, where an
   unparseable value is silently ignored by the browser. */
let every_value_is_css = () => {
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
      || starts_with("#", s)
    )
    && !List.exists(bad => contains(bad, s), ["nan", "inf", "e-", "e+"]);
  check(
    list(string),
    "no colour renders to something CSS cannot parse",
    [],
    evaluated_vars()
    |> List.filter(((_, v)) => !plausible(v))
    |> List.map(((n, v)) => n ++ " = " ++ v),
  );
};

/* The shape the slide is written for. The scheme flags are read at the very
   bottom and NOWHERE else: everything above is written once and reused by all
   three schemes. This is the property the slide was restructured to get, and
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

let words = text =>
  String.split_on_char('\n', text)
  |> String.concat(" ")
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
  /* One branch per flag, and no more: three schemes need exactly two. */
  check(
    int,
    "the slide branches once per scheme flag",
    2,
    occurrences("if"),
  );
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

let tests = [
  (
    "ColorConfiguration",
    [
      test_case(
        "built-in source satisfies expected type",
        `Quick,
        source_satisfies_expected_type,
      ),
      test_case("analysis is engaged", `Quick, analysis_is_engaged),
      test_case("slide matches its contract", `Quick, slide_matches_contract),
      test_case("every value is valid CSS", `Quick, every_value_is_css),
      test_case("scheme flags read once", `Quick, flags_are_read_once),
      test_case(
        "takes the fast parse path",
        `Quick,
        slide_takes_the_fast_parse_path,
      ),
    ],
  ),
];
