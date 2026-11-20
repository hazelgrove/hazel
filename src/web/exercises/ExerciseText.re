/*
 * ExerciseText: Exercises-mode code and theorem exercises as .hzt text
 * (hazel-programs/exercises/, format in that directory's README.md). The
 * files are embedded at compile time (ppx_blob, listed in
 * ExerciseSettings_base) and parsed here at startup, as tutorial lessons
 * are (TutorialText); the section syntax is HztText's. `to_file` is the
 * inverse, behind "Export Exercise Module".
 */
open Haz3lcore;

[@deriving (eq, enumerate)]
type marker =
  | Title
  | Flags
  | Points
  | Prompt
  | Prelude
  | Code
  | YourTests
  | Solution
  | Bug
  | Test
  | Hints
  | Syntax
  | Lemmas
  | Theorem
  /* text before the first marker; ignored */
  | Preamble;

let name_of =
  fun
  | Title => "title"
  | Flags => "flags"
  | Points => "points"
  | Prompt => "prompt"
  | Prelude => "prelude"
  | Code => "code"
  | YourTests => "your-tests"
  | Solution => "solution"
  | Bug => "bug"
  | Test => "test"
  | Hints => "hints"
  | Syntax => "syntax"
  | Lemmas => "lemmas"
  | Theorem => "theorem"
  | Preamble => "";

/* Markers whose line carries the hint shown when the section's check
   fails: `@bug <hint>`, `@syntax <hint>`. */
let takes_arg =
  fun
  | Bug
  | Syntax => true
  | _ => false;

let marker_of = (name: string, arg: string): option(marker) =>
  List.find(all_of_marker, ~f=m =>
    !String.is_empty(name)
    && String.equal(name_of(m), name)
    && (String.is_empty(arg) || takes_arg(m))
  );

/* "oddly-recursive.hzt" -> "Ex_OddlyRecursive", the module name an
   exercise has unless its @flags give `module=`. */
let module_name_of = (rel: string): string =>
  "Ex_"
  ++ (
    fst(Filename.split_extension(Filename.basename(rel)))
    |> String.split(~on='-')
    |> List.map(~f=String.capitalize)
    |> String.concat(~sep="")
  );

/* The inverse, for exporting: "Ex_OddlyRecursive" -> "oddly-recursive.hzt". */
let filename_of = (module_name: string): string => {
  let base = String.chop_prefix_if_exists(module_name, ~prefix="Ex_");
  let buf = Stdlib.Buffer.create(String.length(base) + 8);
  String.iteri(
    base,
    ~f=(i, c) => {
      if (i > 0 && Char.is_uppercase(c)) {
        Stdlib.Buffer.add_char(buf, '-');
      };
      Stdlib.Buffer.add_char(buf, Char.lowercase(c));
    },
  );
  (Stdlib.Buffer.length(buf) == 0 ? "exercise" : Stdlib.Buffer.contents(buf))
  ++ ".hzt";
};

/* ---------- text -> spec ---------- */

let of_text = (~rel: string, text: string): Exercise.t => {
  let fail = msg => failwith("ExerciseText: " ++ rel ++ ": " ++ msg);
  let sections = HztText.sections(~marker_of, ~default=Preamble, text);
  let all = m =>
    List.filter(sections, ~f=(s: HztText.section(marker)) =>
      equal_marker(s.marker, m)
    );
  let lines = m =>
    all(m) |> List.concat_map(~f=(s: HztText.section(marker)) => s.lines);
  let has = m => !List.is_empty(all(m));
  let settings = (m, ~keys) =>
    HztText.tokens(lines(m))
    |> List.map(~f=tok =>
         switch (HztText.key_value(tok)) {
         | Some((k, v)) when List.mem(keys, k, ~equal=String.equal) => (
             k,
             v,
           )
         | _ =>
           fail(
             "@"
             ++ name_of(m)
             ++ ": expected one of "
             ++ String.concat(~sep=", ", List.map(keys, ~f=k => k ++ "="))
             ++ ", got "
             ++ tok,
           )
         }
       );
  let int_setting = (kvs, key, ~default) =>
    switch (List.Assoc.find(kvs, key, ~equal=String.equal)) {
    | None => default
    | Some(v) =>
      switch (Int.of_string_opt(v)) {
      | Some(n) => n
      | None => fail(key ++ "=" ++ v ++ " is not a number")
      }
    };
  let is_theorem = has(Lemmas) || has(Theorem);
  if (is_theorem) {
    switch (
      List.find(
        [Code, YourTests, Solution, Bug, Test, Hints, Syntax],
        ~f=has,
      )
    ) {
    | Some(m) =>
      fail("@" ++ name_of(m) ++ " is not part of a theorem exercise")
    | None => ()
    };
  };
  let flags =
    settings(
      Flags,
      ~keys=
        is_theorem
          ? ["id", "module"]
          : ["id", "module", "required_tests", "provided_tests"],
    );
  let points =
    settings(
      Points,
      ~keys=
        is_theorem
          ? ["max"] : ["test_validation", "mutation_testing", "impl_grading"],
    );
  /* Saved progress is keyed on the id, so it is never derived. */
  let id =
    switch (List.Assoc.find(flags, "id", ~equal=String.equal)) {
    | None => fail("@flags needs id=<uuid>")
    | Some(s) =>
      switch (Id.of_string(s)) {
      | Some(id) => id
      | None => fail("id=" ++ s ++ " is not a UUID")
      }
    };
  let module_name =
    List.Assoc.find(flags, "module", ~equal=String.equal)
    |> Option.value(~default=module_name_of(rel));
  let title =
    switch (HztText.prose(lines(Title))) {
    | "" => fail("@title is missing")
    | t => t
    };
  let prompt = HztText.prose(lines(Prompt));
  let editor = (~label, ls) =>
    HztText.zipper_of(~source=label ++ " of " ++ rel, HztText.code(ls));
  let editor_of = m => editor(~label="@" ++ name_of(m), lines(m));
  if (is_theorem) {
    Theorem({
      id,
      title,
      module_name,
      prompt,
      max_points: int_setting(points, "max", ~default=0),
      prelude: editor_of(Prelude),
      lemmas: editor_of(Lemmas),
      theorem: editor_of(Theorem),
    });
  } else {
    Code({
      id,
      title,
      module_name,
      prompt,
      point_distribution: {
        test_validation: int_setting(points, "test_validation", ~default=0),
        mutation_testing: int_setting(points, "mutation_testing", ~default=0),
        impl_grading: int_setting(points, "impl_grading", ~default=0),
      },
      prelude: editor_of(Prelude),
      correct_impl: editor_of(Solution),
      your_tests: {
        tests: editor_of(YourTests),
        required: int_setting(flags, "required_tests", ~default=0),
        provided: int_setting(flags, "provided_tests", ~default=0),
      },
      your_impl: editor_of(Code),
      hidden_bugs:
        all(Bug)
        |> List.mapi(
             ~f=(i, s: HztText.section(marker)): CodeExercise.wrong_impl(
                                                     Zipper.t,
                                                   ) =>
             {
               impl: editor(~label="@bug #" ++ string_of_int(i + 1), s.lines),
               hint: s.arg,
             }
           ),
      hidden_tests: {
        tests: editor_of(Test),
        hints:
          lines(Hints)
          |> List.map(~f=String.strip)
          |> List.filter(~f=h => !String.is_empty(h)),
      },
      syntax_tests:
        all(Syntax)
        |> List.map(~f=(s: HztText.section(marker)) => {
             let src = HztText.code(s.lines);
             switch (
               Sexplib.Sexp.of_string(src) |> SyntaxTest.predicate_of_sexp
             ) {
             | predicate => (s.arg, predicate)
             | exception _ =>
               fail(
                 "@syntax "
                 ++ s.arg
                 ++ ": expected a predicate like (IsRecursive f), got "
                 ++ src,
               )
             };
           }),
    });
  };
};

/* ---------- spec -> text ---------- */

/* A hint is one line of text after its marker. */
let one_line = (s: string): string =>
  String.split(s, ~on='\n')
  |> List.map(~f=String.strip)
  |> String.concat(~sep=" ");

let section = (~arg="", m: marker, body: string): string =>
  "@"
  ++ name_of(m)
  ++ (String.is_empty(arg) ? "" : " " ++ one_line(arg))
  ++ "\n"
  ++ body
  ++ "\n";

let program = (z: Zipper.t): string => MarkerParse.to_text(z);

/* `module=` only when the filename would not give it back. */
let flags = (~id, ~module_name, settings: list((string, string))): string => {
  let module_flag =
    String.equal(module_name_of(filename_of(module_name)), module_name)
      ? [] : [("module", module_name)];
  [("id", Id.to_string(id)), ...module_flag]
  @ settings
  |> List.map(~f=((k, v)) => k ++ "=" ++ v)
  |> String.concat(~sep=" ");
};

let code_to_text = (s: CodeExercise.spec): string =>
  [
    section(Title, s.title),
    section(
      Flags,
      flags(
        ~id=s.id,
        ~module_name=s.module_name,
        [
          ("required_tests", string_of_int(s.your_tests.required)),
          ("provided_tests", string_of_int(s.your_tests.provided)),
        ],
      ),
    ),
    section(
      Points,
      Printf.sprintf(
        "test_validation=%d mutation_testing=%d impl_grading=%d",
        s.point_distribution.test_validation,
        s.point_distribution.mutation_testing,
        s.point_distribution.impl_grading,
      ),
    ),
    section(Prompt, String.strip(s.prompt)),
    section(Prelude, program(s.prelude)),
    section(Code, program(s.your_impl)),
    section(YourTests, program(s.your_tests.tests)),
    section(Solution, program(s.correct_impl)),
  ]
  @ List.map(
      s.hidden_bugs, ~f=({impl, hint}: CodeExercise.wrong_impl(Zipper.t)) =>
      section(~arg=hint, Bug, program(impl))
    )
  @ [section(Test, program(s.hidden_tests.tests))]
  @ (
    switch (
      s.hidden_tests.hints
      |> List.map(~f=one_line)
      |> List.filter(~f=h => !String.is_empty(h))
    ) {
    | [] => []
    | hints => [section(Hints, String.concat(~sep="\n", hints))]
    }
  )
  @ List.map(s.syntax_tests, ~f=((hint, predicate)) =>
      section(
        ~arg=hint,
        Syntax,
        SyntaxTest.sexp_of_predicate(predicate) |> Sexplib.Sexp.to_string,
      )
    )
  |> String.concat(~sep="\n");

let theorem_to_text = (s: TheoremExercise.spec): string =>
  [
    section(Title, s.title),
    section(Flags, flags(~id=s.id, ~module_name=s.module_name, [])),
    section(Points, "max=" ++ string_of_int(s.max_points)),
    section(Prompt, String.strip(s.prompt)),
    section(Prelude, program(s.prelude)),
    section(Lemmas, program(s.lemmas)),
    section(Theorem, program(s.theorem)),
  ]
  |> String.concat(~sep="\n");

let to_file = (e: Exercise.t): option((string, string)) =>
  switch (e) {
  | Code(s) => Some((filename_of(s.module_name), code_to_text(s)))
  | Theorem(s) => Some((filename_of(s.module_name), theorem_to_text(s)))
  | Derivation(_) => None
  };
