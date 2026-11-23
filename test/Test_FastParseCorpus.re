open Alcotest;
open Haz3lcore;

let rec find_hz = (dir: string): list(string) =>
  switch (Sys.readdir(dir)) {
  | entries =>
    entries
    |> Array.to_list
    |> List.concat_map(entry => {
         let path = Filename.concat(dir, entry);
         switch (Sys.is_directory(path)) {
         | true => find_hz(path)
         | false => Filename.check_suffix(entry, ".hz") ? [path] : []
         | exception _ => []
         };
       })
  | exception _ => []
  };

let read_file = (path: string): string => {
  let ic = open_in_bin(path);
  let n = in_channel_length(ic);
  let s = really_input_string(ic, n);
  close_in(ic);
  s;
};

/* Ratchet: every .hz program in the repo must take the FastParse zip
   path (parse + verbatim token zip). A bail here means some construct
   agents/users write falls back to the quadratic typing parser — fix
   FastParse or the printer rather than tolerating it. Skips silently
   when the corpus is unreachable (sandboxed dune runtest). */
/* Empty as of 2026-08-08: every .hz in the repo takes the fast path.
   A new entry here means a construct regressed off it — fix the
   grammar/printer rather than ledgering, unless the file is a
   deliberately-invalid or delimiter-incomplete exhibit. */
let known_gaps: list(string) = [];

/* a slide's source indentation reaches the editor as spaces */
let slide_keeps_indentation = () => {
  let z =
    PersistentZipper.unpersist(
      PersistentZipper.of_slide_text(
        "module M = {\n  let x = 1\n} in\nM.x\n",
      ),
      ~root=Sort.Exp,
    );
  check(
    string,
    "indented as written",
    "module M = {\n  let x = 1\n} in\nM.x",
    MarkerParse.to_text(Zipper.unzip(Zipper.unselect_and_zip(z))),
  );
};

/* Every editor of every bundled exercise (Exercises mode) and derivation
   slide (Documentation › Derivations), as loaded. These ship as stored
   zippers, so their indentation is whatever spaces were stored. */
let bundled_exercise_editors = (): list((string, Zipper.t)) => {
  let drv = (title, s: Web.DerivationExercise.spec) => {
    let acc = ref([]);
    let _: Web.DerivationExercise.p(unit) =
      Web.DerivationExercise.mapi(s, (pos, z) =>
        acc :=
          [
            (title ++ " " ++ Web.DerivationExercise.show_pos(pos), z),
            ...acc^,
          ]
      );
    List.rev(acc^);
  };
  let named = (title, zs) =>
    List.mapi((i, z) => (title ++ " #" ++ string_of_int(i), z), zs);
  List.concat_map(
    (e: Web.Exercise.t) =>
      switch (e) {
      | Code(s) => named(s.title, Web.CodeExercise.editors(s))
      | Theorem(s) => named(s.title, [s.prelude, s.lemmas, s.theorem])
      | Derivation(s) => drv(s.title, s)
      },
    Web.ExerciseSettings_base.exercises,
  )
  @ List.concat_map(
      ((name, s)) => drv(name, s),
      Web.Init.documentation_drv_slides,
    );
};

/* Bundled exercises load indented. They were written when the editor
   computed indentation at layout time, so they were stored flat (every
   line at column 0) and loaded flat once indentation became the text's
   own spaces. A line that re-indenting (Format › Indent) moves in must
   already be indented; how far is the author's call. */
let exercises_keep_indentation = () => {
  let indented_lines = z =>
    MarkerParse.to_text(z)
    |> String.split_on_char('\n')
    |> List.map(line => String.length(line) > 0 && line.[0] == ' ');
  bundled_exercise_editors()
  |> List.iter(((label, z)) =>
       check(
         list(bool),
         label,
         indented_lines(Perform.reindent_zipper(z)),
         indented_lines(z),
       )
     );
  let prelude =
    switch (List.hd(Web.ExerciseSettings_base.exercises)) {
    | Code(s) => s.prelude
    | _ => fail("expected Oddly Recursive first")
    };
  check(
    string,
    "Oddly Recursive prelude",
    "let not : Bool -> Bool =\n  fun x ->\n    if x then false else true\nin ¿",
    MarkerParse.to_text(prelude),
  );
};

/* A tutorial lesson loads with its .hzt source's indentation: every line
   of each lesson's implementation, solution and hidden tests is a line of
   its source, leading spaces included. */
let tutorials_keep_indentation = () =>
  List.combine(Web.TutorialText.all, Tutorialslides.Slides.all)
  |> List.iter(((lesson: Web.Tutorial.spec, (rel, raw))) => {
       let source_lines = String.split_on_char('\n', raw);
       let check_editor = (part, z) =>
         MarkerParse.to_text(z)
         |> String.split_on_char('\n')
         |> List.filter(line => String.trim(line) != "")
         |> List.iter(line =>
              check(
                bool,
                rel ++ " " ++ part ++ ": " ++ String.escaped(line),
                true,
                List.mem(line, source_lines),
              )
            );
       check_editor("@code", lesson.your_impl);
       /* a lesson without @test gets a default one */
       if (List.exists(l => String.trim(l) == "@test", source_lines)) {
         check_editor("@test", lesson.hidden_tests.tests);
       };
       Option.iter(check_editor("@solution"), lesson.solution);
     });

let tests = (
  "FastParseCorpus",
  [
    /* Slow: it parses every .hz program in the repo */
    test_case(
      "a slide keeps its indentation",
      `Quick,
      slide_keeps_indentation,
    ),
    test_case(
      "bundled exercises keep their indentation",
      `Quick,
      exercises_keep_indentation,
    ),
    test_case(
      "tutorials keep their indentation",
      `Quick,
      tutorials_keep_indentation,
    ),
    test_case(
      "every corpus program fast-paths",
      `Slow,
      () => {
        let files =
          ["hazel-programs", "../hazel-programs"]
          |> List.concat_map(find_hz)
          |> List.sort_uniq(compare);
        let t0 = Sys.time();
        let (ok, bail, worst) =
          List.fold_left(
            ((ok, bail, worst), path) => {
              /* mirror the production load path (of_slide_text keeps
                 committed indentation; the reader strips only the file's
                 final newline — other edge whitespace is content) */
              let src =
                read_file(path) |> Util.StringUtil.strip_final_newline;
              let f0 = Sys.time();
              let known_gap = List.mem(Filename.basename(path), known_gaps);
              let r =
                known_gap
                  ? None
                  : FastParse.of_text(
                      ~materialize=Triggers.invoked_projector,
                      ~collect_refractors=true,
                      ~root=Exp,
                      src,
                    );
              let ms = (Sys.time() -. f0) *. 1000.;
              let worst =
                ms > snd(worst) ? (Filename.basename(path), ms) : worst;
              switch (r) {
              | Some(_) => (ok + 1, bail, worst)
              | None when known_gap => (ok, bail, worst)
              | None =>
                Printf.printf(
                  "BAIL %s: %s\n",
                  path,
                  Option.value(FastParse.bail_note^, ~default="?"),
                );
                (ok, bail + 1, worst);
              };
            },
            (0, 0, ("", 0.)),
            files,
          );
        let total = (Sys.time() -. t0) *. 1000.;
        Printf.printf(
          "HZPROBE: %d files, %d fast-path OK, %d bail, total %.1fms, worst %s %.1fms\n",
          List.length(files),
          ok,
          bail,
          total,
          fst(worst),
          snd(worst),
        );
        check(int, "corpus files that bailed", 0, bail);
      },
    ),
  ],
);
