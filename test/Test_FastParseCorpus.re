open Alcotest;
open Haz3lcore;
open Poly;

let rec find_hz = (dir: string): list(string) =>
  switch (Stdlib.Sys.readdir(dir)) {
  | entries =>
    entries
    |> Array.to_list
    |> List.concat_map(~f=entry => {
         let path = Filename.concat(dir, entry);
         switch (Stdlib.Sys.is_directory(path)) {
         | true => find_hz(path)
         | false => Filename.check_suffix(entry, ".hz") ? [path] : []
         | exception _ => []
         };
       })
  | exception _ => []
  };

let read_file = (path: string): string => {
  let ic = Stdlib.open_in_bin(path);
  let n = Stdlib.in_channel_length(ic);
  let s = Stdlib.really_input_string(ic, n);
  Stdlib.close_in(ic);
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
    List.mapi(zs, ~f=(i, z) => (title ++ " #" ++ Int.to_string(i), z));
  List.concat_map(Web.ExerciseSettings_base.exercises, ~f=(e: Web.Exercise.t) =>
    switch (e) {
    | Code(s) => named(s.title, Web.CodeExercise.editors(s))
    | Theorem(s) => named(s.title, [s.prelude, s.lemmas, s.theorem])
    | Derivation(s) => drv(s.title, s)
    }
  )
  @ List.concat_map(Web.Init.documentation_drv_slides, ~f=((name, s)) =>
      drv(name, s)
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
    |> String.split(~on='\n')
    |> List.map(~f=line => String.length(line) > 0 && line.[0] == ' ');
  bundled_exercise_editors()
  |> List.iter(~f=((label, z)) =>
       check(
         list(bool),
         label,
         indented_lines(Perform.reindent_zipper(z)),
         indented_lines(z),
       )
     );
  let prelude =
    switch (List.hd_exn(Web.ExerciseSettings_base.exercises)) {
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
  List.zip_exn(Web.TutorialText.all, Tutorialslides.Slides.all)
  |> List.iter(~f=((lesson: Web.Tutorial.spec, (rel, raw))) => {
       let source_lines = String.split(raw, ~on='\n');
       let check_editor = (part, z) =>
         MarkerParse.to_text(z)
         |> String.split(~on='\n')
         |> List.filter(~f=line => !String.is_empty(String.strip(line)))
         |> List.iter(~f=line =>
              check(
                bool,
                rel ++ " " ++ part ++ ": " ++ Stdlib.String.escaped(line),
                true,
                List.mem(source_lines, line, ~equal=String.equal),
              )
            );
       check_editor("@code", lesson.your_impl);
       /* a lesson without @test gets a default one */
       if (List.exists(source_lines, ~f=l =>
             String.equal(String.strip(l), "@test")
           )) {
         check_editor("@test", lesson.hidden_tests.tests);
       };
       Option.iter(lesson.solution, ~f=check_editor("@solution"));
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
          |> List.concat_map(~f=find_hz)
          |> List.dedup_and_sort(~compare=Poly.compare);
        let t0 = Stdlib.Sys.time();
        let (ok, bail, worst) =
          List.fold_left(
            ~f=
              ((ok, bail, worst), path) => {
                /* mirror the production load path (of_slide_text keeps
                   committed indentation; the reader strips only the file's
                   final newline — other edge whitespace is content) */
                let src =
                  read_file(path)
                  |> Util.StringUtil.strip_final_newline;
                let f0 = Stdlib.Sys.time();
                let known_gap =
                  List.mem(
                    known_gaps,
                    Filename.basename(path),
                    ~equal=Poly.equal,
                  );
                let r =
                  known_gap
                    ? None
                    : FastParse.of_text(
                        ~materialize=Triggers.invoked_projector,
                        ~collect_refractors=true,
                        ~root=Exp,
                        src,
                      );
                let ms = (Stdlib.Sys.time() -. f0) *. 1000.;
                let worst =
                  ms > snd(worst) ? (Filename.basename(path), ms) : worst;
                switch (r) {
                | Some(_) => (ok + 1, bail, worst)
                | None when known_gap => (ok, bail, worst)
                | None =>
                  Stdlib.Printf.printf(
                    "BAIL %s: %s\n",
                    path,
                    Option.value(FastParse.bail_note^, ~default="?"),
                  );
                  (ok, bail + 1, worst);
                };
              },
            ~init=(0, 0, ("", 0.)),
            files,
          );
        let total = (Stdlib.Sys.time() -. t0) *. 1000.;
        Stdlib.Printf.printf(
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
