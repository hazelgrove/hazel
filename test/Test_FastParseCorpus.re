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
/* A new entry here means a construct regressed off the fast path — fix
   the grammar/printer rather than ledgering, unless the file is a
   deliberately-invalid, stale, or delimiter-incomplete exhibit. The
   study-old sources below are of that kind: they were written against a
   string-equality operator the language no longer has. */
let known_gaps: list((string, string)) = [
  (
    "calculator.hz",
    "study-old slide: uses the removed `$==` string-equality operator",
  ),
  (
    "calculator-bug-associativity.hz",
    "study-old slide: uses the removed `$==` string-equality operator",
  ),
  (
    "calculator-bug-precedence.hz",
    "study-old slide: uses the removed `$==` string-equality operator",
  ),
];

/* Tutorial-mode sources (hazel-programs/tutorial/, see its README) are
   not Hazel programs: they are @prompt/@code/@test/@hint/@hints/
   @reference/@flags sections carrying markdown prose, compiled into
   lessons by `./hazel gen-tutorial`. Both corpus walks skip them. A
   marker line is exactly one of those tokens (b2t2 programs contain
   `@<Type>` application lines, which must not match). */
let tutorial_markers = [
  "@prompt",
  "@code",
  "@test",
  "@hint",
  "@hints",
  "@reference",
  "@flags",
];

let is_tutorial_source = (text: string): bool =>
  String.split_on_char('\n', text)
  |> List.exists(line => List.mem(String.trim(line), tutorial_markers));

let tests = (
  "FastParseCorpus",
  [
    test_case(
      "every corpus program fast-paths",
      `Quick,
      () => {
        let files =
          ["hazel-programs", "../hazel-programs"]
          |> List.concat_map(find_hz)
          |> List.sort_uniq(compare)
          |> List.filter(path => !is_tutorial_source(read_file(path)));
        let t0 = Sys.time();
        let (ok, bail, worst) =
          List.fold_left(
            ((ok, bail, worst), path) => {
              /* mirror the production load path (of_slide_text flattens
                 committed indentation; the reader strips only the file's
                 final newline — other edge whitespace is content) */
              let src =
                read_file(path)
                |> Util.StringUtil.trim_leading
                |> Util.StringUtil.strip_final_newline;
              let f0 = Sys.time();
              let known_gap =
                List.mem_assoc(Filename.basename(path), known_gaps);
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
