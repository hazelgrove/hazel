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
          |> List.sort_uniq(compare);
        let t0 = Sys.time();
        let (ok, bail, worst) =
          List.fold_left(
            ((ok, bail, worst), path) => {
              /* mirror the production load path (of_slide_text flattens
                 committed indentation; the reader strips only the file's
                 final newline — other edge whitespace is content) */
              let src =
                read_file(path)
                |> Util_web.StringUtil.trim_leading
                |> Util_web.StringUtil.strip_final_newline;
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
