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
              let src = read_file(path) |> String.trim;
              let f0 = Sys.time();
              let r =
                FastParse.of_text(
                  ~materialize=Triggers.invoked_projector,
                  ~root=Exp,
                  src,
                );
              let ms = (Sys.time() -. f0) *. 1000.;
              let worst =
                ms > snd(worst) ? (Filename.basename(path), ms) : worst;
              switch (r) {
              | Some(_) => (ok + 1, bail, worst)
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
