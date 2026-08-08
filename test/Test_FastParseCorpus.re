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
/* Slides whose .hz needs menhir grammar the batch parser lacks yet —
   they load correctly via the MarkerParse fallback (fidelity is pinned
   by DocSlides.ReparseBackuptext); shrink this list by filling gaps.
   Classes: labeled fun patterns / labeled tuple types, unit fun
   params, paren sums, statement `;` sequences, big-int literals,
   conversion paren drops in module/forall positions. */
let known_gaps: list(string) = [
  "basic-reference.hz",
  "projectors.hz",
  "adts.hz",
  "tuples.hz",
  "modules.hz",
  "tables.hz",
  "polymorphism.hz",
  "cards.hz",
  "probes.hz",
  "example-tables.hz",
  "table-api-constructors-addrows.hz",
  "table-api-constructors-addcolumn.hz",
  "table-api-constructors-buildcolumn.hz",
  "table-api-constructors-hcat.hz",
  "table-api-constructors-values.hz",
  "table-api-constructors-leftjoin.hz",
  "table-api-properties.hz",
  "table-api-access-subcomponents.hz",
  "table-api-subtable.hz",
  "table-api-ordering.hz",
  "table-api-aggregate.hz",
  "table-api-data-cleaning.hz",
  "table-api-utilities-flatten.hz",
  "table-api-utilities-transformcolumn.hz",
  "table-api-utilities-renamecolumns.hz",
  "table-api-utilities-find.hz",
  "table-api-utilities-groupbyretentive.hz",
  "table-api-utilities-groupbysubtractive.hz",
  "table-api-utilities-selectmany.hz",
  "table-api-utilities-groupjoin.hz",
  "table-api-utilities-join.hz",
  "example-programs-phackinghomogeneous.hz",
  "example-programs-phackingheterogeneous.hz",
  "example-programs-quizscorefilter.hz",
  "example-programs-quizscoreselect.hz",
  "example-programs-groupbyretentive.hz",
  "example-programs-groupbysubtractive.hz",
  "errors-malformed-tables.hz",
  "errors-using-tables-part-1.hz",
  "errors-using-tables-part-2.hz",
  "errors-using-tables-part-3.hz",
];

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
