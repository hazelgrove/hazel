open Alcotest;
open Haz3lcore;
open Poly;

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
    /* Slow: it parses every .hz program in the repo */
    test_case(
      "every corpus program fast-paths",
      `Slow,
      () => {
        let files =
          ["hazel-programs", "../hazel-programs"]
          |> List.concat_map(~f=ParsedCorpus.find_hz)
          |> List.dedup_and_sort(~compare=Poly.compare);
        let t0 = Stdlib.Sys.time();
        let (ok, bail, worst) =
          List.fold_left(
            ~f=
              ((ok, bail, worst), path) => {
                /* mirror the production load path (of_slide_text flattens
                   committed indentation; the reader strips only the file's
                   final newline — other edge whitespace is content) */
                let src =
                  In_channel.read_all(path)
                  |> Util.StringUtil.trim_leading
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
                  printf(
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
        printf(
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
