open Alcotest;
open Haz3lcore;

/* Incremental evaluation against evaluation from scratch, over every
   program in the slide corpus: each program's first few int literals are
   edited in place, one at a time, ids kept (as a text edit keeps them), and
   the edited program evaluated both ways must agree. Programs that do not
   evaluate to a value within the step limit, or have no int literal, are
   skipped; enough must be compared for the test to mean something. */

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
  let s = Stdlib.really_input_string(ic, Stdlib.in_channel_length(ic));
  Stdlib.close_in(ic);
  s;
};

let edits_per_program = 3;
let min_compared = 120;

let tests = (
  "Evaluator.IncrCorpus",
  [
    test_case(
      "incremental eval agrees with fresh eval after literal edits, over the slide corpus",
      `Slow,
      () => {
        let files =
          ["hazel-programs", "../hazel-programs"]
          |> List.concat_map(~f=find_hz)
          |> List.dedup_and_sort(~compare=String.compare);
        let compared = ref(0);
        List.iter(
          files,
          ~f=path => {
            let src =
              read_file(path)
              |> Util.StringUtil.trim_leading
              |> Util.StringUtil.strip_final_newline;
            switch (FastParse.of_text(~root=Exp, src)) {
            | None => ()
            | Some(seg) =>
              let exp = MakeTerm.go(seg).term;
              for (literal_index in 0 to edits_per_program - 1) {
                Test_Evaluator_Properties.check_incremental_against_fresh_after_edit(
                  ~values_only=true,
                  ~step_limit=200000,
                  ~compared,
                  ~literal_index,
                  ~testable=Test_Evaluator_Properties.testable_core_exp,
                  ~msg=
                    Printf.sprintf(
                      "%s, literal %d: incremental and fresh agree",
                      Filename.basename(path),
                      literal_index,
                    ),
                  ~require_comparison=false,
                  exp,
                );
              };
            };
          },
        );
        Printf.eprintf(
          "compared %d literal edits across %d programs\n%!",
          compared^,
          List.length(files),
        );
        if (!List.is_empty(files)) {
          check(
            bool,
            Printf.sprintf(
              "at least %d edits compared (got %d)",
              min_compared,
              compared^,
            ),
            true,
            compared^ >= min_compared,
          );
        };
      },
    ),
  ],
);
