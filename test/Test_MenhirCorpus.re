open Alcotest;
open Haz3lcore;
open Language;

/* Corpus differential ratchet: every .hz program in the repo must parse
   identically through the Menhir parser and the char-by-char editor
   parser. A divergence is NOT a flake — either fix it or add the file to
   known_gaps with a reason; the goal is an empty list. Cases are `Slow
   because the editor parser is quadratic on large files.

   Runs when the corpus is reachable (repo root or test dir cwd, as with
   `bash test/run_node.sh test MenhirCorpus`); skips silently otherwise
   (sandboxed dune runtest). */

/* The docs/B2T2 slide corpus shares Test_FastParseCorpus's ledger:
   those .hz files exercise grammar the menhir parser lacks yet (they
   load via the typing-parser fallback; fidelity is pinned by
   DocSlides.ReparseBackuptext). */
let known_gaps: list((string, string)) =
  [
    (
      "tuples.hz",
      "deliberate error exhibit (1=\"hello\"): MakeTerm reads a MultiHole, menhir a labeled tuple",
    ),
    (
      "table-api-properties.hz",
      "editor tokenizer quirk: [()] reads as [] via MakeTerm (file as editor bug)",
    ),
  ]
  @ List.map(
      name => (name, "menhir grammar gap (Test_FastParseCorpus ledger)"),
      Test_FastParseCorpus.known_gaps,
    );

let corpus_roots = [
  "hazel-programs",
  "../hazel-programs",
  "../../hazel-programs",
];

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

let equal_terms =
  Equality.(
    equality({
      ...syntactic_settings,
      ignore_parens: true,
    }).
      exp
  );

let check_file = (path: string): unit => {
  let txt = read_file(path) |> String.trim;
  let mk =
    switch (Parser.to_zipper(txt, ~root=Exp)) {
    | Some(z) => Some(MakeTerm.from_zip_for_sem(z, ~root=Exp).term)
    | None => None
    };
  let mh =
    switch (MenhirParser.Interface.parse_program(txt)) {
    | ast =>
      Ok(
        Grammar.map_exp_annotation(
          _ => IdTagged.IdTag.fresh(),
          MenhirParser.Conversion.Exp.of_menhir_ast(ast),
        ),
      )
    | exception e => Error(Printexc.to_string(e))
    };
  let gap = List.assoc_opt(path |> Filename.basename, known_gaps);
  switch (mk, mh, gap) {
  | (_, _, Some(_reason)) => () /* known gap: tolerated, tracked above */
  | (Some(mk), Ok(mh), None) =>
    check(bool, "menhir == maketerm: " ++ path, true, equal_terms(mk, mh))
  | (Some(_), Error(err), None) =>
    fail("menhir rejects " ++ path ++ ": " ++ err)
  | (None, _, None) => () /* editor parser rejects it too; out of scope */
  };
};

let tests = (
  "MenhirCorpus",
  {
    let files =
      corpus_roots |> List.concat_map(find_hz) |> List.sort_uniq(compare);
    switch (files) {
    | [] => [
        test_case("corpus unavailable (sandboxed run)", `Quick, () => ()),
      ]
    | files =>
      List.map(
        path =>
          test_case("differential: " ++ path, `Slow, () => check_file(path)),
        files,
      )
    };
  },
);
