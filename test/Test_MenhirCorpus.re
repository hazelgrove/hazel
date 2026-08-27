open Alcotest;
open Haz3lcore;
open Language;

/* Corpus differential ratchet: every .hz program in the repo must parse
   identically through the Menhir parser and the editor parser. A divergence
   is NOT a flake — either fix it or add the file to known_gaps with a
   reason; the goal is an empty list. Cases are `Slow: the editor parse
   dominates and the menhir side is negligible.

   Parses via `Parser.to_segment`, not `Parser.to_zipper`. to_zipper is the
   definitional editor parse — it replays what the editor does per keystroke —
   and to_segment is an optimization over it, so this pins menhir against the
   optimized path. They agree corpus-wide today; asserting that invariant costs
   about as much as using to_zipper here would, so it belongs in a periodic run
   rather than per-PR.

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
    /* The two Properties slides containing `[()]`; nrows is clean. */
    (
      "table-api-properties-header.hz",
      "editor tokenizer quirk: [()] reads as [] via MakeTerm (file as editor bug)",
    ),
    (
      "table-api-properties-ncols.hz",
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
         /* hazel-programs/mega: thousands-of-lines perf corpora — the
            menhir differential on them costs minutes-to-hours per file
            and proves nothing new (they're composed from already-swept
            sources). The FastParseCorpus ratchet still covers them. */
         | true when Filename.basename(path) == "mega" => []
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

/* Strict on parens: the two parsers agree on Parens placement (verified
   corpus-wide — every previous loose-mode reliance was actually the
   Projector-unwrap half of the old bundled flag). ignore_projectors
   because the menhir grammar erases ^^triggers, so its terms never carry
   the Projector wrappers MakeTerm produces for them. */
let equal_terms =
  Equality.(
    equality({
      ...syntactic_settings,
      ignore_projectors: true,
    }).
      exp
  );

let check_file = (path: string): unit => {
  /* Normalize as the load path does, rather than String.trim: that is the
     form the editor ever sees, and it lets ParsedCorpus share the parse with
     DocSlides.ReparseBackuptext, which checks the same programs. */
  let txt = read_file(path) |> ParsedCorpus.normalize;
  let mk =
    switch (ParsedCorpus.to_segment(~root=Exp, txt)) {
    | Some(seg) => Some(MakeTerm.go(seg).term)
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
