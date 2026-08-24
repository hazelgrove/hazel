open Haz3lcore;
open Language;

/* Text-space equivalence: the existing Menhir/MakeTerm property tests the
   canonical-print image of term space; agent chunks live in TEXT space,
   where formatting varies. Print a generated term, rewrite each whitespace
   run (outside string literals) with a random alternative, and require the
   two parsers to still agree. */

let print_core = (exp: Exp.t): string =>
  exp
  |> ExpToSegment.exp_to_segment(
       ~settings=ExpToSegment.Settings.editable(~inline=true),
       _,
     )
  |> Printer.of_segment(~holes="?", ~refractors=[]);

/* Replace the i-th whitespace run according to choices[i mod len]:
   0 → single space, 1 → double space, 2 → newline, 3 → newline+indent,
   4 → space, newline, space. String literals are left untouched
   (double-quote tracking with backslash escapes). */
let fuzz_whitespace = (choices: array(int), s: string): string => {
  let buf = Stdlib.Buffer.create(String.length(s) + 32);
  let n = String.length(s);
  let n_choices = Array.length(choices);
  let rec go = (i: int, run_idx: int, in_string: bool) =>
    if (i >= n) {
      ();
    } else {
      let c = s.[i];
      if (in_string) {
        Stdlib.Buffer.add_char(buf, c);
        switch (c) {
        | '\\' when i + 1 < n =>
          Stdlib.Buffer.add_char(buf, s.[i + 1]);
          go(i + 2, run_idx, true);
        | '"' => go(i + 1, run_idx, false)
        | _ => go(i + 1, run_idx, true)
        };
      } else if (c == ' ' || c == '\n' || c == '\t') {
        /* consume the whole run */
        let j = ref(i);
        while (j^ < n && (s.[j^] == ' ' || s.[j^] == '\n' || s.[j^] == '\t')) {
          incr(j);
        };
        let replacement =
          switch (n_choices == 0 ? 0 : choices[run_idx mod n_choices]) {
          | 0 => " "
          | 1 => "  "
          | 2 => "\n"
          | 3 => "\n  "
          | _ => " \n "
          };
        Stdlib.Buffer.add_string(buf, replacement);
        go(j^, run_idx + 1, false);
      } else {
        Stdlib.Buffer.add_char(buf, c);
        go(i + 1, run_idx, c == '"');
      };
    };
  go(0, 0, false);
  Stdlib.Buffer.contents(buf);
};

let make_term_parse = (s: string): option(Exp.t) =>
  switch (Parser.to_zipper(s, ~root=Exp)) {
  | Some(z) => Some(MakeTerm.from_zip_for_sem(z, ~root=Exp).term)
  | None => None
  };

let menhir_parse = (s: string): option(Exp.t) =>
  switch (MenhirParser.Interface.parse_program(s)) {
  | ast =>
    Some(
      Grammar.map_exp_annotation(
        _ => IdTagged.IdTag.fresh(),
        MenhirParser.Conversion.Exp.of_menhir_ast(ast),
      ),
    )
  | exception _ => None
  };

let equal_terms = (e1: Exp.t, e2: Exp.t): bool =>
  Canonicalize.roundtrip_eq.exp(Canonicalize.exp(e1), Canonicalize.exp(e2));

let arb_fuzzed = {
  open QCheck;
  let gen = {
    open Gen;
    let* core_exp = QCheck_Util.arb_exp_full(~minimal_idents=false, 5).gen;
    let+ choices = array_size(int_bound(24), int_bound(4));
    /* Same module-item gaps the equivalence property carves out. */
    (print_core(core_exp), choices, Test_Menhir.is_carved_out(core_exp));
  };
  make(~print=((txt, _, _)) => txt, gen);
};

let qcheck_fuzz_test =
  QCheck.Test.make(
    ~name="Menhir and MakeTerm agree on whitespace-fuzzed text",
    ~count=300,
    arb_fuzzed,
    ((txt, choices, unsupported)) => {
      QCheck.assume(!unsupported);
      let fuzzed = fuzz_whitespace(choices, txt);
      switch (make_term_parse(fuzzed), menhir_parse(fuzzed)) {
      | (Some(mk), Some(mh)) => equal_terms(mk, mh)
      | (Some(_), None) => false /* menhir must accept what the editor accepts here */
      | (None, _) => QCheck.assume_fail() /* printer artifact; not this test's business */
      };
    },
  );

let tests = ("MenhirFuzz", [QCheck_alcotest.to_alcotest(qcheck_fuzz_test)]);
