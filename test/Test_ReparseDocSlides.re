/* Load-path fidelity for every SHIPPED slide, through the real slide
 * registry (Init.documentation_slides, at each slide's root sort): the
 * load path reproduces the committed text byte-for-byte, and when the
 * fast path succeeds it reads the same term as the typing parse. */

open Web;
open Alcotest;
open Haz3lcore;

let doc_slides: list((string, Sort.t, PersistentZipper.t)) = Init.documentation_slides;

/* mega slides skip the quadratic typing-parse comparison; the text
 * fidelity check (which catches wrong-root loads) still runs */

let doc_slide_reparses = ((name, root, slide: PersistentZipper.t)) => {
  test_case(
    name,
    `Slow,
    () => {
      /* All shipped slides are text-backed (committed .hz): no stored
         sexp, and structural segment equality would be the wrong bar
         anyway — projector MODELS embed freshly-minted ids per
         materialization. The meaningful contract is text-level. */
      let text = slide.backup_text;
      let z = PersistentZipper.from_backup_text(text, ~root);
      check(
        string,
        name ++ ": load path reproduces the committed text",
        String.trim(text),
        String.trim(MarkerParse.to_text(z)),
      );
      if (!CorpusUtil.mega_scale(name)) {
        switch (
          FastParse.of_text(
            ~materialize=Triggers.invoked_projector,
            ~collect_refractors=true,
            ~root,
            String.trim(text),
          ),
          ParsedCorpus.to_segment(~root, text),
        ) {
        | (None, _) => () /* fallback slide (menhir gap): fidelity checked above */
        | (Some(_), None) =>
          Alcotest.fail("Failed to parse segment from slide backup text")
        | (Some(fast_segment), Some(reparsed_segment)) =>
          check(
            bool,
            name ++ ": fast and typing parses read the same term",
            true,
            Language.Equality.(
              equality({
                ...syntactic_settings,
                ignore_parens: false,
              }).
                exp
            )(
              MakeTerm.go(fast_segment).term,
              MakeTerm.go(reparsed_segment).term,
            ),
          )
        };
      };
    },
  );
};

let tests = [
  ("DocSlides.ReparseBackuptext", List.map(doc_slide_reparses, doc_slides)),
];
