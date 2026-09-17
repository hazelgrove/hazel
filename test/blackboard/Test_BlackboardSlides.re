/* Every shipped Blackboard slide must read back as a Blackboard document.
   The slides are Hazel expressions, so a name that collides with a Hazel
   token (`false` is a boolean literal, not an identifier) or an entry the
   grammar cannot shape silently becomes a hole, and the slide still
   round-trips as text -- DocSlides.ReparseBackuptext would not notice.
   This is the ratchet that does.

   Note that reading a document is not checking it: slide 3 deliberately
   contains the paper's own mistakes, which the checker is supposed to
   report. Here we only require that the document is well formed enough to
   hand to the checker at all. */

open Alcotest;
open Haz3lcore;
open Language;

let slides: list((string, string)) =
  Docslides.Slides.all_slides
  |> List.filter_map(((name, z: PersistentZipper.t)) =>
       switch (String.index_opt(name, '/')) {
       | Some(i) when String.sub(name, 0, i) == "Blackboard " =>
         Some((name, z.backup_text))
       | _ => None
       }
     );

/* Every Blackboard document embedded in a slide, in source order. */
let documents_of = (text: string): list(Bb.Term.t) => {
  let root = Sort.Exp;
  switch (PersistentZipper.parse_text(~source="slide", ~root, text)) {
  | None => failf("slide did not parse")
  | Some(z) =>
    let e = MakeTerm.from_zip_for_sem(z, ~root).term;
    let found = ref([]);
    let _ =
      Exp.map_term(
        ~f_exp=
          (continue, e) => {
            switch (IdTagged.term_of(e)) {
            | BbQuote(b) => found := [b, ...found^]
            | _ => ()
            };
            continue(e);
          },
        e,
      );
    List.rev(found^);
  };
};

let tests = (
  "Blackboard slides",
  List.map(
    ((name, text)) =>
      test_case(
        name,
        `Quick,
        () => {
          let docs = documents_of(text);
          check(
            bool,
            name ++ " embeds a Blackboard document",
            true,
            docs != [],
          );
          List.iter(
            b =>
              switch (Bb.doc_to_kernel(b)) {
              | Ok([]) => failf("%s: the document is empty", name)
              | Ok(_) => ()
              | Error({message, _}) =>
                /* A name that collides with a Hazel token, or an entry
                   shape the grammar cannot hold, becomes a hole here. */
                failf(
                  "%s: the document does not read back: %s",
                  name,
                  message,
                )
              },
            docs,
          );
        },
      ),
    slides,
  ),
);
