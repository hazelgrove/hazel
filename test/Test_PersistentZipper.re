open Alcotest;
open Haz3lcore;

/* `PersistentZipper` is the save/load path for every slide, and it degrades
 * silently by design in two places:
 *
 *   unpersist -> if the stored sexp will not parse (an older Hazel wrote it),
 *                fall back to `backup_text`
 *   from_backup_text -> if that will not parse either, LOAD AN EMPTY BUFFER
 *
 * Both are the right policy -- boot has no error channel and one bad blob must
 * not brick the app -- but they mean the backup text is load-bearing: it is what
 * every version upgrade actually reads. If it is lossy, upgrading Hazel quietly
 * corrupts saved work, and the second fallback discards it outright.
 *
 * Test_RefractorSerialization covers `parse |> print` round trips. These cover
 * the persistence path itself: the sexp round trip, the fallback chain, and the
 * losslessness claims the module's own comments make. */

let root = Sort.Exp;

let zipper_of = (text: string): Zipper.t =>
  switch (Parser.to_zipper(~root, text)) {
  | None => fail("could not parse: " ++ text)
  | Some(z) => z
  };

/* The document as text, which is what has to survive. Ids are re-minted by any
   reparse, so text is the right observable. */
let text_of = (z: Zipper.t) => PersistentZipper.to_string(z);

let round_trip = (text: string) => {
  let z = zipper_of(text);
  let restored =
    PersistentZipper.persist(z) |> PersistentZipper.unpersist(~root);
  check(
    string,
    "document survived persist/unpersist",
    text_of(z),
    text_of(restored),
  );
};

/* Force the backup-text path: a stored sexp that will not parse, as an older
   Hazel's serialization looks to a newer one. */
let via_backup = (text: string) => {
  let z = zipper_of(text);
  let persisted = PersistentZipper.persist(z);
  let restored =
    PersistentZipper.unpersist(
      ~root,
      {
        ...persisted,
        zipper: "(this is not a valid zipper sexp",
      },
    );
  check(
    string,
    "document recovered from backup text",
    text_of(z),
    text_of(restored),
  );
};

let programs = [
  ("integer", "1"),
  ("binop", "1 + 2"),
  ("let", "let x = 1 in x"),
  ("function", "fun x -> x + 1"),
  ("case", "case 1 | 1 => 2 | _ => 3 end"),
  ("multiline", "let x = 1 in\nlet y = 2 in\nx + y"),
  /* The module claims hole positions survive, via the ¿ marker. */
  ("hole", "1 + ?"),
  ("hole in a let", "let x = ? in x"),
  /* Refractors are reconstructed from trigger text on the backup path, so they
     are the most likely thing to be dropped by it. */
  ("refractor", "^^probe(1 + 1)"),
  /* The writer appends one newline and the reader strips exactly that one, so
     this only matters on the backup path -- which is why it is in `programs`
     (covered by via_backup) and not only in the sexp round trip. */
  ("trailing blank line", "1 + 1\n"),
];

let tests = (
  "PersistentZipper",
  List.map(
    ((name, text)) =>
      test_case("persist/unpersist: " ++ name, `Quick, () =>
        round_trip(text)
      ),
    programs,
  )
  @ List.map(
      ((name, text)) =>
        test_case("recovers via backup text: " ++ name, `Quick, () =>
          via_backup(text)
        ),
      programs,
    )
  @ [
    /* A text-only slide (`of_text` leaves the sexp empty) must take the backup
       path without warning, since that is the intended encoding for .hz slides. */
    test_case(
      "text-only persistence loads the program",
      `Quick,
      () => {
        let text = "let x = 1 in x";
        let restored =
          PersistentZipper.of_text(text) |> PersistentZipper.unpersist(~root);
        check(string, "document", text, text_of(restored));
      },
    ),
    /* An explicit claim in `persist`: the writer appends exactly one newline and
       the reader strips exactly that one, so a buffer that genuinely ends in a
       blank line keeps it. Losing one newline per save is the classic version of
       this bug. */
    /* Sanity check on the observable itself: the parser really does keep the
       trailing newline, so the round-trip assertions above are meaningful. */
    test_case("a trailing blank line is part of the document", `Quick, () =>
      check(string, "text_of", "1 + 1\n", text_of(zipper_of("1 + 1\n")))
    ),
    /* `of_slide_text` deliberately flattens leading indentation, because Hazel
       computes indentation at layout time and would otherwise double it. */
    test_case(
      "of_slide_text strips leading indentation",
      `Quick,
      () => {
        let restored =
          PersistentZipper.of_slide_text("  let x = 1 in\n    x")
          |> PersistentZipper.unpersist(~root);
        check(string, "flattened", "let x = 1 in\nx", text_of(restored));
      },
    ),
  ],
);
