/* The load path (MarkerParse.of_text -> Parser.to_zipper ~by_run) and the
   Siblings.rescan contract its speed rests on. */
open Alcotest;
open Haz3lcore;

let load = (text: string): Zipper.t =>
  switch (MarkerParse.of_text(~root=Exp, text)) {
  | Some(z) => z
  | None => fail("could not load: " ++ String.escaped(text))
  };

/* A carriage return is skipped, so text saved with Windows line endings
   loads as the same program as its \n form. */
let crlf_loads_as_lf = () =>
  check(
    string,
    "\\r\\n reads as \\n",
    MarkerParse.to_text(load("let x = 1 in\nlet y = x + 2 in\ny")),
    MarkerParse.to_text(load("let x = 1 in\r\nlet y = x + 2 in\r\ny")),
  );

/* rescan_reassemble tells "unchanged" by identity alone, so Siblings.rescan
   must hand back its very argument whenever it changes nothing: with no
   incomplete tile, and with one that has nothing to match. */
let rescan_same_pair = (text, ()) => {
  let sibs = load(text).relatives.siblings;
  check(
    bool,
    "the same pair, not a copy",
    true,
    Siblings.rescan(sibs) === sibs,
  );
};

/* A lone \r is a line break too, not nothing: skipping it glued the
   tokens either side, `in\rx` into `inx`. */
let lone_cr_loads_as_lf = () =>
  check(
    string,
    "\\r reads as \\n",
    MarkerParse.to_text(load("let x = 1 in\nx")),
    MarkerParse.to_text(load("let x = 1 in\rx")),
  );

/* An insertion that fails stops the parse, with None, rather than carrying
   on from a state it could not reach. The caret is inside a token with no
   token after it, where Insert.go has nothing to insert into. */
let failed_insertion_is_none = () => {
  let z = load("x") |> Zipper.Caret.set(Inner(0));
  check(
    bool,
    "None",
    true,
    Option.is_none(
      Parser.to_zipper(~by_run=true, ~root=Exp, ~zipper_init=z, "1"),
    ),
  );
};

let tests = (
  "Parser.LoadPath",
  [
    test_case("\\r\\n loads like \\n", `Quick, crlf_loads_as_lf),
    test_case("a lone \\r loads like \\n", `Quick, lone_cr_loads_as_lf),
    test_case(
      "a failed insertion ends the parse with None",
      `Quick,
      failed_insertion_is_none,
    ),
    test_case(
      "rescan: no incomplete tile -> same pair",
      `Quick,
      rescan_same_pair("let x = 1 in x + 2"),
    ),
    /* One shard only: a multi-shard orphan (`let x = 1`, missing its `in`)
       is presplit into single shards, which IS a change, by design. */
    test_case(
      "rescan: a one-shard incomplete tile with nothing to match -> same pair",
      `Quick,
      rescan_same_pair("fun x"),
    ),
    /* The segmented parser (#2610's load path) reads line endings the same
       way, so 100-character segments still split at a \r\n line end. */
    test_case(
      "\\r\\n segments like \\n",
      `Quick,
      () => {
        let text = n =>
          String.concat(
            n,
            List.init(12, i =>
              "let a"
              ++ string_of_int(i)
              ++ " = "
              ++ string_of_int(i)
              ++ " in"
            ),
          )
          ++ n
          ++ "a11";
        let printed = n =>
          switch (Parser.to_segment(~root=Exp, text(n))) {
          | Some(seg) => MarkerParse.to_text(Zipper.unzip(seg))
          | None => fail("to_segment failed")
          };
        check(string, "same program", printed("\n"), printed("\r\n"));
      },
    ),
  ],
);
