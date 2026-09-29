/* The load path (MarkerParse.of_text -> Parser.to_zipper ~by_run) and the
   Siblings.rescan contract its speed rests on. */
open Alcotest;
open Haz3lcore;

let load = (text: string): Zipper.t =>
  switch (MarkerParse.of_text(~root=Exp, text)) {
  | Some(z) => z
  | None => fail("could not load: " ++ String.escaped(text))
  };

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

let tests = (
  "Parser.LoadPath",
  [
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
  ],
);
