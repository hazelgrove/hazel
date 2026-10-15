open Alcotest;
open Haz3lcore;

/* Dump.to_segment skips the caret walk when no shard is missing anywhere,
   on the grounds that the walk would put nothing down. Check that against
   the walk itself, at caret positions across the shipped slides. */

/* From the repository root locally, from _build/default/test under CI's
   `dune test`: try both rather than depend on the working directory. */
let resolve = path =>
  List.find_opt(Sys.file_exists, [path, Filename.concat("../../..", path)])
  |> Option.value(~default=path);

let read_file = path => {
  let ic = open_in_bin(path);
  let s = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  s;
};

let slides = dir => {
  let dir = resolve(dir);
  Sys.readdir(dir)
  |> Array.to_list
  |> List.filter(f => Filename.check_suffix(f, ".hz"))
  |> List.sort(compare)
  |> List.map(f => (f, read_file(Filename.concat(dir, f))));
};

let rec to_start = z =>
  switch (Zipper.move(Left, z)) {
  | Some(z) => to_start(z)
  | None => z
  };

/* Every caret position, as a list; a slide has a few thousand. */
let positions = z => {
  let rec go = (acc, z) =>
    switch (Zipper.move(Right, z)) {
    | Some(z') => go([z', ...acc], z')
    | None => List.rev(acc)
    };
  go([z], to_start(z));
};

let agrees = (name, text) => {
  /* The loader slides open with (FastParse first): the zipper the editor
     starts from, and linear, where Parser.to_zipper types the slide in
     one character at a time and takes up to a minute on the larger ones. */
  switch (PersistentZipper.parse_text(~source=name, ~root=Exp, text)) {
  | None => fail(name ++ ": failed to parse")
  | Some(z) =>
    let ps = positions(z);
    /* ~8 positions per slide, first and last included. From the start
       the walk crosses the whole slide, a move per piece. */
    let step = max(1, List.length(ps) / 8);
    List.iteri(
      (i, z) =>
        if (i mod step == 0 || i == List.length(ps) - 1) {
          check(
            bool,
            Printf.sprintf("%s at caret position %d", name, i),
            true,
            Dump.to_segment(z, ~root=Exp)
            == Dump.to_segment_by_walking(z, ~root=Exp),
          );
        },
      ps,
    );
  };
};

let slides_agree = (dir, ()) =>
  slides(dir) |> List.iter(((name, text)) => agrees(name, text));

/* The control: with a shard missing, the walk still runs and does put it
   down, so the fast path is not taken and the answer is not the program
   as it stands. */
let missing_shard_still_walks = () => {
  let z =
    switch (Parser.to_zipper(~root=Exp, "1")) {
    | Some(z) => z
    | None => fail("failed to parse 1")
    };
  let z =
    Test_Editing.perform(
      to_start(z),
      List.map(c => Action.Insert(c), ["l", "e", "t", " "]),
    );
  let zipped = Zipper.unselect_and_zip(z);
  check(
    bool,
    "the program has a missing shard",
    true,
    Segment.global_missing_shards(zipped) != [],
  );
  check(
    bool,
    "the walk changes it",
    true,
    Dump.to_segment(z, ~root=Exp) != zipped,
  );
  /* Two walks mint different ids for what they put down, so compare
     ignoring ids. */
  check(
    EditingPrelude.segment,
    "and to_segment is the walk",
    Dump.to_segment_by_walking(z, ~root=Exp),
    Dump.to_segment(z, ~root=Exp),
  );
};

let tests = [
  (
    "Dump",
    [
      test_case(
        "livelit slides: fast path is the walk",
        `Quick,
        slides_agree("hazel-programs/docs/livelits"),
      ),
      test_case(
        "reference slides: fast path is the walk",
        `Quick,
        slides_agree("hazel-programs/docs/reference"),
      ),
      test_case(
        "a missing shard still walks",
        `Quick,
        missing_shard_still_walks,
      ),
    ],
  ),
];
