open Alcotest;
open Haz3lcore;

/* Move.jump_to_side_of_id builds the zipper the walk would reach when the
   piece is reached through tiles alone. It must be the same zipper, ids
   and all, as Move.jump_to_side_of_id_by_walking. */

let resolve = path =>
  List.find_opt(Sys.file_exists, [path, Filename.concat("../../..", path)])
  |> Option.value(~default=path);

let read_file = path => {
  let ic = open_in_bin(path);
  let s = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  s;
};

let slides = (~limit=max_int, dir) => {
  let dir = resolve(dir);
  Sys.readdir(dir)
  |> Array.to_list
  |> List.filter(f => Filename.check_suffix(f, ".hz"))
  |> List.sort(compare)
  |> List.filteri((i, _) => i < limit)
  |> List.map(f => (f, read_file(Filename.concat(dir, f))));
};

/* Every piece id, in document order, including inside projectors (whose
   targets the direct path leaves to the walk). */
let rec ids = (seg: Segment.t): list(Id.t) =>
  List.concat_map(
    (p: Piece.t) =>
      [
        Piece.id(p),
        ...switch (p) {
           | Tile(t) => List.concat_map(ids, t.children)
           | Projector(pr) => ids([pr.syntax])
           | Grout(_)
           | Secondary(_) => []
           },
      ],
    seg,
  );

let rec nth_right = (n, z) =>
  n <= 0
    ? z
    : (
      switch (Zipper.move(Right, z)) {
      | Some(z) => nth_right(n - 1, z)
      | None => z
      }
    );

let agree = (name, text) =>
  switch (PersistentZipper.parse_text(~source=name, ~root=Exp, text)) {
  | None => fail(name ++ ": failed to parse")
  | Some(z0) =>
    let all = ids(Zipper.zip(z0));
    let n = List.length(all);
    /* ~15 targets a slide: each is checked against a full walk, which is
       the test's whole cost. */
    let step = max(1, n / 15);
    let targets = List.filteri((i, _) => i mod step == 0, all);
    /* Two starting carets: the top, and part way down. */
    let starts = [z0, nth_right(n / 3, z0)];
    List.iter(
      z =>
        List.iter(
          id =>
            List.iter(
              (d: Util.Direction.t) => {
                let fast = Move.jump_to_side_of_id(d, z, id);
                let walked = Move.jump_to_side_of_id_by_walking(d, z, id);
                if (fast != walked) {
                  fail(
                    Printf.sprintf(
                      "%s: %s side of %s differs",
                      name,
                      d == Left ? "left" : "right",
                      Id.to_string(id),
                    ),
                  );
                };
              },
              [Left, Right],
            ),
          targets,
        ),
      starts,
    );
    List.length(targets);
  };

let slides_agree = (~limit=?, dir, ()) => {
  let before = Move.direct_jumps^;
  let checked =
    slides(~limit?, dir)
    |> List.fold_left((acc, (name, text)) => acc + agree(name, text), 0);
  check(bool, "some targets", true, checked > 0);
  /* The control: most jumps took the direct path, so the comparison is of
     it and not only of the walk against itself. */
  check(
    bool,
    "direct path taken",
    true,
    Move.direct_jumps^ - before > checked,
  );
};

/* Single-token tiles are shards of their own, in document order. */
let rec single_shards = (seg: Segment.t): list((Id.t, int)) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        (
          switch (t.shards) {
          | [idx] => [(t.id, idx)]
          | _ => []
          }
        )
        @ List.concat_map(single_shards, t.children)
      | Projector(pr) => single_shards([pr.syntax])
      | Grout(_)
      | Secondary(_) => []
      },
    seg,
  );

/* Move.jump_to_shard (collapsing a char-level selection) places the caret
   the same way; it must match Move.jump_to_shard_by_walking. */
let shards_agree = (name, text) =>
  switch (PersistentZipper.parse_text(~source=name, ~root=Exp, text)) {
  | None => fail(name ++ ": failed to parse")
  | Some(z0) =>
    let all = single_shards(Zipper.zip(z0));
    let n = List.length(all);
    let step = max(1, n / 15);
    let targets = List.filteri((i, _) => i mod step == 0, all);
    let starts = [z0, nth_right(n / 3, z0)];
    List.iter(
      z =>
        List.iter(
          ((id, idx)) =>
            if (Move.jump_to_shard(z, id, idx)
                != Move.jump_to_shard_by_walking(z, id, idx)) {
              fail(
                Printf.sprintf(
                  "%s: shard %d of %s differs",
                  name,
                  idx,
                  Id.to_string(id),
                ),
              );
            },
          targets,
        ),
      starts,
    );
    List.length(targets);
  };

let slides_shards_agree = (dir, ()) => {
  let before = Move.direct_jumps^;
  let checked =
    slides(dir)
    |> List.fold_left(
         (acc, (name, text)) => acc + shards_agree(name, text),
         0,
       );
  check(bool, "some targets", true, checked > 0);
  check(
    bool,
    "direct path taken",
    true,
    Move.direct_jumps^ - before > checked,
  );
};

let tests = (
  "Move.JumpToId",
  [
    test_case(
      "reference slides: direct jump is the walk",
      `Slow,
      slides_agree("hazel-programs/docs/reference"),
    ),
    test_case(
      "B2T2 slides (first five): direct jump is the walk",
      `Slow,
      slides_agree(~limit=5, "hazel-programs/docs/b2t2"),
    ),
    test_case(
      "reference slides: direct shard jump is the walk",
      `Slow,
      slides_shards_agree("hazel-programs/docs/reference"),
    ),
  ],
);
