open Alcotest;
open Haz3lcore;
open Base;

let print_seg = Printer.of_segment(~holes="?");

/* Compare two projectors for equality, ignoring IDs.
 * Since projector models use GADTs, we compare by kind and underlying term. */
let equal_projector = (p1: projector, p2: projector): bool =>
  Projector.kind_of_model(p1.model) == Projector.kind_of_model(p2.model)
  && p1.mold == p2.mold
  && Projector.term_of_model(p1.model) == Projector.term_of_model(p2.model);

// Id ignoring equality for tiles
let rec equal_segment = (a: segment, b: segment) => {
  List.equal(equal_piece, a, b);
}
and equal_piece = (a: piece, b: piece) => {
  switch (a, b) {
  | (Tile(t1), Tile(t2)) =>
    t1.label == t2.label
    && List.equal(equal_segment, t1.children, t2.children)
    && t1.mold == t2.mold
    && t1.shards == t2.shards
  | (Grout(g1), Grout(g2)) => g1.shape == g2.shape
  | (Secondary(s1), Secondary(s2)) => s1.content == s2.content
  | (Projector(p1), Projector(p2)) => equal_projector(p1, p2)
  | _ => false
  };
};

let segment = testable(Fmt.using(Segment.show, Fmt.string), equal_segment);
