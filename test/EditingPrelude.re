open Alcotest;
open Haz3lcore;
open Base;

let print_seg = Printer.of_segment(~holes="?", ~refractors=[]);

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
  | (Projector(p1), Projector(p2)) =>
    p1.kind == p2.kind
    && p1.model == p2.model
    && equal_piece(p1.syntax, p2.syntax)
  | _ => false
  };
};

let segment = testable(Fmt.using(Segment.show, Fmt.string), equal_segment);
