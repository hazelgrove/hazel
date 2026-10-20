open Haz3lcore;
open Util;

/* installed by the test runner: every sparse remold/regrout also runs the
   global pass and fails on disagreement */

let remold_regrout_global = (d: Direction.t, z: Zipper.t, ~root): Zipper.t => {
  let z' = z |> Zipper.remold(~root) |> Zipper.regrout(d);
  {
    ...z',
    relatives: Zipper.restore_relatives(z.relatives, z'.relatives),
  };
};

/* grout and redeemed-space secondary ids are minted fresh per pass, so
   parity compares modulo them */
let rec scrub_grout_ids = (seg: Segment.t): Segment.t =>
  List.map(
    (p: Piece.t) =>
      switch (p) {
      | Grout(g) =>
        Piece.Grout({
          ...g,
          id: Id.invalid,
        })
      | Secondary(w) =>
        Piece.Secondary({
          ...w,
          id: Id.invalid,
        })
      | Tile(t) =>
        Piece.Tile({
          ...t,
          children: List.map(scrub_grout_ids, t.children),
        })
      | Splice(s) => Piece.mk_splice(~id=s.id, scrub_grout_ids(s.content))
      | Projector(_) => p
      },
    seg,
  );

let relatives_equiv = (a: Relatives.t, b: Relatives.t): bool => {
  let sibs_equiv = ((p1, s1): Siblings.t, (p2, s2): Siblings.t) =>
    compare(scrub_grout_ids(p1), scrub_grout_ids(p2)) == 0
    && compare(scrub_grout_ids(s1), scrub_grout_ids(s2)) == 0;
  sibs_equiv(a.siblings, b.siblings)
  && List.length(a.ancestors) == List.length(b.ancestors)
  && List.for_all2(
       ((aa, asibs): Ancestors.generation, (ba, bsibs)) =>
         compare(aa, ba) == 0 && sibs_equiv(asibs, bsibs),
       a.ancestors,
       b.ancestors,
     );
};

let brief = (p: Piece.t): string =>
  switch (p) {
  | Tile(t) => "tile " ++ String.concat("", Tile.effective_label(t))
  | Grout({shape: Convex, _}) => "convex grout"
  | Grout({shape: Concave, _}) => "concave grout"
  | Secondary(w) => Secondary.is_linebreak(w) ? "linebreak" : "space"
  | Projector(_) => "projector"
  | Splice(_) => "splice"
  };

/* where two segments first differ, modulo grout ids */
let first_diff = (what, a: Segment.t, b: Segment.t): option(string) => {
  let rec go = (i, xs, ys) =>
    switch (xs, ys) {
    | ([], []) => None
    | ([x, ...xs], [y, ...ys]) when compare(x, y) == 0 =>
      go(i + 1, xs, ys)
    | _ =>
      let at = (
        fun
        | [p, ..._] => brief(p)
        | [] => "the end"
      );
      Some(
        Printf.sprintf(
          "%s at %d: sparse %s, global %s",
          what,
          i,
          at(xs),
          at(ys),
        ),
      );
    };
  go(0, scrub_grout_ids(a), scrub_grout_ids(b));
};

let check = (d: Direction.t, z: Zipper.t, root: Sort.t): Zipper.t => {
  /* the first pass consumes the owed-space ref; replay it for the second */
  let owed = Grout.suppressed_space^;
  let zs = Zipper.remold_regrout_sparse(d, z, ~root);
  Grout.suppressed_space := owed;
  let zg = remold_regrout_global(d, z, ~root);
  if (!relatives_equiv(zs.relatives, zg.relatives)) {
    let ((sp, ss), (gp, gs)) = (
      zs.relatives.siblings,
      zg.relatives.siblings,
    );
    failwith(
      "sparse normalization differs from the global pass: "
      ++ (
        switch (first_diff("prefix", sp, gp), first_diff("suffix", ss, gs)) {
        | (Some(diff), _)
        | (None, Some(diff)) => diff
        | (None, None) => "in the ancestors"
        }
      ),
    );
  };
  zs;
};

let install = () => Zipper.normalize_check := Some(check);
