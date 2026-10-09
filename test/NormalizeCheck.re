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

let check = (d: Direction.t, z: Zipper.t, root: Sort.t): Zipper.t => {
  /* the first pass consumes the owed-space ref; replay it for the second */
  let owed = Grout.suppressed_space^;
  let zs = Zipper.remold_regrout_sparse(d, z, ~root);
  Grout.suppressed_space := owed;
  let zg = remold_regrout_global(d, z, ~root);
  if (!relatives_equiv(zs.relatives, zg.relatives)) {
    let diff = (tag, a: Segment.t, b: Segment.t) => {
      let (a, b) = (scrub_grout_ids(a), scrub_grout_ids(b));
      if (compare(a, b) != 0) {
        let rec first = (i, xs, ys) =>
          switch (xs, ys) {
          | ([x, ...xs], [y, ...ys]) when compare(x, y) == 0 =>
            first(i + 1, xs, ys)
          | _ => i
          };
        let i = first(0, a, b);
        let at = (seg, i) =>
          switch (List.nth_opt(seg, i)) {
          | Some(p) =>
            String.sub(
              Piece.show(p) ++ "",
              0,
              min(200, String.length(Piece.show(p))),
            )
          | None => "<end>"
          };
        print_endline(
          Printf.sprintf(
            "[parity] %s differs at %d (lens %d vs %d)\n  sparse: %s\n  global: %s",
            tag,
            i,
            List.length(a),
            List.length(b),
            at(a, i),
            at(b, i),
          ),
        );
      };
    };
    diff("pre", fst(zs.relatives.siblings), fst(zg.relatives.siblings));
    diff("suf", snd(zs.relatives.siblings), snd(zg.relatives.siblings));
    let brief = (p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        Printf.sprintf("T(%s)", String.concat("", Tile.effective_label(t)))
      | Grout(g) =>
        Printf.sprintf(
          "G(%s)",
          switch (g.shape) {
          | Convex => "cvx"
          | Concave => "ccv"
          },
        )
      | Secondary(w) => Secondary.is_linebreak(w) ? "LB" : "ws"
      | Projector(_) => "Proj"
      };
    let dump = (tag, seg: Segment.t) =>
      print_endline(
        Printf.sprintf(
          "[parity] %s: [%s]",
          tag,
          String.concat(" ", List.map(brief, seg)),
        ),
      );
    dump("z.pre     ", fst(z.relatives.siblings));
    dump("z.suf     ", snd(z.relatives.siblings));
    dump("sparse.pre", fst(zs.relatives.siblings));
    dump("global.pre", fst(zg.relatives.siblings));
    dump("sparse.suf", snd(zs.relatives.siblings));
    dump("global.suf", snd(zg.relatives.siblings));
    print_endline(
      Printf.sprintf(
        "[parity] sel=%d anc=%d d=%s",
        List.length(z.selection.content),
        List.length(z.relatives.ancestors),
        d == Left ? "L" : "R",
      ),
    );
    failwith("sparse normalize PARITY MISMATCH");
  };
  zs;
};

let install = () => Zipper.normalize_check := Some(check);
