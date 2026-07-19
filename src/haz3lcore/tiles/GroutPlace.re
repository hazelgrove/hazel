open Util;
open Base;

/* Artifact-side grout: computed FRESH from the segment, never
 * maintained. `place` re-derives every hole at each nib-shape conflict
 * per the HolePlacement policy, dropping any grout already present in
 * its input (grout is derived material — the tiles and secondaries are
 * the information, the holes are their forced consequence). So
 * placement is a pure function of the grout-free segment: independent
 * derivations of the same artifact agree on hole positions AND ids,
 * with no coordination and nothing stored to go stale.
 *
 * Ids are deterministic (uuid-v5 of parent-slot ctx + nearest
 * non-secondary neighbor ids + shape — the virtual-grout HoleId
 * scheme), so a hole in the display's artifact coincides with the
 * hole statics analyzed by construction.
 *
 * Placement is INSERT-ONLY: a hole is a piece occupying its own cell,
 * and the run's secondaries all survive around it. Consuming the
 * space at the decided index (virtual-grout's thick-replaces-its-cell
 * rendering) was tried and rejected by the invariants: destroying a
 * secondary makes place(place(s)) != place(s) and un-derivable from
 * its own output — the pad_holes non-idempotence family. Insert-only
 * placement destroys nothing, so re-derivation from any stage agrees
 * and serializing holes as nothing round-trips exactly. */

let root_ctx = "root";

let child_ctx = (~tile: Id.t, ~child: int): string =>
  Id.to_string(tile) ++ ":" ++ string_of_int(child);

let hole_id =
    (~ctx: string, ~l: option(Id.t), ~r: option(Id.t), ~shape: Grout.shape)
    : Id.t => {
  let part =
    fun
    | Some(id) => Id.to_string(id)
    | None => "edge";
  let tag =
    switch (shape) {
    | Grout.Convex => "cx"
    | Concave => "cc"
    };
  Id.mk_str(
    "hole:" ++ ctx ++ ":" ++ part(l) ++ ":" ++ part(r) ++ ":" ++ tag,
  );
};

let shape_of_nib = (s: Nib.Shape.t): Grout.shape =>
  switch (s) {
  | Convex => Convex
  | Concave(_) => Concave
  };

let rec strip = (seg: segment): segment =>
  List.filter_map(
    (p: piece) =>
      switch (p) {
      | Grout(_) => None
      | Tile(t) =>
        Some(
          Tile({
            ...t,
            children: List.map(strip, t.children),
          }),
        )
      | p => Some(p)
      },
    seg,
  );

let rec grout_free = (seg: segment): bool =>
  List.for_all(
    (p: piece) =>
      switch (p) {
      | Grout(_) => false
      | Tile(t) => List.for_all(grout_free, t.children)
      | _ => true
      },
    seg,
  );

/* the run owned by a conflict boundary, with the grout inserted
 * before run[index] */
let weave = (run: list(Secondary.t), g: piece, index: int): list(piece) => {
  let n = List.length(run);
  let i = index < 0 ? 0 : index > n ? n : index;
  let (before, rest) = ListUtil.split_n(i, run);
  let secs = List.map(w => Secondary(w));
  secs(before) @ [g, ...secs(rest)];
};

let rec place' = (~ctx: string, ~top_level: bool, seg: segment): segment => {
  /* emit the pending secondary run between two non-secondary pieces
   * (or a segment edge), minting a hole iff the flanking nib shapes
   * conflict */
  let flush =
      (
        ~prev_r: Nib.Shape.t,
        ~l_id: option(Id.t),
        ~next: option((Nib.Shape.t, Id.t)),
        ~at_leading: bool,
        run: list(Secondary.t),
      )
      : list(piece) => {
    let (l_shape, r_id) =
      switch (next) {
      | Some((s, id)) => (s, Some(id))
      | None => (Nib.Shape.concave(), None)
      };
    if (Nib.Shape.fits(prev_r, l_shape)) {
      List.map(w => Secondary(w), run);
    } else {
      let shape = shape_of_nib(Nib.Shape.flip(prev_r));
      let id = hole_id(~ctx, ~l=l_id, ~r=r_id, ~shape);
      let placed: HolePlacement.t =
        HolePlacement.decide(
          ~at_boundary=next == None && top_level,
          ~leading=at_leading && top_level,
          run,
        );
      weave(
        run,
        Grout({
          id,
          shape,
        }),
        placed.index,
      );
    };
  };
  let rec go = (~prev_r, ~l_id, ~at_leading, ~run_rev, ~acc, rest: segment) =>
    switch (rest) {
    | [] =>
      List.rev(acc)
      @ flush(~prev_r, ~l_id, ~next=None, ~at_leading, List.rev(run_rev))
    | [Secondary(w), ...tl] =>
      go(~prev_r, ~l_id, ~at_leading, ~run_rev=[w, ...run_rev], ~acc, tl)
    | [Grout(_), ...tl] =>
      /* input grout is derived material: dropped here, re-derived by
       * this very pass */
      go(~prev_r, ~l_id, ~at_leading, ~run_rev, ~acc, tl)
    | [p, ...tl] =>
      let (p_l, p_r) =
        switch (Piece.shapes(p)) {
        | Some(ss) => ss
        | None => (Nib.Shape.concave(), Nib.Shape.concave())
        };
      let p =
        switch (p) {
        | Tile(t) =>
          Tile({
            ...t,
            children:
              List.mapi(
                (i, kid) =>
                  place'(
                    ~ctx=child_ctx(~tile=t.id, ~child=i),
                    ~top_level=false,
                    kid,
                  ),
                t.children,
              ),
          })
        | p => p
        };
      let emitted =
        flush(
          ~prev_r,
          ~l_id,
          ~next=Some((p_l, Piece.id(p))),
          ~at_leading,
          List.rev(run_rev),
        );
      go(
        ~prev_r=p_r,
        ~l_id=Some(Piece.id(p)),
        ~at_leading=false,
        ~run_rev=[],
        ~acc=[p, ...List.rev_append(emitted, acc)],
        tl,
      );
    };
  go(
    ~prev_r=Nib.Shape.concave(),
    ~l_id=None,
    ~at_leading=true,
    ~run_rev=[],
    ~acc=[],
    seg,
  );
};

let place = (seg: segment): segment =>
  place'(~ctx=root_ctx, ~top_level=true, seg);
