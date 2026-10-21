/* Correspondence for rewrite gestures, built on #2593's IdMatch.
   Agent edits retain its conservative default. Rewrites additionally match
   moved isomorphic subtrees across levels and record clone/merge provenance.
   Provenance is a relation; only the one-to-one part becomes syntax ids. */
open Util;

type node = {
  piece: Piece.t,
  path: list((Id.t, int)),
  seat: int,
  size: int,
};

type destination =
  | Live(Id.t)
  | Result(Id.t);
type t = {
  segment: Segment.t,
  /* Fresh-result id -> source id, before transplantation. */
  retained: Id.Map.t(Id.t),
  /* Consumed source -> surviving source, and new clone -> source. */
  merges: list((Id.t, Id.t)),
  copies: list((Id.t, Id.t)),
  destination: option(destination),
};

let nodes = seg => {
  let rec walk = (path, seg: Segment.t): list(node) =>
    seg
    |> List.filter((p: Piece.t) =>
         switch (p) {
         | Tile(_)
         | Projector(_) => true
         | _ => false
         }
       )
    |> List.mapi((seat, piece: Piece.t) => {
         let children =
           switch (piece) {
           | Tile(t) =>
             t.children
             |> List.mapi((i, s) => walk(path @ [(t.id, i)], s))
             |> List.concat
           | Projector(p) => walk(path, [p.syntax])
           | _ => []
           };
         [
           {
             piece,
             path,
             seat,
             size: 1 + List.length(children),
           },
           ...children,
         ];
       })
    |> List.concat;
  walk([], seg)
  |> List.fold_left(
       ((seen, acc), n) => {
         let id = Piece.id(n.piece);
         Id.Set.mem(id, seen)
           ? (seen, acc) : (Id.Set.add(id, seen), acc @ [n]);
       },
       (Id.Set.empty, []),
     )
  |> snd;
};

/* A whole-subtree match must agree with every already-known identity. */
let rec compatible = (st: IdMatch.state, a: Piece.t, b: Piece.t) => {
  let old_id = Piece.id(a);
  let new_id = Piece.id(b);
  let own =
    switch (Id.Map.find_opt(new_id, st.pairs)) {
    | Some(id) => id == old_id
    | None => !Id.Set.mem(old_id, st.claimed)
    };
  own
  && st.can_match(old_id, new_id)
  && (
    switch (a, b) {
    | (Tile(a), Tile(b)) =>
      List.for_all2(
        (xs, ys) => List.for_all2(compatible(st), xs, ys),
        a.children,
        b.children,
      )
    | (Projector(a), Projector(b)) => compatible(st, a.syntax, b.syntax)
    | _ => true
    }
  );
};

let infer =
    (
      ~can_match: (Id.t, Id.t) => bool=(_, _) => true,
      ~grab: Id.t,
      ~old_seg: Segment.t,
      ~new_seg: Segment.t,
      (),
    )
    : t => {
  let olds = nodes(old_seg);
  let news = nodes(new_seg);
  let old_ids = Id.Set.of_list(IdMatch.ids_seg(old_seg));
  let new_ids = Id.Set.of_list(IdMatch.ids_seg(new_seg));
  let anchors =
    Id.Set.inter(old_ids, new_ids)
    |> Id.Set.elements
    |> List.map(id => (id, id));
  let policy = {
    ...IdMatch.Policy.default,
    match_secondary: false,
    match_grout: false,
  };
  let st =
    IdMatch.matching(~policy, ~anchors, ~can_match, ~old_seg, ~new_seg, ());
  /* Largest first: a moved unchanged expression keeps its identity as a
     whole, before its individual tokens get paired elsewhere. */
  news
  |> List.sort((a, b) => compare(b.size, a.size))
  |> List.iter(n =>
       if (!Id.Map.mem(Piece.id(n.piece), st.pairs)) {
         olds
         |> List.find_opt(o =>
              IdMatch.same_piece(o.piece, n.piece)
              && compatible(st, o.piece, n.piece)
            )
         |> Option.iter(o => IdMatch.claim_iso(~policy, st, o.piece, n.piece));
       }
     );
  /* Equal unmatched pieces are evidence for convergence, not permission
     to assign two live pieces the same id. Ambiguous twins stay unpaired. */
  let merges =
    olds
    |> List.filter_map(o => {
         let id = Piece.id(o.piece);
         if (Id.Set.mem(id, st.claimed)) {
           None;
         } else {
           switch (
             List.filter(
               n =>
                 IdMatch.same_piece(o.piece, n.piece)
                 && can_match(id, Piece.id(n.piece)),
               news,
             )
           ) {
           | [n] when Id.Map.mem(Piece.id(n.piece), st.pairs) =>
             Some((id, Piece.id(n.piece)))
           | _ => None
           };
         };
       });
  /* Keep the hand's copy when two occurrences coalesce. Its rail still
     ends at the other occurrence's LIVE position, before layout reacts. */
  let live_landing =
    merges
    |> List.find_map(((old_id, new_id)) =>
         old_id == grab
           ? {
             let survivor = Id.Map.find(new_id, st.pairs);
             st.pairs = Id.Map.add(new_id, grab, st.pairs);
             st.claimed =
               Id.Set.add(grab, Id.Set.remove(survivor, st.claimed));
             Some(survivor);
           }
           : None
       );
  let merges =
    merges
    |> List.map(((old_id, new_id)) => {
         let survivor = Id.Map.find(new_id, st.pairs);
         let old_id =
           old_id == survivor
             ? Option.value(~default=old_id, live_landing) : old_id;
         (old_id, survivor);
       });
  let copies =
    news
    |> List.filter_map(n => {
         let id = Piece.id(n.piece);
         if (Id.Map.mem(id, st.pairs)) {
           None;
         } else {
           switch (
             List.filter(
               o =>
                 IdMatch.same_piece(o.piece, n.piece)
                 && can_match(Piece.id(o.piece), id),
               olds,
             )
           ) {
           | [o] when Id.Set.mem(Piece.id(o.piece), st.claimed) =>
             Some((id, Piece.id(o.piece)))
           | _ => None
           };
         };
       });
  let segment = IdMatch.rewrite_seg(st.pairs, new_seg);
  let out_ids = IdMatch.ids_seg(segment);
  if (!IdMatch.same_seg(new_seg, segment)
      || List.length(out_ids) != Id.Set.cardinal(Id.Set.of_list(out_ids))) {
    {
      segment: new_seg,
      retained: Id.Map.empty,
      merges: [],
      copies: [],
      destination: None,
    };
  } else {
    let after = nodes(segment);
    let position = (id, ns) =>
      ns
      |> List.find_opt(n => Piece.id(n.piece) == id)
      |> Option.map(n => (n.path, n.seat));
    let destination =
      switch (live_landing) {
      | Some(id) => Some(Live(id))
      | None =>
        switch (position(grab, olds), position(grab, after)) {
        | (Some(a), Some(b)) when a != b => Some(Result(grab))
        | _ =>
          copies
          |> List.find_map(((id, from)) =>
               from == grab ? Some(Result(id)) : None
             )
        }
      };
    {
      segment,
      retained: st.pairs,
      merges,
      copies,
      destination,
    };
  };
};
