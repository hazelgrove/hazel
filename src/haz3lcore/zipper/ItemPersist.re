open Util;

/* Per-item persistence: a document's top-level item slices are stored
   as individual sexp values plus an ordered ROSTER, so autosave
   writes only the items an edit touched (~3-10ms measured, vs
   0.1-1.5s for the whole zipper) and reload restores the segment
   EXACTLY — incomplete tiles, grout and all — with no text parse,
   which removes the quadratic-recovery reload class structurally.
   The whole-doc text blob remains the fallback and migration path
   (no roster ⟹ the old text load), owned by the caller
   (ScratchPersist), as is the HazelDB glue: this module is pure over
   an abstract string store.

   Write order is items → roster → GC of orphans, so an interrupted
   save leaves either the previous consistent view or benignly newer
   content under unchanged ids — never a roster naming a missing key.

   Item slicing reuses the incremental parser's top-level slices
   (identical boundaries and ids as the rest of the system); slices
   partition the piece list, so concatenating the stored items
   reproduces the segment verbatim. Item identity = the slice's first
   piece (nonempty partition ⟹ unique). */

type store = {
  get: string => option(string),
  set: (string, string) => unit,
  remove: string => unit,
};

[@deriving sexp]
type roster_entry = {
  r_id: Id.t,
  /* piece count: a cheap consistency stamp against stale values */
  r_pieces: int,
};

[@deriving sexp]
type roster = list(roster_entry);

let roster_key = "roster";
let item_key = (id: Id.t): string => "item:" ++ Id.to_string(id);

let items_of = (seg: Segment.t): list((Id.t, Segment.t)) =>
  MakeTerm.Incr.slices(seg)
  |> List.filter_map(slice =>
       switch (slice) {
       | [] => None
       | [p, ..._] => Some((Piece.id(p), slice))
       }
     );

/* the previously-saved slices, held as the actual segments: pieces
   are shared across ticks when unchanged (the identity-preservation
   discipline), so dirtiness is a per-item pointer walk */
type saved = list((Id.t, Segment.t));

let save = (~store: store, ~prev: saved, seg: Segment.t): saved => {
  let items = items_of(seg);
  let dirty = (id, s) =>
    switch (List.assoc_opt(id, prev)) {
    | Some(s0) => !Segment.ptr_eq(s0, s)
    | None => true
    };
  List.iter(
    ((id, s)) =>
      if (dirty(id, s)) {
        store.set(
          item_key(id),
          Sexplib.Sexp.to_string(Segment.sexp_of_t(s)),
        );
      },
    items,
  );
  let roster =
    List.map(
      ((id, s)) =>
        {
          r_id: id,
          r_pieces: List.length(s),
        },
      items,
    );
  store.set(roster_key, Sexplib.Sexp.to_string(sexp_of_roster(roster)));
  /* GC only after the roster names the survivors */
  List.iter(
    ((id, _)) =>
      if (!List.mem_assoc(id, items)) {
        store.remove(item_key(id));
      },
    prev,
  );
  items;
};

/* None on ANY inconsistency (missing roster/key, undecodable value,
   stamp mismatch): the caller falls back to the text blob */
let load = (~store: store): option(Segment.t) => {
  let decode_roster = (r: string): option(roster) =>
    switch (roster_of_sexp(Sexplib.Sexp.of_string(r))) {
    | roster => Some(roster)
    | exception _ => None
    };
  let decode_item = (v: string): option(Segment.t) =>
    switch (Segment.t_of_sexp(Sexplib.Sexp.of_string(v))) {
    | seg => Some(seg)
    | exception _ => None
    };
  switch (Option.bind(store.get(roster_key), decode_roster)) {
  | None => None
  | Some(roster) =>
    let rec go = (entries, acc) =>
      switch (entries) {
      | [] => Some(List.concat(List.rev(acc)))
      | [{r_id, r_pieces}, ...rest] =>
        switch (Option.bind(store.get(item_key(r_id)), decode_item)) {
        | Some(s) when List.length(s) == r_pieces => go(rest, [s, ...acc])
        | _ => None
        }
      };
    go(roster, []);
  };
};
