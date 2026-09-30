open Util;

/* per-item persistence: top-level item slices stored as separate sexp
   values plus an ordered roster, so autosave writes only touched items
   and reload restores the segment exactly (grout and all) with no text
   parse. pure over an abstract string store; the caller owns the
   text-blob fallback for when there's no roster.

   write order is items → roster → GC, so an interrupted save never
   leaves a roster naming a missing key. slices partition the piece
   list; an item is keyed by its first piece's id. */

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

/* the previously saved slices, kept as segments: unchanged pieces
   keep their identity, so dirtiness is a per-item pointer walk */
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

/* None on any inconsistency; the caller falls back to the text blob */
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
