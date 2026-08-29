open Util;

/* W2a worker-resident program state (plans/w2-worker-residency.md).
 *
 * SEGMENTS are the synced truth — main is authoritative and ships
 * them; the term and per-item statics DERIVE here. Every sync is
 * self-describing: either the full segment, or per-item replacements
 * plus the complete (item id, fingerprint) roster. The receiver
 * verifies the roster against its stored fingerprints and answers
 * RosterMismatch on any difference, which the sender repairs with a
 * full sync — so a lost, duplicated, or reordered message can only
 * yield a stale generation, never a divergent one.
 *
 * Fingerprints are computed by the SENDER for the items it ships
 * (O(changed item)); the receiver only stores and compares ints
 * (O(#items) per sync). Item identity = the slice's first piece id,
 * matching MakeTerm.Incr's memo keying.
 *
 * NOTE: term derivation reuses MakeTerm.Incr, whose memos are
 * module-level single slots — one resident program per bundle. The
 * worker holds only the current slide (multi-slide residency evicts
 * to this same shape; see plan §4.8), and the main-thread bundle's
 * slots belong to the master editor. */

type item = {
  i_id: Id.t, /* first piece id of the slice */
  i_seg: Segment.t,
  i_print: int /* sender-computed content fingerprint */
};

type t = {
  generation: int,
  root: Sort.t,
  items: list(item),
  statics: DefStatics.t,
};

type sync_error =
  | RosterMismatch
  | UnknownItem(Id.t);

let item_id = (seg: Segment.t): option(Id.t) =>
  switch (seg) {
  | [] => None
  | [p, ..._] => Some(Piece.id(p))
  };

/* content fingerprint: the slice's text. Ids are NOT hashed — content
 * edits can keep piece ids stable, and text is the sync contract the
 * load path already round-trips. */
let fingerprint = (seg: Segment.t): int =>
  Hashtbl.hash(Printer.of_segment(~holes="?", seg));

let items_of_segment = (seg: Segment.t): list(item) =>
  MakeTerm.Incr.slices(seg)
  |> List.filter_map(slice =>
       switch (item_id(slice)) {
       | None => None
       | Some(id) =>
         Some({
           i_id: id,
           i_seg: slice,
           i_print: fingerprint(slice),
         })
       }
     );

let segment_of_items = (items: list(item)): Segment.t =>
  List.concat_map(it => it.i_seg, items);

let derive = (~settings, ~root, ~prev: option(DefStatics.t), items) => {
  let seg = segment_of_items(items);
  let whole = MakeTerm.Incr.term_of_root(~root, seg);
  DefStatics.calc(~settings, ~prev?, whole);
};

let sync_full =
    (~settings, ~generation: int, ~root: Sort.t, seg: Segment.t, prev): t => {
  let items = items_of_segment(seg);
  let prev_statics =
    switch (prev) {
    | Some(p) when p.root == root => Some(p.statics)
    | _ => None
    };
  {
    generation,
    root,
    items,
    statics: derive(~settings, ~root, ~prev=prev_statics, items),
  };
};

/* Per-item sync: [changed] replaces slices by id; [roster] is the
 * sender's complete post-change (id, fingerprint) list, in item
 * order, and must match the receiver's post-change state exactly.
 * Item INSERTIONS/DELETIONS change the roster shape and are shipped
 * as full syncs by the sender (an edit that changes the item count
 * is restructure-class; see plan §4.9). */
let sync_items =
    (
      ~settings,
      ~generation: int,
      ~changed: list((Id.t, Segment.t, int)),
      ~roster: list((Id.t, int)),
      prev: t,
    )
    : result(t, sync_error) => {
  let missing =
    changed
    |> List.find_opt(((id, _, _)) =>
         !List.exists(it => Id.equal(it.i_id, id), prev.items)
       );
  switch (missing) {
  | Some((id, _, _)) => Error(UnknownItem(id))
  | None =>
    let items =
      prev.items
      |> List.map(it =>
           switch (
             List.find_opt(((id, _, _)) => Id.equal(id, it.i_id), changed)
           ) {
           | Some((_, seg, print)) =>
             /* the replacement slice may lead with a fresh piece:
                re-key to ITS first piece id */
             switch (item_id(seg)) {
             | Some(new_id) => {
                 i_id: new_id,
                 i_seg: seg,
                 i_print: print,
               }
             | None => it
             }
           | None => it
           }
         );
    let ours = List.map(it => (it.i_id, it.i_print), items);
    if (List.length(ours) == List.length(roster)
        && List.for_all2(
             ((a, ha), (b, hb)) => Id.equal(a, b) && ha == hb,
             ours,
             roster,
           )) {
      Ok({
        generation,
        root: prev.root,
        items,
        statics:
          derive(
            ~settings,
            ~root=prev.root,
            ~prev=Some(prev.statics),
            items,
          ),
      });
    } else {
      Error(RosterMismatch);
    };
  };
};

/* The cross-boundary statics summary: per-item error/warning id sets.
 * Both sides compute it from their own DefStatics results, so the
 * shadow-mode comparison (plan §4.6) is definitionally apples-to-
 * apples. */
module Summary = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type item_summary = {
    s_id: Id.t,
    s_errors: list(Id.t),
    s_warnings: list(Id.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    s_generation: int,
    s_items: list(item_summary),
  };

  let sorted = ids => List.sort_uniq(Id.compare, ids);

  let of_def_statics = (~generation: int, ds: DefStatics.t): t => {
    s_generation: generation,
    s_items:
      ds.items
      |> List.map((it: DefStatics.item) =>
           {
             s_id: it.d_id,
             s_errors: sorted(it.d_error_ids),
             s_warnings: sorted(it.d_warning_ids),
           }
         ),
  };

  let equal = (a: t, b: t): bool => a.s_items == b.s_items; /* generations compare separately */
};

let summarize = (t: t): Summary.t =>
  Summary.of_def_statics(~generation=t.generation, t.statics);
