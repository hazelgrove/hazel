/* Capture the haz3lcore `Secondary` before `open Language` shadows it with
 * the term-level `Language.Secondary` (which has no `mk_newline`). */
let mk_newline = Secondary.mk_newline;

open Language;
open Util;

/* A concrete transformation payload that can be routed through editor actions.
   The final rewrite still goes through apply_exp_transform. */
[@deriving (show({with_path: false}), sexp, yojson)]
type patch = {
  target_id: option(Id.t),
  replacement: Exp.t,
};

let mk_patch = (~target_id=?, replacement: Exp.t): patch => {
  target_id,
  replacement,
};

let rewrite_exp =
    (~target_id=?, f: Exp.t => Exp.t, root_exp: Exp.t): (Exp.t, bool) =>
  switch (target_id) {
  | None => (f(root_exp), true)
  | Some(target_id) =>
    let found = ref(false);
    let rewritten =
      Exp.map_term(
        ~f_exp=
          (cont, exp) =>
            if (Exp.rep_id(exp) == target_id) {
              found := true;
              f(exp);
            } else {
              cont(exp);
            },
        root_exp,
      );
    (found^ ? rewritten : root_exp, found^);
  };

let exp_to_segment = (exp: Exp.t): Segment.t =>
  ExpToSegment.exp_to_segment(
    exp,
    ~settings=ExpToSegment.Settings.of_core(~inline=false, CoreSettings.on),
  );

/* Pretty-print only the sub-segment rooted at `root_id`, leaving the rest of
 * `seg` exactly as `exp_to_segment` produced it. The replacement's outermost
 * node id appears as a top-level piece of the tile child that holds the
 * subtree (e.g. the `proof` child of a `Theorem` tile), so we locate that
 * child and run the generic `PrettySegment.prettify` on it. `wrap_block`
 * brackets the reflowed child with linebreaks so a proof body lays out as a
 * block under `proof`. Falls back to prettifying the whole segment when the
 * id is only present at the top level (e.g. a root-level replacement). */
let reflow_subtree =
    (~root_id: Id.t, ~wrap_block: bool, seg: Segment.t): Segment.t => {
  let nl = (): Piece.t => Piece.secondary(mk_newline(Id.mk()));
  let format = (child: Segment.t): Segment.t => {
    let pretty = PrettySegment.prettify(child);
    wrap_block ? [nl()] @ pretty @ [nl()] : pretty;
  };
  let contains_root = (child: Segment.t): bool =>
    List.exists((p: Piece.t) => Piece.id(p) == root_id, child);
  let rec reflow_seg = (seg: Segment.t): (Segment.t, bool) =>
    List.fold_left(
      ((acc, found), p: Piece.t) =>
        if (found) {
          (acc @ [p], true);
        } else {
          switch (p) {
          | Tile(t) =>
            let (children, found') = reflow_children(t.children);
            (
              acc
              @ [
                Tile({
                  ...t,
                  children,
                }),
              ],
              found',
            );
          | Grout(_)
          | Secondary(_)
          | Projector(_) => (acc @ [p], false)
          };
        },
      ([], false),
      seg,
    )
  and reflow_children =
      (children: list(Segment.t)): (list(Segment.t), bool) =>
    List.fold_left(
      ((acc, found), child: Segment.t) =>
        if (found) {
          (acc @ [child], true);
        } else if (contains_root(child)) {
          (acc @ [format(child)], true);
        } else {
          let (child', found') = reflow_seg(child);
          (acc @ [child'], found');
        },
      ([], false),
      children,
    );
  let (out, found) = reflow_seg(seg);
  found ? out : PrettySegment.prettify(seg);
};

let apply_exp_transform =
    (~target_id=?, ~reflow_id=?, zipper: Zipper.t, f: Exp.t => Exp.t)
    : Zipper.t => {
  let root_exp = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
  let (rewritten_exp, _found) = rewrite_exp(~target_id?, f, root_exp);
  let segment = exp_to_segment(rewritten_exp);
  let segment =
    switch (reflow_id) {
    | Some(id) => reflow_subtree(~root_id=id, ~wrap_block=false, segment)
    | None => segment
    };
  CaretPreserving.transform(zipper, _ => segment);
};

let apply_patch = (zipper: Zipper.t, {target_id, replacement}: patch) =>
  apply_exp_transform(~target_id?, zipper, _ => replacement);
