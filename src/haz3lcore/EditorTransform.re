/* Capture the haz3lcore `Secondary` before `open Language` shadows it with
 * the term-level `Language.Secondary` (which has no `mk_newline`). */
let mk_newline = Secondary.mk_newline;

open Language;
open Util;

/* A concrete transformation payload that can be routed through editor
 * actions. The final rewrite still re-segments via ExpToSegment and
 * threads back through CaretPreserving.
 *
 * Two flavours so callers can target either an expression sub-node or
 * a proof sub-node (proofs are nested inside `Theorem` expressions and
 * have their own id space). */
[@deriving (show({with_path: false}), sexp, yojson)]
type patch =
  | ExpPatch({
      target_id: option(Id.t),
      replacement: Exp.t,
      /* When set, pretty-print just the replacement sub-segment after
       * splicing it in (see `reflow_subtree`). Defaults on. */
      reflow: bool,
    })
  | ProofPatch({
      target_id: Id.t,
      replacement: Proof.t,
      reflow: bool,
    });

let mk_patch = (~target_id=?, ~reflow=true, replacement: Exp.t): patch =>
  ExpPatch({
    target_id,
    replacement,
    reflow,
  });

let mk_proof_patch =
    (~target_id: Id.t, ~reflow=true, replacement: Proof.t): patch =>
  ProofPatch({
    target_id,
    replacement,
    reflow,
  });

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

/* Find and rewrite a proof sub-term reachable from an Exp.t root.
 *
 * `Proof.map_term`'s `~f_proof` callback only fires for the outer-most
 * Proof node reached from each enclosing Exp (its recursive helper
 * doesn't re-enter `f_proof` for nested children like the tail of a
 * `Seq`). So to support targeting a hole sitting inside `Seq(head,
 * EmptyHole)` we walk the proof sub-tree ourselves and re-invoke the
 * match check at every node.
 *
 * The boolean indicates whether the target id was actually located so
 * callers can no-op cleanly when the proof sub-term has disappeared. */
let rewrite_proof_in_exp =
    (~target_id: Id.t, f: Proof.t => Proof.t, root_exp: Exp.t): (Exp.t, bool) => {
  let found = ref(false);
  let rec walk_proof = (p: Proof.t): Proof.t =>
    if (Proof.rep_id(p) == target_id) {
      found := true;
      f(p);
    } else {
      {
        ...p,
        term:
          switch (p.term) {
          | EmptyHole
          | Invalid(_)
          | MultiHole(_)
          | AxiomStep(_)
          | AlgebriteStep(_)
          | EvalStep(_) => p.term
          | Seq(p1, p2) => Seq(walk_proof(p1), walk_proof(p2))
          | Induction(e, cases) =>
            Induction(
              e,
              List.map(((pt, body)) => (pt, walk_proof(body)), cases),
            )
          | Forall(x, body) => Forall(x, walk_proof(body))
          },
      };
    };
  let rewritten =
    Exp.map_term(~f_proof=(_cont, proof) => walk_proof(proof), root_exp);
  (found^ ? rewritten : root_exp, found^);
};

/* Patches re-serialize the whole program around the rewritten proof,
 * so this MUST be the faithful editable writer: literal lexemes keep
 * the user's original tokens, and nothing display-related (folded
 * function bodies, hidden ascriptions, ...) may leak into the written
 * syntax — `of_core` folds `fun` bodies into projector chips when
 * show_fn_bodies is off, silently destroying user code on every
 * proof-side patch. */
let exp_to_segment = (exp: Exp.t): Segment.t =>
  ExpToSegment.exp_to_segment(
    exp,
    ~settings={
      ...ExpToSegment.Settings.editable(~inline=false),
      use_literal_lexemes: false,
    },
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
          /* Induction lays out as a unit (scrutinee on the keyword
           * line, each case body on its own indented line — see
           * PrettySegment's induction/end rule), so a replacement
           * landing in its slot reflows the whole tile rather than the
           * bare slot content, which wouldn't know it's an induction. */
          | Tile(t)
              when
                t.label == ["induction", "end"]
                && List.exists(contains_root, t.children) => (
              acc @ PrettySegment.prettify([Tile(t)]),
              true,
            )
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

let apply_proof_transform =
    (~target_id: Id.t, ~reflow_id=?, zipper: Zipper.t, f: Proof.t => Proof.t)
    : Zipper.t => {
  let root_exp = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
  let (rewritten_exp, _found) =
    rewrite_proof_in_exp(~target_id, f, root_exp);
  let segment = exp_to_segment(rewritten_exp);
  let segment =
    switch (reflow_id) {
    | Some(id) => reflow_subtree(~root_id=id, ~wrap_block=true, segment)
    | None => segment
    };
  CaretPreserving.transform(zipper, _ => segment);
};

let apply_patch = (zipper: Zipper.t, patch: patch) =>
  switch (patch) {
  | ExpPatch({target_id, replacement, reflow}) =>
    let reflow_id = reflow ? Some(Exp.rep_id(replacement)) : None;
    apply_exp_transform(~target_id?, ~reflow_id?, zipper, _ => replacement);
  | ProofPatch({target_id, replacement, reflow}) =>
    let reflow_id = reflow ? Some(Proof.rep_id(replacement)) : None;
    apply_proof_transform(~target_id, ~reflow_id?, zipper, _ => replacement);
  };
