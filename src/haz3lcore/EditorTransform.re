/* Capture the haz3lcore `Secondary` before `open Language` shadows it with
 * the term-level `Language.Secondary` (which has no `mk_newline`). */
let mk_newline = Secondary.mk_newline;
let secondary_is_linebreak = Secondary.is_linebreak;

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
    })
  /* Remove a proof step outright: splice it out of its enclosing `Seq`
   * (the `;` goes with it), or collapse to an EmptyHole when it is the
   * whole proof. Unlike replacing the step with a hole, this leaves no
   * residue in the text — the proof reads exactly as if the step were
   * never written. */
  | ProofRemovePatch({target_id: Id.t});

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

let mk_proof_remove_patch = (~target_id: Id.t): patch =>
  ProofRemovePatch({target_id: target_id});

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

/* Find and rewrite a proof sub-term inside a Proof.t root.
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
let rewrite_proof =
    (~target_id: Id.t, f: Proof.t => Proof.t, root: Proof.t): (Proof.t, bool) => {
  let found = ref(false);
  let rec walk = (p: Proof.t): Proof.t =>
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
          | Contradiction(_)
          | EvalStep(_) => p.term
          | Seq(p1, p2) => Seq(walk(p1), walk(p2))
          | Induction(e, cases) =>
            Induction(
              e,
              List.map(((pt, body)) => (pt, walk(body)), cases),
            )
          | Forall(x, body) => Forall(x, walk(body))
          | Assume(e, body) => Assume(e, walk(body))
          | Generalize(e, body) => Generalize(e, walk(body))
          | Revert(e, inst, body) => Revert(e, inst, walk(body))
          | Have(e, sub, body) => Have(e, walk(sub), walk(body))
          },
      };
    };
  let rewritten = walk(root);
  (found^ ? rewritten : root, found^);
};

/* Same rewrite, for proofs nested inside program syntax. */
let rewrite_proof_in_exp =
    (~target_id: Id.t, f: Proof.t => Proof.t, root_exp: Exp.t): (Exp.t, bool) => {
  let found = ref(false);
  let rewritten =
    Exp.map_term(
      ~f_proof=
        (_cont, proof) => {
          let (proof, found') = rewrite_proof(~target_id, f, proof);
          found := found^ || found';
          proof;
        },
      root_exp,
    );
  (found^ ? rewritten : root_exp, found^);
};

/* Locate the `Seq` whose immediate child is `target_id`, returning the
 * Seq's id and the sibling that survives the removal (see
 * `ProofRemovePatch`). Walks every proof position. */
let rec find_seq_parent =
        (~target_id: Id.t, p: Proof.t): option((Id.t, Proof.t)) =>
  switch (p.term) {
  | Seq(p1, p2) =>
    if (Proof.rep_id(p1) == target_id) {
      Some((Proof.rep_id(p), p2));
    } else if (Proof.rep_id(p2) == target_id) {
      Some((Proof.rep_id(p), p1));
    } else {
      switch (find_seq_parent(~target_id, p1)) {
      | Some(_) as r => r
      | None => find_seq_parent(~target_id, p2)
      };
    }
  | Forall(_, body)
  | Assume(_, body)
  | Generalize(_, body)
  | Revert(_, _, body) => find_seq_parent(~target_id, body)
  | Have(_, sub, body) =>
    switch (find_seq_parent(~target_id, sub)) {
    | Some(_) as r => r
    | None => find_seq_parent(~target_id, body)
    }
  | Induction(_, cases) =>
    List.find_map(((_, body)) => find_seq_parent(~target_id, body), cases)
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | Contradiction(_)
  | EvalStep(_) => None
  };

/* Same search, over every proof nested in program syntax. */
let find_seq_parent_in_exp =
    (~target_id: Id.t, root_exp: Exp.t): option((Id.t, Proof.t)) => {
  let result = ref(None);
  let _: Exp.t =
    Exp.map_term(
      ~f_proof=
        (_cont, proof) => {
          switch (result^) {
          | None => result := find_seq_parent(~target_id, proof)
          | Some(_) => ()
          };
          proof;
        },
      root_exp,
    );
  result^;
};

/* Fallback writer for when a patch can't be spliced locally (see
 * `try_local_splice`): re-serializes the whole program around the
 * rewritten sub-term, so this MUST be the faithful editable writer —
 * nothing display-related (folded function bodies, hidden ascriptions,
 * ...) may leak into the written syntax. `of_core` folds `fun` bodies
 * into projector chips when show_fn_bodies is off, silently destroying
 * user code on every proof-side patch. */
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

/* ---------- Local splice ----------
 *
 * Re-serializing the whole program around a rewritten sub-term reformats
 * every line of the user's code (AutoFormat secondary, canonicalized
 * lexemes, re-derived parens). Instead, locate the pieces the old
 * sub-term came from in the current segment and replace just that range
 * with a freshly serialized sub-segment — text anywhere else in the
 * program is preserved byte-for-byte.
 *
 * The extent is found by id: term ids are minted from the pieces they
 * were parsed from, and the serializer stamps term ids back onto the
 * pieces it emits, so serializing the OLD sub-term and collecting the
 * segment's ids yields exactly the piece ids the sub-term occupies in
 * the current zipper segment (plus fresh secondary ids, which are
 * harmless — they match nothing).
 *
 * Falls back to the whole-program writer (`None`) when the extent can't
 * be located or the splice wouldn't reparse faithfully. */

let any_to_segment_editable = (a: Any.t): Segment.t =>
  ExpToSegment.any_to_segment(
    ~settings={
      ...ExpToSegment.Settings.editable(~inline=false),
      use_literal_lexemes: false,
    },
    a,
  );

let piece_is_secondary: Piece.t => bool =
  fun
  | Secondary(_) => true
  | _ => false;

let piece_is_linebreak: Piece.t => bool =
  fun
  | Secondary(w) => secondary_is_linebreak(w)
  | _ => false;

/* Bracket `seg` with linebreaks, but only where the splice context
 * doesn't already provide one — checking the (all-secondary) margin of
 * the surrounding pieces. Unconditional brackets (like the fallback's
 * `wrap_block`) would accumulate a blank line per patch here, since the
 * surrounding text survives the splice. */
let ensure_boundary_linebreaks =
    (~pre: Segment.t, ~post: Segment.t, seg: Segment.t): Segment.t => {
  let margin_has_linebreak = (pieces: list(Piece.t)): bool => {
    let rec go = (ps: list(Piece.t)) =>
      switch (ps) {
      | [p, ...rest] when piece_is_secondary(p) =>
        piece_is_linebreak(p) || go(rest)
      | _ => false
      };
    go(pieces);
  };
  let nl = (): Piece.t => Piece.secondary(mk_newline(Id.mk()));
  let lead = margin_has_linebreak(List.rev(pre)) ? [] : [nl()];
  let trail = margin_has_linebreak(post) ? [] : [nl()];
  lead @ seg @ trail;
};

type splice_outcome =
  | NotFound /* extent not in this subtree */
  | Spliced /* replaced and reflowed in place */
  | NeedsParentPrettify /* replaced raw in a direct child of an induction
                           tile; that tile lays out as a unit, so the
                           parent must prettify the whole tile */
  | Failed; /* extent found but can't be spliced safely — use fallback */

let try_local_splice =
    (
      ~old_node: Any.t,
      ~replacement: Any.t,
      ~reflow: bool,
      ~wrap_block: bool,
      seg: Segment.t,
    )
    : option(Segment.t) => {
  let old_ids =
    old_node
    |> any_to_segment_editable
    |> Segment.ids
    |> List.to_seq
    |> Seq.map(id => (id, ()))
    |> Id.Map.of_seq;
  let raw = any_to_segment_editable(replacement);
  let raw_atomic =
    List.length(List.filter(p => !piece_is_secondary(p), raw)) <= 1;
  let is_match = (p: Piece.t) => Id.Map.mem(Piece.id(p), old_ids);
  let rec go =
          (~parent_is_induction: bool, seg: Segment.t)
          : (Segment.t, splice_outcome) => {
    let matched_idxs =
      seg
      |> List.mapi((i, p) => is_match(p) ? Some(i) : None)
      |> List.filter_map(Fun.id);
    switch (matched_idxs) {
    | [] => descend(seg)
    | [first, ...rest] =>
      let lo = first; /* mapi indices are ascending */
      let hi = List.fold_left(max, first, rest);
      /* every non-secondary piece inside the range must belong to the
       * old sub-term; a foreign tile wedged in means the id mapping is
       * broken and splicing would eat unrelated syntax */
      let range_ok =
        seg
        |> List.mapi((i, p) => (i, p))
        |> List.for_all(((i, p)) =>
             i < lo || i > hi || is_match(p) || piece_is_secondary(p)
           );
      if (!range_ok) {
        (seg, Failed);
      } else {
        let pre = List.filteri((i, _) => i < lo, seg);
        let post = List.filteri((i, _) => i > hi, seg);
        /* If the old sub-term shares this level with sibling pieces
         * (operators, other steps), a multi-piece replacement could
         * re-associate on reparse. Expressions get a defensive Parens;
         * proofs have no parens form, so bail to the fallback. */
        let partial = List.exists(p => !piece_is_secondary(p), pre @ post);
        let new_seg =
          if (!partial || raw_atomic) {
            Some(raw);
          } else {
            switch (replacement) {
            | Exp(e) =>
              Some(any_to_segment_editable(Exp(Exp.fresh(Parens(e)))))
            | _ => None
            };
          };
        switch (new_seg) {
        | None => (seg, Failed)
        | Some(new_seg) =>
          if (reflow && parent_is_induction) {
            (pre @ new_seg @ post, NeedsParentPrettify);
          } else {
            let formatted =
              reflow ? PrettySegment.prettify(new_seg) : new_seg;
            let formatted =
              reflow && wrap_block
                ? ensure_boundary_linebreaks(~pre, ~post, formatted)
                : formatted;
            (pre @ formatted @ post, Spliced);
          }
        };
      };
    };
  }
  and descend = (seg: Segment.t): (Segment.t, splice_outcome) => {
    let (rev_pieces, outcome) =
      List.fold_left(
        ((acc, outcome), p: Piece.t) =>
          switch (outcome, p) {
          | (NotFound, Tile(t)) =>
            let (rev_children, outcome) =
              List.fold_left(
                ((cs, oc), child) =>
                  switch (oc) {
                  | NotFound =>
                    let (child', oc') =
                      go(
                        ~parent_is_induction=t.label == ["induction", "end"],
                        child,
                      );
                    ([child', ...cs], oc');
                  | _ => ([child, ...cs], oc)
                  },
                ([], NotFound),
                t.children,
              );
            let tile: Piece.t =
              Tile({
                ...t,
                children: List.rev(rev_children),
              });
            switch (outcome) {
            | NeedsParentPrettify => (
                List.rev(PrettySegment.prettify([tile])) @ acc,
                Spliced,
              )
            | Spliced => ([tile, ...acc], Spliced)
            | NotFound
            | Failed => ([p, ...acc], outcome)
            };
          | _ => ([p, ...acc], outcome)
          },
        ([], NotFound),
        seg,
      );
    (List.rev(rev_pieces), outcome);
  };
  switch (go(~parent_is_induction=false, seg)) {
  | (seg', Spliced) => Some(seg')
  | (_, NotFound | Failed | NeedsParentPrettify) => None
  };
};

let apply_exp_transform =
    (~target_id=?, ~reflow_id=?, zipper: Zipper.t, f: Exp.t => Exp.t)
    : Zipper.t => {
  let root_exp = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
  let old_node = ref(None);
  let new_node = ref(None);
  let f = e => {
    old_node := Some(e);
    let e' = f(e);
    new_node := Some(e');
    e';
  };
  let (rewritten_exp, found) = rewrite_exp(~target_id?, f, root_exp);
  if (!found) {
    zipper;
  } else {
    CaretPreserving.transform(
      zipper,
      seg => {
        let local =
          switch (old_node^, new_node^) {
          | (Some(o), Some(n)) =>
            try_local_splice(
              ~old_node=Exp(o),
              ~replacement=Exp(n),
              ~reflow=Option.is_some(reflow_id),
              ~wrap_block=false,
              seg,
            )
          | _ => None
          };
        switch (local) {
        | Some(seg) => seg
        | None =>
          let segment = exp_to_segment(rewritten_exp);
          switch (reflow_id) {
          | Some(id) =>
            reflow_subtree(~root_id=id, ~wrap_block=false, segment)
          | None => segment
          };
        };
      },
    );
  };
};

let apply_proof_transform =
    (~target_id: Id.t, ~reflow_id=?, zipper: Zipper.t, f: Proof.t => Proof.t)
    : Zipper.t => {
  let root_exp = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
  let old_node = ref(None);
  let new_node = ref(None);
  let f = p => {
    old_node := Some(p);
    let p' = f(p);
    new_node := Some(p');
    p';
  };
  let (rewritten_exp, found) = rewrite_proof_in_exp(~target_id, f, root_exp);
  if (!found) {
    zipper;
  } else {
    CaretPreserving.transform(
      zipper,
      seg => {
        let local =
          switch (old_node^, new_node^) {
          | (Some(o), Some(n)) =>
            try_local_splice(
              ~old_node=Proof(o),
              ~replacement=Proof(n),
              ~reflow=Option.is_some(reflow_id),
              ~wrap_block=true,
              seg,
            )
          | _ => None
          };
        switch (local) {
        | Some(seg) => seg
        | None =>
          let segment = exp_to_segment(rewritten_exp);
          switch (reflow_id) {
          | Some(id) => reflow_subtree(~root_id=id, ~wrap_block=true, segment)
          | None => segment
          };
        };
      },
    );
  };
};

/* Apply a patch to a proof that is owned directly as an AST rather than
 * as a slice of program syntax (the result stepper's document). No
 * segmentation happens, so `reflow` — which only governs pretty-printing
 * of the written tiles — has nothing to do here. `ExpPatch` has no
 * meaning without a program around the proof. */
let apply_patch_to_proof = (root: Proof.t, patch: patch): Proof.t =>
  switch (patch) {
  | ProofPatch({target_id, replacement, reflow: _}) =>
    let (rewritten, _found) =
      rewrite_proof(~target_id, _ => replacement, root);
    rewritten;
  | ProofRemovePatch({target_id}) =>
    switch (find_seq_parent(~target_id, root)) {
    | Some((seq_id, remaining)) =>
      rewrite_proof(~target_id=seq_id, _ => remaining, root) |> fst
    | None =>
      /* Not inside a Seq: the sole step of a proof. Removing it leaves
       * an empty proof, i.e. a hole. */
      rewrite_proof(~target_id, _ => Proof.fresh(EmptyHole), root) |> fst
    }
  | ExpPatch(_) => root
  };

let apply_patch = (zipper: Zipper.t, patch: patch) =>
  switch (patch) {
  | ExpPatch({target_id, replacement, reflow}) =>
    let reflow_id = reflow ? Some(Exp.rep_id(replacement)) : None;
    apply_exp_transform(~target_id?, ~reflow_id?, zipper, _ => replacement);
  | ProofPatch({target_id, replacement, reflow}) =>
    let reflow_id = reflow ? Some(Proof.rep_id(replacement)) : None;
    apply_proof_transform(~target_id, ~reflow_id?, zipper, _ => replacement);
  | ProofRemovePatch({target_id}) =>
    let root_exp = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
    switch (find_seq_parent_in_exp(~target_id, root_exp)) {
    | Some((seq_id, remaining)) =>
      /* Rewrite the enclosing Seq down to the surviving sibling — the
       * removed step and its `;` disappear from the text together. */
      apply_proof_transform(
        ~target_id=seq_id, ~reflow_id=Proof.rep_id(remaining), zipper, _ =>
        remaining
      )
    | None =>
      let hole = Proof.fresh(EmptyHole);
      apply_proof_transform(
        ~target_id, ~reflow_id=Proof.rep_id(hole), zipper, _ =>
        hole
      );
    };
  };
