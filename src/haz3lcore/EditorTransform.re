/* Capture the haz3lcore `Secondary` before `open Language` shadows it with
 * the term-level `Language.Secondary` (which has no `mk_newline`). */
let mk_newline = Secondary.mk_newline;

open Language;
open Util;

/* A rewrite of one sub-term, routed through editor actions. It targets
 * either an expression or a proof node (proofs sit inside `Theorem`
 * expressions and have their own id space). */
[@deriving (show({with_path: false}), sexp, yojson)]
type patch =
  | ExpPatch({
      target_id: option(Id.t),
      replacement: Exp.t,
      /* When set, pretty-print just the replacement after splicing it in.
       * Defaults on. */
      reflow: bool,
    })
  | ProofPatch({
      target_id: Id.t,
      replacement: Proof.t,
      reflow: bool,
    })
  /* Remove a proof step, with its `;`, from its enclosing `Seq`, or
   * replace it with a hole when it is the whole proof. */
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

/* Replacing the sub-term with a given id. Each replacement returns the
 * term it replaced and the new root, or None when no sub-term has the id
 * (it may have gone since the patch was made). */
module Rewrite = {
  let replace = (~map_term, ~rep_id, ~target_id: Id.t, replacement, root) => {
    let replaced = ref(None);
    let root =
      map_term(
        (cont, node) =>
          if (rep_id(node) == target_id) {
            replaced := Some(node);
            replacement;
          } else {
            cont(node);
          },
        root,
      );
    Option.map(old => (old, root), replaced^);
  };

  /* With no `target_id`, replaces the root. */
  let exp =
      (~target_id: option(Id.t), replacement: Exp.t, root: Exp.t)
      : option((Exp.t, Exp.t)) =>
    switch (target_id) {
    | None => Some((root, replacement))
    | Some(target_id) =>
      replace(
        ~map_term=(f_exp, e) => Exp.map_term(~f_exp, e),
        ~rep_id=Exp.rep_id,
        ~target_id,
        replacement,
        root,
      )
    };

  let proof =
      (~target_id: Id.t, replacement: Proof.t, root: Proof.t)
      : option((Proof.t, Proof.t)) =>
    replace(
      ~map_term=(f_proof, p) => Proof.map_term(~f_proof, p),
      ~rep_id=Proof.rep_id,
      ~target_id,
      replacement,
      root,
    );

  /* For proofs nested in program syntax. */
  let proof_in_exp =
      (~target_id: Id.t, replacement: Proof.t, root: Exp.t)
      : option((Proof.t, Exp.t)) =>
    replace(
      ~map_term=(f_proof, e) => Exp.map_term(~f_proof, e),
      ~rep_id=Proof.rep_id,
      ~target_id,
      replacement,
      root,
    );

  /* The `Seq` whose direct child is `target_id`, and its other child. */
  let rec seq_parent =
          (~target_id: Id.t, p: Proof.t): option((Id.t, Proof.t)) =>
    switch (p.term) {
    | Seq(p1, p2) when Proof.rep_id(p1) == target_id =>
      Some((Proof.rep_id(p), p2))
    | Seq(p1, p2) when Proof.rep_id(p2) == target_id =>
      Some((Proof.rep_id(p), p1))
    | _ => List.find_map(seq_parent(~target_id), Proof.children(p))
    };

  /* For proofs nested in program syntax. */
  let seq_parent_in_exp =
      (~target_id: Id.t, root: Exp.t): option((Id.t, Proof.t)) => {
    let result = ref(None);
    let _: Exp.t =
      Exp.map_term(
        ~f_proof=
          (_cont, p) => {
            switch (result^) {
            | None => result := seq_parent(~target_id, p)
            | Some(_) => ()
            };
            p;
          },
        root,
      );
    result^;
  };

  /* Removing a step rewrites its enclosing `Seq` to the surviving sibling,
   * so the step and its `;` leave the text together. A step that is the
   * whole proof becomes a hole. Returns the id to replace and what
   * replaces it. */
  let step_removal =
      (~target_id: Id.t, seq_parent: option((Id.t, Proof.t)))
      : (Id.t, Proof.t) =>
    switch (seq_parent) {
    | Some(found) => found
    | None => (target_id, Proof.fresh(EmptyHole))
    };
};

/* Writing a replaced sub-term back into the program's segment.
 *
 * Re-serializing the whole program around a replaced sub-term reformats
 * all of the user's code (secondary, lexemes, parens). So `local` instead
 * finds the pieces the old sub-term came from in the current segment and
 * replaces just that range, leaving text elsewhere exactly as it was.
 * `whole_program` is the fallback. */
module Splice = {
  /* The editable writer: display settings (folded function bodies, hidden
   * ascriptions) would destroy user code. */
  let to_segment = (a: Any.t): Segment.t =>
    ExpToSegment.any_to_segment(
      ~settings={
        ...ExpToSegment.Settings.editable(~inline=false),
        use_literal_lexemes: false,
      },
      a,
    );

  let newline = (): Piece.t => Piece.secondary(mk_newline(Id.mk()));

  type outcome =
    | NotFound /* extent not in this subtree */
    | Spliced /* replaced and reflowed in place */
    | NeedsParentPrettify /* replaced raw in a direct child of an induction
                             tile; that tile lays out as a unit, so the
                             parent must prettify the whole tile */
    | Failed; /* extent found but can't be spliced safely — use fallback */

  /* The range is found by id: term ids come from the pieces they were
   * parsed from, and the serializer stamps term ids back onto the pieces
   * it emits, so serializing the old sub-term yields the ids of the
   * pieces it occupies (plus fresh secondary ids, which match nothing).
   *
   * None when the range can't be found or the splice wouldn't reparse
   * faithfully. */
  let local =
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
      |> to_segment
      |> Segment.ids
      |> List.to_seq
      |> Seq.map(id => (id, ()))
      |> Id.Map.of_seq;
    let raw = to_segment(replacement);
    let raw_atomic =
      List.length(List.filter(p => !Piece.is_secondary(p), raw)) <= 1;
    let is_match = (p: Piece.t) => Id.Map.mem(Piece.id(p), old_ids);
    /* Bracket `seg` with linebreaks, but only where the splice context
     * doesn't already provide one, checking the (all-secondary) margin of
     * the surrounding pieces. Unconditional brackets (like the fallback's
     * `wrap_block`) would add a blank line per patch here, since the
     * surrounding text survives the splice. */
    let ensure_boundary_linebreaks =
        (~pre: Segment.t, ~post: Segment.t, seg: Segment.t): Segment.t => {
      let rec margin_has_linebreak = (ps: list(Piece.t)): bool =>
        switch (ps) {
        | [p, ...rest] when Piece.is_secondary(p) =>
          Piece.is_linebreak(p) || margin_has_linebreak(rest)
        | _ => false
        };
      let lead = margin_has_linebreak(List.rev(pre)) ? [] : [newline()];
      let trail = margin_has_linebreak(post) ? [] : [newline()];
      lead @ seg @ trail;
    };
    let rec go =
            (~parent_is_induction: bool, seg: Segment.t)
            : (Segment.t, outcome) => {
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
               i < lo || i > hi || is_match(p) || Piece.is_secondary(p)
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
          let partial = List.exists(p => !Piece.is_secondary(p), pre @ post);
          let new_seg =
            if (!partial || raw_atomic) {
              Some(raw);
            } else {
              switch (replacement) {
              | Exp(e) => Some(to_segment(Exp(Exp.fresh(Parens(e)))))
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
    and descend = (seg: Segment.t): (Segment.t, outcome) => {
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
                          ~parent_is_induction=
                            Tile.has_label(t, ["induction", "end"]),
                          child,
                        );
                      ([child', ...cs], oc');
                    | Spliced
                    | NeedsParentPrettify
                    | Failed => ([child, ...cs], oc)
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

  /* Re-serialize the whole program. With `reflow_root`, pretty-print just
   * the sub-segment rooted there and leave the rest as serialized. The
   * root's id is a top-level piece of the tile child that holds it (e.g.
   * the `proof` child of a `Theorem` tile), so that child is the one
   * prettified. `wrap_block` brackets it with linebreaks so a proof body
   * lays out as a block under `proof`. When the id is only at the top
   * level (a root-level replacement), the whole segment is prettified. */
  let whole_program =
      (~reflow_root: option(Id.t), ~wrap_block: bool, root: Exp.t): Segment.t => {
    let seg = to_segment(Exp(root));
    switch (reflow_root) {
    | None => seg
    | Some(root_id) =>
      let format = (child: Segment.t): Segment.t => {
        let pretty = PrettySegment.prettify(child);
        wrap_block ? [newline()] @ pretty @ [newline()] : pretty;
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
               * line, each case body on its own indented line; see
               * PrettySegment's induction/end rule), so a replacement
               * landing in its slot reflows the whole tile rather than
               * the bare slot content, which wouldn't know it's an
               * induction. */
              | Tile(t)
                  when
                    Tile.has_label(t, ["induction", "end"])
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
  };
};

let apply_patch = (zipper: Zipper.t, patch: patch): Zipper.t => {
  let root = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
  /* A proof body lays out as a block under its keyword; an expression
   * splices in as it is. */
  let write = (~reflow, ~wrap_block, ~old_node, ~new_node, new_root) =>
    CaretPreserving.transform(zipper, seg =>
      switch (
        Splice.local(
          ~old_node,
          ~replacement=new_node,
          ~reflow,
          ~wrap_block,
          seg,
        )
      ) {
      | Some(seg) => seg
      | None =>
        let reflow_root = reflow ? Some(Any.rep_id(new_node)) : None;
        Splice.whole_program(~reflow_root, ~wrap_block, new_root);
      }
    );
  let replace_proof = (~reflow, ~target_id, replacement) =>
    switch (Rewrite.proof_in_exp(~target_id, replacement, root)) {
    | Some((old, new_root)) =>
      write(
        ~reflow,
        ~wrap_block=true,
        ~old_node=Proof(old),
        ~new_node=Proof(replacement),
        new_root,
      )
    | None => zipper
    };
  switch (patch) {
  | ExpPatch({target_id, replacement, reflow}) =>
    switch (Rewrite.exp(~target_id, replacement, root)) {
    | Some((old, new_root)) =>
      write(
        ~reflow,
        ~wrap_block=false,
        ~old_node=Exp(old),
        ~new_node=Exp(replacement),
        new_root,
      )
    | None => zipper
    }
  | ProofPatch({target_id, replacement, reflow}) =>
    replace_proof(~reflow, ~target_id, replacement)
  | ProofRemovePatch({target_id}) =>
    let (target_id, replacement) =
      Rewrite.step_removal(
        ~target_id,
        Rewrite.seq_parent_in_exp(~target_id, root),
      );
    replace_proof(~reflow=true, ~target_id, replacement);
  };
};

/* Apply a patch to a proof that is owned directly as an AST rather than
 * as a slice of program syntax (the result stepper's document). No
 * segmentation happens, so `reflow` has nothing to do here. `ExpPatch`
 * has no meaning without a program around the proof. */
let apply_patch_to_proof = (root: Proof.t, patch: patch): Proof.t => {
  let replace = (~target_id, replacement) =>
    switch (Rewrite.proof(~target_id, replacement, root)) {
    | Some((_, root)) => root
    | None => root
    };
  switch (patch) {
  | ProofPatch({target_id, replacement, reflow: _}) =>
    replace(~target_id, replacement)
  | ProofRemovePatch({target_id}) =>
    let (target_id, replacement) =
      Rewrite.step_removal(~target_id, Rewrite.seq_parent(~target_id, root));
    replace(~target_id, replacement);
  | ExpPatch(_) => root
  };
};
