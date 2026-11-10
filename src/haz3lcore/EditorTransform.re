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
      /* When set, lay out the proof of the theorem the replacement is in
       * (see `Reflow.proof`). Defaults on. */
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

  /* The theorem whose proof has the node `proof_id`: its id and its whole
   * proof. */
  let theorem_around =
      (~proof_id: Id.t, root: Exp.t): option((Id.t, Proof.t)) => {
    let rec has_node = (p: Proof.t): bool =>
      Proof.rep_id(p) == proof_id
      || List.exists(has_node, Proof.children(p));
    let result = ref(None);
    let _: Exp.t =
      Exp.map_term(
        ~f_exp=
          (cont, e) => {
            switch (e.term) {
            | Theorem(_, _, proof, _) when result^ == None && has_node(proof) =>
              result := Some((Exp.rep_id(e), proof))
            | _ => ()
            };
            cont(e);
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

  type outcome =
    | NotFound /* extent not in this subtree */
    | Spliced
    | Failed; /* extent found but can't be spliced safely */

  /* The range is found by id: term ids come from the pieces they were
   * parsed from, and the serializer stamps term ids back onto the pieces
   * it emits, so serializing the old sub-term yields the ids of the
   * pieces it occupies (plus fresh secondary ids, which match nothing).
   *
   * With `reflow` the replacement is pretty-printed by itself.
   *
   * None when the range can't be found or the splice wouldn't reparse
   * faithfully. */
  let local =
      (~old_node: Any.t, ~replacement: Any.t, ~reflow: bool, seg: Segment.t)
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
    let rec go = (seg: Segment.t): (Segment.t, outcome) => {
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
           * re-associate on reparse. Expressions get a defensive Parens.
           * Proofs fail here, and `apply_patch` replaces the theorem's
           * whole proof instead, which has its slot to itself. */
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
            let new_seg = reflow ? PrettySegment.prettify(new_seg) : new_seg;
            (pre @ new_seg @ post, Spliced);
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
                      let (child', oc') = go(child);
                      ([child', ...cs], oc');
                    | Spliced
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
              ([tile, ...acc], outcome);
            | _ => ([p, ...acc], outcome)
            },
          ([], NotFound),
          seg,
        );
      (List.rev(rev_pieces), outcome);
    };
    switch (go(seg)) {
    | (seg', Spliced) => Some(seg')
    | (_, NotFound | Failed) => None
    };
  };

  /* Re-serialize the whole program. That discards its layout, so with
   * `reflow` all of it is pretty-printed. */
  let whole_program = (~reflow: bool, root: Exp.t): Segment.t => {
    let seg = to_segment(Exp(root));
    reflow ? PrettySegment.prettify(seg) : seg;
  };
};

/* Laying out a theorem's proof again after an edit to it. */
module Reflow = {
  /* The proof is laid out as a whole and inside its theorem, because an
   * edit to one step changes the lines around it: a second step turns a
   * one-line proof into a block, and the block is indented from where the
   * theorem sits. Only the slot of the proof is rewritten, so the
   * statement and the program around the theorem keep their text. */
  let proof = (~theorem_id: Id.t, seg: Segment.t): Segment.t => {
    /* `f` gives the new contents of a slot of steps, from the theorem's
     * tile and the slot's index in it. */
    let map_steps = (f: (Tile.t, int) => Segment.t): (Segment.t => Segment.t) =>
      Segment.map_deep(
        List.map((p: Piece.t) =>
          switch (p) {
          | Tile(t) when t.id == theorem_id =>
            Piece.Tile({
              ...t,
              children:
                List.mapi(
                  (i, (sort, child)) =>
                    Sort.equal(sort, Proof) ? f(t, i) : child,
                  Tile.sorted_children(t),
                ),
            })
          | _ => p
          }
        ),
      );
    let laid_out =
      map_steps(
        (t, i) =>
          /* Spacing first: the spliced-in steps come from the serializer,
           * which writes `| x:: t =>`, and the pretty printer keeps a
           * rule's pattern as it finds it. */
          switch (
            [Piece.Tile(t)]
            |> SpaceNormalize.go(~canonicalize=true)
            |> PrettySegment.prettify
          ) {
          | [Tile(pretty)]
              when List.length(pretty.children) == List.length(t.children) =>
            List.nth(pretty.children, i)
          | _ => List.nth(t.children, i)
          },
        seg,
      );
    let levels = Indentation.level_map(laid_out);
    map_steps(
      (t, i) =>
        Indentation.fix_indentation_in_segment(
          levels,
          List.nth(t.children, i),
        ),
      laid_out,
    );
  };
};

let apply_patch = (zipper: Zipper.t, patch: patch): Zipper.t => {
  let root = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
  /* A proof's replacement is written in as it is serialized, and with
   * `reflow` the theorem's whole proof is then laid out. */
  let replace_proof = (~reflow, ~target_id, replacement) =>
    switch (Rewrite.proof_in_exp(~target_id, replacement, root)) {
    | Some((old, new_root)) =>
      let theorem =
        Rewrite.theorem_around(
          ~proof_id=Proof.rep_id(replacement),
          new_root,
        );
      /* A replacement of several pieces can't be spliced in beside the
       * old node's siblings, but its theorem's whole proof can take the
       * place of the old one. */
      let whole_proofs =
        switch (Rewrite.theorem_around(~proof_id=target_id, root), theorem) {
        | (Some((_, before)), Some((_, after))) => [(before, after)]
        | _ => []
        };
      CaretPreserving.transform(
        zipper,
        seg => {
          let spliced =
            List.find_map(
              ((before, after)) =>
                Splice.local(
                  ~old_node=Proof(before),
                  ~replacement=Proof(after),
                  ~reflow=false,
                  seg,
                ),
              [(old, replacement), ...whole_proofs],
            );
          let seg =
            switch (spliced) {
            | Some(seg) => seg
            | None => Splice.whole_program(~reflow, new_root)
            };
          switch (theorem) {
          | Some((theorem_id, _)) when reflow =>
            Reflow.proof(~theorem_id, seg)
          | _ => seg
          };
        },
      );
    | None => zipper
    };
  switch (patch) {
  | ExpPatch({target_id, replacement, reflow}) =>
    switch (Rewrite.exp(~target_id, replacement, root)) {
    | Some((old, new_root)) =>
      CaretPreserving.transform(zipper, seg =>
        switch (
          Splice.local(
            ~old_node=Exp(old),
            ~replacement=Exp(replacement),
            ~reflow,
            seg,
          )
        ) {
        | Some(seg) => seg
        | None => Splice.whole_program(~reflow, new_root)
        }
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
