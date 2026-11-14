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
      /* When set, lay out the new steps in the proof of the theorem the
       * replacement is in (see `Reflow.proof` and `Keep`). Defaults on. */
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
   * ascriptions) would destroy user code. It keeps literals as they were
   * spelled (`007`). */
  let to_segment = (a: Any.t): Segment.t =>
    ExpToSegment.any_to_segment(
      ~settings=ExpToSegment.Settings.editable(~inline=false),
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
   * statement and the program around the theorem keep their text, and
   * `Keep.text` then puts back the proof's own text wherever the edit
   * didn't change it. */
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

/* Putting the user's whitespace and comments back after an edit.
 *
 * The edit itself is made by printing terms, so the edited segment's
 * tokens are right but its layout is the printer's. `text` keeps those
 * tokens and gives each gap between two of them the text it had before
 * the edit, when the tokens on either side of it are both still there.
 * Only gaps next to new tokens take the printer's layout. So separators
 * between surviving steps, comments and hand indentation all stay.
 *
 * Tokens are matched by id first: the printer stamps term ids onto the
 * pieces it writes, so a carried-over sub-term's tokens keep theirs. A
 * node the edit rebuilt has fresh ids, and its tokens are matched to the
 * old ones by label between the id matches, so that rebuilding an
 * induction to add a case keeps the layout around its other cases. */
module Keep = {
  /* A segment as its tokens (tile shards, grout, projectors), with the
   * run of secondary before each one and one more for the run at the
   * end. A gap's `owner` is the tile whose child it lies in, and its
   * depth is how many tiles' children it lies in. */
  type flat = {
    tokens: array((Id.t, string)),
    gaps: array(Segment.t),
    owners: array(option(Id.t)),
    depths: array(int),
    /* For each tile child, the indices of its first gap and its last:
     * the runs just inside its delimiters. */
    children: list((int, int)),
  };

  let flatten = (seg: Segment.t): flat => {
    let tokens = ref([]);
    let gaps = ref([]);
    let owners = ref([]);
    let depths = ref([]);
    let children = ref([]);
    let run = ref([]);
    let count = ref(0);
    let token = (~owner, ~depth, tok) => {
      gaps := [List.rev(run^), ...gaps^];
      owners := [owner, ...owners^];
      depths := [depth, ...depths^];
      tokens := [tok, ...tokens^];
      run := [];
      count := count^ + 1;
    };
    let rec level = (~owner, ~depth, seg: Segment.t) =>
      List.iter(
        (p: Piece.t) =>
          switch (p) {
          | Secondary(_) => run := [p, ...run^]
          | Tile(t) =>
            List.iteri(
              (i, shard) => {
                if (i > 0) {
                  let first = count^;
                  level(
                    ~owner=Some(t.id),
                    ~depth=depth + 1,
                    List.nth(t.children, i - 1),
                  );
                  children := [(first, count^), ...children^];
                };
                let (owner, depth) =
                  i == 0 ? (owner, depth) : (Some(t.id), depth + 1);
                token(~owner, ~depth, (t.id, Tile.token(t, shard)));
              },
              t.shards,
            )
          | Grout(g) => token(~owner, ~depth, (g.id, ""))
          | Projector(pr) => token(~owner, ~depth, (pr.id, ""))
          },
        seg,
      );
    level(~owner=None, ~depth=0, seg);
    gaps := [List.rev(run^), ...gaps^];
    owners := [None, ...owners^];
    depths := [0, ...depths^];
    {
      tokens: Array.of_list(List.rev(tokens^)),
      gaps: Array.of_list(List.rev(gaps^)),
      owners: Array.of_list(List.rev(owners^)),
      depths: Array.of_list(List.rev(depths^)),
      children: children^,
    };
  };

  /* Which old token each new token is, and back. Tokens with the same id
   * and label are matched first, in order; between those, a longest
   * common subsequence of labels matches the tokens of rebuilt nodes. */
  let align =
      (old: flat, new_: flat): (array(option(int)), array(option(int))) => {
    let n_old = Array.length(old.tokens);
    let n_new = Array.length(new_.tokens);
    let to_new = Array.make(n_old, None);
    let to_old = Array.make(n_new, None);
    let pair = (i, j) => {
      to_new[i] = Some(j);
      to_old[j] = Some(i);
    };
    let old_index = Hashtbl.create(n_old);
    Array.iteri((i, tok) => Hashtbl.replace(old_index, tok, i), old.tokens);
    let anchors =
      Array.to_list(new_.tokens)
      |> List.mapi((j, tok) => (j, Hashtbl.find_opt(old_index, tok)))
      |> List.fold_left(
           ((acc, last), (j, i)) =>
             switch (i) {
             | Some(i) when i > last => ([(i, j), ...acc], i)
             | _ => (acc, last)
             },
           ([], (-1)),
         )
      |> fst
      |> List.rev;
    /* Matches labels in old[i0, i1) against new[j0, j1). Stretches are
     * the insides of rebuilt nodes, so they are short; a long one is
     * left unmatched rather than paying for the table. Holes have no
     * label: a hole the edit filled must not match one it wrote. */
    let by_label = (i0, i1, j0, j1) => {
      let (m, n) = (i1 - i0, j1 - j0);
      if (m > 0 && n > 0 && m * n <= 250_000) {
        let label_at = (flat, k) => snd(flat.tokens[k]);
        let same = (i, j) =>
          label_at(old, i0 + i) != ""
          && label_at(old, i0 + i) == label_at(new_, j0 + j);
        let table = Array.make_matrix(m + 1, n + 1, 0);
        for (i in m - 1 downto 0) {
          for (j in n - 1 downto 0) {
            table[i][j] =
              same(i, j)
                ? table[i + 1][j + 1] + 1
                : max(table[i + 1][j], table[i][j + 1]);
          };
        };
        let rec walk = (i, j) =>
          if (i < m && j < n) {
            if (same(i, j)) {
              pair(i0 + i, j0 + j);
              walk(i + 1, j + 1);
            } else if (table[i + 1][j] >= table[i][j + 1]) {
              walk(i + 1, j);
            } else {
              walk(i, j + 1);
            };
          };
        walk(0, 0);
      };
    };
    let last =
      List.fold_left(
        ((i0, j0), (i, j)) => {
          by_label(i0, i, j0, j);
          pair(i, j);
          (i + 1, j + 1);
        },
        (0, 0),
        anchors,
      );
    by_label(fst(last), n_old, snd(last), n_new);
    (to_new, to_old);
  };

  let is_comment = (p: Piece.t) =>
    Piece.is_secondary(p) && !Piece.is_whitespace(p);

  /* A gap's pieces up to its last comment, or nothing if it has none. */
  let through_last_comment = (gap: Segment.t): Segment.t => {
    let rec drop = (rev: Segment.t) =>
      switch (rev) {
      | [p, ...rest] when !is_comment(p) => drop(rest)
      | _ => rev
      };
    gap |> List.rev |> drop |> List.rev;
  };

  /* `after`, an edit of `before`, with `before`'s layout wherever the
   * edit left it alone. */
  let text = (~before: Segment.t, ~after: Segment.t): Segment.t => {
    let old = flatten(before);
    let new_ = flatten(after);
    let (to_new, to_old) = align(old, new_);
    let (n_old, n_new) = (
      Array.length(old.tokens),
      Array.length(new_.tokens),
    );
    /* Indices -1 and n stand for the start and end of the program. */
    let old_of = j =>
      j < 0 ? Some(-1) : j >= n_new ? Some(n_old) : to_old[j];
    let new_of = i =>
      i < 0 ? Some(-1) : i >= n_old ? Some(n_new) : to_new[i];
    /* Tiles with a token in `after`, under their old ids. */
    let kept_tiles = Hashtbl.create(n_old);
    Array.iteri(
      (i, j) =>
        if (j != None) {
          Hashtbl.replace(kept_tiles, fst(old.tokens[i]), ());
        },
      to_new,
    );
    let survives = (owner: option(Id.t)) =>
      switch (owner) {
      | None => true
      | Some(id) => Hashtbl.mem(kept_tiles, id)
      };
    /* Gap j was a gap of `before` too: the tokens around it were next to
     * each other there. */
    let unchanged = j =>
      switch (old_of(j - 1), old_of(j)) {
      | (Some(x), Some(y)) => y == x + 1
      | _ => false
      };
    /* The gaps just inside a child whose contents changed: when such a
     * child becomes a block, its first and last lines move. */
    let at_changed_edge = Array.make(n_new + 1, false);
    List.iter(
      ((first, last)) =>
        if (!
              List.for_all(
                unchanged,
                List.init(last - first + 1, k => first + k),
              )) {
          at_changed_edge[first] = true;
          at_changed_edge[last] = true;
        },
      new_.children,
    );
    /* The old gap before each old token stays before it, unless the token
     * that preceded it there now comes after it. When new tokens went in
     * between those two, the gap goes where they are shallowest, latest
     * among equals: before a step inserted ahead of the token, but outside
     * parens opened around it. A comment that ended the earlier token's
     * line stays on that line, ahead of the first new line break. */
    let old_gap_at = Array.make(n_new + 1, None);
    let line_ends = Array.make(n_new + 1, []);
    for (j in 0 to n_new) {
      switch (old_of(j)) {
      | Some(y) =>
        let gap = old.gaps[y];
        switch (new_of(y - 1)) {
        | Some(x) when x >= j => ()
        | Some(x) when x < j - 1 =>
          let target =
            List.init(j - x - 1, k => x + 2 + k)
            |> List.fold_left(
                 (best, k) => new_.depths[k] <= new_.depths[best] ? k : best,
                 x + 1,
               );
          let (line_end, lb, rest) = ListUtil.split(gap, Piece.is_linebreak);
          let first_break =
            List.init(target - x - 1, k => x + 1 + k)
            |> List.find_opt(k =>
                 List.exists(Piece.is_linebreak, new_.gaps[k])
               );
          switch (lb, first_break) {
          | (Some(lb), Some(k)) when List.exists(is_comment, line_end) =>
            line_ends[k] = line_end;
            old_gap_at[target] = Some([lb, ...rest]);
          | _ => old_gap_at[target] = Some(gap)
          };
        | _ => old_gap_at[j] = Some(gap)
        };
      | None => ()
      };
    };
    /* The comments of the old gaps `is`, unless they were inside a
     * deleted node. */
    let comments_of = (is: list(int)): Segment.t =>
      is
      |> List.filter(i => survives(old.owners[i]))
      |> List.concat_map(i => old.gaps[i])
      |> through_last_comment;
    let breaks = (gap: Segment.t) =>
      List.length(List.filter(Piece.is_linebreak, gap));
    /* The edited segment can hold old trivia where the edit didn't reach.
     * Placing that is up to the rules here, so a printed gap loses its old
     * comments, each with the whitespace before it, and gives the rest of
     * its old whitespace under fresh ids. */
    let old_trivia = Hashtbl.create(64);
    Array.iter(
      List.iter(p => Hashtbl.replace(old_trivia, Piece.id(p), ())),
      old.gaps,
    );
    let printed = j =>
      List.fold_left(
        (acc: Segment.t, p: Piece.t) =>
          switch (p) {
          | Secondary(s) when Hashtbl.mem(old_trivia, s.id) =>
            if (is_comment(p)) {
              let rec drop_whitespace = (rev: Segment.t) =>
                switch (rev) {
                | [q, ...rest] when Piece.is_whitespace(q) =>
                  drop_whitespace(rest)
                | _ => rev
                };
              drop_whitespace(acc);
            } else {
              [
                Piece.Secondary({
                  ...s,
                  id: Id.mk(),
                }),
                ...acc,
              ];
            }
          | _ => [p, ...acc]
          },
        [],
        new_.gaps[j],
      )
      |> List.rev;
    let gap = j => {
      let printed = printed(j);
      /* The tokens deleted right after the token at j - 1. */
      let deleted =
        switch (old_of(j - 1)) {
        | Some(x) =>
          let rec from = i =>
            i < n_old && new_of(i) == None ? [i, ...from(i + 1)] : [];
          from(x + 1);
        | None => []
        };
      let kept = Option.value(~default=printed, old_gap_at[j]);
      /* Text replaced by new tokens leaves its first gap before them. A
       * deletion between two surviving tokens leaves one of the gaps
       * around it: the one with more line breaks, so that the lines on
       * either side of a deleted step stay where they were. */
      let (before, base, after) =
        switch (deleted) {
        | [first, ...inside] when old_of(j) == None => (
            [],
            old.gaps[first],
            comments_of(inside),
          )
        | [first, ...inside]
            when
              old_of(j) == Some(first + List.length(deleted))
              && breaks(old.gaps[first]) > breaks(kept) => (
            [],
            old.gaps[first],
            comments_of(inside) @ through_last_comment(kept),
          )
        | _ => (comments_of(deleted), kept, [])
        };
      let base =
        switch (base) {
        /* Two tokens that were never next to each other could fuse. */
        | [] when printed != [] && !unchanged(j) => printed
        /* A block whose contents changed takes the new line breaks at its
         * edges, as a one-line proof does when it gets a second step. */
        | _
            when
              at_changed_edge[j] && breaks(base) == 0 && breaks(printed) > 0 =>
          through_last_comment(base) @ snd(Segment.split_space_run(printed))
        | _ => base
        };
      let before = line_ends[j] @ before;
      let space = Indentation.make_indent_spaces(1);
      let before =
        switch (before, base) {
        | ([], _) => []
        | (_, [p, ..._]) when Piece.is_whitespace(p) => before
        | _ => before @ space
        };
      before @ base @ (after == [] ? [] : after @ space);
    };
    let gaps = Array.init(n_new + 1, gap);
    let next = ref(0);
    let take_gap = () => gaps[next^];
    let rec level = (seg: Segment.t): Segment.t => {
      let pieces =
        seg
        |> List.filter(p => !Piece.is_secondary(p))
        |> List.concat_map(p => {
             let g = take_gap();
             g @ [piece(p)];
           });
      pieces @ take_gap();
    }
    and piece = (p: Piece.t): Piece.t =>
      switch (p) {
      | Tile(t) =>
        incr(next);
        let children =
          List.map(
            child => {
              let child = level(child);
              incr(next);
              child;
            },
            t.children,
          );
        Tile({
          ...t,
          children,
        });
      | Grout(_)
      | Secondary(_)
      | Projector(_) =>
        incr(next);
        p;
      };
    let merged = level(after);
    /* An old line keeps its indentation, moved by as much as its depth in
     * the program changed; a new one gets the indentation of its depth. */
    let old_levels = Indentation.level_map(before);
    let new_levels = Indentation.level_map(merged);
    let reindent = (seg: Segment.t): Segment.t => {
      let rec go = (seg: Segment.t) =>
        switch (seg) {
        | [lb, ...rest] when Piece.is_linebreak(lb) =>
          let (spaces, rest) = Segment.split_space_run(rest);
          let id = Piece.id(lb);
          let width =
            switch (
              Id.Map.find_opt(id, old_levels),
              Id.Map.find_opt(id, new_levels),
            ) {
            | (Some(was), Some(is)) =>
              max(0, List.length(spaces) + is - was)
            | (None, Some(is)) => is
            | (_, None) => List.length(spaces)
            };
          let spaces =
            List.length(spaces) == width
              ? spaces : Indentation.make_indent_spaces(width);
          [lb, ...spaces] @ go(rest);
        | [p, ...rest] => [p, ...go(rest)]
        | [] => []
        };
      go(seg);
    };
    Segment.map_deep(reindent, merged);
  };
};

let apply_patch = (zipper: Zipper.t, patch: patch): Zipper.t => {
  let root = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
  /* A proof's replacement is written in as it is serialized, and with
   * `reflow` the theorem's whole proof is then laid out. Either way,
   * `Keep.text` gives the result back the user's text wherever the edit
   * didn't change it. */
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
          let edited =
            switch (spliced) {
            | Some(edited) => edited
            | None => Splice.whole_program(~reflow, new_root)
            };
          let edited =
            switch (theorem) {
            | Some((theorem_id, _)) when reflow =>
              Reflow.proof(~theorem_id, edited)
            | _ => edited
            };
          Keep.text(~before=seg, ~after=edited);
        },
      );
    | None => zipper
    };
  switch (patch) {
  | ExpPatch({target_id, replacement, reflow}) =>
    switch (Rewrite.exp(~target_id, replacement, root)) {
    | Some((old, new_root)) =>
      CaretPreserving.transform(
        zipper,
        seg => {
          let edited =
            switch (
              Splice.local(
                ~old_node=Exp(old),
                ~replacement=Exp(replacement),
                ~reflow,
                seg,
              )
            ) {
            | Some(edited) => edited
            | None => Splice.whole_program(~reflow, new_root)
            };
          Keep.text(~before=seg, ~after=edited);
        },
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
