/* Shared term-surgery kit for refactorings: secondary/slot
 * machinery, name/mention analysis, tree walking, id discipline,
 * splice parens, def-line helpers. */
open Language;

/* Term-level refactorings, built on the segment<->term roundtrip:
 * parse the buffer to a term (via canonical completion), transform the
 * term, print with roundtrip settings, and splice back as the new
 * zipper. Ids survive the trip, so probes, statics, and incremental
 * caches survive the refactor. */

let roundtrip_settings: ExpToSegment.Settings.t = {
  secondary: PreserveExact,
  parenthesization: Structural,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  use_literal_lexemes: true,
  project_tables: false,
};

/* Structural printing adds no defensive parens, so a non-atomic
 * definition must be wrapped before substitution or the inlined text
 * can reassociate (`let x = 1 + 2 in x * x` -> `1 + 2 * 1 + 2`) */
let needs_parens = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(_)
  | Atom(_)
  | EmptyHole
  | Parens(_)
  | ListLit(_)
  | Tuple([])
  | Ap(Forward, _, _)
  | TypAp(_)
  | Dot(_)
  | Constructor(_, _) => false
  | _ => true
  };

let secondary_run_pieces = (seg: Segment.t): list(Secondary.t) => {
  let rec go = (acc, seg: Segment.t) =>
    switch (seg) {
    | [Piece.Secondary(w), ...rest] =>
      go(
        [
          {
            Secondary.id: w.id,
            content: w.content,
          },
          ...acc,
        ],
        rest,
      )
    | _ => List.rev(acc)
    };
  go([], seg);
};

let drop_secondary = (ids: list(Id.t), e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let keep = (ws: list(Secondary.t)) =>
          ws |> List.filter((w: Secondary.t) => !List.mem(w.id, ids));
        let (before, after) = e.annotation.secondary;
        {
          ...e,
          annotation: {
            ...e.annotation,
            secondary: (keep(before), keep(after)),
          },
        }
        |> cont;
      },
    e,
  );

/* A node's slot is its textual boundary whitespace: the runs at its
 * printed edges. Whitespace attaches to adjacent LEAF nodes (a
 * region's boundary spacing lives on its first leaf's `before` and
 * last leaf's `after`), and map_term visits constructor args in
 * evaluation order (right to left), so boundary runs are determined
 * TEXTUALLY — print the region, take the edge Secondary runs — never
 * structurally. Refactorings that replace or move a node keep the
 * slot in place: the new occupant takes it over. */
/* A term's OUTER whitespace, read off the printed form. Use these —
   not fst/snd(e.annotation.secondary) — whenever the question is
   "what sits before/after this construct": edge runs live on
   whichever descendant prints first/last (see the trap note on
   IdTagged.secondary_runs), and these aggregate correctly for every
   form. Corollary for scanners: a subtree CONTAINS its own
   sitting-position runs, so "does e span lines" must strip
   lead/trail first (strip_leading/strip_trailing) before
   scanning. */
module Slot = {
  type t = {
    lead: list(Secondary.t),
    trail: list(Secondary.t),
  };
  let of_exp = (e: Exp.t): t => {
    let seg = ExpToSegment.exp_to_segment(~settings=roundtrip_settings, e);
    {
      lead: secondary_run_pieces(seg),
      trail: List.rev(secondary_run_pieces(List.rev(seg))),
    };
  };
  let lead_of = (e: Exp.t): t => {
    ...of_exp(e),
    trail: [],
  };
  let trail_of = (e: Exp.t): t => {
    ...of_exp(e),
    lead: [],
  };
  /* remove the slot's pieces (by id) wherever they occur in a term */
  let drop = (s: t, e: Exp.t): Exp.t =>
    drop_secondary(List.map((w: Secondary.t) => w.id, s.lead @ s.trail), e);
  /* attach at a node's outer boundary */
  let give = (s: t, e: Exp.t): Exp.t => {
    let (b, a) = e.annotation.secondary;
    {
      ...e,
      annotation: {
        ...e.annotation,
        secondary: (s.lead @ b, a @ s.trail),
      },
    };
  };
  /* the replacement takes over the replaced node's slot; parts of the
   * old node reused inside the replacement shed the boundary pieces
   * first so they aren't duplicated */
  let takeover = (~of_: Exp.t, result: Exp.t): Exp.t => {
    let s = of_exp(of_);
    give(s, drop(s, result));
  };
  /* region moves into a vacated position: shed its own lead, then
   * take over that position's boundary secondary (lead before, trail
   * after) */
  let occupy =
      (slot: (list(Secondary.t), list(Secondary.t)), region: Exp.t): Exp.t => {
    let region = drop(lead_of(region), region);
    give(
      {
        lead: fst(slot),
        trail: snd(slot),
      },
      region,
    );
  };
};

let strip_boundaries = (e: Exp.t): Exp.t => Slot.(drop(of_exp(e), e));
let strip_leading = (e: Exp.t): Exp.t => Slot.(drop(lead_of(e), e));
let strip_trailing = (e: Exp.t): Exp.t => Slot.(drop(trail_of(e), e));

let is_comment_piece = (w: Secondary.t): bool =>
  switch (w.content) {
  | Comment(_) => true
  | _ => false
  };

/* strip only the WHITESPACE pieces of a run, leaving comments where
   they live (P4: prose is content with a place, not spacing) */
let ws_of_slot = (s: Slot.t): Slot.t => {
  lead: s.lead |> List.filter(w => !is_comment_piece(w)),
  trail: s.trail |> List.filter(w => !is_comment_piece(w)),
};
let strip_boundaries_keep_comments = (e: Exp.t): Exp.t =>
  Slot.(drop(ws_of_slot(of_exp(e)), e));
let strip_leading_keep_comments = (e: Exp.t): Exp.t =>
  Slot.(drop(ws_of_slot(lead_of(e)), e));
let strip_trailing_keep_comments = (e: Exp.t): Exp.t =>
  Slot.(drop(ws_of_slot(trail_of(e)), e));

/* the comment pieces of a node's boundary runs (for re-homing when
   the node's line dissolves, e.g. inline) */
let boundary_comments = (e: Exp.t): list(Secondary.t) =>
  Slot.of_exp(e).lead
  @ Slot.of_exp(e).trail
  |> List.filter(is_comment_piece);

/* prose belongs to the ORIGINAL: minted COPIES drop comments rather
   than duplicating the text (the first/traveling copy keeps them) */
let strip_comments = (e: Exp.t): Exp.t => {
  /* a dropped comment takes its own line break with it (else the
     copy keeps a blank line where the prose was) */
  let is_break = (w: Secondary.t) =>
    switch (w.content) {
    | Whitespace("\n") => true
    | _ => false
    };
  let rec keep = (ws: list(Secondary.t)) =>
    switch (ws) {
    | [] => []
    | [c, nl, ...rest] when is_comment_piece(c) && is_break(nl) =>
      keep(rest)
    | [c, ...rest] when is_comment_piece(c) => keep(rest)
    | [w, ...rest] => [w, ...keep(rest)]
    };
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let (b, a) = e.annotation.secondary;
        cont({
          ...e,
          annotation: {
            ...e.annotation,
            secondary: (keep(b), keep(a)),
          },
        });
      },
    ~f_pat=
      (cont, p: Pat.t) => {
        let (b, a) = p.annotation.secondary;
        cont({
          ...p,
          annotation: {
            ...p.annotation,
            secondary: (keep(b), keep(a)),
          },
        });
      },
    ~f_typ=
      (cont, t: Typ.t) => {
        let (b, a) = t.annotation.secondary;
        cont({
          ...t,
          annotation: {
            ...t.annotation,
            secondary: (keep(b), keep(a)),
          },
        });
      },
    e,
  );
};

/* Split a line's lead at its ATTACHED DOC BLOCK: the trailing run
   of comment lines touching the content below (no blank line between
   block and content — andrew's heuristic; no blank required above,
   so comment-fn-comment-fn style attaches each block down). The
   attached part starts at the first comment after the last blank-
   line boundary and carries its own line breaks + trailing indent;
   the break separating it from what's ABOVE stays with the position.
   Movement redistributes the two parts — pieces are only relocated,
   never dropped or copied (comments exist once, always). */
let split_doc_block =
    (ws: list(Secondary.t)): (list(Secondary.t), list(Secondary.t)) => {
  let is_break = (w: Secondary.t) =>
    switch (w.content) {
    | Whitespace(x) => String.contains(x, '\n')
    | _ => false
    };
  let is_space = (w: Secondary.t) =>
    switch (w.content) {
    | Whitespace(x) => !String.contains(x, '\n')
    | _ => false
    };
  let arr = Array.of_list(ws);
  let n = Array.length(arr);
  /* last blank-line boundary: two breaks with only spaces between */
  let last_blank_end = ref(0);
  let prev_break = ref(false);
  Array.iteri(
    (i, w) =>
      if (is_break(w)) {
        if (prev_break^) {
          last_blank_end := i + 1;
        };
        prev_break := true;
      } else if (!is_space(w)) {
        prev_break := false;
      },
    arr,
  );
  let first_comment = ref(-1);
  Array.iteri(
    (i, w) =>
      if (first_comment^ < 0 && i >= last_blank_end^ && is_comment_piece(w)) {
        first_comment := i;
      },
    arr,
  );
  switch (first_comment^) {
  | c when c >= 0 => (
      Array.to_list(Array.sub(arr, 0, c)),
      Array.to_list(Array.sub(arr, c, n - c)),
    )
  | _ => (ws, [])
  };
};

/* strip a plain-whitespace lead; a lead containing COMMENTS is user
   content and stays put (RemoveUnusedLet once ate the next line's
   comment block via a bare strip) */
let strip_leading_ws = (e: Exp.t): Exp.t => {
  let has_comment =
    Slot.lead_of(e).lead
    |> List.exists((w: Secondary.t) =>
         switch (w.content) {
         | Comment(_) => true
         | _ => false
         }
       );
  has_comment ? e : strip_leading(e);
};

let space = (): list(Secondary.t) => [
  {
    id: Id.mk(),
    content: Whitespace(" "),
  },
];

/* a fresh separator copying a run's LINE SHAPE only: newline + the
 * indentation after its last linebreak. Never copies comments — lead
 * runs include them, and copy_runs once duplicated a comment block
 * above an extraction site. */
let sep_like = (run: list(Secondary.t)): list(Secondary.t) => {
  let text =
    run
    |> List.filter_map((w: Secondary.t) =>
         switch (w.content) {
         | Whitespace(s) => Some(s)
         | _ => None
         }
       )
    |> String.concat("");
  switch (String.rindex_opt(text, '\n')) {
  | Some(i) =>
    /* atomic pieces only: the renderer accepts exactly " " or "\n"
       per Secondary (a compound "\n    " crashes Code.of_secondary) */
    let indent = String.length(text) - i - 1;
    [
      {
        Secondary.id: Id.mk(),
        content: Whitespace("\n"),
      },
    ]
    @ List.init(indent, _ =>
        {
          Secondary.id: Id.mk(),
          content: Secondary.Whitespace(" "),
        }
      );
  | None => space()
  };
};

let newline = (): list(Secondary.t) => [
  {
    id: Id.mk(),
    content: Whitespace("\n"),
  },
];

let has_newline = (ws: list(Secondary.t)): bool =>
  ws
  |> List.exists((w: Secondary.t) =>
       switch (w.content) {
       | Whitespace(s) => String.contains(s, '\n')
       | _ => false
       }
     );

/* single spaces at a synthesized node's edges (can't be globalized:
 * user-authored tight junctions like `(a=1)` are legitimate) */
let pad = (e: IdTagged.t('a)): IdTagged.t('a) => {
  ...e,
  annotation: {
    ...e.annotation,
    secondary: (space(), space()),
  },
};

/* deliberately monomorphic (also _pat/_typ below): a generic
 * IdTagged.t('a) version breaks constructor disambiguation at inline
 * record-build call sites — same reason fresh/fresh_pat/fresh_typ
 * stay separate */
let with_secondary =
    (secondary: (list(Secondary.t), list(Secondary.t)), e: Exp.t): Exp.t => {
  ...e,
  annotation: {
    ...e.annotation,
    secondary,
  },
};

let with_secondary_pat =
    (secondary: (list(Secondary.t), list(Secondary.t)), p: Pat.t): Pat.t => {
  ...p,
  annotation: {
    ...p.annotation,
    secondary,
  },
};

let with_secondary_typ =
    (secondary: (list(Secondary.t), list(Secondary.t)), t: Typ.t): Typ.t => {
  ...t,
  annotation: {
    ...t.annotation,
    secondary,
  },
};

let fresh = (term): Exp.t => {
  annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
  term,
};
let fresh_pat = (term): Pat.t => {
  annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
  term,
};

/* fresh ids for a statics-derived type before it enters the buffer */
let refresh_annotation = (a: IdTagged.IdTag.t): IdTagged.IdTag.t => {
  let refresh_sec = (ws: list(Secondary.t)) =>
    ws
    |> List.map((w: Secondary.t) =>
         {
           ...w,
           id: Id.mk(),
         }
       );
  let (before, after) = a.secondary;
  {
    /* fresh id PER id — the tail carries real structure (a Match's
       rule-tile ids live there); truncating to one id starved the
       printer into deriving rule ids from Id.invalid, which is
       base-independent — every starved case in the program got THE
       SAME derived ids (duplicate |=> tiles: glommed indication,
       caret jumping to the wrong twin) */
    ids: a.ids == [] ? [Id.mk()] : a.ids |> List.map(_ => Id.mk()),
    secondary: (refresh_sec(before), refresh_sec(after)),
    incomplete: [],
    lexeme: a.lexeme,
  };
};

let refresh_typ_ids = (t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=
      (cont, t: Typ.t) =>
        cont({
          ...t,
          annotation: refresh_annotation(t.annotation),
        }),
    t,
  );

let fresh_typ = (term): Typ.t => {
  annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
  term,
};

let typ_known = (t: Typ.t): bool => {
  let unknown = ref(false);
  let _ =
    Typ.map_term(
      ~f_typ=
        (cont, t: Typ.t) => {
          switch (IdTagged.term_of(t)) {
          | Unknown(_) => unknown := true
          | _ => ()
          };
          cont(t);
        },
      t,
    );
  ! unknown^;
};

let drop_secondary_typ = (ids: list(Id.t), t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=
      (cont, t: Typ.t) => {
        let keep = (ws: list(Secondary.t)) =>
          ws |> List.filter((w: Secondary.t) => !List.mem(w.id, ids));
        let (before, after) = t.annotation.secondary;
        {
          ...t,
          annotation: {
            ...t.annotation,
            secondary: (keep(before), keep(after)),
          },
        }
        |> cont;
      },
    t,
  );

let strip_typ_boundaries = (t: Typ.t): Typ.t => {
  let seg = ExpToSegment.typ_to_segment(~settings=roundtrip_settings, t);
  let ids = ws => ws |> List.map((w: Secondary.t) => w.id);
  let lead = secondary_run_pieces(seg) |> ids;
  let trail = secondary_run_pieces(List.rev(seg)) |> ids;
  drop_secondary_typ(lead @ trail, t);
};

/* pat-side Slot ops: a compound pat (e.g. Some(x)) stores its lead
   on its first LEAF, so node-level secondary exchange stacks the
   slot's run on top of the traveling one — read/drop textually */
let drop_secondary_pat = (ids: list(Id.t), p: Pat.t): Pat.t =>
  Pat.map_term(
    ~f_pat=
      (cont, p: Pat.t) => {
        let keep = (ws: list(Secondary.t)) =>
          ws |> List.filter((w: Secondary.t) => !List.mem(w.id, ids));
        let (before, after) = p.annotation.secondary;
        {
          ...p,
          annotation: {
            ...p.annotation,
            secondary: (keep(before), keep(after)),
          },
        }
        |> cont;
      },
    p,
  );

let pat_slot = (p: Pat.t): Slot.t => {
  let seg = ExpToSegment.pat_to_segment(~settings=roundtrip_settings, p);
  {
    lead: secondary_run_pieces(seg),
    trail: List.rev(secondary_run_pieces(List.rev(seg))),
  };
};
let pat_slot_drop = (s: Slot.t, p: Pat.t): Pat.t =>
  drop_secondary_pat(
    List.map((w: Secondary.t) => w.id, s.lead @ s.trail),
    p,
  );
let pat_slot_give = (s: Slot.t, p: Pat.t): Pat.t => {
  let (b, a) = p.annotation.secondary;
  {
    ...p,
    annotation: {
      ...p.annotation,
      secondary: (s.lead @ b, a @ s.trail),
    },
  };
};
let pat_slot_lead = (p: Pat.t): Slot.t => {
  ...pat_slot(p),
  trail: [],
};

let typ_slot = (t: Typ.t): Slot.t => {
  let seg = ExpToSegment.typ_to_segment(~settings=roundtrip_settings, t);
  {
    lead: secondary_run_pieces(seg),
    trail: List.rev(secondary_run_pieces(List.rev(seg))),
  };
};
let typ_slot_lead = (t: Typ.t): Slot.t => {
  ...typ_slot(t),
  trail: [],
};
let typ_slot_drop = (s: Slot.t, t: Typ.t): Typ.t =>
  drop_secondary_typ(
    List.map((w: Secondary.t) => w.id, s.lead @ s.trail),
    t,
  );
let typ_slot_give = (s: Slot.t, t: Typ.t): Typ.t => {
  let (b, a) = t.annotation.secondary;
  {
    ...t,
    annotation: {
      ...t.annotation,
      secondary: (s.lead @ b, a @ s.trail),
    },
  };
};

/* The inserted copy takes over the replaced occurrence's stored
 * whitespace (its slot in the line); the definition keeps its own
 * interior spacing */
/* The copy's root adopts the replaced occurrence's ids: the
 * occurrence id stays valid post-substitution (unique — the original
 * node is gone), giving inline a stable focus target */
/* keep_ids: the copy keeps the def's ids (P7 identity — the def IS
   the moved construct when the binding dissolves, so it must keep
   its identity for animation continuity); default adopts the
   occurrence's ids (multi-copy inline needs per-site identity). */
let inserted =
    (~parens: bool, ~keep_ids: bool=false, def: Exp.t, at: Exp.t): Exp.t => {
  let secondary = at.annotation.secondary;
  let def = strip_boundaries(def);
  if (parens) {
    {
      annotation: {
        ...
          IdTagged.IdTag.mk_internal(
            keep_ids ? [Id.mk()] : at.annotation.ids,
          ),
        secondary,
      },
      term: Parens(def),
    };
  } else {
    let (before, after) = secondary;
    let (def_before, def_after) = def.annotation.secondary;
    {
      ...def,
      annotation: {
        ...def.annotation,
        ids: keep_ids ? def.annotation.ids : at.annotation.ids,
        secondary: (def_before @ before, def_after @ after),
      },
    };
  };
};

let var_pat_name = (p: Pat.t): option(string) =>
  switch (IdTagged.term_of(p)) {
  | Var(name) => Some(name)
  | _ => None
  };

/* binder name through an annotation */
let rec let_head_name = (p: Pat.t): option(string) =>
  switch (IdTagged.term_of(p)) {
  | Var(y) => Some(y)
  | Asc(inner, _) => let_head_name(inner)
  | _ => None
  };

/* the head binder var node: through annotations, and through the
 * f(x)-sugar Ap to the fn name */
let rec head_var_pat = (p: Pat.t): option(Pat.t) =>
  switch (IdTagged.term_of(p)) {
  | Var(_) => Some(p)
  | Asc(inner, _) => head_var_pat(inner)
  | Ap(fv, _) => head_var_pat(fv)
  | _ => None
  };

/* collect over every subnode: gather what `f` yields at each, in
   pre-order. The building block under the family of id/name walks. */
let collect_exp = (f: Exp.t => list('a), e: Exp.t): list('a) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e': Exp.t) => {
          acc := f(e') @ acc^;
          cont(e');
        },
      e,
    );
  acc^;
};
let collect_pat = (f: Pat.t => list('a), p: Pat.t): list('a) => {
  let acc = ref([]);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p': Pat.t) => {
          acc := f(p') @ acc^;
          cont(p');
        },
      p,
    );
  acc^;
};

let pat_var_names = (p: Pat.t): list(string) =>
  p
  |> collect_pat(p' =>
       switch (IdTagged.term_of(p')) {
       | Var(x) => [x]
       | _ => []
       }
     );

let tpat_names = (tp: TPat.t): list(string) =>
  switch (IdTagged.term_of(tp)) {
  | Var(name) => [name]
  | _ => []
  };

/* type names mentioned in any type position of e (ascriptions, pat
 * annotations, tyalias defs) — the capture check for moves across a
 * type binder */
let mentions_typ_names = (names: list(string), e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_typ=
        (cont, t: Typ.t) => {
          switch (IdTagged.term_of(t)) {
          | Var(y) when List.mem(y, names) => found := true
          | _ => ()
          };
          cont(t);
        },
      e,
    );
  found^;
};

/* Does the pattern bind this name? (occurrences under such binders are
 * shadowed and must not be substituted) */
let binds = (x: string, p: Pat.t): bool => {
  let found = ref(false);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p: Pat.t) => {
          switch (IdTagged.term_of(p)) {
          | Var(y) when y == x => found := true
          | _ => ()
          };
          cont(p);
        },
      p,
    );
  found^;
};

let children_of = (e: Exp.t): list(Exp.t) => {
  let acc = ref([]);
  let entered = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e': Exp.t) =>
          if (entered^) {
            acc := [e', ...acc^];
            e';
          } else {
            entered := true;
            cont(e');
          },
      e,
    );
  acc^;
};

/* does x occur FREE in e (an occurrence under a rebinding of x
 * doesn't count)? Movement gates use this rather than raw mentions so
 * shadowed reuses of a name don't block legal moves. */
let rec free_in = (x: string, e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(y) => y == x
  | Let(p, d, body) => free_in(x, d) || !binds(x, p) && free_in(x, body)
  | Fun(p, body, _, _)
  | FixF(p, body, _) => binds(x, p) ? false : free_in(x, body)
  | Match(scrut, rules) =>
    free_in(x, scrut)
    || rules
    |> List.exists(((p, b)) => !binds(x, p) && free_in(x, b))
  | _ => children_of(e) |> List.exists(free_in(x))
  };

let sugar_fn_name = (p: Pat.t): option(string) => {
  let rec go = (p: Pat.t) =>
    switch (IdTagged.term_of(p)) {
    | Ap(fv, _) => var_pat_name(fv)
    | Asc(inner, _) => go(inner)
    | _ => None
    };
  go(p);
};

/* every var/pat-var name in the program (over-approximates scope) */
let used_names = (program: Exp.t): list(string) => {
  let names = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | Var(x) => names := [x, ...names^]
          | _ => ()
          };
          cont(e);
        },
      ~f_pat=
        (cont, p: Pat.t) => {
          switch (IdTagged.term_of(p)) {
          | Var(x) => names := [x, ...names^]
          | _ => ()
          };
          cont(p);
        },
      program,
    );
  names^;
};

/* minted binder names come from the nonsense pool (obviously
   placeholders — see PlaceholderNames.re); numbered fallback only
   when the pool is exhausted */
let pick_placeholder = (~transform=x => x, used: list(string)): string => {
  /* deterministic variety: rotate the pool by a hash of the names
     in scope — the same program state always mints the same name
     (tests, undo, replay stay stable) but different programs start
     elsewhere in the pool */
  let pool = PlaceholderNames.pool;
  let n = List.length(pool);
  let start = n == 0 ? 0 : Hashtbl.hash(used) mod n;
  let rotated =
    List.init(n, i => transform(List.nth(pool, (start + i) mod n)));
  switch (rotated |> List.find_opt(c => !List.mem(c, used))) {
  | Some(c) => c
  | None =>
    let rec pick = k => {
      let cand = transform("x" ++ string_of_int(k));
      List.mem(cand, used) ? pick(k + 1) : cand;
    };
    pick(1);
  };
};

let fresh_name = (program: Exp.t): string =>
  pick_placeholder(used_names(program));

let rename_pat_var = (y: string, y': string, p: Pat.t): Pat.t =>
  Pat.map_term(
    ~f_pat=
      (cont, p': Pat.t) =>
        switch (IdTagged.term_of(p')) {
        | Var(z) when z == y => {
            ...p',
            term: Var(y'),
          }
        | _ => cont(p')
        },
    p,
  );

/* One shadow-aware traversal: descends everything except scopes that
 * rebind ~skip; ~f_var fires on every Var node (hook filters names),
 * ~f_ap on forward applications before generic descent (None falls
 * through). patch_calls/swap_call_args/rename_* are instances; subst
 * stays bespoke (it also freshens colliding binders). */
let rec map_unshadowed =
        (
          ~skip: string,
          ~f_var: Exp.t => option(Exp.t),
          ~f_ap: (Exp.t, Exp.t, Exp.t, Exp.t => Exp.t) => option(Exp.t)=(
                                                                    _,
                                                                    _,
                                                                    _,
                                                                    _,
                                                                    ) =>
                                                                    None,
          e: Exp.t,
        )
        : Exp.t => {
  let go = map_unshadowed(~skip, ~f_var, ~f_ap);
  let descend = e =>
    Exp.map_term(
      ~f_exp={
        let entered = ref(false);
        (cont, e': Exp.t) =>
          if (entered^) {
            go(e');
          } else {
            entered := true;
            cont(e');
          };
      },
      e,
    );
  let (term, rewrap) = Exp.unwrap(e);
  switch (term) {
  | Var(_) => f_var(e) |> Option.value(~default=e)
  | Ap(Forward, fn, arg) =>
    switch (f_ap(e, fn, arg, go)) {
    | Some(e') => e'
    | None => descend(e)
    }
  | Let(p, d, body) =>
    rewrap(Let(p, go(d), binds(skip, p) ? body : go(body)))
  | Fun(p, body, t, n) when binds(skip, p) => rewrap(Fun(p, body, t, n))
  | FixF(p, body, env) when binds(skip, p) => rewrap(FixF(p, body, env))
  | Match(scrut, rules) =>
    rewrap(
      Match(
        go(scrut),
        rules
        |> List.map(((p, body)) => (p, binds(skip, p) ? body : go(body))),
      ),
    )
  | _ => descend(e)
  };
};

/* rename occurrences of y bound at this scope (skipping inner
 * rebindings of y) */
let rename_syntactic = (y: string, y': string, e: Exp.t): Exp.t =>
  map_unshadowed(
    ~skip=y,
    ~f_var=
      e' =>
        switch (IdTagged.term_of(e')) {
        | Var(z) when z == y =>
          Some({
            ...e',
            term: Var(y'),
          })
        | _ => None
        },
    e,
  );

let refresh_ids = (e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) =>
        cont({
          ...e,
          annotation: refresh_annotation(e.annotation),
        }),
    ~f_pat=
      (cont, p: Pat.t) =>
        cont({
          ...p,
          annotation: refresh_annotation(p.annotation),
        }),
    ~f_typ=
      (cont, t: Typ.t) =>
        cont({
          ...t,
          annotation: refresh_annotation(t.annotation),
        }),
    e,
  );

/* Shadow-aware, capture-avoiding substitution of def for x, preserving
 * each occurrence's whitespace slot. Binders that would capture a free
 * variable of def (~avoid) are renamed to a fresh name first. */
let rec subst =
        (
          ~parens_for: Exp.t => bool,
          ~avoid: list(string),
          ~used: ref(list(string)),
          ~first: ref(bool),
          x: string,
          def: Exp.t,
          e: Exp.t,
        )
        : Exp.t => {
  let go = subst(~parens_for, ~avoid, ~used, ~first, x, def);
  /* rename p's avoid-colliding binders in p + the given scopes */
  let freshen = (p: Pat.t, scopes: list(Exp.t)): (Pat.t, list(Exp.t)) =>
    pat_var_names(p)
    |> List.sort_uniq(compare)
    |> List.filter(y => List.mem(y, avoid))
    |> List.fold_left(
         ((p, scopes), y) => {
           let rec pick = n => {
             let c = y ++ string_of_int(n);
             List.mem(c, used^) ? pick(n + 1) : c;
           };
           let y' = pick(1);
           used := [y', ...used^];
           (
             rename_pat_var(y, y', p),
             scopes |> List.map(rename_syntactic(y, y')),
           );
         },
         (p, scopes),
       );
  let (term, rewrap) = Exp.unwrap(e);
  switch (term) {
  | Var(y) when y == x =>
    /* the FIRST copy carries the def's ids INCLUDING its root (it
       travels, and focus follows it — P7); later copies are FULLY
       fresh, root included — occurrence-root adoption made them
       pair as MOVES from the occurrence (an invisible shift) instead
       of ENTERED clones flying from the def (the fan-out), and left
       subtree duplicates for dedupe to heal besides; fresh copies
       also shed prose (comments live once, on the traveling copy) */
    let is_first = first^;
    first := false;
    let d = is_first ? def : strip_comments(refresh_ids(def));
    inserted(~parens=parens_for(e), ~keep_ids=true, d, e);
  | Let(p, d, body) =>
    let recursive =
      switch (IdTagged.term_of(d)) {
      | Fun(_) => true
      | _ => false
      };
    if (binds(x, p)) {
      /* x shadowed in body; also in a recursive def */
      rewrap(
        Let(p, recursive ? d : go(d), body),
      );
    } else {
      switch (freshen(p, recursive ? [d, body] : [body])) {
      | (p', [d', body']) => rewrap(Let(p', go(d'), go(body')))
      | (p', [body']) => rewrap(Let(p', go(d), go(body')))
      | _ => e
      };
    };
  | Fun(p, body, t, n) when binds(x, p) => rewrap(Fun(p, body, t, n))
  | Fun(p, body, t, n) =>
    switch (freshen(p, [body])) {
    | (p', [body']) => rewrap(Fun(p', go(body'), t, n))
    | _ => e
    }
  | FixF(p, body, env) when binds(x, p) => rewrap(FixF(p, body, env))
  | FixF(p, body, env) =>
    switch (freshen(p, [body])) {
    | (p', [body']) => rewrap(FixF(p', go(body'), env))
    | _ => e
    }
  | Match(scrut, rules) =>
    rewrap(
      Match(
        go(scrut),
        rules
        |> List.map(((p, body)) =>
             if (binds(x, p)) {
               (p, body);
             } else {
               switch (freshen(p, [body])) {
               | (p', [body']) => (p', go(body'))
               | _ => (p, body)
               };
             }
           ),
      ),
    )
  | _ =>
    /* one level of generic descent: map each direct child, without
       re-entering this node (map_term's f is called on the node
       itself first, so guard on identity once) */
    Exp.map_term(
      ~f_exp={
        let entered = ref(false);
        (cont, e': Exp.t) =>
          if (entered^) {
            go(e');
          } else {
            entered := true;
            cont(e');
          };
      },
      e,
    )
  };
};

let vars_of = (e: Exp.t): list(string) =>
  e
  |> collect_exp(e' =>
       switch (IdTagged.term_of(e')) {
       | Var(z) => [z]
       | _ => []
       }
     )
  |> List.sort_uniq(compare);

/* Every substituted occurrence carries the def's ids; re-id all but
 * one so the buffer never contains duplicates (one copy keeps the
 * originals, so probes on the definition follow it somewhere) */
/* Fresh ids for every node AND secondary piece, keeping whitespace
 * content and lexemes (Exp.replace_all_ids drops secondary, which
 * would strip a duplicated copy's spacing) */

let refresh_pat_ids = (p: Pat.t): Pat.t =>
  Pat.map_term(
    ~f_pat=
      (cont, p: Pat.t) =>
        cont({
          ...p,
          annotation: refresh_annotation(p.annotation),
        }),
    ~f_typ=
      (cont, t: Typ.t) =>
        cont({
          ...t,
          annotation: refresh_annotation(t.annotation),
        }),
    p,
  );

/* healing counter: dedupe re-mints SILENTLY, which converts travel
   into rebirth (animation identity loss) without failing anything —
   tests reset this and assert it stayed zero, so a dupe-introducing
   transform is caught even though its output looks clean */
let dedupe_healed = ref(0);

let dedupe_ids = (e: Exp.t): Exp.t => {
  let seen = ref(Id.Map.empty);
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let ids = IdTagged.ids(e);
        if (List.exists(id => Id.Map.mem(id, seen^), ids)) {
          dedupe_healed := dedupe_healed^ + 1;
          refresh_ids(e);
        } else {
          List.iter(id => seen := Id.Map.add(id, (), seen^), ids);
          cont(e);
        };
      },
    e,
  );
};

/* all node ids within an expression subtree */
let exp_subtree_ids = (e: Exp.t): list(Id.t) =>
  collect_exp(IdTagged.ids, e);

/* A let's target zone is its delimiters plus its pattern (not def or
 * body: those are their own expressions with their own menus) */
let pat_subtree_ids = (p: Pat.t): list(Id.t) => {
  let acc = ref([]);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p: Pat.t) => {
          acc := IdTagged.ids(p) @ acc^;
          cont(p);
        },
      ~f_typ=
        (cont, t: Typ.t) => {
          acc := IdTagged.ids(t) @ acc^;
          cont(t);
        },
      p,
    );
  acc^;
};

/* If target indicates a variable occurrence, the id of its binder
 * (nearest, shadow-correct via ctx lookup) */
let binder_of_occurrence =
    (~info_map: Statics.Map.t, ~target: Id.t, program: Exp.t): option(Id.t) => {
  let occ = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | Var(name) when List.mem(target, IdTagged.ids(e)) =>
            occ := Some((name, Exp.rep_id(e)))
          | _ => ()
          };
          cont(e);
        },
      program,
    );
  switch (occ^) {
  | Some((name, id)) =>
    switch (Id.Map.find_opt(id, info_map)) {
    | Some(InfoExp({ctx, _})) =>
      Ctx.lookup_var(ctx, name)
      |> Option.map((entry: Ctx.var_entry) => entry.id)
    | _ => None
    }
  | None => None
  };
};

/* Oracle for necessary parenthesization: does the transformed program
 * survive print -> reparse unchanged? (structural printing adds no
 * defensive parens, so extent/precedence problems show up as a
 * different reparse) */
let reparses_same = (term: Exp.t): bool => {
  let seg =
    ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term)
    |> SpaceNormalize.go;
  let text = Printer.of_segment(~holes="?", ~refractors=[], seg);
  switch (Parser.to_segment(text, ~root=Exp)) {
  | None => false
  | Some(seg2) => Exp.fast_equal(MakeTerm.go(seg2).term, term)
  };
};

/* === Registry ===
 * A refactoring is one record; menus and dispatch iterate the
 * registry, so adding a transform means adding a kind + an entry. */

type impl = {
  label: string,
  tooltip: string,
  /* Some((program', focus_id)) when applicable at `target` */
  prepare:
    (~info_map: Statics.Map.t, ~target: Id.t, Exp.t) => option((Exp.t, Id.t)),
};

/* Replace a node (matched by ~hit) via ~rewrite; the replacement
 * takes over the node's whitespace slot */
let rewrite_node =
    (
      ~hit: Exp.t => bool,
      ~rewrite: Exp.t => option((Exp.t, Id.t)),
      program: Exp.t,
    )
    : option((Exp.t, Id.t)) => {
  let focus = ref(None);
  let program' =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) =>
          if (focus^ == None && hit(e)) {
            switch (rewrite(e)) {
            | Some((result, f)) =>
              focus := Some(f);
              Slot.takeover(~of_=e, result);
            | None => cont(e)
            };
          } else {
            cont(e);
          },
      program,
    );
  focus^ |> Option.map(f => (dedupe_ids(program'), f));
};

let hit_node = (target: Id.t, e: Exp.t): bool =>
  List.mem(target, IdTagged.ids(e));

/* a let is targetable at its delimiters or its pattern */
let hit_let = (target: Id.t, e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Let(p, _, _) =>
    hit_node(target, e) || List.mem(target, pat_subtree_ids(p))
  | _ => false
  };

/* a type line is targetable at its delimiters or its type pattern
   (mirrors hit_let: the def side is not a line affordance) */
let hit_tyalias = (target: Id.t, e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | TyAlias(tp, _, _) =>
    hit_node(target, e) || List.mem(target, IdTagged.ids(tp))
  | _ => false
  };

/* definition line items: lets and type aliases move through the
   same flows (andrew: one definition flow) */
let hit_def_line = (target: Id.t, e: Exp.t): bool =>
  hit_let(target, e) || hit_tyalias(target, e);

let hit_fun = (target: Id.t, e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Fun(p, _, _, _) =>
    hit_node(target, e) || List.mem(target, pat_subtree_ids(p))
  | _ => false
  };

let find_hit = (~hit: Exp.t => bool, program: Exp.t): option(Exp.t) => {
  let found = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) =>
          if (found^ == None && hit(e)) {
            found := Some(e);
            e;
          } else {
            cont(e);
          },
      program,
    );
  found^;
};

/* an arm's pattern targets that arm (never the case delimiters:
 * which arm would they mean?) */
let hit_match_pat = (target: Id.t, e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Match(_, rules) =>
    rules |> List.exists(((p, _)) => List.mem(target, pat_subtree_ids(p)))
  | _ => false
  };

/* Replace a Let with a rewrite of its parts */
let rewrite_let =
    (
      ~target: Id.t,
      ~matches: (Pat.t, Exp.t, Exp.t) => bool,
      ~rewrite: (Pat.t, Exp.t, Exp.t) => (Exp.t, Id.t),
      program: Exp.t,
    )
    : option((Exp.t, Id.t)) =>
  rewrite_node(
    ~hit=hit_let(target),
    ~rewrite=
      e =>
        switch (IdTagged.term_of(e)) {
        | Let(p, def, body) when matches(p, def, body) =>
          let (result, f) = rewrite(p, def, body);
          Some((strip_leading_ws(result), f));
        | _ => None
        },
    program,
  );

/* root-to-node path to the first node matched by ~hit */
let rec find_path = (~hit: Exp.t => bool, e: Exp.t): option(list(Exp.t)) =>
  if (hit(e)) {
    Some([e]);
  } else {
    children_of(e)
    |> List.find_map(c => find_path(~hit, c))
    |> Option.map(rest => [e, ...rest]);
  };

let same_node = (a: Exp.t, b: Exp.t): bool =>
  Exp.rep_id(a) == Exp.rep_id(b);

let replace_node = (~at: Id.t, ~with_: Exp.t, e: Exp.t): Exp.t => {
  let done_ = ref(false);
  Exp.map_term(
    ~f_exp=
      (cont, e': Exp.t) =>
        if (! done_^ && Exp.rep_id(e') == at) {
          done_ := true;
          with_;
        } else {
          cont(e');
        },
    e,
  );
};

/* unshadowed occurrences of x (the nodes themselves) */
let occurrences_of = (x: string, e: Exp.t): list(Exp.t) => {
  let acc = ref([]);
  let _ =
    map_unshadowed(
      ~skip=x,
      ~f_var=
        e' => {
          switch (IdTagged.term_of(e')) {
          | Var(z) when z == x => acc := [e', ...acc^]
          | _ => ()
          };
          None;
        },
      e,
    );
  acc^;
};

/* The smallest delimiter-bounded region containing the node: a slot
 * whose extent is closed by its parent tile's shards (a def, parens
 * or ap-arg interior, a then-branch, a scrutinee, a list element).
 * Extent effects cannot cross shards, so a reparse check of this
 * region alone is sound — and cheap, unlike reparsing the program. */
let bounded_region = (occ_id: Id.t, program: Exp.t): Exp.t => {
  let bounded = (parent: Exp.t, child: Exp.t): bool =>
    switch (IdTagged.term_of(parent)) {
    | Let(_, d, _) => same_node(d, child)
    | Parens(inner) => same_node(inner, child)
    | Ap(Forward, _, arg) => same_node(arg, child)
    | If(_, t, _) => same_node(t, child)
    | Match(scrut, _) => same_node(scrut, child)
    | ListLit(items) => items |> List.exists(it => same_node(it, child))
    | _ => false
    };
  switch (find_path(~hit=e => Exp.rep_id(e) == occ_id, program)) {
  | None => program
  | Some(path) =>
    let rec lowest = (region: Exp.t, path: list(Exp.t)): Exp.t =>
      switch (path) {
      | [parent, child, ...rest] =>
        lowest(bounded(parent, child) ? child : region, [child, ...rest])
      | _ => region
      };
    lowest(program, path);
  };
};

let typ_names_mentioned = (e: Exp.t): list(string) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_typ=
        (cont, t: Typ.t) => {
          switch (IdTagged.term_of(t)) {
          | Var(y) => acc := [y, ...acc^]
          | _ => ()
          };
          cont(t);
        },
      e,
    );
  List.sort_uniq(compare, acc^);
};

/* capture by a crossed `use` only — the refusal that remains after
   alias-capture was upgraded to freshening (imports can't be
   enumerated, so they can't be renamed around) */
let typ_captured_by_use_at = (occ_id: Id.t, moved: Exp.t, body: Exp.t): bool =>
  typ_names_mentioned(moved) == []
    ? false
    : (
      switch (find_path(~hit=e => Exp.rep_id(e) == occ_id, body)) {
      | None => false
      | Some(path) =>
        path
        |> List.exists((e: Exp.t) =>
             switch (IdTagged.term_of(e)) {
             | Use(_) => true
             | _ => false
             }
           )
      }
    );

/* rename type name x to y throughout e's typ positions, respecting
   rebinding (an inner binder of x ends the rename; a rebinding
   tyalias's own def still sees the outer x). Renamed tokens keep
   their ids (P7); lexeme cleared so the printer respells. */
let rename_typ_var = (x: string, y: string, e: Exp.t): Exp.t => {
  /* stop under rebinders of x (not ours) AND of y (renamed
     occurrences there would capture); a rebinding tyalias's own
     def still sees the outer names */
  let stops = tp =>
    List.mem(x, tpat_names(tp)) || List.mem(y, tpat_names(tp));
  let in_typ = (ty: Typ.t): Typ.t =>
    Typ.map_term(
      ~f_typ=
        (cont, t: Typ.t) =>
          switch (IdTagged.term_of(t)) {
          | Var(z) when z == x => {
              annotation: {
                ...t.annotation,
                lexeme: None,
              },
              term: Var(y),
            }
          | Rec(tp, _)
          | Poly(tp, _) when stops(tp) => t
          | _ => cont(t)
          },
      ty,
    );
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) =>
        switch (IdTagged.term_of(e)) {
        | TyAlias(tp, d, b) when stops(tp) => {
            ...e,
            term: TyAlias(tp, in_typ(d), b),
          }
        | TypFun(tp, _, _) when stops(tp) => e
        | _ => cont(e)
        },
    ~f_typ=(_cont, ty: Typ.t) => in_typ(ty),
    e,
  );
};

/* is this type name mentioned FREELY in e (some occurrence not
   under a rebinder)? Same pruned walk as rename_typ_var. */
let mentions_typ_free = (x: string, e: Exp.t): bool => {
  let found = ref(false);
  let in_typ = (ty: Typ.t): Typ.t =>
    Typ.map_term(
      ~f_typ=
        (cont, t: Typ.t) =>
          switch (IdTagged.term_of(t)) {
          | Var(z) when z == x =>
            found := true;
            t;
          | Rec(tp, _)
          | Poly(tp, _) when List.mem(x, tpat_names(tp)) => t
          | _ => cont(t)
          },
      ty,
    );
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) =>
          switch (IdTagged.term_of(e)) {
          | TyAlias(tp, d, b) when List.mem(x, tpat_names(tp)) =>
            let _ = in_typ(d);
            {
              ...e,
              term: TyAlias(tp, d, b),
            };
          | TypFun(tp, _, _) when List.mem(x, tpat_names(tp)) => e
          | _ => cont(e)
          },
      ~f_typ=(_cont, ty: Typ.t) => in_typ(ty),
      e,
    );
  found^;
};

/* all type names visible in e (mentioned or bound) — the avoid set
   for freshening */
let typ_names_in = (e: Exp.t): list(string) => {
  let acc = ref(typ_names_mentioned(e));
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | TyAlias(tp, _, _)
          | TypFun(tp, _, _) => acc := tpat_names(tp) @ acc^
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  List.sort_uniq(compare, acc^);
};

/* rename the alias BOUND at this TyAlias node to y: the binder
   token morphs in place (ids kept) and its scope's uses follow */
let rename_alias_binding = (~alias_id: Id.t, y: string, e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) =>
        if (IdTagged.rep_id(e) == alias_id) {
          switch (IdTagged.term_of(e)) {
          | TyAlias(tp, d, b) =>
            switch (tpat_names(tp)) {
            | [x] =>
              let tp': TPat.t = {
                annotation: {
                  ...tp.annotation,
                  lexeme: None,
                },
                term: Var(y),
              };
              {
                ...e,
                term: TyAlias(tp', d, rename_typ_var(x, y, b)),
              };
            | _ => cont(e)
            }
          | _ => cont(e)
          };
        } else {
          cont(e);
        },
    e,
  );

/* Crossed aliases that would capture the moved material's type
   names at an occurrence of x are FRESHENED (t -> t1, binder +
   scope) — mirroring the exp-var rename strategy. This is the
   incidental half of alias capture; ESCAPES (a reference moving
   above its own binder, the hoist direction) stay refusals. */
let freshen_crossed_aliases = (~x: string, ~moved: Exp.t, body: Exp.t): Exp.t => {
  let mentioned = typ_names_mentioned(moved);
  if (mentioned == []) {
    body;
  } else {
    let victims = ref([]);
    let _ =
      Exp.map_term(
        ~f_exp=
          (cont, e: Exp.t) => {
            switch (IdTagged.term_of(e)) {
            | TyAlias(tp, _, b)
                when
                  tpat_names(tp)
                  |> List.exists(n => List.mem(n, mentioned))
                  && occurrences_of(x, b) != [] =>
              switch (tpat_names(tp)) {
              | [n] => victims := [(IdTagged.rep_id(e), n), ...victims^]
              | _ => ()
              }
            | _ => ()
            };
            cont(e);
          },
        body,
      );
    let used = ref(typ_names_in(body) @ mentioned);
    victims^
    |> List.fold_left(
         (acc, (id, n)) => {
           let rec pick = k => {
             let c = n ++ string_of_int(k);
             List.mem(c, used^) ? pick(k + 1) : c;
           };
           let y = pick(1);
           used := [y, ...used^];
           rename_alias_binding(~alias_id=id, y, acc);
         },
         body,
       );
  };
};

/* === Static splice parenthesization ===
   The parser molds by precedence, so whether a spliced term needs
   parens is decidable from the same tables — no printing, no
   reparse. Two-sided:

   EXPOSURE (per side of the spliced term d): the loosest precedence
   at which d's structure is exposed to capture on that side; None
   when that edge is delimited (keyword, parens, case..end).

   BOUND (per side of the target slot): each operator that will sit
   adjacent on that side in the print, found by walking up the
   ancestor fringe (a child on its parent's left fringe adjoins, on
   its left, the operator of the first ancestor where the fringe
   breaks); a delimiter edge ends the walk.

   Parens are needed iff some bound CAPTURES that side's exposure:
   bound tighter than exposure, or equal without associativity on
   d's side. Anything not understood (MultiHole, Invalid, modules)
   exposes at Precedence.min => conservative parens. Reparse
   identity remains the TEST oracle (reparse-safety + fuzz). */

/* looser = higher int (Precedence: higher precedence = lower int) */
let sp_loosest = (a: option(Precedence.t), b: option(Precedence.t)) =>
  switch (a, b) {
  | (None, x)
  | (x, None) => x
  | (Some(a), Some(b)) => Some(a > b ? a : b)
  };

/* per-side exposure of a term: None = that edge is delimited */
let rec sp_exposure =
        (~side: Util.Direction.t, e: Exp.t): option(Precedence.t) => {
  let root = ExpToSegment.external_precedence(e);
  let open_child: option(Exp.t) =
    switch (IdTagged.term_of(e), side) {
    /* both edges are operands */
    | (BinOp(_, l, _), Util.Direction.Left)
    | (Cons(l, _), Util.Direction.Left)
    | (ListConcat(l, _), Util.Direction.Left)
    | (Seq(l, _), Util.Direction.Left)
    | (TupleExtension(l, _), Util.Direction.Left)
    | (Asc(l, _), Util.Direction.Left)
    | (Dot(l, _), Util.Direction.Left) => Some(l)
    | (BinOp(_, _, r), Util.Direction.Right)
    | (Cons(_, r), Util.Direction.Right)
    | (ListConcat(_, r), Util.Direction.Right)
    | (Seq(_, r), Util.Direction.Right)
    | (TupleExtension(_, r), Util.Direction.Right) => Some(r)
    /* tuple: first/last element */
    | (Tuple([x, ..._]), Util.Direction.Left) => Some(x)
    | (Tuple(xs), Util.Direction.Right) when xs != [] =>
      Some(List.nth(xs, List.length(xs) - 1))
    /* ap: fn side open, arg side closed by its parens */
    | (Ap(Forward, f, _), Util.Direction.Left) => Some(f)
    | (Ap(Forward, _, _), Util.Direction.Right) => None
    | (Ap(Reverse, l, _), Util.Direction.Left) => Some(l)
    | (Ap(Reverse, _, r), Util.Direction.Right) => Some(r)
    /* right-open keyword forms: left edge is the keyword */
    | (Let(_, _, b), Util.Direction.Right)
    | (TyAlias(_, _, b), Util.Direction.Right)
    | (Use(_, b), Util.Direction.Right)
    | (Theorem(_, _, b), Util.Direction.Right)
    | (Fun(_, b, _, _), Util.Direction.Right)
    | (TypFun(_, b, _), Util.Direction.Right)
    | (FixF(_, b, _), Util.Direction.Right)
    | (Forall(_, b), Util.Direction.Right)
    | (If(_, _, b), Util.Direction.Right)
    | (UnOp(_, b), Util.Direction.Right)
    | (Filter(_, b), Util.Direction.Right) => Some(b)
    | (Let(_), Util.Direction.Left)
    | (TyAlias(_), Util.Direction.Left)
    | (Use(_), Util.Direction.Left)
    | (Theorem(_), Util.Direction.Left)
    | (Fun(_), Util.Direction.Left)
    | (TypFun(_), Util.Direction.Left)
    | (FixF(_), Util.Direction.Left)
    | (Forall(_), Util.Direction.Left)
    | (If(_), Util.Direction.Left)
    | (UnOp(_), Util.Direction.Left)
    | (Filter(_), Util.Direction.Left) => None
    /* Asc right side is a TYPE: expose at the asc level itself
       (type-side capture, e.g. `1 : Int , Bool`, is guarded by the
       root exposure; type operators looser than comma don't exist
       in exp contexts) */
    | (Asc(_), Util.Direction.Right) => None
    | (Dot(_), Util.Direction.Right) => None
    | (TupLabel(_, x), Util.Direction.Right) => Some(x)
    | (TupLabel(_), Util.Direction.Left) => None
    | _ => None
    };
  let self =
    switch (IdTagged.term_of(e)) {
    /* delimited-both forms expose nothing themselves */
    | Var(_)
    | Atom(_)
    | EmptyHole
    | Constructor(_)
    | Label(_)
    | BuiltinFun(_)
    | Undefined
    | Deferral(_)
    | LivelitName(_)
    | Parens(_)
    | ListLit(_)
    | Test(_)
    | HintedTest(_)
    | Match(_)
    | Tuple([]) => None
    /* keyword forms: delimited on the left only */
    | Let(_)
    | TyAlias(_)
    | Use(_)
    | Theorem(_)
    | Fun(_)
    | TypFun(_)
    | FixF(_)
    | Forall(_)
    | If(_)
    | UnOp(_)
    | Filter(_) => side == Left ? None : Some(root)
    /* ap exposes fn-side only */
    | Ap(Forward, _, _) => side == Left ? Some(root) : None
    | _ => Some(root)
    };
  let deeper =
    switch (open_child) {
    | Some(c) => sp_exposure(~side, c)
    | None => None
    };
  sp_loosest(self, deeper);
};

/* the slot's adjacent-operator bounds on one side: walk up from the
   spliced position; each (parent, child) step either ends at a
   delimiter, contributes the parent operator's bound, or continues
   up an open fringe. Bounds carry which side of THAT OPERATOR the
   spliced material sits on (for the associativity tie-break). */
type sp_bound = {
  prec: Precedence.t,
  /* d sits on this side of the adjacent operator */
  d_side: Util.Direction.t,
};

let sp_bounds = (~side: Util.Direction.t, path: list(Exp.t)): list(sp_bound) => {
  /* innermost-first pairs: (parent, child) */
  let rec pairs = (path: list(Exp.t)) =>
    switch (path) {
    | [p, c, ...rest] => [(p, c), ...pairs([c, ...rest])]
    | _ => []
    };
  let steps = List.rev(pairs(path));
  let rec walk = (steps: list((Exp.t, Exp.t)), acc: list(sp_bound)) =>
    switch (steps) {
    | [] => acc
    | [(p, c), ...rest] =>
      let cid = Exp.rep_id(c);
      let is = (x: Exp.t) => Exp.rep_id(x) == cid;
      /* (this side's classification) */
      let cls: [
        | `Delim
        | `Op(Precedence.t, Util.Direction.t)
        | `Fringe
        | `Opaque
      ] = {
        let op = prec =>
          `Op((
            prec,
            side == Util.Direction.Left
              ? Util.Direction.Right : Util.Direction.Left,
          ));
        /* d_side: bound found on d's LEFT means d is on that
           operator's RIGHT, and vice versa */
        switch (IdTagged.term_of(p), side) {
        | (BinOp(o, l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.of_bin_op(o)) : `Opaque
        | (BinOp(o, l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.of_bin_op(o)) : `Opaque
        | (Cons(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.cons) : `Opaque
        | (Cons(l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.cons) : `Opaque
        | (ListConcat(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.concat) : `Opaque
        | (ListConcat(l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.concat) : `Opaque
        | (Seq(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.semi) : `Opaque
        | (Seq(l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.semi) : `Opaque
        | (TupleExtension(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.plus) : `Opaque
        | (TupleExtension(l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.plus) : `Opaque
        | (Tuple(xs), Util.Direction.Left) =>
          xs != [] && is(List.hd(xs))
            ? `Fringe : List.exists(is, xs) ? op(Precedence.comma) : `Opaque
        | (Tuple(xs), Util.Direction.Right) =>
          xs != [] && is(List.nth(xs, List.length(xs) - 1))
            ? `Fringe : List.exists(is, xs) ? op(Precedence.comma) : `Opaque
        | (Asc(l, _), Util.Direction.Left) => is(l) ? `Fringe : `Delim
        | (Asc(l, _), Util.Direction.Right) =>
          is(l) ? op(Precedence.asc) : `Delim
        | (Dot(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.dot) : `Opaque
        | (Dot(l, _), Util.Direction.Right) =>
          is(l) ? op(Precedence.dot) : `Delim
        /* ap: fn slot is left-fringe / right-delimited (the arg
           parens); arg slot fully delimited */
        | (Ap(Forward, f, _), Util.Direction.Left) =>
          is(f) ? `Fringe : `Delim
        | (Ap(Forward, f, _), Util.Direction.Right) =>
          is(f) ? op(Precedence.ap) : `Delim
        | (Ap(Reverse, l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.eqs) : `Opaque
        | (Ap(Reverse, l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.eqs) : `Opaque
        /* keyword forms: interior slots delimited; final body is
           left-delimited, right-fringe */
        | (Let(_, d, b), _) =>
          if (is(d)) {
            `Delim; /* = ... in */
          } else if (is(b)) {
            side == Left ? `Delim : `Fringe;
          } else {
            `Opaque;
          }
        | (TyAlias(_, _, b), _)
        | (Use(_, b), _)
        | (Filter(_, b), _) =>
          is(b) ? side == Left ? `Delim : `Fringe : `Delim
        | (Theorem(_, d, b), _) =>
          if (is(d)) {
            `Delim;
          } else if (is(b)) {
            side == Left ? `Delim : `Fringe;
          } else {
            `Opaque;
          }
        | (Fun(_, b, _, _), _)
        | (TypFun(_, b, _), _)
        | (FixF(_, b, _), _)
        | (Forall(_, b), _) =>
          is(b) ? side == Left ? `Delim : `Fringe : `Opaque
        | (If(c1, t, alt), _) =>
          if (is(c1) || is(t)) {
            `Delim;
          } else if (is(alt)) {
            side == Left ? `Delim : `Fringe;
          } else {
            `Opaque;
          }
        | (Match(scrut, rules), _) =>
          is(scrut) || List.exists(((_, b)) => is(b), rules)
            ? `Delim : `Opaque
        | (Parens(_), _)
        | (ListLit(_), _)
        | (Test(_), _)
        | (HintedTest(_), _) => `Delim
        | (TupLabel(_, x), _) =>
          is(x) ? side == Left ? `Delim : `Fringe : `Delim
        | (UnOp(_, x), _) =>
          is(x) ? side == Left ? `Delim : `Fringe : `Opaque
        | _ => `Opaque
        };
      };
      switch (cls) {
      | `Delim => acc
      | `Op(p_, ds) =>
        walk(
          rest,
          [
            {
              prec: p_,
              d_side: ds,
            },
            ...acc,
          ],
        )
      | `Fringe => walk(rest, acc)
      | `Opaque => [
          {
            prec: Precedence.max,
            d_side: side == Left ? Util.Direction.Right : Util.Direction.Left,
          },
          ...acc,
        ] /* unknown parent: tightest bound = conservative parens */
      };
    };
  walk(steps, []);
};

let sp_captures = (bound: sp_bound, exposure: option(Precedence.t)): bool =>
  switch (exposure) {
  | None => false
  | Some(x) =>
    if (bound.prec < x) {
      true; /* adjacent op binds tighter: it steals */
    } else if (bound.prec > x) {
      false;
    } else {
      /* equal precedence: safe only on the operator's associative
         side */
      switch (Precedence.associativity(bound.prec)) {
      | Some(a) => a != bound.d_side
      | None => true
      };
    }
  };

/* parens needed to splice d at the position of `at` in program —
   static, table-driven, no printing */
let splice_parens_needed = (~program: Exp.t, ~at: Id.t, d: Exp.t): bool =>
  switch (find_path(~hit=e => Exp.rep_id(e) == at, program)) {
  | None
  | Some([_]) => false /* root: nothing adjacent */
  | Some(path) =>
    let check = side =>
      sp_bounds(~side, path)
      |> List.exists(b => sp_captures(b, sp_exposure(~side, d)));
    check(Util.Direction.Left) || check(Util.Direction.Right);
  };

let reparses_region = (region: Exp.t): bool => {
  let seg =
    ExpToSegment.exp_to_segment(~settings=roundtrip_settings, region)
    |> SpaceNormalize.go;
  let text = Printer.of_segment(~holes="?", ~refractors=[], seg);
  switch (Parser.to_segment(text, ~root=Exp)) {
  | None => false
  | Some(seg2) => Exp.fast_equal(MakeTerm.go(seg2).term, region)
  };
};

/* shared inline gate (impl ~matches AND menu gating): non-recursive
   def; no occurrence typ-captured under a rebinding tyalias/use. The
   moved material is the def PLUS any pattern annotation (substitution
   can re-introduce it as an ascription). */

/* unshadowed-use check is conservative: any occurrence counts */
let mentions = (x: string, e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | Var(y) when y == x => found := true
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  found^;
};

let copy_runs = (ws: list(Secondary.t)): list(Secondary.t) =>
  ws
  |> List.map((w: Secondary.t) =>
       {
         ...w,
         id: Id.mk(),
       }
     );

/* === Lines ===
 * A node is in LINE position when it occupies a slot in a block: a
 * let's body, a fun body, a case-arm or if-branch body, or the root.
 * Extract binds at the nearest enclosing line; every binder-
 * introducing construct's body is itself a line, so the climb never
 * escapes a binder (the recursive-let def is the one exception,
 * checked explicitly). */

let line_child = (parent: Exp.t, child: Exp.t): bool =>
  switch (IdTagged.term_of(parent)) {
  | Let(_, _, body) => same_node(body, child)
  /* a statement chain's tail is a line slot: extract inserts the
     new let just above the target item, scoping over the remaining
     items (definition nature of `;` — andrew) */
  | Seq(_, tail) => same_node(tail, child)
  /* a type line is a definition line: extract binds BELOW it, not
     above (also keeps extracted ascriptions inside the alias scope) */
  | TyAlias(_, _, body) => same_node(body, child)
  | Fun(_, body, _, _) => same_node(body, child)
  | FixF(_, body, _) => same_node(body, child)
  | Match(_, rules) =>
    rules |> List.exists(((_, b)) => same_node(b, child))
  | If(_, t, alt) => same_node(t, child) || same_node(alt, child)
  | _ => false
  };

let lowest_line = (path: list(Exp.t)): Exp.t => {
  let rec go = (line: Exp.t, path: list(Exp.t)): Exp.t =>
    switch (path) {
    | [parent, child, ...rest] =>
      go(line_child(parent, child) ? child : line, [child, ...rest])
    | _ => line
    };
  switch (path) {
  | [root, ..._] => go(root, path)
  | [] => failwith("lowest_line: empty path")
  };
};

/* names bound over the climb from line down to the target: only the
 * recursive-let def position introduces one (fun/arm bodies are
 * themselves lines, so the climb stops before crossing them) */
let crossed_rec_binders = (line: Exp.t, path: list(Exp.t)): list(string) => {
  let rec go = (started: bool, path: list(Exp.t)): list(string) =>
    switch (path) {
    | [parent, child, ...rest] =>
      let started = started || same_node(parent, line);
      let here =
        if (started) {
          switch (IdTagged.term_of(parent)) {
          | Let(p, def, _) when same_node(def, child) =>
            switch (IdTagged.term_of(def)) {
            | Fun(_) => pat_var_names(p)
            | _ => []
            }
          | _ => []
          };
        } else {
          [];
        };
      here @ go(started, [child, ...rest]);
    | _ => []
    };
  go(false, path);
};

/* structural equality modulo whitespace/comments and ids (strip all
   secondary, compare syntactically); outer parens transparent */
let strip_all_secondary = (e: Exp.t): Exp.t => {
  let clear = ((_, _)) => ([], []);
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) =>
        cont({
          ...e,
          annotation: {
            ...e.annotation,
            secondary: clear(e.annotation.secondary),
          },
        }),
    ~f_pat=
      (cont, p: Pat.t) =>
        cont({
          ...p,
          annotation: {
            ...p.annotation,
            secondary: clear(p.annotation.secondary),
          },
        }),
    ~f_typ=
      (cont, t: Typ.t) =>
        cont({
          ...t,
          annotation: {
            ...t.annotation,
            secondary: clear(t.annotation.secondary),
          },
        }),
    e,
  );
};

let rec unparens = (e: Exp.t): Exp.t =>
  switch (IdTagged.term_of(e)) {
  | Parens(inner) => unparens(inner)
  | _ => e
  };

let eq_defs = (a: Exp.t, b: Exp.t): bool =>
  Exp.fast_equal(
    strip_all_secondary(unparens(a)),
    strip_all_secondary(unparens(b)),
  );

/* does anything in e BIND this name? (conservative capture guard
   for absorption's use-repointing) */
let binds_somewhere = (x: string, e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_pat=
        (cont, p: Pat.t) => {
          switch (IdTagged.term_of(p)) {
          | Var(z) when z == x => found := true
          | _ => ()
          };
          cont(p);
        },
      e,
    );
  found^;
};

let typ_unknown = (): Typ.t => {
  annotation: {
    ...IdTagged.IdTag.mk_internal([Id.mk()]),
    secondary: (space(), []),
  },
  term: Unknown(Hole(EmptyHole)),
};

/* === Hoist / Sink ===
 * Move a binding up or down ONE level per invocation. These are the
 * explicit opt-in to evaluation-count/conditionality changes that
 * extract deliberately avoids: hoisting out of a fun evaluates once
 * instead of per call; sinking into an arm evaluates only when the
 * arm matches. Gates are conservative name checks (mentions), so a
 * blocked move is simply not offered. */

let names_mentioned = (names: list(string), e: Exp.t): bool =>
  names |> List.exists(n => free_in(n, e));

let disjoint_names = (a: list(string), b: list(string)): bool =>
  !(a |> List.exists(n => List.mem(n, b)));

/* === Definition lines ===
 * A binding line item — let or type alias (module forms later).
 * Chain movement treats them uniformly; both node kinds are
 * shard-headed (`let`/`type` is the node's first token), so their
 * line leads are node-level and node-level slot exchange is exact. */
type def_line =
  | LetLine(Pat.t, Exp.t)
  | TypeLine(TPat.t, Typ.t);

let def_line_of = (e: Exp.t): option((def_line, Exp.t)) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, d, body) => Some((LetLine(p, d), body))
  | TyAlias(tp, ty, body) => Some((TypeLine(tp, ty), body))
  | _ => None
  };

let def_line_rebuild = (e: Exp.t, body: Exp.t): Exp.t =>
  switch (IdTagged.term_of(e)) {
  | Let(p, d, _) => {
      ...e,
      term: Let(p, d, body),
    }
  | TyAlias(tp, ty, _) => {
      ...e,
      term: TyAlias(tp, ty, body),
    }
  | _ => e
  };

let line_exp_names = (l: def_line): list(string) =>
  switch (l) {
  | LetLine(p, _) => pat_var_names(p)
  | TypeLine(_) => []
  };
let line_typ_names = (l: def_line): list(string) =>
  switch (l) {
  | LetLine(_) => []
  | TypeLine(tp, _) => tpat_names(tp)
  };
/* the line's own material (binder + def; body excluded) as an exp,
   for the mention checks */
let line_material = (l: def_line): Exp.t =>
  switch (l) {
  | LetLine(p, d) => fresh(Let(p, d, fresh(EmptyHole)))
  | TypeLine(tp, ty) => fresh(TyAlias(tp, ty, fresh(EmptyHole)))
  };

/* may two adjacent definition lines exchange positions? Checked in
   both orders (conservative, like the Let/Let chain gate): neither
   line's material may reference names the other binds — exp names
   or alias names (an annotation crossing its alias would unbind:
   the tyalias soundness class) — and bound names stay disjoint per
   namespace. */
let lines_swappable = (a: def_line, b: def_line): bool => {
  let ma = line_material(a);
  let mb = line_material(b);
  disjoint_names(line_exp_names(a), line_exp_names(b))
  && disjoint_names(line_typ_names(a), line_typ_names(b))
  && !names_mentioned(line_exp_names(a), mb)
  && !names_mentioned(line_exp_names(b), ma)
  && !mentions_typ_names(line_typ_names(a), mb)
  && !mentions_typ_names(line_typ_names(b), ma);
};

/* may a def line cross one statement (Seq item)? The statement
   enters/leaves the line's scope, so it must not reference the
   line's names (either namespace). */
let rec run_before_in_seg =
        (id: Id.t, seg: Segment.t): option(list(Secondary.t)) => {
  let rec go = (i, prev: list(Secondary.t), rest: Segment.t) =>
    switch (rest) {
    | [] => None
    | [pc, ...tail] =>
      if (Piece.id(pc) == id) {
        Some(List.rev(prev));
      } else {
        let deeper =
          switch (pc) {
          | Piece.Tile({children, _}) =>
            children |> List.find_map(run_before_in_seg(id))
          | _ => None
          };
        switch (deeper) {
        | Some(r) => Some(r)
        | None =>
          switch (pc) {
          | Piece.Secondary(w) => go(i + 1, [w, ...prev], tail)
          | _ => go(i + 1, [], tail)
          }
        };
      }
    };
  go(0, [], seg);
};

/* Re-home attached doc blocks after a line movement: read each
   moved line's block from the TEXTUAL pre-image (holders of a
   line's lead vary — parent after-runs, node before-runs), drop
   those pieces from the result wherever they landed, and append
   them to the moved line's lead. Pieces are relocated, never
   copied or dropped; a block outside the term (buffer start)
   never prints in the pre-image, so it stays untouched. */
let carry_attached_docs =
    (~line_ids: list(Id.t), ~pre: Exp.t, post: Exp.t): Exp.t => {
  let seg = ExpToSegment.exp_to_segment(~settings=roundtrip_settings, pre);
  let moves =
    line_ids
    |> List.filter_map(id =>
         switch (run_before_in_seg(id, seg)) {
         | Some(run) =>
           let (_, att) = split_doc_block(run);
           att == [] ? None : Some((id, att));
         | None => None
         }
       );
  /* a carried line must still EXIST in the result (absorption
     dissolves lines): docs of a vanished line stay put — relocate
     or leave, never drop */
  let still_there = id => {
    let found = ref(false);
    let _ =
      Exp.map_term(
        ~f_exp=
          (cont, e: Exp.t) => {
            if (IdTagged.rep_id(e) == id) {
              found := true;
            };
            cont(e);
          },
        post,
      );
    found^;
  };
  let moves = moves |> List.filter(((id, _)) => still_there(id));
  if (moves == []) {
    post;
  } else {
    let att_ids =
      moves
      |> List.concat_map(((_, att)) =>
           att |> List.map((w: Secondary.t) => w.id)
         );
    let post = drop_secondary(att_ids, post);
    moves
    |> List.fold_left(
         (acc, (id, att)) =>
           Exp.map_term(
             ~f_exp=
               (cont, e: Exp.t) =>
                 if (IdTagged.rep_id(e) == id) {
                   let (b, a) = e.annotation.secondary;
                   {
                     ...e,
                     annotation: {
                       ...e.annotation,
                       secondary: (b @ att, a),
                     },
                   };
                 } else {
                   cont(e);
                 },
             acc,
           ),
         post,
       );
  };
};

let fresh_names = (k: int, program: Exp.t): list(string) => {
  let used = ref(used_names(program));
  List.init(
    k,
    _ => {
      let c = pick_placeholder(used^);
      used := [c, ...used^];
      c;
    },
  );
};

/* arity of an arrow's argument side, syntactically */
let arrow_arity = (a: Typ.t): int =>
  switch (IdTagged.term_of(a)) {
  | Prod(items) => List.length(items)
  | Parens(inner) =>
    switch (IdTagged.term_of(inner)) {
    | Prod(items) => List.length(items)
    | _ => 1
    }
  | _ => 1
  };

let replace_typ_node = (~at: Id.t, ~with_: Typ.t, e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_typ=
      (cont, ty: Typ.t) => IdTagged.rep_id(ty) == at ? with_ : cont(ty),
    e,
  );

/* alias-escape guard: the extracted type moves up to `line`; any
   type binder crossed on the way (alias/typfun between line and the
   host) that it mentions would be escaped — refuse (no rename fixes
   an escape) */
let crossed_typ_binders = (line: Exp.t, path: list(Exp.t)): list(string) => {
  let rec go = (started: bool, path: list(Exp.t)): list(string) =>
    switch (path) {
    | [parent, child, ...rest] =>
      let started = started || same_node(parent, line);
      let here =
        started
          ? switch (IdTagged.term_of(parent)) {
            | TyAlias(tp, _, _)
            | TypFun(tp, _, _) => tpat_names(tp)
            | _ => []
            }
          : [];
      here @ go(started, [child, ...rest]);
    | _ => []
    };
  go(false, path);
};

/* extraction targets: not a bare Var (alias-of-alias pair), not the
   whole def of an existing alias (same pair) */

/* === Remedied hoists (the "Remedied moves" tier) ===
 * A blocked hoist names its blockers and applies the remedy:
 *  - dependency lines above -> CARRY them (convoy): one press finds
 *    the maximal CONTIGUOUS run of lines directly above that the
 *    grabbed line transitively depends on, and the whole block
 *    crosses the first non-dependency line together (andrew: "to
 *    move this one line up, what do we need to do" — nothing is
 *    pushed preemptively).
 *  - the enclosing lambda's binder -> ABSTRACT over it (lift): the
 *    definition leaves the function as a helper, gaining the crossed
 *    params (prepended: helper = fun x -> OLD_DEF, so every use
 *    rewrites uniformly u -> u(x), bare or applied).
 * Keyboard reaches these by INSIST (press again on the shake);
 * menu shows them as their own entries (P10). */

let line_depends_on = (member: def_line, upper: def_line): bool =>
  names_mentioned(line_exp_names(upper), line_material(member))
  || mentions_typ_names(line_typ_names(upper), line_material(member));

/* === The convoy walk ===
 * From the grabbed def line C, walk up the path accumulating the
 * contiguous run of def lines above it that C (transitively) depends
 * on. Three refactorings share this walk and differ only in what they
 * make of where it stopped: hoist-with-deps needs the line X above the
 * run, lift needs the enclosure at the ceiling, hoist-blockers reports
 * the collision at X. */
type dep_stop =
  | StopLine(Exp.t, def_line) /* a non-dependency def line X above the run */
  | StopCeiling(int, Exp.t) /* path index + the non-def-line ancestor */
  | StopTop; /* walked past the root */

type dep_run = {
  block: list(Exp.t), /* T1..C, top-to-bottom, C last */
  block_dls: list(def_line),
  stop: dep_stop,
};

let dep_run_walk = (path: list(Exp.t)): option(dep_run) => {
  let n = List.length(path);
  let c = List.nth(path, n - 1);
  switch (def_line_of(c)) {
  | None => None
  | Some((c_dl, _)) =>
    let rec walk = (i, block_dls: list(def_line), block: list(Exp.t)) =>
      if (i < 0) {
        {
          block,
          block_dls,
          stop: StopTop,
        };
      } else {
        let anc = List.nth(path, i);
        let child = List.nth(path, i + 1);
        switch (def_line_of(anc)) {
        | Some((dl, body)) when same_node(body, child) =>
          block_dls |> List.exists(m => line_depends_on(m, dl))
            ? walk(i - 1, [dl, ...block_dls], [anc, ...block])
            : {
              block,
              block_dls,
              stop: StopLine(anc, dl),
            }
        | _ => {
            block,
            block_dls,
            stop: StopCeiling(i, anc),
          }
        };
      };
    Some(walk(n - 2, [c_dl], [c]));
  };
};

/* nodes top-to-bottom: [X, T1..Tm, C]; X = the line crossed, T's =
   the carried dependencies, C = the grabbed line */
let var_use_ids = (names: list(string), e: Exp.t): list(Id.t) =>
  e
  |> collect_exp(e' =>
       switch (IdTagged.term_of(e')) {
       | Var(x) when List.mem(x, names) => [Exp.rep_id(e')]
       | _ => []
       }
     );

/* ids of pattern BINDERS of any of `names` (a shadowing rebind is
   the culprit token) */
let pat_binder_ids = (names: list(string), p: Pat.t): list(Id.t) =>
  p
  |> collect_pat(p' =>
       switch (IdTagged.term_of(p')) {
       | Var(x) when List.mem(x, names) => [Pat.rep_id(p')]
       | _ => []
       }
     );

let binder_ids_in = (names: list(string), e: Exp.t): list(Id.t) =>
  e
  |> collect_exp(e' =>
       switch (IdTagged.term_of(e')) {
       | Let(p, _, _)
       | Fun(p, _, _, _) => pat_binder_ids(names, p)
       | Match(_, rules) =>
         rules |> List.concat_map(((rp, _)) => pat_binder_ids(names, rp))
       | _ => []
       }
     );

/* out-channel for lift refusals: each failing wall reports its
   culprit ids; set by lift_site, read by lift_wall_blockers on dead
   presses — same-call-stack use only */

/* a sep is spliced at several positions — each use mints fresh ids */
let sep_copy = (run: list(Secondary.t)): list(Secondary.t) =>
  run
  |> List.map((w: Secondary.t) =>
       {
         Secondary.id: Id.mk(),
         content: w.content,
       }
     );

/* landing-block for INTRODUCED bindings (bind-arm / bind-argument /
   split-let; unfold-call inherits): every introduced `in` breaks —
   one binding per line, unconditionally (andrew 2026-07-12: a let
   is a definition-shaped form and gets line structure regardless of
   the rotated construct's extent; slot-aware refinement possible
   later if parenthesized mid-expression lets read badly). Indent
   copies the physical line the construct starts on (nearest
   self-or-ancestor lead with a break). Direct reductions pass
   ~landing=false: their lets are transient and the substituted
   result keeps the site's own layout. */
let intro_sep = (~program: Exp.t, ~at: Id.t): list(Secondary.t) =>
  switch (find_path(~hit=hit_node(at), program)) {
  | None => newline()
  | Some(path) =>
    path
    |> List.rev
    |> List.find_opt(e => has_newline(Slot.of_exp(e).lead))
    |> Option.map(e => sep_like(Slot.of_exp(e).lead))
    |> Option.value(~default=newline())
  };
