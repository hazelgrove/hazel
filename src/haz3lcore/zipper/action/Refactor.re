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
    ids: [Id.mk()],
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

let pat_var_names = (p: Pat.t): list(string) => {
  let acc = ref([]);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p': Pat.t) => {
          switch (IdTagged.term_of(p')) {
          | Var(x) => acc := [x, ...acc^]
          | _ => ()
          };
          cont(p');
        },
      p,
    );
  acc^;
};

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

let fresh_name = (program: Exp.t): string => {
  let used = used_names(program);
  let rec pick = n => {
    let cand = "x" ++ string_of_int(n);
    List.mem(cand, used) ? pick(n + 1) : cand;
  };
  List.mem("x", used) ? pick(1) : "x";
};

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

let vars_of = (e: Exp.t): list(string) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e': Exp.t) => {
          switch (IdTagged.term_of(e')) {
          | Var(z) => acc := [z, ...acc^]
          | _ => ()
          };
          cont(e');
        },
      e,
    );
  acc^ |> List.sort_uniq(compare);
};

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
let exp_subtree_ids = (e: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          acc := IdTagged.ids(e) @ acc^;
          cont(e);
        },
      e,
    );
  acc^;
};

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

/* would substituting `moved` at occ_id capture a type name? A
   TyAlias crossed on the way rebinds its name; a crossed `use`
   rebinds unknowably (refuse whenever moved mentions any type name).
   Aliases aren't locally renameable, so capture means refusal. */
let typ_captured_at = (occ_id: Id.t, moved: Exp.t, body: Exp.t): bool => {
  let mentioned = typ_names_mentioned(moved);
  mentioned == []
    ? false
    : (
      switch (find_path(~hit=e => Exp.rep_id(e) == occ_id, body)) {
      | None => false
      | Some(path) =>
        path
        |> List.exists((e: Exp.t) =>
             switch (IdTagged.term_of(e)) {
             | TyAlias(tp, _, _) =>
               tpat_names(tp) |> List.exists(n => List.mem(n, mentioned))
             | Use(_) => true
             | _ => false
             }
           )
      }
    );
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
let inline_matches = (p: Pat.t, def: Exp.t, body: Exp.t): bool => {
  let moved: Exp.t = fresh(Let(p, def, fresh(EmptyHole)));
  let ok = f =>
    !free_in(f, def)
    && occurrences_of(f, body)
    |> List.for_all(o => !typ_captured_at(Exp.rep_id(o), moved, body));
  (
    switch (let_head_name(p)) {
    | Some(f) => ok(f)
    | None => false
    }
  )
  || (
    switch (sugar_fn_name(p)) {
    | Some(f) => ok(f)
    | None => false
    }
  );
};

let inline_let_impl: impl = {
  label: "Inline",
  tooltip: "Replace this let by substituting its definition",
  /* also offered at occurrences of the bound var */
  prepare: (~info_map, ~target, program) => {
    let attempt = target =>
      rewrite_let(
        ~target,
        /* self-recursive defs are gated on BOTH paths: consuming
           the binding would orphan the copied body's self-
           references (feed still unfolds them one use at a time,
           where the binding survives) */
        ~matches=inline_matches,
        ~rewrite=
          (p, def, body) => {
            /* An annotated head's type is analysis context, not
               decoration (a bare lambda re-synthesizes ? -> ?), so
               substitution keeps it as an ascription — unless the
               def synthesizes the same hole-free type on its own. */
            let asc_def = (ann: Typ.t, def: Exp.t): Exp.t => {
              let base = strip_boundaries(def);
              let base = needs_parens(base) ? fresh(Parens(base)) : base;
              fresh(
                Asc(
                  with_secondary(([], space()), base),
                  with_secondary_typ(
                    (space(), []),
                    strip_typ_boundaries(refresh_typ_ids(ann)),
                  ),
                ),
              );
            };
            let redundant = (ann: Typ.t, def: Exp.t): bool => {
              let st =
                CachedStatics.init_from_term(
                  ~settings=CoreSettings.on,
                  ~is_dynamic_term=false,
                  def,
                );
              switch (Id.Map.find_opt(Exp.rep_id(def), st.info_map)) {
              | Some(InfoExp({ty, _})) =>
                typ_known(ty)
                && (
                  switch (Id.Map.find_opt(Exp.rep_id(def), info_map)) {
                  | Some(InfoExp({ctx, _})) =>
                    Typ.equal(
                      Typ.normalize(ctx, ann),
                      Typ.normalize(ctx, ty),
                    )
                  | _ => false
                  }
                )
              | _ => false
              };
            };
            /* f(x)-sugar inlines as a lambda (gated non-recursive) */
            let (x, def) =
              switch (let_head_name(p)) {
              | Some(x) =>
                switch (IdTagged.term_of(p)) {
                | Asc(_, ann) when !redundant(ann, def) => (
                    x,
                    asc_def(ann, def),
                  )
                | _ => (x, def)
                }
              | None =>
                let f = Option.get(sugar_fn_name(p));
                let rec arg_of = (p: Pat.t) =>
                  switch (IdTagged.term_of(p)) {
                  | Ap(_, argp) => Some(argp)
                  | Asc(inner, _) => arg_of(inner)
                  | _ => None
                  };
                let argp = Option.get(arg_of(p));
                let param: Pat.t =
                  switch (IdTagged.term_of(argp)) {
                  | Tuple(_) => pad(fresh_pat(Parens(argp)))
                  | _ => pad(argp)
                  };
                let lam = fresh(Fun(param, def, None, None));
                switch (IdTagged.term_of(p)) {
                | Asc(_, ret) =>
                  let arrow =
                    fresh_typ(
                      Arrow(
                        with_secondary_typ(
                          ([], space()),
                          fresh_typ(Unknown(Hole(EmptyHole))),
                        ),
                        with_secondary_typ(
                          (space(), []),
                          strip_typ_boundaries(refresh_typ_ids(ret)),
                        ),
                      ),
                    );
                  (f, asc_def(arrow, lam));
                | _ => (f, lam)
                };
              };
            let avoid = vars_of(def) |> List.filter(v => free_in(v, def));
            let used = ref(used_names(program));
            /* Per-occurrence parens: an occurrence goes bare iff its
               smallest delimiter-bounded region reparses identically
               with the bare def spliced in. */
            let bare_ids =
              needs_parens(def)
                ? occurrences_of(x, body)
                  |> List.filter_map(occ => {
                       let region = bounded_region(Exp.rep_id(occ), program);
                       /* an unbounded (whole-program) region would
                          mean a whole-program reparse: just take
                          parens there */
                       if (same_node(region, program)) {
                         None;
                       } else {
                         let candidate =
                           replace_node(
                             ~at=Exp.rep_id(occ),
                             ~with_=inserted(~parens=false, def, occ),
                             region,
                           );
                         reparses_region(candidate)
                           ? Some(Exp.rep_id(occ)) : None;
                       };
                     })
                : occurrences_of(x, body) |> List.map(Exp.rep_id);
            let parens_for = occ =>
              needs_parens(def) && !List.mem(Exp.rep_id(occ), bare_ids);
            let body' =
              subst(
                ~parens_for,
                ~avoid,
                ~used,
                ~first=ref(true),
                x,
                def,
                body,
              );
            /* the def's BOUNDARY comments stay at the vacated line
               (andrew: comments live where they live) — the copies
               are stripped of them (never duplicate prose), so
               re-home them above the surviving body */
            let body' =
              switch (boundary_comments(def)) {
              | [] => body'
              | comments =>
                let (b, a) = body'.annotation.secondary;
                {
                  ...body',
                  annotation: {
                    ...body'.annotation,
                    secondary: (comments @ newline() @ b, a),
                  },
                };
              };
            /* caret follows the moved content (P2/P7): the def's
               ids travel into the first copy; later copies are fully
               fresh (they FLY as fan-out clones), so no occurrence
               id survives to focus on. */
            let focus =
              occurrences_of(x, body) == []
                ? Exp.rep_id(body') : Exp.rep_id(def);
            (body', focus);
          },
        program,
      );
    switch (attempt(target)) {
    | Some(r) => Some(r)
    | None =>
      switch (binder_of_occurrence(~info_map, ~target, program)) {
      | Some(binder) => attempt(binder)
      | None => None
      }
    };
  },
};

/* === Feed (per-occurrence inline) ===
 * Down's value-flow move (D2 locality): substitute the definition
 * into ONE use — the nearest below the def, or the invoked
 * occurrence — keeping the binding while other uses remain; the last
 * feed consumes it (delegates to full inline). The binder never
 * moves, so no scope gate is needed; capture of the def's free vars
 * by binders over the target occurrence is GATED (dead press) rather
 * than repaired — renames would be a distant surprise. */

/* the textually-first unshadowed occurrence (pre-order; children_of
 * yields children left-to-right) */
let first_occurrence = (x: string, body: Exp.t): option(Exp.t) => {
  let unshadowed = occurrences_of(x, body) |> List.map(Exp.rep_id);
  let rec go = (e: Exp.t): option(Exp.t) =>
    List.mem(Exp.rep_id(e), unshadowed)
      ? Some(e) : children_of(e) |> List.find_map(go);
  go(body);
};

/* names bound between the body root and the occurrence (conservative:
 * every binder on the path counts) */
let binders_over = (occ_id: Id.t, body: Exp.t): list(string) =>
  switch (find_path(~hit=e => Exp.rep_id(e) == occ_id, body)) {
  | None => []
  | Some(path) =>
    path
    |> List.concat_map((e: Exp.t) =>
         switch (IdTagged.term_of(e)) {
         | Let(p, _, _)
         | Fun(p, _, _, _)
         | FixF(p, _, _) => pat_var_names(p)
         | Match(_, rules) =>
           rules |> List.concat_map(((rp, _)) => pat_var_names(rp))
         | _ => []
         }
       )
  };

/* resolve a feed: the let, its bound name, and the occurrence to
 * feed (None = nearest) */
let feed_site =
    (
      ~prefer_def_host: bool=false,
      ~info_map: Statics.Map.t,
      ~target: Id.t,
      program: Exp.t,
    )
    : option((Exp.t, string, option(Id.t))) => {
  let of_let = (l: Exp.t, occ: option(Id.t)) =>
    switch (IdTagged.term_of(l)) {
    | Let(p, _, _) => let_head_name(p) |> Option.map(x => (l, x, occ))
    | _ => None
    };
  /* the innermost let whose DEF subtree holds the target: grabbing
     the value itself feeds it (the def is the persistent element —
     the D2-conformant handle); resolution order keeps occurrence
     targeting first so feed-at-use semantics are unchanged */
  let def_host = (): option(Exp.t) =>
    switch (find_path(~hit=hit_node(target), program)) {
    | None => None
    | Some(path) =>
      let rec scan = (best, path: list(Exp.t)) =>
        switch (path) {
        | [parent, child, ...rest] =>
          let best =
            switch (IdTagged.term_of(parent)) {
            | Let(_, def, _) when same_node(def, child) => Some(parent)
            | _ => best
            };
          scan(best, [child, ...rest]);
        | _ => best
        };
      scan(None, path);
    };
  switch (find_hit(~hit=hit_let(target), program)) {
  | Some(l) => of_let(l, None)
  | None =>
    /* a token can be BOTH inside a def and an occurrence of an
       outer binder. Keyboard keeps occurrence-first (feed-at-use);
       the drag prefers the def-host reading when the at-use track
       degenerates (grabbing the value should move the value). */
    let by_occurrence = () =>
      switch (binder_of_occurrence(~info_map, ~target, program)) {
      | Some(binder) =>
        switch (find_hit(~hit=hit_let(binder), program)) {
        | Some(l) => of_let(l, Some(target))
        | None => None
        }
      | None => None
      };
    let by_def_host = () =>
      switch (def_host()) {
      | Some(l) => of_let(l, None)
      | None => None
      };
    let (first, second) =
      prefer_def_host
        ? (by_def_host, by_occurrence) : (by_occurrence, by_def_host);
    switch (first()) {
    | Some(r) => Some(r)
    | None => second()
    };
  };
};

/* the plan (print-free): which occurrence a feed would hit, or that
 * the sole remaining use consumes the binding */
type feed_plan =
  | Consume(Exp.t) /* last use: full inline (of this let) */
  | Feed(Exp.t, Exp.t, Exp.t) /* let, def, occurrence */;

let feed_plan =
    (
      ~prefer_def_host: bool=false,
      ~info_map: Statics.Map.t,
      ~target: Id.t,
      program: Exp.t,
    )
    : option(feed_plan) =>
  switch (feed_site(~prefer_def_host, ~info_map, ~target, program)) {
  | None => None
  | Some((l, x, occ_pref)) =>
    switch (IdTagged.term_of(l)) {
    | Let(_, def, body) =>
      let occs = occurrences_of(x, body);
      switch (occs) {
      | [] => None
      /* the last feed CONSUMES the binding — except for a
         self-recursive def, whose copied body's self-references
         would be orphaned: those feed WITHOUT consuming (the
         binding survives; unfolding never eliminates a recursive
         definition). `let f = ... in f(x)` steps this way. */
      | [_] when !free_in(x, def) => Some(Consume(l))
      | _ =>
        let occ =
          switch (occ_pref) {
          | Some(oid) =>
            occs
            |> List.find_opt((o: Exp.t) => List.mem(oid, IdTagged.ids(o)))
          | None => first_occurrence(x, body)
          };
        switch (occ) {
        | None => None
        | Some(occ) =>
          let free = vars_of(def) |> List.filter(v => free_in(v, def));
          let captured =
            binders_over(Exp.rep_id(occ), body)
            |> List.exists(b => List.mem(b, free))
            || typ_captured_at(Exp.rep_id(occ), def, body);
          captured ? None : Some(Feed(l, def, occ));
        };
      };
    | _ => None
    }
  };

let feed_prepare = (~prefer_def_host=false, ~info_map, ~target, program) =>
  switch (feed_plan(~prefer_def_host, ~info_map, ~target, program)) {
  | None => None
  | Some(Consume(l)) =>
    /* delegate at the LET, not the raw target — a def-interior
       grab resolves here but not in inline's own targeting */
    inline_let_impl.prepare(~info_map, ~target=Exp.rep_id(l), program)
  | Some(Feed(l, def, occ)) =>
    /* parens: the same per-occurrence policy as inline */
    let parens =
      needs_parens(def)
      && {
        let region = bounded_region(Exp.rep_id(occ), program);
        same_node(region, program)
          ? true
          : !{
              let candidate =
                replace_node(
                  ~at=Exp.rep_id(occ),
                  ~with_=inserted(~parens=false, def, occ),
                  region,
                );
              reparses_region(candidate);
            };
      };
    /* the copy is a SPAWNED CLONE (dragology's fruit-bowl: the def
       survives, so the copy is wholly new — fresh ids throughout,
       keep_ids so the root doesn't adopt the occurrence's). The
       occurrence properly EXITS; the clone's own identity is what
       emerge animation correlates on. */
    /* the def survives, so the copy sheds prose too */
    let copy =
      inserted(
        ~parens,
        ~keep_ids=true,
        strip_comments(refresh_ids(def)),
        occ,
      );
    let at_use = occ |> IdTagged.ids |> List.mem(target);
    let focus =
      at_use
        /* invoked at the use: caret follows the clone (the
           occurrence's ids are gone) */
        ? Exp.rep_id(copy)
        /* invoked at the let: caret stays for the next feed */
        : Exp.rep_id(l);
    rewrite_node(
      ~hit=same_node(l),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Let(p', def', body') =>
            let body'' =
              replace_node(~at=Exp.rep_id(occ), ~with_=copy, body');
            Some((
              {
                ...e,
                term: Let(p', def', body''),
              },
              focus,
            ));
          | _ => None
          },
      program,
    );
  };

let feed_let_impl: impl = {
  label: "Inline next use",
  tooltip: "Substitute the definition into its nearest use; the last use consumes the binding",
  prepare: (~info_map, ~target, program) =>
    feed_prepare(~info_map, ~target, program),
};

/* Statics-gated: the binding's pattern var carries an UnusedVar
 * warning (co-ctx says the body never uses it) */
let pat_unused = (~info_map: Statics.Map.t, p: Pat.t): bool =>
  switch (Id.Map.find_opt(Pat.rep_id(p), info_map)) {
  | Some(InfoPat({warnings, _})) =>
    warnings
    |> List.exists((w: Warning.list_item) =>
         switch (w) {
         | Pat(UnusedVar(_)) => true
         }
       )
  | _ => false
  };

let remove_unused_let_impl: impl = {
  label: "Remove unused",
  tooltip: "Delete this binding: its variable is never used",
  prepare: (~info_map, ~target, program) =>
    rewrite_let(
      ~target,
      ~matches=
        (p, _, _) =>
          switch (head_var_pat(p)) {
          | Some(hv) => pat_unused(~info_map, hv)
          | None => false
          },
      ~rewrite=(_, _, body) => (body, Exp.rep_id(body)),
      program,
    ),
};

let exp_ty = (~info_map: Statics.Map.t, e: Exp.t): option(Typ.t) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(InfoExp({ty, _})) => Some(ty)
  | _ => None
  };

let add_annotation_impl: impl = {
  label: "Annotate",
  tooltip: "Annotate this binding with its inferred type",
  prepare: (~info_map, ~target, program) => {
    /* a bare spliced type can change the reparse (a Prod's comma
       breaks the let: `let p : Int,Bool = ...`) — oracle-gated parens
       like inline/extract */
    let attempt = (~parens: bool) =>
      rewrite_node(
        ~hit=hit_let(target),
        ~rewrite=
          e =>
            switch (IdTagged.term_of(e)) {
            | Let(p, def, body) when var_pat_name(p) != None =>
              switch (exp_ty(~info_map, def)) {
              | Some(ty) when typ_known(ty) =>
                let bare = refresh_typ_ids(ty);
                let ty =
                  pad(
                    parens
                      ? fresh_typ(
                          Parens(with_secondary_typ(([], []), bare)),
                        )
                      : bare,
                  );
                /* p keeps its runs: its old pre-`=` space now sits
                   before the `:` */
                let p' = fresh_pat(Asc(p, ty));
                Some((
                  {
                    ...e,
                    term: Let(p', def, body),
                  },
                  Typ.rep_id(ty),
                ));
              | _ => None
              }
            | _ => None
            },
        program,
      );
    /* static: single-token types go bare; anything wider (Prod's
       comma especially breaks the let) takes parens */
    let simple =
      switch (find_hit(~hit=hit_let(target), program)) {
      | Some(e) =>
        switch (IdTagged.term_of(e)) {
        | Let(_, def, _) =>
          switch (exp_ty(~info_map, def)) {
          | Some(ty) =>
            switch (IdTagged.term_of(ty)) {
            | Unknown(_)
            | Atom(_)
            | Var(_) => true
            | _ => false
            }
          | None => false
          }
        | _ => false
        }
      | None => false
      };
    attempt(~parens=!simple);
  },
};

let bool_pat = (p: Pat.t): option(bool) =>
  switch (IdTagged.term_of(p)) {
  | Atom(Bool(b)) => Some(b)
  | _ => None
  };

let if_to_case_impl: impl = {
  label: "To case",
  tooltip: "Rewrite this if/then/else as a case on true and false",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_node(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | If(c, t, alt) =>
            /* `end` starts a new slot: give it its own line when the
               arms are multiline */
            let multiline =
              has_newline(Slot.trail_of(t).trail)
              || has_newline(Slot.lead_of(t).lead)
              || has_newline(Slot.lead_of(alt).lead);
            let alt =
              multiline
                ? Slot.give(
                    {
                      Slot.lead: [],
                      trail: newline(),
                    },
                    alt,
                  )
                : alt;
            Some((
              fresh(
                Match(
                  c,
                  [
                    (fresh_pat(Atom(Bool(true))), t),
                    (fresh_pat(Atom(Bool(false))), alt),
                  ],
                ),
              ),
              Exp.rep_id(c),
            ));
          | _ => None
          },
      program,
    ),
};

let case_to_if_impl: impl = {
  label: "To if",
  tooltip: "Rewrite this true/false case as if/then/else",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_node(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Match(scrut, [(p1, e1), (p2, e2)]) =>
            switch (bool_pat(p1), bool_pat(p2)) {
            | (Some(true), Some(false)) =>
              Some((
                fresh(If(scrut, e1, strip_trailing_keep_comments(e2))),
                Exp.rep_id(scrut),
              ))
            | (Some(false), Some(true)) =>
              Some((
                fresh(If(scrut, e2, strip_trailing_keep_comments(e1))),
                Exp.rep_id(scrut),
              ))
            | _ => None
            }
          | _ => None
          },
      program,
    ),
};

let extractable = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  /* bare references extract to a pointless alias; constructors are
     morally variables here. (Literals stay extractable: naming a
     magic number is a real refactoring.) */
  | Var(_)
  | Constructor(_)
  | EmptyHole
  | Let(_)
  | Seq(_)
  | Filter(_) => false
  /* extracting a parens node is redundant with extracting its child,
     and the bare use-var can be load-bearing where the parens were
     (e.g. a Dot's lhs) */
  | Parens(_) => false
  | _ => true
  };

/* Resolve an extraction target. A bare var/ctor HEAD of an
 * application retargets to the whole application: the head is how
 * people refer to the call, the caret lands there naturally
 * (`Â¦f(x)` indicates f), and a bare head is otherwise a dead
 * press. Also home to the shared gates: extractable + no Dot/
 * MultiHole ancestors (a bare use-var as a Dot-rhs reparses as a
 * projection label). Returns the effective path. */
let extract_path = (~target: Id.t, program: Exp.t): option(list(Exp.t)) =>
  switch (find_path(~hit=hit_node(target), program)) {
  | None => None
  | Some(path) =>
    let n = List.length(path);
    let t = List.nth(path, n - 1);
    let head_of_ap =
      n >= 2
      && (
        switch (IdTagged.term_of(t)) {
        | Var(_)
        | Constructor(_) =>
          switch (IdTagged.term_of(List.nth(path, n - 2))) {
          | Ap(Forward, f, _) => same_node(f, t)
          | _ => false
          }
        | _ => false
        }
      );
    let path = head_of_ap ? List.filteri((i, _) => i < n - 1, path) : path;
    let t = List.nth(path, List.length(path) - 1);
    /* extracting a let's ENTIRE def just manufactures an alias pair
       (`let x = def in let orig = x`) — the whole-reference rule
       again. Parens wrappers are transparent for this check. */
    let whole_def = {
      let rec check = (k: int, child: Exp.t) =>
        k >= 0
        && (
          switch (IdTagged.term_of(List.nth(path, k))) {
          | Let(_, def, _) => same_node(def, child)
          | Parens(_) => check(k - 1, List.nth(path, k))
          | _ => false
          }
        );
      check(List.length(path) - 2, t);
    };
    let blocked_ancestor =
      path
      |> List.filteri((i, _) => i < List.length(path) - 1)
      |> List.exists((a: Exp.t) =>
           switch (IdTagged.term_of(a)) {
           | Dot(_)
           | MultiHole(_) => true
           | _ => false
           }
         );
    extractable(t) && !blocked_ancestor && !whole_def ? Some(path) : None;
  };

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

let extract_let_impl: impl = {
  label: "Extract",
  tooltip: "Bind this expression to a fresh variable at the enclosing line",
  prepare: (~info_map as _, ~target, program) => {
    let x = fresh_name(program);
    /* STATIC parens policy (the whole-program reparse oracle cost
       ~0.5s/invocation; reparse-safety is covered by tests instead):
       to_block always goes bare — lowest_line only yields line slots
       (chain slots, fun/arm/branch bodies, root), where a bare
       `let..in`'s rightward extent is exactly the intended body and
       arm/branch slots are delimiter-bounded. in_place goes bare only
       when extracting a whole line; the rec-blocked fallback sits at
       an arbitrary (possibly comma/operand) position, so it takes
       parens. */
    /* fallback, and the degenerate case of extracting a whole line */
    /* t is the extract_path-resolved node (which may be the whole
       application when invoked from its head) */
    let in_place = (~parens: bool, t: Exp.t) =>
      rewrite_node(
        ~hit=same_node(t),
        ~rewrite=
          e => {
            let def = pad(e |> strip_leading |> strip_trailing);
            /* focus the fresh binder: keeps the caret in the
               let's gesture zone (next Up = hoist) and ready for
               rename */
            let xp = pad(fresh_pat(Var(x)));
            let let_node = fresh(Let(xp, def, fresh(Var(x))));
            Some((
              parens ? fresh(Parens(let_node)) : let_node,
              Pat.rep_id(xp),
            ));
          },
        program,
      );
    /* the new binding lands at the nearest enclosing line; the use
       site takes over the extracted node's whitespace slot, and the
       line keeps its layout via a fresh copy of its leading run */
    let to_block = (path: list(Exp.t), line: Exp.t, t: Exp.t) => {
      let s = Slot.of_exp(t);
      let def = pad(Slot.drop(s, t));
      let use = Slot.give(s, fresh(Var(x)));
      /* an introduced let ALWAYS gets its own line (andrew: extract's
         directionality only holds if the displaced content actually
         moves). Line slots that already start a line keep their lead
         shape; INLINE sub-slots (fun/arm bodies, chain body slots)
         synthesize a break with the nearest enclosing line's indent
         + 2 — the ancestor path supplies the indent. */
      let sep = {
        let s = sep_like(Slot.of_exp(line).lead);
        if (has_newline(s)) {
          s;
        } else {
          /* nearest ancestor (deepest-first, above the line) whose
             slot starts a line: copy its break + indent, plus 2 */
          let rec prefix_to = (acc, path: list(Exp.t)) =>
            switch (path) {
            | [] => acc
            | [n, ..._] when same_node(n, line) => acc
            | [n, ...rest] => prefix_to([n, ...acc], rest)
            };
          let ancestor_sep =
            prefix_to([], path)
            |> List.find_map((a: Exp.t) => {
                 let al = sep_like(Slot.of_exp(a).lead);
                 has_newline(al) ? Some(al) : None;
               });
          switch (ancestor_sep) {
          | _ when same_node(line, program) =>
            /* root line: column 0, no indent */
            newline()
          | Some(al) => al @ space() @ space()
          | None => newline() @ space() @ space()
          };
        };
      };
      let build = (~parens: bool) =>
        rewrite_node(
          ~hit=same_node(line),
          ~rewrite=
            ln => {
              let body = replace_node(~at=Exp.rep_id(t), ~with_=use, ln);
              let (b, a) = body.annotation.secondary;
              let body = {
                ...body,
                annotation: {
                  ...body.annotation,
                  secondary: (sep @ b, a),
                },
              };
              let xp = pad(fresh_pat(Var(x)));
              let let_node = fresh(Let(xp, def, body));
              Some((
                parens ? fresh(Parens(let_node)) : let_node,
                Pat.rep_id(xp),
              ));
            },
          program,
        );
      build(~parens=false);
    };
    switch (extract_path(~target, program)) {
    | None => None
    | Some(path) =>
      let t = List.nth(path, List.length(path) - 1);
      let line = lowest_line(path);
      let blocked =
        crossed_rec_binders(line, path) |> List.exists(n => mentions(n, t));
      !blocked && !same_node(line, t)
        ? to_block(path, line, t) : in_place(~parens=!same_node(line, t), t);
    };
  },
};

let eta_reduce_impl: impl = {
  label: "Eta-Reduce",
  tooltip: "Simplify `fun x -> f(x)` to `f`",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_node(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Fun(p, body, _, _) =>
            switch (var_pat_name(p), IdTagged.term_of(body)) {
            | (Some(x), Ap(Forward, f, arg)) =>
              switch (IdTagged.term_of(arg)) {
              | Var(y) when y == x && !mentions(x, f) =>
                let f = f |> strip_leading |> strip_trailing;
                Some((f, Exp.rep_id(f)));
              | _ => None
              }
            | _ => None
            }
          | _ => None
          },
      program,
    ),
};

let negate_if_impl: impl = {
  label: "Flip branches",
  tooltip: "Flip this if: negate the condition, swap then/else",
  prepare: (~info_map as _, ~target, program) => {
    let attempt = (~parens: bool) =>
      rewrite_node(
        ~hit=hit_node(target),
        ~rewrite=
          e =>
            switch (IdTagged.term_of(e)) {
            | If(c, t, alt) =>
              /* arms swap UNTOUCHED so their formatting (incl.
                 multi-line layout) survives; the pre-`else` boundary
                 run belongs to the SLOT, not the arm — the new
                 then-arm takes it over. The condition sheds its
                 post-`if` lead (it now sits after `!`). Parens by
                 reparse oracle; the If node keeps its id. */
              /* the run between the condition and `then` belongs to
                 the SLOT: capture it and re-give it to the new
                 condition so a multiline if keeps its line breaks */
              let old_trail = Slot.trail_of(c).trail;
              let c = c |> strip_leading_keep_comments |> strip_trailing;
              /* self-inverse: negating a negation unwraps it */
              let cond =
                switch (IdTagged.term_of(c)) {
                | UnOp(Bool(Not), inner) =>
                  /* also shed the negation's own parens: the cond
                     slot is bidelimited, bare is always safe */
                  let inner =
                    switch (IdTagged.term_of(inner)) {
                    | Parens(q) => q
                    | _ => inner
                    };
                  with_secondary(
                    (space(), old_trail),
                    strip_boundaries_keep_comments(inner),
                  );
                | _ =>
                  let c = parens ? fresh(Parens(c)) : c;
                  {
                    ...fresh(UnOp(Bool(Not), c)),
                    annotation: {
                      ...IdTagged.IdTag.mk_internal([Id.mk()]),
                      secondary: (space(), old_trail),
                    },
                  };
                };
              let boundary = Slot.trail_of(t);
              Some((
                {
                  ...e,
                  term:
                    If(
                      cond,
                      Slot.give(boundary, alt),
                      Slot.drop(boundary, t),
                    ),
                },
                Exp.rep_id(cond),
              ));
            | _ => None
            },
        program,
      );
    /* static: !cond needs parens exactly when cond isn't
       self-delimited */
    let parens =
      switch (find_hit(~hit=hit_node(target), program)) {
      | Some(e) =>
        switch (IdTagged.term_of(e)) {
        | If(c, _, _) => needs_parens(strip_boundaries(c))
        | _ => true
        }
      | None => true
      };
    attempt(~parens);
  },
};

/* witness for an inexhaustive match, from statics marks */
let match_witness = (~info_map: Statics.Map.t, e: Exp.t): option(Pat.t) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(InfoExp({marks, _})) =>
    marks
    |> List.find_map((m: Mark.t) =>
         switch (m) {
         | InexhaustiveMatch(_, _, Pat(p)) => Some(p)
         | _ => None
         }
       )
  | _ => None
  };

/* coverage witnesses cite literals; an infinite domain (ints, floats,
 * strings) can't be finished literal-by-literal, so those become _ */
let wildify = (p: Pat.t): Pat.t =>
  Pat.map_term(
    ~f_pat=
      (cont, p: Pat.t) =>
        switch (IdTagged.term_of(p)) {
        | Atom(Int(_) | SInt(_) | Nat(_) | Float(_) | String(_)) => {
            ...p,
            term: Wild,
          }
        | _ => cont(p)
        },
    p,
  );

let pat_needs_parens = (p: Pat.t): bool =>
  switch (IdTagged.term_of(p)) {
  | Tuple(_) => true
  | _ => false
  };

let add_case_arm_impl: impl = {
  label: "Add arm",
  tooltip: "Append an arm for an unhandled pattern",
  prepare: (~info_map, ~target, program) =>
    rewrite_node(
      ~hit=hit_node(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Match(scrut, rules) when rules != [] =>
            switch (match_witness(~info_map, e)) {
            | Some(w) =>
              let w = wildify(refresh_pat_ids(w));
              let w = pat_needs_parens(w) ? fresh_pat(Parens(w)) : w;
              let (_, last_body) = List.nth(rules, List.length(rules) - 1);
              /* the last body's trailing run stays put, becoming the
                 separator before the new |; the new body gets a fresh
                 copy so end keeps its position style */
              let sep = sep_like(Slot.trail_of(last_body).trail);
              let body = {
                ...fresh(EmptyHole),
                annotation: {
                  ...IdTagged.IdTag.mk_internal([Id.mk()]),
                  secondary: (space(), sep),
                },
              };
              Some((
                {
                  ...e,
                  term: Match(scrut, rules @ [(pad(w), body)]),
                },
                Exp.rep_id(body),
              ));
            | None => None
            }
          | _ => None
          },
      program,
    ),
};

let is_var_named = (x: string, e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(y) => y == x
  | _ => false
  };

/* Patch every unshadowed application of x: f(a) -> f(a, ?). A bare
 * (non-applied) use sets ~bare_use — arity extension can't fix a
 * function passed as a value, so callers gate on it. */
let patch_calls = (~bare_use: ref(bool), x: string, e: Exp.t): Exp.t =>
  map_unshadowed(
    ~skip=x,
    ~f_var=
      e' =>
        switch (IdTagged.term_of(e')) {
        | Var(z) when z == x =>
          bare_use := true;
          Some(e');
        | _ => None
        },
    ~f_ap=
      (ap, fn, arg, go) =>
        if (is_var_named(x, fn)) {
          let hole = {
            ...fresh(EmptyHole),
            annotation: {
              ...IdTagged.IdTag.mk_internal([Id.mk()]),
              secondary: (space(), []),
            },
          };
          let arg': Exp.t =
            switch (IdTagged.term_of(arg)) {
            | Tuple(items) => {
                ...arg,
                term: Tuple(List.map(go, items) @ [hole]),
              }
            | _ => fresh(Tuple([go(arg), hole]))
            };
          Some({
            ...ap,
            term: Ap(Forward, fn, arg'),
          });
        } else {
          None;
        },
    e,
  );

/* Extend a fun's parameter (pattern) with a fresh trailing var */
let extended_pat = (p: Pat.t, name: string): (Pat.t, Id.t) => {
  let newvar = {
    ...fresh_pat(Var(name)),
    annotation: {
      ...IdTagged.IdTag.mk_internal([Id.mk()]),
      secondary: (space(), []),
    },
  };
  let focus = Pat.rep_id(newvar);
  let clear = (p: Pat.t) => {
    ...p,
    annotation: {
      ...p.annotation,
      secondary: ([], []),
    },
  };
  let p': Pat.t =
    switch (IdTagged.term_of(p)) {
    | Parens(inner) =>
      switch (IdTagged.term_of(inner)) {
      | Tuple(items) => {
          ...p,
          term:
            Parens({
              ...inner,
              term: Tuple(items @ [newvar]),
            }),
        }
      | _ => {
          ...p,
          term: Parens(fresh_pat(Tuple([clear(inner), newvar]))),
        }
      }
    | _ =>
      /* the parens wrapper takes over the old param's outer runs */
      let (b, a) = p.annotation.secondary;
      {
        annotation: {
          ...IdTagged.IdTag.mk_internal([Id.mk()]),
          secondary: (b, a),
        },
        term: Parens(fresh_pat(Tuple([clear(p), newvar]))),
      };
    };
  (p', focus);
};

let typ_unknown = (): Typ.t => {
  annotation: {
    ...IdTagged.IdTag.mk_internal([Id.mk()]),
    secondary: (space(), []),
  },
  term: Unknown(Hole(EmptyHole)),
};

let clear_typ = (t: Typ.t): Typ.t => {
  ...t,
  annotation: {
    ...t.annotation,
    secondary: ([], []),
  },
};

/* Extend the argument side of an annotation's arrow with a hole type:
 * A -> B becomes (A, ?) -> B. None when the annotation isn't
 * syntactically an arrow (alias, hole) — rewriting it blind would
 * leave a lying annotation. */
let extended_arrow = (ann: Typ.t): option(Typ.t) =>
  switch (IdTagged.term_of(ann)) {
  | Arrow(a, b) =>
    let a': Typ.t =
      switch (IdTagged.term_of(a)) {
      | Parens(inner) =>
        switch (IdTagged.term_of(inner)) {
        | Prod(items) => {
            ...a,
            term:
              Parens({
                ...inner,
                term: Prod(items @ [typ_unknown()]),
              }),
          }
        | _ => {
            ...a,
            term:
              Parens(fresh_typ(Prod([clear_typ(inner), typ_unknown()]))),
          }
        }
      | _ =>
        /* single arg type: the parens wrapper takes over its runs */
        let (b_, a_) = a.annotation.secondary;
        {
          annotation: {
            ...IdTagged.IdTag.mk_internal([Id.mk()]),
            secondary: (b_, a_),
          },
          term: Parens(fresh_typ(Prod([clear_typ(a), typ_unknown()]))),
        };
      };
    Some({
      ...ann,
      term: Arrow(a', b),
    });
  | _ => None
  };

/* Extend f(x)-sugar's argument pattern (possibly under a return-type
 * ascription) with a fresh var */
let extended_ap_pat = (p: Pat.t, name: string): option((Pat.t, Id.t)) => {
  let newvar = {
    ...fresh_pat(Var(name)),
    annotation: {
      ...IdTagged.IdTag.mk_internal([Id.mk()]),
      secondary: (space(), []),
    },
  };
  let focus = Pat.rep_id(newvar);
  let extend_arg = (arg: Pat.t): Pat.t =>
    switch (IdTagged.term_of(arg)) {
    | Tuple(items) => {
        ...arg,
        term: Tuple(items @ [newvar]),
      }
    | _ => fresh_pat(Tuple([arg, newvar]))
    };
  let rec go = (p: Pat.t): option(Pat.t) =>
    switch (IdTagged.term_of(p)) {
    | Ap(fv, arg) =>
      switch (IdTagged.term_of(fv)) {
      | Var(_) =>
        Some({
          ...p,
          term: Ap(fv, extend_arg(arg)),
        })
      | _ => None
      }
    | Asc(inner, ann) =>
      go(inner)
      |> Option.map((inner': Pat.t): Pat.t =>
           {
             ...p,
             term: Asc(inner', ann),
           }
         )
    | _ => None
    };
  go(p) |> Option.map(p' => (p', focus));
};

/* Shared by applies and prepare (no printing/parsing here, so gating
 * can afford the full build; rewrite_node adds the slot takeover on
 * invocation only). Shapes: let f = fun ...; let f : A -> B = fun ...
 * (annotation's arrow rewritten); let f(x) = ... (opt : Ret). */
/* the Parens pat wrapping a function's parameters, both shapes */
let param_paren = (e: Exp.t): option(Pat.t) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, def, _) =>
    let rec of_sugar = (p: Pat.t): option(Pat.t) =>
      switch (IdTagged.term_of(p)) {
      /* the sugar form's (...) are the Ap tile's own delimiters */
      | Ap(_, _) => Some(p)
      | Asc(inner, _) => of_sugar(inner)
      | _ => None
      };
    switch (of_sugar(p)) {
    | Some(_) as r => r
    | None =>
      switch (IdTagged.term_of(def)) {
      | Fun(fp, _, _, _) =>
        switch (IdTagged.term_of(fp)) {
        | Parens(_) => Some(fp)
        | _ => None
        }
      | _ => None
      }
    };
  | _ => None
  };

/* AddParameter is targetable from the let zone or the param parens
 * (the fun-shape's parens live in the def, outside hit_let) */
let hit_add_param = (target: Id.t, e: Exp.t): bool =>
  hit_let(target, e)
  || (
    switch (param_paren(e)) {
    | Some(paren) => List.mem(target, IdTagged.ids(paren))
    | None => false
    }
  );

let add_param_rewrite = (~program: Exp.t, e: Exp.t): option((Exp.t, Id.t)) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, def, body) =>
    let name = fresh_name(program);
    let bare_use = ref(false);
    let patched = (f, x) => patch_calls(~bare_use, f, x);
    let pieces: option((string, Pat.t, Exp.t, Id.t)) =
      switch (sugar_fn_name(p)) {
      | Some(f) =>
        extended_ap_pat(p, name)
        |> Option.map(((p', focus)) => (f, p', patched(f, def), focus))
      | None =>
        switch (IdTagged.term_of(p), IdTagged.term_of(def)) {
        | (Var(f), Fun(fp, fbody, fty, nm)) =>
          let (fp', focus) = extended_pat(fp, name);
          let fbody' = binds(f, fp) ? fbody : patched(f, fbody);
          Some((
            f,
            p,
            {
              ...def,
              term: Fun(fp', fbody', fty, nm),
            },
            focus,
          ));
        | (Asc(inner, ann), Fun(fp, fbody, fty, nm)) =>
          switch (var_pat_name(inner), extended_arrow(ann)) {
          | (Some(f), Some(ann')) =>
            let (fp', focus) = extended_pat(fp, name);
            let fbody' = binds(f, fp) ? fbody : patched(f, fbody);
            Some((
              f,
              {
                ...p,
                term: Asc(inner, ann'),
              },
              {
                ...def,
                term: Fun(fp', fbody', fty, nm),
              },
              focus,
            ));
          | _ => None
          }
        | _ => None
        }
      };
    switch (pieces) {
    | Some((f, p', def', focus)) =>
      let body' = patched(f, body);
      bare_use^
        ? None
        : Some((
            {
              ...e,
              term: Let(p', def', body'),
            },
            focus,
          ));
    | None => None
    };
  | _ => None
  };

let add_param_impl: impl = {
  label: "Add param",
  tooltip: "Extend this function with a parameter; call sites get a hole",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_add_param(target),
      ~rewrite=add_param_rewrite(~program),
      program,
    ),
};

/* === RenameFree: repair-style rename ===
 * The user renames a binder by ordinary editing; the old uses go free
 * (statics marks them). This binds them to the indicated binder. It
 * only ever gives meaning to currently-unbound occurrences — never
 * re-points a bound one — so it needs no knowledge of the old name's
 * history. Blind spot: uses that silently rebound to an OUTER binder
 * of the old name are invisible here (no Free marks). */

let free_marked = (~info_map: Statics.Map.t, e: Exp.t): bool =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(InfoExp({marks, _})) =>
    marks
    |> List.exists((m: Mark.t) =>
         switch (m) {
         | Free(_) => true
         | _ => false
         }
       )
  | _ => false
  };

let free_vars_in = (~info_map: Statics.Map.t, e: Exp.t): list(string) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e': Exp.t) => {
          switch (IdTagged.term_of(e')) {
          | Var(x) when free_marked(~info_map, e') => acc := [x, ...acc^]
          | _ => ()
          };
          cont(e');
        },
      e,
    );
  acc^;
};

/* The rename sites a binder node offers: each bound name paired with
 * the regions it scopes over. Sugar params scope over the RHS only;
 * the sugar fn name (recursive) and fun-valued lets scope over def
 * and body; arm pats scope over their own body (arm chosen by
 * target). */
/* (name, own ids) for each var bound in a pattern: targeting a
 * specific binder token narrows the offer to that name */
let pat_var_id_sites = (p: Pat.t): list((string, list(Id.t))) => {
  let acc = ref([]);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p': Pat.t) => {
          switch (IdTagged.term_of(p')) {
          | Var(x) => acc := [(x, IdTagged.ids(p')), ...acc^]
          | _ => ()
          };
          cont(p');
        },
      p,
    );
  acc^;
};

let rename_sites_unfiltered =
    (~target: Id.t, e: Exp.t): list((string, list(Exp.t))) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, def, body) =>
    switch (sugar_fn_name(p)) {
    | Some(f) =>
      let params =
        pat_var_names(p)
        |> List.sort_uniq(compare)
        |> List.filter(v => v != f);
      [(f, [def, body])] @ (params |> List.map(v => (v, [def])));
    | None =>
      switch (let_head_name(p)) {
      | Some(y) =>
        let regions =
          switch (IdTagged.term_of(def)) {
          | Fun(_) => [def, body]
          | _ => [body]
          };
        [(y, regions)];
      | None => []
      }
    }
  | Fun(p, body, _, _) =>
    pat_var_names(p)
    |> List.sort_uniq(compare)
    |> List.map(y => (y, [body]))
  | Match(_, rules) =>
    rules
    |> List.concat_map(((p, body)) =>
         List.mem(target, pat_subtree_ids(p))
           ? pat_var_names(p)
             |> List.sort_uniq(compare)
             |> List.map(y => (y, [body]))
           : []
       )
  | _ => []
  };

/* Targeting: the binder-name TOKEN is the only rename affordance —
 * not the construct's delimiters, not pattern punctuation (a caret on
 * the comma in `let f(x, y)` means neither f nor x). */
let rename_sites = (~target: Id.t, e: Exp.t): list((string, list(Exp.t))) => {
  let pats =
    switch (IdTagged.term_of(e)) {
    | Let(p, _, _)
    | Fun(p, _, _, _) => [p]
    | Match(_, rules) =>
      rules
      |> List.filter_map(((p, _)) =>
           List.mem(target, pat_subtree_ids(p)) ? Some(p) : None
         )
    | _ => []
    };
  switch (
    pats
    |> List.concat_map(pat_var_id_sites)
    |> List.find_opt(((_, ids)) => List.mem(target, ids))
  ) {
  | Some((y, _)) =>
    rename_sites_unfiltered(~target, e) |> List.filter(((y', _)) => y' == y)
  | None => []
  };
};

let hit_rename = (target: Id.t, e: Exp.t): bool =>
  hit_let(target, e) || hit_fun(target, e) || hit_match_pat(target, e);

/* (free name, binder name) pairs offered at the hit node */
let rename_pairs =
    (~info_map: Statics.Map.t, ~target: Id.t, e: Exp.t)
    : list((string, string)) =>
  rename_sites(~target, e)
  |> List.concat_map(((y, regions)) =>
       regions
       |> List.concat_map(free_vars_in(~info_map))
       |> List.sort_uniq(compare)
       |> List.filter(x => x != y)
       |> List.map(x => (x, y))
     )
  |> List.sort_uniq(compare);

/* rewrite free occurrences of x to y, skipping scopes where y is
 * rebound (they'd capture to the wrong binder); bound x's are already
 * excluded by the Free-mark filter */
let rename_free_in =
    (
      ~info_map: Statics.Map.t,
      ~count: ref(int),
      x: string,
      y: string,
      e: Exp.t,
    )
    : Exp.t =>
  map_unshadowed(
    ~skip=y,
    ~f_var=
      e' =>
        switch (IdTagged.term_of(e')) {
        | Var(z) when z == x && free_marked(~info_map, e') =>
          count := count^ + 1;
          Some({
            annotation: {
              ...e'.annotation,
              lexeme: None,
            },
            term: Var(y),
          });
        | _ => None
        },
    e,
  );

let rename_free_impl = (x: string, y: string): impl => {
  label: "Rename " ++ x ++ " to " ++ y,
  tooltip: "Bind free occurrences of " ++ x ++ " at this binding",
  prepare: (~info_map, ~target, program) =>
    x == y
      ? None
      : rewrite_node(
          ~hit=hit_rename(target),
          ~rewrite=
            e =>
              switch (
                rename_sites(~target, e)
                |> List.find_opt(((y', _)) => y' == y)
              ) {
              | Some((_, regions)) =>
                let count = ref(0);
                let ren = rename_free_in(~info_map, ~count, x, y);
                let e' =
                  regions
                  |> List.fold_left(
                       (e, r: Exp.t) =>
                         replace_node(~at=Exp.rep_id(r), ~with_=ren(r), e),
                       e,
                     );
                /* caret stays on the binder token (where the user
                   was editing in the repair flow) */
                let focus = {
                  let pats =
                    switch (IdTagged.term_of(e')) {
                    | Let(p, _, _)
                    | Fun(p, _, _, _) => [p]
                    | Match(_, rules) =>
                      rules
                      |> List.filter_map(((p, _)) =>
                           List.mem(target, pat_subtree_ids(p))
                             ? Some(p) : None
                         )
                    | _ => []
                    };
                  switch (
                    pats
                    |> List.concat_map(pat_var_id_sites)
                    |> List.find_opt(((n, _)) => n == y)
                  ) {
                  | Some((_, [id, ..._])) => id
                  | _ => Exp.rep_id(e')
                  };
                };
                count^ > 0 ? Some((e', focus)) : None;
              | None => None
              },
          program,
        ),
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

/* one hoist step for the let at the end of ~path; returns the parent
 * node to rewrite, its replacement, and a focus id. ~fixup as in
 * sink_step: invocation moves the released body's textual lead into
 * the vacated slot (prints; gating passes false). */
let hoist_step =
    (~fixup: bool, path: list(Exp.t)): option((Exp.t, Exp.t, Id.t)) => {
  let occupy =
      (slot: (list(Secondary.t), list(Secondary.t)), region: Exp.t) =>
    if (fixup) {
      let s = Slot.lead_of(region);
      let region = Slot.drop(s, region);
      let (b, a) = region.annotation.secondary;
      with_secondary((fst(slot) @ b, a @ snd(slot)), region);
    } else {
      region;
    };
  let n = List.length(path);
  if (n < 2) {
    None;
  } else {
    let l = List.nth(path, n - 1);
    let direct = List.nth(path, n - 2);
    /* a def-position let is usually parenthesized; treat the parens
       as packaging */
    let (p, c) =
      switch (IdTagged.term_of(direct)) {
      | Parens(_) when n >= 3 => (List.nth(path, n - 3), direct)
      | _ => (direct, l)
      };
    switch (IdTagged.term_of(l)) {
    | Let(lp, ldef, lbody) =>
      let l_names = pat_var_names(lp);
      switch (IdTagged.term_of(p)) {
      | Let(mp, mdef, mbody)
          when
            same_node(mbody, c)
            && same_node(c, l)
            && disjoint_names(l_names, pat_var_names(mp))
            && !names_mentioned(pat_var_names(mp), ldef)
            && !names_mentioned(l_names, mdef) =>
        /* chain swap; the two lets exchange line slots */
        let m': Exp.t =
          with_secondary(
            l.annotation.secondary,
            {
              ...p,
              term: Let(mp, mdef, lbody),
            },
          );
        let l': Exp.t =
          with_secondary(
            p.annotation.secondary,
            {
              ...l,
              term: Let(lp, ldef, m'),
            },
          );
        Some((p, l', Exp.rep_id(l)));
      | Let(mp, mdef, mbody)
          when
            same_node(mdef, c)
            && disjoint_names(l_names, pat_var_names(mp))
            && !names_mentioned(l_names, mbody) =>
        /* out of a def: above that line */
        let c': Exp.t =
          same_node(c, l)
            /* the def slot's occupant is now lbody, which brings its
               own lead; l's trailing run (before the outer in) stays
               with the def position */
            ? {
              let (cb, ca) = lbody.annotation.secondary;
              let (_, l_after) = l.annotation.secondary;
              with_secondary((cb, ca @ l_after), lbody);
            }
            : {
              let (_, after) = lbody.annotation.secondary;
              {
                ...c,
                term: Parens(with_secondary(([], after), lbody)),
              };
            };
        /* the pushed-down let starts a NEW line slot: synthesize its
           lead from the slot above (P's own lead), else a bare newline
           when the def was multiline, else stay inline */
        let (p_lead, p_after) = p.annotation.secondary;
        let (l_lead, _) = l.annotation.secondary;
        let sep = {
          let multiline =
            has_newline(l_lead)
            || has_newline(fst(lbody.annotation.secondary));
          switch (p_lead) {
          | [_, ..._] => sep_like(p_lead)
          | [] => multiline ? newline() : []
          };
        };
        let m': Exp.t =
          with_secondary(
            (sep, p_after),
            {
              ...p,
              term: Let(mp, c', mbody),
            },
          );
        let l': Exp.t =
          with_secondary(
            ([], []),
            {
              ...l,
              term: Let(lp, ldef, m'),
            },
          );
        Some((p, l', Exp.rep_id(l)));
      | Match(scrut, rules)
          when
            same_node(c, l)
            && rules
            |> List.exists(((rp, rb)) =>
                 same_node(rb, c)
                 && disjoint_names(l_names, pat_var_names(rp))
                 && !names_mentioned(pat_var_names(rp), ldef)
               )
            && !(
                 l_names
                 |> List.exists(x =>
                      free_in(
                        x,
                        replace_node(
                          ~at=Exp.rep_id(l),
                          ~with_=fresh(EmptyHole),
                          p,
                        ),
                      )
                    )
               ) =>
        /* out of an arm: evaluates unconditionally now */
        let rules' =
          rules
          |> List.map(((rp, rb)) =>
               same_node(rb, c)
                 ? (rp, occupy(l.annotation.secondary, lbody)) : (rp, rb)
             );
        /* the hoisted let takes the match's slot secondary; the
           match (now the let's body) gets a FRESH COPY of the lead —
           sharing the same secondary pieces in two nodes drops one
           copy downstream (duplicate piece ids) */
        let match': Exp.t =
          with_secondary(
            (sep_like(fst(p.annotation.secondary)), []),
            {
              ...p,
              term: Match(scrut, rules'),
            },
          );
        let l': Exp.t =
          with_secondary(
            p.annotation.secondary,
            {
              ...l,
              term: Let(lp, ldef, match'),
            },
          );
        Some((p, l', Exp.rep_id(l)));
      | If(cond_, t_, alt_)
          when
            same_node(c, l)
            && (same_node(t_, c) || same_node(alt_, c))
            && !(
                 l_names
                 |> List.exists(x =>
                      free_in(
                        x,
                        replace_node(
                          ~at=Exp.rep_id(l),
                          ~with_=fresh(EmptyHole),
                          p,
                        ),
                      )
                    )
               ) =>
        let sub = occupy(l.annotation.secondary, lbody);
        let if': Exp.t = {
          ...p,
          term:
            same_node(t_, c) ? If(cond_, sub, alt_) : If(cond_, t_, sub),
        };
        let l': Exp.t =
          with_secondary(
            p.annotation.secondary,
            {
              ...l,
              term: Let(lp, ldef, if'),
            },
          );
        Some((p, l', Exp.rep_id(l)));
      | Fun(fp, fbody, ft, fn)
          when
            same_node(fbody, c)
            && same_node(c, l)
            && disjoint_names(l_names, pat_var_names(fp))
            && !names_mentioned(pat_var_names(fp), ldef) =>
        /* out of a lambda: evaluates once instead of per call. The
           fun starts a NEW line slot; synthesize its lead like the
           def-exit case */
        let (p_lead, p_after) = p.annotation.secondary;
        let (l_lead, _) = l.annotation.secondary;
        let sep = {
          let multiline =
            has_newline(l_lead)
            || has_newline(fst(lbody.annotation.secondary));
          switch (p_lead) {
          | [_, ..._] => sep_like(p_lead)
          | [] => multiline ? newline() : []
          };
        };
        let fun': Exp.t =
          with_secondary(
            (sep, p_after),
            {
              ...p,
              term: Fun(fp, lbody, ft, fn),
            },
          );
        let l': Exp.t =
          with_secondary(
            ([], []),
            {
              ...l,
              term: Let(lp, ldef, fun'),
            },
          );
        Some((p, l', Exp.rep_id(l)));
      | Let(_)
      | Fun(_)
      | FixF(_) => None
      /* an arm body whose exit was gated above (pattern binders used
         in the def) must NOT fall into the generic case below — that
         path knows nothing about the crossed binders */
      | Match(_, rules)
          when rules |> List.exists(((_, rb)) => same_node(rb, c)) =>
        None
      /* binder-introducing bodies the generic case can't gate: Use
         imports names we can't enumerate here; the rest bind exp/typ
         names the moving def could reference. Dead press beats
         unbinding. */
      | Use(_)
      | TypFun(_)
      | Theorem(_)
      | Forall(_)
      | ModuleExp(_) => None
      /* a type binder IS enumerable: refuse only when the moving
         parts (pat annotations, def) mention the alias name */
      | TyAlias(tp, _, _)
          when
            mentions_typ_names(
              tpat_names(tp),
              fresh(Let(lp, ldef, fresh(EmptyHole))),
            ) =>
        None
      | _ =>
        /* generic tight position: g(let x = e in b) -> let x = e in
           g(b). No binder is crossed (binder-introducing bodies are
           handled above); gate on capture via free_in with l cut out */
        same_node(c, l)
        && !(
             l_names
             |> List.exists(x =>
                  free_in(
                    x,
                    replace_node(
                      ~at=Exp.rep_id(l),
                      ~with_=fresh(EmptyHole),
                      p,
                    ),
                  )
                )
           )
          ? {
            let p' =
              replace_node(
                ~at=Exp.rep_id(l),
                ~with_=occupy(l.annotation.secondary, lbody),
                p,
              );
            let l': Exp.t =
              with_secondary(
                ([], []),
                {
                  ...l,
                  term: Let(lp, ldef, p'),
                },
              );
            Some((p, l', Exp.rep_id(l)));
          }
          : None
      };
    | _ => None
    };
  };
};

/* one sink step: push the let into its body's head construct.
 * ~fixup: move the target region's TEXTUAL lead (which lives on
 * leaves) onto the inserted let — prints, so gating passes false and
 * discards the runs */
/* a def is a PROPER BLOCK iff its root (Parens-transparent) has line
   slots to descend into: internal let lines, a fun (params are
   morally a binder line; the body is a line slot), or a case/if
   (arm/branch bodies are line slots). Sinking steps INTO blocky defs
   (incremental descent); bare expressions have no rungs — Down feeds
   or inlines directly, restoring the one-press extract/inline
   inverse (andrew's bare-vs-blocky criterion, which extract's
   line-slot landing already encoded going up). */
let rec is_proper_block = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Parens(inner) => is_proper_block(inner)
  | Let(_)
  | Fun(_)
  | Match(_)
  | If(_) => true
  | _ => false
  };

/* the destination slot is EXACTLY a bare use of the binding: sinking
   there yields the pure wrapper `let x = d in x` — inline territory,
   so the sink yields (keeps Up/Down inverses at the elevator's
   bottom). Structural check only — no printing in gating paths. */
let rec is_bare_use = (names: list(string), e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(y) => List.mem(y, names)
  | Parens(inner) => is_bare_use(names, inner)
  | _ => false
  };

let sink_step = (~fixup: bool, l: Exp.t): option((Exp.t, Id.t)) => {
  let take_lead = (region: Exp.t): (list(Secondary.t), Exp.t) =>
    if (fixup) {
      let s = Slot.lead_of(region);
      (s.lead, Slot.drop(s, region));
    } else {
      (
        fst(region.annotation.secondary),
        with_secondary(([], snd(region.annotation.secondary)), region),
      );
    };
  /* LANDING-BLOCK (andrew): displaced slot content keeps — or gets —
     its own line, so Down keeps pointing down through every
     intermediate state. Taken lead had a newline: re-give a copy
     (the displaced first line keeps its line). Taken lead was
     inline: SYNTHESIZE a break at the sinking let's line indent + 2
     — but only in multiline contexts; one-line programs stay one
     line. The inverse on exit is automatic: hoist's occupy gives
     the vacated slot the departing let's own lead, so an
     inline-headed let's body rejoins the host line (bystander
     breaks belong to other constructs' leads and are never
     touched). */
  let relead = (taken: list(Secondary.t), region: Exp.t): Exp.t =>
    if (!fixup) {
      region;
    } else {
      let sep =
        has_newline(taken)
          ? sep_like(taken)
          : {
            let hs = sep_like(fst(l.annotation.secondary));
            has_newline(hs) ? hs @ space() @ space() : [];
          };
      if (sep == []) {
        region;
      } else {
        let (b, a) = region.annotation.secondary;
        {
          ...region,
          annotation: {
            ...region.annotation,
            secondary: (sep @ b, a),
          },
        };
      };
    };
  switch (IdTagged.term_of(l)) {
  | Let(lp, ldef, lbody) =>
    let l_names = pat_var_names(lp);
    switch (IdTagged.term_of(lbody)) {
    | Let(mp, mdef, mbody)
        when
          disjoint_names(l_names, pat_var_names(mp))
          && !names_mentioned(l_names, mdef)
          && !names_mentioned(pat_var_names(mp), ldef) =>
      let l': Exp.t =
        with_secondary(
          lbody.annotation.secondary,
          {
            ...l,
            term: Let(lp, ldef, mbody),
          },
        );
      let m': Exp.t =
        with_secondary(
          l.annotation.secondary,
          {
            ...lbody,
            term: Let(mp, mdef, l'),
          },
        );
      Some((m', Exp.rep_id(l)));
    | Let(mp, mdef, mbody)
        when
          disjoint_names(l_names, pat_var_names(mp))
          && names_mentioned(l_names, mdef)
          && !names_mentioned(l_names, mbody)
          && is_proper_block(mdef) =>
      /* into the def that solely uses it: pure scope-minimization
         (same evaluation, narrower scope) — the missing edge of the
         region graph; inverse of hoist-out-of-def */
      let (d_lead, mdef') = take_lead(mdef);
      /* a def gaining an internal let goes multiline (andrew): break
         before the sunk let and after its `in`, indented past the
         host line — unless the def already had its own layout */
      let host_sep = () => fixup ? sep_like(Slot.lead_of(lbody).lead) : [];
      let nesting = fixup && !has_newline(d_lead) && has_newline(host_sep());
      let mdef' =
        if (nesting) {
          let (b, a) = mdef'.annotation.secondary;
          {
            ...mdef',
            annotation: {
              ...mdef'.annotation,
              secondary: (host_sep() @ space() @ space() @ b, a),
            },
          };
        } else if (fixup && has_newline(d_lead)) {
          /* multiline block: the lead moved to the sunk let, so the
             displaced first line gets a fresh copy — else both lets
             land on one line and hoist/sink layouts oscillate */
          let (b, a) = mdef'.annotation.secondary;
          {
            ...mdef',
            annotation: {
              ...mdef'.annotation,
              secondary: (sep_like(d_lead) @ b, a),
            },
          };
        } else {
          mdef';
        };
      let l': Exp.t =
        with_secondary(
          (nesting ? host_sep() @ space() @ space() : d_lead, []),
          {
            ...l,
            term: Let(lp, ldef, mdef'),
          },
        );
      let m': Exp.t =
        with_secondary(
          l.annotation.secondary,
          {
            ...lbody,
            term: Let(mp, l', mbody),
          },
        );
      Some((m', Exp.rep_id(l)));
    | Fun(fp, fbody, ft, fn)
        when
          disjoint_names(l_names, pat_var_names(fp))
          && !names_mentioned(pat_var_names(fp), ldef)
          && !is_bare_use(l_names, fbody) =>
      /* into a lambda: evaluates per call */
      let (fb_lead, fbody') = take_lead(fbody);
      let fbody' = relead(fb_lead, fbody');
      let l': Exp.t =
        with_secondary(
          (fb_lead, []),
          {
            ...l,
            term: Let(lp, ldef, fbody'),
          },
        );
      let fun': Exp.t =
        with_secondary(
          l.annotation.secondary,
          {
            ...lbody,
            term: Fun(fp, l', ft, fn),
          },
        );
      Some((fun', Exp.rep_id(l)));
    | Match(scrut, rules) when !names_mentioned(l_names, scrut) =>
      /* into the single arm that uses the binding */
      switch (
        rules
        |> List.mapi((i, r) => (i, r))
        |> List.filter(((_, (_, b))) => names_mentioned(l_names, b))
      ) {
      | [(i, (rp, rb))]
          when
            disjoint_names(l_names, pat_var_names(rp))
            && !names_mentioned(pat_var_names(rp), ldef)
            && !is_bare_use(l_names, rb) =>
        let (rb_lead, rb') = take_lead(rb);
        let rb' = relead(rb_lead, rb');
        let l': Exp.t =
          with_secondary(
            (rb_lead, []),
            {
              ...l,
              term: Let(lp, ldef, rb'),
            },
          );
        let rules' = rules |> List.mapi((j, r) => j == i ? (rp, l') : r);
        let match': Exp.t =
          with_secondary(
            l.annotation.secondary,
            {
              ...lbody,
              term: Match(scrut, rules'),
            },
          );
        Some((match', Exp.rep_id(l)));
      | _ => None
      }
    | If(c, t, alt) when !names_mentioned(l_names, c) =>
      switch (
        names_mentioned(l_names, t) && !is_bare_use(l_names, t),
        names_mentioned(l_names, alt) && !is_bare_use(l_names, alt),
      ) {
      | (true, false) when !names_mentioned(l_names, alt) =>
        let (t_lead, t') = take_lead(t);
        let t' = relead(t_lead, t');
        let l': Exp.t =
          with_secondary(
            (t_lead, []),
            {
              ...l,
              term: Let(lp, ldef, t'),
            },
          );
        Some((
          with_secondary(
            l.annotation.secondary,
            {
              ...lbody,
              term: If(c, l', alt),
            },
          ),
          Exp.rep_id(l),
        ));
      | (false, true) when !names_mentioned(l_names, t) =>
        let (a_lead, alt') = take_lead(alt);
        let alt' = relead(a_lead, alt');
        let l': Exp.t =
          with_secondary(
            (a_lead, []),
            {
              ...l,
              term: Let(lp, ldef, alt'),
            },
          );
        Some((
          with_secondary(
            l.annotation.secondary,
            {
              ...lbody,
              term: If(c, t, l'),
            },
          ),
          Exp.rep_id(l),
        ));
      | _ => None
      }
    | _ => None
    };
  | _ => None
  };
};

let hoist_let_impl: impl = {
  label: "Hoist",
  tooltip: "Move this binding up one level",
  prepare: (~info_map as _, ~target, program) =>
    switch (find_path(~hit=hit_let(target), program)) {
    | Some(path) =>
      switch (hoist_step(~fixup=true, path)) {
      /* movement never parenthesizes, so no invocation oracle: the
         whole-program reparse cost ~0.5s per press on a few-page
         buffer. Reparse-safety is covered by the movement reparse
         tests in Test_Refactor instead. */
      | Some((pnode, result, focus)) =>
        rewrite_node(
          ~hit=same_node(pnode),
          ~rewrite=_ => Some((result, focus)),
          program,
        )
      | None => None
      }
    | None => None
    },
};

let sink_let_impl: impl = {
  label: "Sink",
  tooltip: "Move this binding down into the scope that uses it",
  prepare: (~info_map as _, ~target, program) =>
    switch (
      find_path(~hit=hit_let(target), program)
      |> Option.map(path => List.nth(path, List.length(path) - 1))
    ) {
    | Some(l) =>
      switch (sink_step(~fixup=true, l)) {
      | Some((result, focus)) =>
        rewrite_node(
          ~hit=same_node(l),
          ~rewrite=_ => Some((result, focus)),
          program,
        )
      | None => None
      }
    | None => None
    },
};

let fresh_names = (k: int, program: Exp.t): list(string) => {
  let used = ref(used_names(program));
  List.init(
    k,
    _ => {
      let rec pick = n => {
        let c = n == 0 ? "x" : "x" ++ string_of_int(n);
        List.mem(c, used^) ? pick(n + 1) : c;
      };
      let c = pick(0);
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

let eta_expand_impl: impl = {
  label: "Eta-Expand",
  tooltip: "Wrap this function value as fun x -> f(x)",
  prepare: (~info_map, ~target, program) => {
    let attempt = (~parens: bool) =>
      rewrite_node(
        ~hit=hit_node(target),
        ~rewrite=
          e =>
            switch (IdTagged.term_of(e)) {
            | Fun(_)
            | Parens(_) => None
            | _ =>
              switch (exp_ty(~info_map, e)) {
              | Some(ty) =>
                switch (IdTagged.term_of(ty)) {
                | Arrow(a, _) =>
                  let names = fresh_names(arrow_arity(a), program);
                  let sep_lead = i => i == 0 ? ([], []) : (space(), []);
                  let var_pats =
                    names
                    |> List.mapi((i, n) =>
                         {
                           ...fresh_pat(Var(n)),
                           annotation: {
                             ...IdTagged.IdTag.mk_internal([Id.mk()]),
                             secondary: sep_lead(i),
                           },
                         }
                       );
                  let param: Pat.t =
                    switch (var_pats) {
                    | [v] => pad(v)
                    | vs => pad(fresh_pat(Parens(fresh_pat(Tuple(vs)))))
                    };
                  let args: Exp.t =
                    switch (names) {
                    | [n] => fresh(Var(n))
                    | ns =>
                      fresh(
                        Tuple(
                          ns
                          |> List.mapi((i, n) =>
                               {
                                 ...fresh(Var(n)),
                                 annotation: {
                                   ...IdTagged.IdTag.mk_internal([Id.mk()]),
                                   secondary: sep_lead(i),
                                 },
                               }
                             ),
                        ),
                      )
                    };
                  let fn = strip_boundaries_keep_comments(e);
                  let fn = needs_parens(fn) ? fresh(Parens(fn)) : fn;
                  let body = {
                    ...fresh(Ap(Forward, fn, args)),
                    annotation: {
                      ...IdTagged.IdTag.mk_internal([Id.mk()]),
                      secondary: (space(), []),
                    },
                  };
                  let lam = fresh(Fun(param, body, None, None));
                  Some((
                    parens ? fresh(Parens(lam)) : lam,
                    Pat.rep_id(List.hd(var_pats)),
                  ));
                | _ => None
                }
              | None => None
              }
            },
        program,
      );
    /* static: bare iff the node sits in a delimiter-bounded slot
       (the lambda can't leak past its region); otherwise parens */
    let bounded =
      switch (find_hit(~hit=hit_node(target), program)) {
      | Some(e) =>
        !same_node(bounded_region(Exp.rep_id(e), program), program)
      | None => false
      };
    attempt(~parens=!bounded);
  },
};

/* is this term already literal-value syntax? (also the gate for what
 * evaluation results are spliceable) */
let rec is_value_literal = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Atom(_)
  | Constructor(_, _) => true
  | ListLit(xs)
  | Tuple(xs) => xs |> List.for_all(is_value_literal)
  | TupLabel(_, x) => is_value_literal(x)
  | Parens(x) => is_value_literal(x)
  | Ap(Forward, f, arg) =>
    switch (IdTagged.term_of(f)) {
    | Constructor(_, _) => is_value_literal(arg)
    | _ => false
    }
  | _ => false
  };

/* evaluator-built values carry no secondary; SpaceNormalize can't
 * space commas (self-delimiting), so do it here */
let space_commas = (e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let spaced = xs =>
          xs
          |> List.mapi((i, x: Exp.t) =>
               i == 0
                 ? x
                 : {
                   ...x,
                   annotation: {
                     ...x.annotation,
                     secondary: (
                       space() @ fst(x.annotation.secondary),
                       snd(x.annotation.secondary),
                     ),
                   },
                 }
             );
        let e: Exp.t =
          switch (IdTagged.term_of(e)) {
          | ListLit(xs) => {
              ...e,
              term: ListLit(spaced(xs)),
            }
          | Tuple(xs) => {
              ...e,
              term: Tuple(spaced(xs)),
            }
          | _ => e
          };
        cont(e);
      },
    e,
  );

let reduce_prepare =
    (~hit: Exp.t => bool, ~build: Exp.t => option((Exp.t, Id.t)), program) =>
  switch (find_hit(~hit, program)) {
  | None => None
  | Some(e) =>
    let parens = {
      /* region == program is fine: the oracle runs on the whole
         program (blanket parens here wrapped every tail-position
         reduction and compounded per step) */
      let region = bounded_region(Exp.rep_id(e), program);
      switch (build(e)) {
      | Some((bare, _)) =>
        let candidate = replace_node(~at=Exp.rep_id(e), ~with_=bare, region);
        !reparses_region(candidate);
      | None => true
      };
    };
    rewrite_node(
      ~hit,
      ~rewrite=
        e =>
          build(e)
          |> Option.map(((built, focus)) =>
               (parens ? fresh(Parens(built)) : built, focus)
             ),
      program,
    );
  };

let evaluate_in_place_impl: impl = {
  label: "Evaluate",
  tooltip: "Replace this expression with its value",
  prepare: (~info_map as _, ~target, program) => {
    let hit = (e: Exp.t) => hit_node(target, e) && !is_value_literal(e);
    let build = (e: Exp.t) => {
      /* elaborate the subterm standalone: free vars elaborate to
         holes, evaluate to indet, and fail the value gate — so
         closedness needs no separate check here */
      let elab =
        CachedStatics.init_from_term(
          ~settings=CoreSettings.on,
          ~is_dynamic_term=false,
          e,
        ).
          elaborated;
      switch (
        Evaluator.evaluate_and_limit(
          ~step_limit=10000,
          ~env=Builtins.env_init,
          elab,
        )
      ) {
      | Completed((v, _)) when is_value_literal(v) =>
        let v = space_commas(v);
        Some((v, Exp.rep_id(v)));
      | _ => None
      };
    };
    /* parens via the reparse oracle (the old static boundedness rule
       didn't know if-cond and similar slots — `if (false) then`) */
    reduce_prepare(~hit, ~build, program);
  },
};

/* === Bind Argument (beta via let-intro) ===
 * `(fun p -> body)(arg)` becomes `let p = arg in body`: a pure
 * structural rotation — nothing is substituted and no variable
 * changes scope, so no capture can arise. Composed with the existing
 * let toolkit (feed / inline / evaluate) this is beta reduction,
 * decomposed; the binder, argument, and body all keep their ids
 * (persistent elements — they travel, nothing is reborn). */

let beta_parts = (e: Exp.t): option((Pat.t, Exp.t, Exp.t)) =>
  switch (IdTagged.term_of(e)) {
  | Ap(Forward, f, arg) =>
    let rec fn = (f: Exp.t) =>
      switch (IdTagged.term_of(f)) {
      | Parens(inner) => fn(inner)
      | Fun(p, body, _, _) => Some((p, body))
      | _ => None
      };
    fn(f) |> Option.map(((p, body)) => (p, arg, body));
  | _ => None
  };

/* targetable at the ap tile, the fn's parens, or the fun's own
   delimiters */
let hit_beta = (target: Id.t, e: Exp.t): bool =>
  beta_parts(e) != None
  && (
    hit_node(target, e)
    || (
      switch (IdTagged.term_of(e)) {
      | Ap(_, f, _) =>
        let rec fn_ids = (f: Exp.t) =>
          IdTagged.ids(f)
          @ (
            switch (IdTagged.term_of(f)) {
            | Parens(inner) => fn_ids(inner)
            | _ => []
            }
          );
        List.mem(target, fn_ids(f));
      | _ => false
      }
    )
  );

let ap_to_let_impl: impl = {
  label: "Bind argument",
  tooltip: "Rewrite this application of a function literal as a let binding its argument",
  prepare: (~info_map as _, ~target, program) => {
    let build = (e: Exp.t): option((Exp.t, Id.t)) =>
      beta_parts(e)
      |> Option.map(((p, arg, body)) => {
           let p = pad(p);
           let def = pad(arg |> strip_leading |> strip_trailing);
           /* the body keeps its own lead: an inline fun body lands
              inline after the in; a multiline one keeps its break */
           (fresh(Let(p, def, body)), Pat.rep_id(p));
         });
    /* parens per the feed policy: region-scoped reparse oracle;
       conservative parens when no bounded region (a root-level ap
       can have right siblings the bare let would absorb) */
    switch (find_hit(~hit=hit_beta(target), program)) {
    | None => None
    | Some(e) =>
      let parens = {
        /* no bounded ancestor just means the region IS the program:
           run the oracle on it (one whole-program print+reparse per
           invocation). The old blanket-parens rule here wrapped
           every reduction at the program TAIL — exactly where
           stepping happens — and the parens compounded per step
           (andrew). */
        let region = bounded_region(Exp.rep_id(e), program);
        switch (build(e)) {
        | Some((bare, _)) =>
          let candidate =
            replace_node(~at=Exp.rep_id(e), ~with_=bare, region);
          !reparses_region(candidate);
        | None => true
        };
      };
      rewrite_node(
        ~hit=hit_beta(target),
        ~rewrite=
          e =>
            build(e)
            |> Option.map(((let_node, focus)) =>
                 (parens ? fresh(Parens(let_node)) : let_node, focus)
               ),
        program,
      );
    };
  },
};

/* === Beta-reduce ===
 * One reduction step: `(fun x -> b)(a)` becomes `b` with `a`
 * substituted for `x` — the ap-to-let rotation composed with inline,
 * so capture renaming, per-occurrence parens, and whitespace policy
 * are all inherited. Var-pattern parameters only (inline's own
 * gate); tuple parameters keep Bind argument + tuple-let tooling. */

let beta_reduce_impl: impl = {
  label: "Beta-reduce",
  tooltip: "Apply this function literal: substitute the argument for its parameter",
  prepare: (~info_map, ~target, program) =>
    switch (find_hit(~hit=hit_beta(target), program)) {
    | None => None
    | Some(e) =>
      switch (beta_parts(e)) {
      | Some((p, _, _)) when let_head_name(p) != None =>
        switch (ap_to_let_impl.prepare(~info_map, ~target, program)) {
        | Some((prog', binder_id)) =>
          /* info_map is stale for prog' — inline at the binder is
             syntactic (name, occurrences, subst), so it doesn't
             matter */
          inline_let_impl.prepare(~info_map, ~target=binder_id, prog')
        | None => None
        }
      | _ => None
      }
    },
};

/* === Take branch / Take arm (if- and case-reduction) ===
 * Syntactic reduction steps that work on OPEN branches (Evaluate
 * can't): a decided literal condition commits the if to its live
 * branch; a value-literal scrutinee matching an arm commits the case
 * to that arm, binding its pattern variables via the same let-intro
 * decomposition as beta (nested lets, ids preserved). */

let rec lit_bool = (e: Exp.t): option(bool) =>
  switch (IdTagged.term_of(e)) {
  | Parens(inner) => lit_bool(inner)
  | Atom(Bool(b)) => Some(b)
  | _ => None
  };

/* three-valued syntactic matcher: an Unknown ABOVE the matching arm
   gates the whole reduction (we can't soundly skip an arm we can't
   decide) */
type pat_match =
  | Matched(list((Pat.t, Exp.t)))
  | NoMatch
  | Unknown;

let rec match_value = (p: Pat.t, v: Exp.t): pat_match => {
  let pat_ctor = (p: Pat.t) =>
    switch (IdTagged.term_of(p)) {
    | Constructor(name, _) => Some(name)
    | _ => None
    };
  let exp_ctor = (v: Exp.t) =>
    switch (IdTagged.term_of(v)) {
    | Constructor(name, _) => Some(name)
    | _ => None
    };
  let elementwise = (ps, vs) =>
    List.length(ps) != List.length(vs)
      ? NoMatch
      : List.combine(ps, vs)
        |> List.fold_left(
             (acc, (p, v)) =>
               switch (acc) {
               | Matched(bs) =>
                 switch (match_value(p, v)) {
                 | Matched(bs') => Matched(bs @ bs')
                 | r => r
                 }
               | r => r
               },
             Matched([]),
           );
  switch (IdTagged.term_of(p), IdTagged.term_of(v)) {
  | (Parens(p'), _) => match_value(p', v)
  | (_, Parens(v')) => match_value(p, v')
  /* labels are positional wrappers (PatternMatch unwraps without
     comparing names) — recurse through either side */
  | (TupLabel(_, p'), _) => match_value(p', v)
  | (_, TupLabel(_, v')) => match_value(p, v')
  | (Wild, _) => Matched([])
  | (Var(_), _) => Matched([(p, v)])
  | (Atom(a), Atom(b)) => a == b ? Matched([]) : NoMatch
  | (Constructor(cp, _), Constructor(cv, _)) =>
    cp == cv ? Matched([]) : NoMatch
  | (Constructor(_), Ap(_))
  | (Ap(_), Constructor(_)) => NoMatch
  | (Ap(pf, parg), Ap(Forward, vf, varg)) =>
    switch (pat_ctor(pf), exp_ctor(vf)) {
    | (Some(cp), Some(cv)) => cp == cv ? match_value(parg, varg) : NoMatch
    | _ => Unknown
    }
  | (Tuple(ps), Tuple(vs))
  | (ListLit(ps), ListLit(vs)) => elementwise(ps, vs)
  /* cons vs list literal: split head/tail. The tail REUSES the
     original list's node (same brackets, same ids) around the
     surviving elements — the scrutinee dies in the reduction, so
     nothing duplicates, and the brackets TRAVEL instead of being
     reborn (andrew: reuse when possible) */
  | (Cons(ph, pt), ListLit(vs)) =>
    switch (vs) {
    | [] => NoMatch
    | [v0, ...vrest] =>
      let tail: Exp.t = {
        ...v,
        term:
          ListLit(
            vrest |> List.mapi((i, el) => i == 0 ? strip_leading(el) : el),
          ),
      };
      switch (match_value(ph, v0)) {
      | Matched(bs) =>
        switch (match_value(pt, tail)) {
        | Matched(bs') => Matched(bs @ bs')
        | r => r
        }
      | r => r
      };
    }
  | _ => Unknown
  };
};

/* the first arm the scrutinee decidably matches */
let pick_arm =
    (scrut: Exp.t, rules: list((Pat.t, Exp.t)))
    : option((list((Pat.t, Exp.t)), Exp.t)) => {
  let rec go = rules =>
    switch (rules) {
    | [] => None
    | [(p, body), ...rest] =>
      switch (match_value(p, scrut)) {
      | Matched(bs) => Some((bs, body))
      | NoMatch => go(rest)
      | Unknown => None
      }
    };
  go(rules);
};

/* nested lets over the bindings; each binder and value keeps its ids
   (they travel), the final body keeps its own lead */
let rec wrap_bindings = (bs: list((Pat.t, Exp.t)), body: Exp.t): Exp.t =>
  switch (bs) {
  | [] => body
  | [(x, v), ...rest] =>
    let inner = wrap_bindings(rest, body);
    /* lead only: the nested let follows the outer `in`; its right
       edge is the body's own end */
    let inner = rest == [] ? inner : with_secondary((space(), []), inner);
    fresh(Let(pad(x), pad(v |> strip_leading |> strip_trailing), inner));
  };

/* shared scaffold: replace `e` with `built` under the feed parens
   policy (region-scoped reparse; conservative at an unbounded root) */
let reduce_if_impl: impl = {
  label: "Take branch",
  tooltip: "The condition is decided: replace the if with the live branch",
  prepare: (~info_map as _, ~target, program) => {
    let hit = (e: Exp.t) =>
      hit_node(target, e)
      && (
        switch (IdTagged.term_of(e)) {
        | If(c, _, _) => lit_bool(c) != None
        | _ => false
        }
      );
    let build = (e: Exp.t) =>
      switch (IdTagged.term_of(e)) {
      | If(c, t, alt) =>
        lit_bool(c)
        |> Option.map(b => {
             let branch = (b ? t : alt) |> strip_leading |> strip_trailing;
             (branch, Exp.rep_id(branch));
           })
      | _ => None
      };
    reduce_prepare(~hit, ~build, program);
  },
};

let case_hit = (~target, e: Exp.t) =>
  hit_node(target, e)
  && (
    switch (IdTagged.term_of(e)) {
    | Match(scrut, rules) =>
      is_value_literal(scrut) && pick_arm(scrut, rules) != None
    | _ => false
    }
  );

/* the let-intro form: arm bindings become nested lets (the two-step
   sibling, like Bind argument for beta) */
let case_to_lets = (~target, program): option((Exp.t, Id.t)) => {
  let build = (e: Exp.t) =>
    switch (IdTagged.term_of(e)) {
    | Match(scrut, rules) =>
      pick_arm(scrut, rules)
      |> Option.map(((bs, body)) => {
           let body = body |> strip_leading |> strip_trailing;
           let built = wrap_bindings(bs, body);
           (built, Exp.rep_id(built));
         })
    | _ => None
    };
  reduce_prepare(~hit=case_hit(~target), ~build, program);
};

let bind_arm_impl: impl = {
  label: "Bind arm",
  tooltip: "Replace the case with its matching arm, binding the pattern via lets",
  prepare: (~info_map as _, ~target, program) =>
    /* only when there is something to bind — otherwise identical to
       Take arm and the menu would stutter */
    switch (find_hit(~hit=case_hit(~target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Match(scrut, rules) =>
        switch (pick_arm(scrut, rules)) {
        | Some(([_, ..._], _)) => case_to_lets(~target, program)
        | _ => None
        }
      | _ => None
      }
    | None => None
    },
};

let reduce_case_impl: impl = {
  label: "Take arm",
  tooltip: "The scrutinee is a value: replace the case with its matching arm, substituting the pattern",
  prepare: (~info_map, ~target, program) =>
    /* the let-intro form composed with inline per binder — capture
       renaming and whitespace policy inherited, like Beta-reduce */
    switch (find_hit(~hit=case_hit(~target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Match(scrut, rules) =>
        switch (pick_arm(scrut, rules)) {
        | Some((bs, _)) =>
          let binder_ids = bs |> List.map(((x, _)) => Pat.rep_id(x));
          binder_ids
          |> List.fold_left(
               (acc, bid) =>
                 Option.bind(acc, ((prog, _)) =>
                   inline_let_impl.prepare(~info_map, ~target=bid, prog)
                 ),
               case_to_lets(~target, program),
             );
        | None => None
        }
      | _ => None
      }
    | None => None
    },
};

/* === Split let ===
 * Destructure a pattern let over a structurally matching def:
 * `let (a, b) = (e1, e2) in body` -> `let a = e1 in let b = e2 in
 * body`. Components stay in order (cost/effect order preserved);
 * wildcard components drop like unused lets; the nested lets share
 * the original's rightward extent, so no parens question arises.
 * Var-headed lets are Inline's territory, not Split's. */

let split_let_impl: impl = {
  label: "Split let",
  tooltip: "Destructure this pattern binding into one let per variable",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_let(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Let(p, def, body) when let_head_name(p) == None =>
            switch (match_value(p, def)) {
            | Matched(bs) =>
              let built = wrap_bindings(bs, body);
              let focus =
                switch (bs) {
                | [(x, _), ..._] => Pat.rep_id(x)
                | [] => Exp.rep_id(built)
                };
              Some((built, focus));
            | _ => None
            }
          | _ => None
          },
      program,
    ),
};

/* === Expand Wildcard ===
 * Replace a `_` arm with one arm per unhandled variant of the
 * scrutinee's (normalized) sum type, each with a copy of the wildcard
 * arm's body — meaning-preserving by construction. Targeted at the
 * `_` token itself. */

let handled_ctor = (p: Pat.t): option(string) => {
  let rec go = (p: Pat.t) =>
    switch (IdTagged.term_of(p)) {
    | Constructor(name, _) => Some(name)
    | Ap(f, _) => go(f)
    | Parens(inner) => go(inner)
    | _ => None
    };
  go(p);
};

let wildcard_expansion =
    (~info_map: Statics.Map.t, ~target: Id.t, e: Exp.t)
    : option((int, list((string, bool)))) =>
  switch (IdTagged.term_of(e)) {
  | Match(scrut, rules) =>
    let wild_idx =
      rules
      |> List.mapi((i, (p, _)) => (i, p))
      |> List.find_opt(((_, p: Pat.t)) =>
           switch (IdTagged.term_of(p)) {
           | Wild => List.mem(target, IdTagged.ids(p))
           | _ => false
           }
         );
    switch (wild_idx) {
    | None => None
    | Some((i, _)) =>
      switch (Id.Map.find_opt(Exp.rep_id(scrut), info_map)) {
      | Some(InfoExp({ty, ctx, _})) =>
        switch (IdTagged.term_of(Typ.normalize(ctx, ty))) {
        | Sum(variants) =>
          let handled =
            rules
            |> List.filteri((j, _) => j != i)
            |> List.filter_map(((p, _)) => handled_ctor(p));
          let missing =
            variants
            |> List.filter_map((v: ConstructorMap.variant(Typ.t)) =>
                 switch (v) {
                 | Variant(name, _, arg) when !List.mem(name, handled) =>
                   Some((name, arg != None))
                 | _ => None
                 }
               );
          missing == [] ? None : Some((i, missing));
        | _ => None
        }
      | _ => None
      }
    };
  | _ => None
  };

let expand_wildcard_impl: impl = {
  label: "Expand _",
  tooltip: "Replace this _ with the unhandled constructors",
  prepare: (~info_map, ~target, program) =>
    rewrite_node(
      ~hit=hit_match_pat(target),
      ~rewrite=
        e =>
          switch (wildcard_expansion(~info_map, ~target, e)) {
          | Some((i, missing)) =>
            switch (IdTagged.term_of(e)) {
            | Match(scrut, rules) =>
              let (wild_pat, wild_body) = List.nth(rules, i);
              let ctor_pat = ((name, has_arg)) => {
                let c: Pat.t = fresh_pat(Constructor(name, None));
                has_arg ? fresh_pat(Ap(c, fresh_pat(Wild))) : c;
              };
              let new_rules =
                missing
                |> List.mapi((k, m) => {
                     let p: Pat.t =
                       k == 0
                         ? with_secondary_pat(
                             wild_pat.annotation.secondary,
                             ctor_pat(m),
                           )
                         : pad(ctor_pat(m));
                     let b = k == 0 ? wild_body : refresh_ids(wild_body);
                     (p, b);
                   });
              let rules' =
                rules
                |> List.mapi((j, r) => j == i ? new_rules : [r])
                |> List.concat;
              Some((
                {
                  ...e,
                  term: Match(scrut, rules'),
                },
                Pat.rep_id(fst(List.hd(new_rules))),
              ));
            | _ => None
            }
          | None => None
          },
      program,
    ),
};

/* === Swap Parameters ===
 * Swap adjacent params i, i+1 at the definition and in every call
 * site's argument tuple. Runs stay with POSITIONS (slot principle):
 * swapped elements exchange leads. ~fixup as in hoist/sink. */

let swap_exp_items = (~fixup: bool, i: int, items: list(Exp.t)): list(Exp.t) => {
  let a = List.nth(items, i);
  let b = List.nth(items, i + 1);
  let (a', b') =
    if (fixup) {
      let sa = Slot.lead_of(a);
      let sb = Slot.lead_of(b);
      let a0 = Slot.drop(sa, a);
      let b0 = Slot.drop(sb, b);
      (Slot.give(sa, b0), Slot.give(sb, a0));
    } else {
      (
        with_secondary(a.annotation.secondary, b),
        with_secondary(b.annotation.secondary, a),
      );
    };
  items
  |> List.mapi((j, x) =>
       if (j == i) {
         a';
       } else if (j == i + 1) {
         b';
       } else {
         x;
       }
     );
};

let swap_pat_items = (i: int, items: list(Pat.t)): list(Pat.t) => {
  let a = List.nth(items, i);
  let b = List.nth(items, i + 1);
  /* slot-wise (leads), as swap_exp_items: node-level exchange stacks
     runs on compound pats whose lead lives on a leaf */
  let sa = pat_slot_lead(a);
  let sb = pat_slot_lead(b);
  let a' = pat_slot_give(sa, pat_slot_drop(sb, b));
  let b' = pat_slot_give(sb, pat_slot_drop(sa, a));
  items
  |> List.mapi((j, x) =>
       if (j == i) {
         a';
       } else if (j == i + 1) {
         b';
       } else {
         x;
       }
     );
};

let swap_typ_items = (i: int, items: list(Typ.t)): list(Typ.t) => {
  let a = List.nth(items, i);
  let b = List.nth(items, i + 1);
  let sa = typ_slot_lead(a);
  let sb = typ_slot_lead(b);
  let a' = typ_slot_give(sa, typ_slot_drop(sb, b));
  let b' = typ_slot_give(sb, typ_slot_drop(sa, a));
  items
  |> List.mapi((j, x) =>
       if (j == i) {
         a';
       } else if (j == i + 1) {
         b';
       } else {
         x;
       }
     );
};

/* swap args at every unshadowed call of x; a bare use or a call whose
 * arg isn't a wide-enough tuple defeats the transform */
let swap_call_args =
    (
      ~fixup: bool,
      ~bare_use: ref(bool),
      ~ok: ref(bool),
      i: int,
      x: string,
      e: Exp.t,
    )
    : Exp.t =>
  map_unshadowed(
    ~skip=x,
    ~f_var=
      e' =>
        switch (IdTagged.term_of(e')) {
        | Var(z) when z == x =>
          bare_use := true;
          Some(e');
        | _ => None
        },
    ~f_ap=
      (ap, fn, arg, go) =>
        if (is_var_named(x, fn)) {
          switch (IdTagged.term_of(arg)) {
          | Tuple(items) when List.length(items) > i + 1 =>
            let items = items |> List.map(go);
            let arg': Exp.t = {
              ...arg,
              term: Tuple(swap_exp_items(~fixup, i, items)),
            };
            Some({
              ...ap,
              term: Ap(Forward, fn, arg'),
            });
          | _ =>
            ok := false;
            Some(ap);
          };
        } else {
          None;
        },
    e,
  );

let swap_fun_pat = (i: int, fp: Pat.t): option(Pat.t) =>
  switch (IdTagged.term_of(fp)) {
  | Parens(inner) =>
    switch (IdTagged.term_of(inner)) {
    | Tuple(items) when List.length(items) > i + 1 =>
      Some({
        ...fp,
        term:
          Parens({
            ...inner,
            term: Tuple(swap_pat_items(i, items)),
          }),
      })
    | _ => None
    }
  | Tuple(items) when List.length(items) > i + 1 =>
    Some({
      ...fp,
      term: Tuple(swap_pat_items(i, items)),
    })
  | _ => None
  };

let swap_arrow_ann = (i: int, ann: Typ.t): option(Typ.t) =>
  switch (IdTagged.term_of(ann)) {
  | Arrow(a, b) =>
    let swap_in = (a: Typ.t): option(Typ.t) =>
      switch (IdTagged.term_of(a)) {
      | Parens(inner) =>
        switch (IdTagged.term_of(inner)) {
        | Prod(items) when List.length(items) > i + 1 =>
          Some({
            ...a,
            term:
              Parens({
                ...inner,
                term: Prod(swap_typ_items(i, items)),
              }),
          })
        | _ => None
        }
      | Prod(items) when List.length(items) > i + 1 =>
        Some({
          ...a,
          term: Prod(swap_typ_items(i, items)),
        })
      | _ => None
      };
    swap_in(a)
    |> Option.map((a': Typ.t): Typ.t =>
         {
           ...ann,
           term: Arrow(a', b),
         }
       );
  | _ => None
  };

let swap_params_rewrite =
    (~fixup: bool, i: int, e: Exp.t): option((Exp.t, Id.t)) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, def, body) =>
    let bare_use = ref(false);
    let ok = ref(true);
    let pieces: option((string, Pat.t, Exp.t)) =
      switch (sugar_fn_name(p)) {
      | Some(f) =>
        let rec swap_in_pat = (p: Pat.t): option(Pat.t) =>
          switch (IdTagged.term_of(p)) {
          | Ap(fv, argp) =>
            switch (IdTagged.term_of(argp)) {
            | Tuple(items) when List.length(items) > i + 1 =>
              Some({
                ...p,
                term:
                  Ap(
                    fv,
                    {
                      ...argp,
                      term: Tuple(swap_pat_items(i, items)),
                    },
                  ),
              })
            | _ => None
            }
          | Asc(inner, ann) =>
            swap_in_pat(inner)
            |> Option.map((inner': Pat.t): Pat.t =>
                 {
                   ...p,
                   term: Asc(inner', ann),
                 }
               )
          | _ => None
          };
        swap_in_pat(p)
        |> Option.map(p' =>
             (f, p', swap_call_args(~fixup, ~bare_use, ~ok, i, f, def))
           );
      | None =>
        switch (let_head_name(p), IdTagged.term_of(def)) {
        | (Some(f), Fun(fp, fbody, ft, fn)) =>
          let p': option(Pat.t) =
            switch (IdTagged.term_of(p)) {
            | Var(_) => Some(p)
            | Asc(inner, ann) =>
              swap_arrow_ann(i, ann)
              |> Option.map((ann': Typ.t): Pat.t =>
                   {
                     ...p,
                     term: Asc(inner, ann'),
                   }
                 )
            | _ => None
            };
          switch (p', swap_fun_pat(i, fp)) {
          | (Some(p'), Some(fp')) =>
            let fbody' =
              binds(f, fp)
                ? fbody : swap_call_args(~fixup, ~bare_use, ~ok, i, f, fbody);
            Some((
              f,
              p',
              {
                ...def,
                term: Fun(fp', fbody', ft, fn),
              },
            ));
          | _ => None
          };
        | _ => None
        }
      };
    switch (pieces) {
    | Some((f, p', def')) =>
      let body' = swap_call_args(~fixup, ~bare_use, ~ok, i, f, body);
      ok^ && ! bare_use^
        ? Some((
            {
              ...e,
              term: Let(p', def', body'),
            },
            Exp.rep_id(e),
          ))
        : None;
    | None => None
    };
  | _ => None
  };

let swap_param_names = (e: Exp.t): list(string) => {
  let items_of = (p: Pat.t): list(Pat.t) => {
    let rec go = (p: Pat.t) =>
      switch (IdTagged.term_of(p)) {
      | Parens(inner) => go(inner)
      | Tuple(items) => items
      | _ => []
      };
    go(p);
  };
  switch (IdTagged.term_of(e)) {
  | Let(p, def, _) =>
    let rec sugar_items = (p: Pat.t) =>
      switch (IdTagged.term_of(p)) {
      | Ap(_, argp) => items_of(argp)
      | Asc(inner, _) => sugar_items(inner)
      | _ => []
      };
    let items =
      switch (sugar_items(p), IdTagged.term_of(def)) {
      | ([_, ..._] as xs, _) => xs
      | ([], Fun(fp, _, _, _)) => items_of(fp)
      | _ => []
      };
    items
    |> List.mapi((k, it) =>
         switch (var_pat_name(it)) {
         | Some(n) => n
         | None => "#" ++ string_of_int(k + 1)
         }
       );
  | _ => []
  };
};

/* === Remove Unused Parameter ===
 * Targeted at the param var token itself (binder-token affordance,
 * like rename). Drops the param from the pattern (and the
 * annotation's arrow Prod) and the argument at that position from
 * every call site. Gated: param unused in the body (syntactic
 * free_in — Hazel infers nothing from usage, so this is exact),
 * n >= 2 params, every call a matching-width tuple, no bare uses. */

let drop_exp_item = (i: int, items: list(Exp.t)): list(Exp.t) => {
  let items = items |> List.filteri((j, _) => j != i);
  /* a new first element takes over the old tight-lead slot */
  i == 0
    ? switch (items) {
      | [x, ...rest] => [with_secondary(([], []), x), ...rest]
      | [] => []
      }
    : items;
};

let drop_pat_item = (i: int, items: list(Pat.t)): list(Pat.t) => {
  let items = items |> List.filteri((j, _) => j != i);
  i == 0
    ? switch (items) {
      | [x, ...rest] => [with_secondary_pat(([], []), x), ...rest]
      | [] => []
      }
    : items;
};

let drop_typ_item = (i: int, items: list(Typ.t)): list(Typ.t) => {
  let items = items |> List.filteri((j, _) => j != i);
  i == 0
    ? switch (items) {
      | [x, ...rest] => [with_secondary_typ(([], []), x), ...rest]
      | [] => []
      }
    : items;
};

/* param items of a let-bound fn (sugar arg or fun pat), through
 * parens/ascriptions */
let param_items = (e: Exp.t): list(Pat.t) => {
  let tuple_items = (p: Pat.t): list(Pat.t) => {
    let rec go = (p: Pat.t) =>
      switch (IdTagged.term_of(p)) {
      | Parens(inner) => go(inner)
      | Tuple(items) => items
      | _ => []
      };
    go(p);
  };
  switch (IdTagged.term_of(e)) {
  | Let(p, def, _) =>
    let rec sugar = (p: Pat.t) =>
      switch (IdTagged.term_of(p)) {
      | Ap(_, argp) => tuple_items(argp)
      | Asc(inner, _) => sugar(inner)
      | _ => []
      };
    switch (sugar(p), IdTagged.term_of(def)) {
    | ([_, ..._] as xs, _) => xs
    | ([], Fun(fp, _, _, _)) => tuple_items(fp)
    | _ => []
    };
  | _ => []
  };
};

/* === Swap Arms ===
 * Swap adjacent case arms i, i+1 (pat and body move together; runs
 * stay with POSITIONS, as in Swap Params). Only offered when the two
 * patterns are provably disjoint: arm order is match priority, so
 * reordering overlapping arms changes meaning. */

let rec pats_disjoint = (a: Pat.t, b: Pat.t): bool => {
  let rec strip = (p: Pat.t) =>
    switch (IdTagged.term_of(p)) {
    | Parens(inner) => strip(inner)
    | _ => p
    };
  let (a, b) = (strip(a), strip(b));
  let ctor_head = (p: Pat.t) =>
    switch (IdTagged.term_of(p)) {
    | Constructor(n, _) => Some((n, None))
    | Ap(f, arg) =>
      switch (IdTagged.term_of(f)) {
      | Constructor(n, _) => Some((n, Some(arg)))
      | _ => None
      }
    | _ => None
    };
  switch (IdTagged.term_of(a), IdTagged.term_of(b)) {
  /* two distinct atoms (literals) can't match the same value */
  | (Atom(x), Atom(y)) => x != y
  | (Tuple(xs), Tuple(ys)) when List.length(xs) == List.length(ys) =>
    List.exists2(pats_disjoint, xs, ys)
  | _ =>
    switch (ctor_head(a), ctor_head(b)) {
    | (Some((n, _)), Some((m, _))) when n != m => true
    | (Some((n, Some(x))), Some((m, Some(y)))) when n == m =>
      pats_disjoint(x, y)
    | _ => false
    }
  };
};

/* the arm's slot |/=> delimiter ids: Match.ids = [case/end tile id,
 * ...rule tile ids], tail-aligned with the rules (MakeTerm's "Match
 * absorption"). Positional — delimiters belong to slots, not arms. */
let arm_slot_ids = (e: Exp.t): list(Id.t) =>
  switch (e.annotation.ids) {
  | [_case_end, ...rule_ids] => rule_ids
  | [] => []
  };

/* ~fixup as in swap_exp_items: gating passes false (the Slot ops
   PRINT the bodies, and nothing in the gating path may print — this
   ran per render before the flag) */
let swap_arms_rewrite =
    (~fixup: bool, ~target: Id.t, i: int, e: Exp.t): option((Exp.t, Id.t)) =>
  switch (IdTagged.term_of(e)) {
  | Match(scrut, rules) when i >= 0 && List.length(rules) > i + 1 =>
    let (pa, ba) = List.nth(rules, i);
    let (pb, bb) = List.nth(rules, i + 1);
    if (pats_disjoint(pa, pb)) {
      /* boundary runs belong to SLOTS, and they may be stored
         node-level OR leaf-deep (mixed storage doubled a lead here
         once): exchange bodies via the textual Slot ops — each slot
         keeps its own lead/trail, content swaps stripped */
      let (ba', bb') =
        if (fixup) {
          let sa = Slot.of_exp(ba);
          let sb = Slot.of_exp(bb);
          (
            Slot.give(sa, Slot.drop(sb, bb)),
            Slot.give(sb, Slot.drop(sa, ba)),
          );
        } else {
          (
            with_secondary(ba.annotation.secondary, bb),
            with_secondary(bb.annotation.secondary, ba),
          );
        };
      /* arm pats: slot-wise like the bodies — an atomic pat's run is
         node-level but a compound pat's (Some(x)) lives on its first
         leaf; node-level exchange stacked one space per swap */
      let spa = pat_slot(pa);
      let spb = pat_slot(pb);
      let pa' = pat_slot_give(spa, pat_slot_drop(spb, pb));
      let pb' = pat_slot_give(spb, pat_slot_drop(spa, pa));
      let rules' =
        rules
        |> List.mapi((j, r) =>
             if (j == i) {
               (pa', ba');
             } else if (j == i + 1) {
               (pb', bb');
             } else {
               r;
             }
           );
      /* the RULE is the persistent element: its |/=> delimiter ids
         TRAVEL with the arm (andrew), so the delimiter tokens and
         their decorations animate as part of the move, and a caret
         on a delimiter follows its arm with no explicit hop */
      let ids' =
        switch (e.annotation.ids) {
        | [head, ...slot_ids] =>
          let swapped =
            slot_ids
            |> List.mapi((j, id) =>
                 if (j == i) {
                   List.nth_opt(slot_ids, i + 1) |> Option.value(~default=id);
                 } else if (j == i + 1) {
                   List.nth_opt(slot_ids, i) |> Option.value(~default=id);
                 } else {
                   id;
                 }
               );
          [head, ...swapped];
        | [] => []
        };
      let focus =
        if (List.mem(target, pat_subtree_ids(pa))
            || List.mem(target, pat_subtree_ids(pb))
            || List.mem(target, arm_slot_ids(e))) {
          target;
        } else {
          Pat.rep_id(pa);
        };
      Some((
        {
          term: Match(scrut, rules'),
          annotation: {
            ...e.annotation,
            ids: ids',
          },
        },
        focus,
      ));
    } else {
      None;
    };
  | _ => None
  };

/* index of the arm whose pattern subtree contains the target */
/* an arm is targetable at its pattern or its slot's |/=> delimiters */
let arm_index_at = (target: Id.t, e: Exp.t): option(int) =>
  switch (IdTagged.term_of(e)) {
  | Match(_, rules) =>
    let by_pat =
      rules
      |> List.mapi((j, (p, _)) => (j, p))
      |> List.find_opt(((_, p)) => List.mem(target, pat_subtree_ids(p)))
      |> Option.map(fst);
    switch (by_pat) {
    | Some(_) => by_pat
    | None =>
      arm_slot_ids(e)
      |> List.mapi((j, id) => (j, id))
      |> List.find_opt(((j, id)) => id == target && j < List.length(rules))
      |> Option.map(fst)
    };
  | _ => None
  };

let hit_arm = (target: Id.t, e: Exp.t): bool =>
  arm_index_at(target, e) != None;

/* === Swap tuple-pattern components ===
 * `let (lo, hi) = (0, 100)` + swap => `let (hi, lo) = (100, 0)`:
 * pattern components and the MATCHING definition components rotate
 * together, so every binding keeps its value. Gated on the def being
 * a (possibly parenthesized) literal tuple of matching arity. Never
 * collides with Swap Params: a tuple pat can't be a fn binding. */
let tuple_pat_items = (p: Pat.t): list(Pat.t) => {
  let rec go = (p: Pat.t) =>
    switch (IdTagged.term_of(p)) {
    | Parens(inner) => go(inner)
    | Tuple(items) => items
    | _ => []
    };
  go(p);
};

let tuple_def_items = (d: Exp.t): list(Exp.t) => {
  let rec go = (d: Exp.t) =>
    switch (IdTagged.term_of(d)) {
    | Parens(inner) => go(inner)
    | Tuple(items) => items
    | _ => []
    };
  go(d);
};

let swap_tuple_pat_rewrite =
    (~fixup: bool, ~target: Id.t, i: int, e: Exp.t): option((Exp.t, Id.t)) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, def, body) =>
    let rec swap_in_pat = (p: Pat.t): option(Pat.t) =>
      switch (IdTagged.term_of(p)) {
      | Parens(inner) =>
        swap_in_pat(inner)
        |> Option.map((inner': Pat.t): Pat.t =>
             {
               ...p,
               term: Parens(inner'),
             }
           )
      | Tuple(items) when i >= 0 && List.length(items) > i + 1 =>
        Some({
          ...p,
          term: Tuple(swap_pat_items(i, items)),
        })
      | _ => None
      };
    let rec swap_in_def = (d: Exp.t): option(Exp.t) =>
      switch (IdTagged.term_of(d)) {
      | Parens(inner) =>
        swap_in_def(inner)
        |> Option.map((inner': Exp.t): Exp.t =>
             {
               ...d,
               term: Parens(inner'),
             }
           )
      | Tuple(items)
          when
            i >= 0
            && List.length(items) > i
            + 1
            && List.length(items) == List.length(tuple_pat_items(p)) =>
        /* the bounds guard matters even though the PATTERN side is
           guarded: this pair-component is evaluated eagerly, so an
           unguarded nth here crashed before the match saw the
           pattern side's None (drag probes all four directions, so
           Right-on-the-last-component hit this every time) */
        Some({
          ...d,
          term: Tuple(swap_exp_items(~fixup, i, items)),
        })
      | _ => None
      };
    switch (swap_in_pat(p), swap_in_def(def)) {
    | (Some(p'), Some(def')) =>
      let pitems = tuple_pat_items(p);
      let ditems = tuple_def_items(def);
      let in_swapped = (j: int) =>
        j < List.length(pitems)
        && List.mem(target, pat_subtree_ids(List.nth(pitems, j)))
        || j < List.length(ditems)
        && List.mem(target, exp_subtree_ids(List.nth(ditems, j)));
      /* caret follows the component it was on (ids travel through
         the swap, either side) */
      let focus =
        in_swapped(i) || in_swapped(i + 1) ? target : Exp.rep_id(e);
      Some((
        {
          ...e,
          term: Let(p', def', body),
        },
        focus,
      ));
    | _ => None
    };
  | _ => None
  };

/* index of the component whose subtree contains target — on either
 * side: the pattern or the definition tuple */
let tuple_pat_index_at = (target: Id.t, e: Exp.t): option(int) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, def, _) =>
    let by_pat =
      tuple_pat_items(p)
      |> List.mapi((j, it) => (j, it))
      |> List.find_opt(((_, it)) => List.mem(target, pat_subtree_ids(it)))
      |> Option.map(fst);
    switch (by_pat) {
    | Some(_) => by_pat
    | None =>
      tuple_pat_items(p) == []
        ? None
        : tuple_def_items(def)
          |> List.mapi((j, it) => (j, it))
          |> List.find_opt(((_, it)) =>
               List.mem(target, exp_subtree_ids(it))
             )
          |> Option.map(fst)
    };
  | _ => None
  };

let hit_tuple_swap = (target: Id.t, e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Let(_) => hit_let(target, e) || tuple_pat_index_at(target, e) != None
  | _ => false
  };

let swap_tuple_pat_impl = (i: int): impl => {
  label: "Swap components",
  tooltip: "Swap these tuple components in the pattern and the definition",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_tuple_swap(target),
      ~rewrite=e => swap_tuple_pat_rewrite(~fixup=true, ~target, i, e),
      program,
    ),
};

let swap_arms_impl = (i: int): impl => {
  label: "Move arm",
  tooltip: "Swap this arm with its neighbor (patterns must not overlap)",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_arm(target),
      ~rewrite=e => swap_arms_rewrite(~fixup=true, ~target, i, e),
      program,
    ),
};

/* the let is hit when the target is one of its param var tokens */
/* index of the param var whose token is the target */
let param_index_at = (target: Id.t, e: Exp.t): option(int) =>
  param_items(e)
  |> List.mapi((j, it) => (j, it))
  |> List.find_opt(((_, it: Pat.t)) => List.mem(target, IdTagged.ids(it)))
  |> Option.map(fst);

let hit_param = (target: Id.t, e: Exp.t): bool =>
  param_items(e)
  |> List.exists((it: Pat.t) =>
       switch (IdTagged.term_of(it)) {
       | Var(_)
       | EmptyHole
       | Wild => List.mem(target, IdTagged.ids(it))
       | _ => false
       }
     );

/* Resolve a swap/param target to its function's LET, from any of:
 * the let's own zone, a param var in the def's fun pattern, or an
 * argument at a call site (via the callee's binder). Also gives the
 * param/arg index at the target when it names one. */
let swap_site =
    (~info_map: Statics.Map.t, ~target: Id.t, program: Exp.t)
    : option((Exp.t, option(int))) => {
  let let_of = (id: Id.t) => find_hit(~hit=hit_let(id), program);
  switch (let_of(target)) {
  | Some(l) => Some((l, param_index_at(target, l)))
  | None =>
    switch (find_hit(~hit=hit_param(target), program)) {
    | Some(l) => Some((l, param_index_at(target, l)))
    | None =>
      /* call site: the argument's position within f(a, b, ...) */
      let hit_call = (e: Exp.t) =>
        switch (IdTagged.term_of(e)) {
        | Ap(Forward, f, arg) =>
          switch (IdTagged.term_of(f), IdTagged.term_of(arg)) {
          | (Var(_), Tuple(items))
          | (Var(_), Parens({term: Tuple(items), _})) =>
            items |> List.exists(it => List.mem(target, exp_subtree_ids(it)))
          | _ => false
          }
        | _ => false
        };
      switch (find_hit(~hit=hit_call, program)) {
      | Some(ap) =>
        switch (IdTagged.term_of(ap)) {
        | Ap(Forward, f, arg) =>
          let items =
            switch (IdTagged.term_of(arg)) {
            | Tuple(items) => items
            | Parens({term: Tuple(items), _}) => items
            | _ => []
            };
          let j =
            items
            |> List.mapi((k, it) => (k, it))
            |> List.find_opt(((_, it)) =>
                 List.mem(target, exp_subtree_ids(it))
               )
            |> Option.map(fst);
          switch (
            binder_of_occurrence(~info_map, ~target=Exp.rep_id(f), program)
          ) {
          | Some(binder) => let_of(binder) |> Option.map(l => (l, j))
          | None => None
          };
        | _ => None
        }
      | None => None
      };
    }
  };
};

let swap_params_impl = (i: int): impl => {
  label: "Swap Params",
  tooltip: "Swap these adjacent parameters at the definition and all call sites",
  prepare: (~info_map, ~target, program) =>
    switch (swap_site(~info_map, ~target, program)) {
    | Some((l, at_item)) =>
      switch (swap_params_rewrite(~fixup=true, i, l)) {
      | Some((result, focus)) =>
        /* invoked from a param or call-site argument: the caret
           follows it (item nodes keep their ids through the swap;
           repeat presses keep pushing it). From the let zone: the
           let. */
        let focus = at_item == None ? focus : target;
        rewrite_node(
          ~hit=same_node(l),
          ~rewrite=_ => Some((result, focus)),
          program,
        );
      | None => None
      }
    | None => None
    },
};

let drop_fun_pat = (i: int, fp: Pat.t): option(Pat.t) =>
  switch (IdTagged.term_of(fp)) {
  | Parens(inner) =>
    switch (IdTagged.term_of(inner)) {
    | Tuple(items) when List.length(items) > 1 =>
      switch (drop_pat_item(i, items)) {
      | [single] => Some(pad(with_secondary_pat(([], []), single)))
      | items' =>
        Some({
          ...fp,
          term:
            Parens({
              ...inner,
              term: Tuple(items'),
            }),
        })
      }
    | _ => None
    }
  | _ => None
  };

let drop_arrow_ann = (i: int, ann: Typ.t): option(Typ.t) =>
  switch (IdTagged.term_of(ann)) {
  | Arrow(a, b) =>
    let dropped: option(Typ.t) =
      switch (IdTagged.term_of(a)) {
      | Parens(inner) =>
        switch (IdTagged.term_of(inner)) {
        | Prod(items) when List.length(items) > 1 =>
          switch (drop_typ_item(i, items)) {
          | [single] =>
            let (b_, a_) = a.annotation.secondary;
            Some(
              with_secondary_typ(
                (b_, a_),
                with_secondary_typ(([], []), single) |> (x => x),
              ),
            );
          | items' =>
            Some({
              ...a,
              term:
                Parens({
                  ...inner,
                  term: Prod(items'),
                }),
            })
          }
        | _ => None
        }
      | _ => None
      };
    dropped
    |> Option.map((a': Typ.t): Typ.t =>
         {
           ...ann,
           term: Arrow(a', b),
         }
       );
  | _ => None
  };

/* drop the i-th arg at every unshadowed call of x */
let drop_call_args =
    (~bare_use: ref(bool), ~ok: ref(bool), i: int, x: string, e: Exp.t)
    : Exp.t =>
  map_unshadowed(
    ~skip=x,
    ~f_var=
      e' =>
        switch (IdTagged.term_of(e')) {
        | Var(z) when z == x =>
          bare_use := true;
          Some(e');
        | _ => None
        },
    ~f_ap=
      (ap, fn, arg, go) =>
        if (is_var_named(x, fn)) {
          switch (IdTagged.term_of(arg)) {
          | Tuple(items) when List.length(items) > i =>
            let items = items |> List.map(go);
            let arg': Exp.t =
              switch (drop_exp_item(i, items)) {
              | [single] => single
              | items' => {
                  ...arg,
                  term: Tuple(items'),
                }
              };
            Some({
              ...ap,
              term: Ap(Forward, fn, arg'),
            });
          | _ =>
            ok := false;
            Some(ap);
          };
        } else {
          None;
        },
    e,
  );

/* Resolve a RemoveParameter target: a param var directly, or the
 * param parens meaning "the last parameter" (the Left-at-`)` gesture
 * mirrors Right-at-`)` = append; move a param right, then shed it) */
let remove_param_target = (~target: Id.t, e: Exp.t): option(Id.t) =>
  if (hit_param(target, e)) {
    Some(target);
  } else {
    switch (param_paren(e)) {
    | Some(paren) when List.mem(target, IdTagged.ids(paren)) =>
      switch (List.rev(param_items(e))) {
      | [last, ..._] =>
        switch (IdTagged.term_of(last)) {
        | Var(_)
        | EmptyHole
        | Wild => Some(Pat.rep_id(last))
        | _ => None
        }
      | [] => None
      }
    | _ => None
    };
  };

let remove_param_rewrite = (~target: Id.t, e: Exp.t): option((Exp.t, Id.t)) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, def, body) =>
    let items = param_items(e);
    let idx =
      items
      |> List.mapi((i, it) => (i, it))
      |> List.find_opt(((_, it: Pat.t)) =>
           switch (IdTagged.term_of(it)) {
           /* a hole/wild param binds nothing — trivially removable */
           | Var(_)
           | EmptyHole
           | Wild => List.mem(target, IdTagged.ids(it))
           | _ => false
           }
         );
    switch (idx) {
    | Some((i, item)) when List.length(items) >= 2 =>
      let used_in = (e: Exp.t) =>
        switch (var_pat_name(item)) {
        | Some(n) => free_in(n, e)
        | None => false
        };
      let bare_use = ref(false);
      let ok = ref(true);
      let pieces: option((string, Pat.t, Exp.t)) =
        switch (sugar_fn_name(p)) {
        | Some(f) =>
          !used_in(def)
            ? {
              let rec drop_in_pat = (p: Pat.t): option(Pat.t) =>
                switch (IdTagged.term_of(p)) {
                | Ap(fv, argp) =>
                  switch (IdTagged.term_of(argp)) {
                  | Tuple(its) when List.length(its) > 1 =>
                    switch (drop_pat_item(i, its)) {
                    | [single] =>
                      Some({
                        ...p,
                        term: Ap(fv, with_secondary_pat(([], []), single)),
                      })
                    | its' =>
                      Some({
                        ...p,
                        term:
                          Ap(
                            fv,
                            {
                              ...argp,
                              term: Tuple(its'),
                            },
                          ),
                      })
                    }
                  | _ => None
                  }
                | Asc(inner, ann) =>
                  drop_in_pat(inner)
                  |> Option.map((inner': Pat.t): Pat.t =>
                       {
                         ...p,
                         term: Asc(inner', ann),
                       }
                     )
                | _ => None
                };
              drop_in_pat(p)
              |> Option.map(p' =>
                   (f, p', drop_call_args(~bare_use, ~ok, i, f, def))
                 );
            }
            : None
        | None =>
          switch (let_head_name(p), IdTagged.term_of(def)) {
          | (Some(f), Fun(fp, fbody, ft, fn)) =>
            !used_in(fbody)
              ? {
                let p': option(Pat.t) =
                  switch (IdTagged.term_of(p)) {
                  | Var(_) => Some(p)
                  | Asc(inner, ann) =>
                    drop_arrow_ann(i, ann)
                    |> Option.map((ann': Typ.t): Pat.t =>
                         {
                           ...p,
                           term: Asc(inner, ann'),
                         }
                       )
                  | _ => None
                  };
                switch (p', drop_fun_pat(i, fp)) {
                | (Some(p'), Some(fp')) =>
                  let fbody' =
                    binds(f, fp)
                      ? fbody : drop_call_args(~bare_use, ~ok, i, f, fbody);
                  Some((
                    f,
                    p',
                    {
                      ...def,
                      term: Fun(fp', fbody', ft, fn),
                    },
                  ));
                | _ => None
                };
              }
              : None
          | _ => None
          }
        };
      switch (pieces) {
      | Some((f, p', def')) =>
        let body' = drop_call_args(~bare_use, ~ok, i, f, body);
        ok^ && ! bare_use^
          ? Some((
              {
                ...e,
                term: Let(p', def', body'),
              },
              Exp.rep_id(e),
            ))
          : None;
      | None => None
      };
    | _ => None
    };
  | _ => None
  };

let remove_param_impl: impl = {
  label: "Remove param",
  tooltip: "Drop this parameter and its argument at every call site",
  prepare: (~info_map as _, ~target, program) =>
    switch (
      find_path(~hit=e => remove_param_target(~target, e) != None, program)
      |> Option.map(path => List.nth(path, List.length(path) - 1))
    ) {
    | Some(l) =>
      switch (
        remove_param_rewrite(
          ~target=
            remove_param_target(~target, l) |> Option.value(~default=target),
          l,
        )
      ) {
      | Some((result, focus)) =>
        rewrite_node(
          ~hit=same_node(l),
          ~rewrite=_ => Some((result, focus)),
          program,
        )
      | None => None
      }
    | None => None
    },
};

let impl: Action.refactor => impl =
  fun
  | InlineLet => inline_let_impl
  | FeedLet => feed_let_impl
  | RemoveUnusedLet => remove_unused_let_impl
  | AddTypeAnnotation => add_annotation_impl
  | EtaExpand => eta_expand_impl
  | EvaluateInPlace => evaluate_in_place_impl
  | AddCaseArm => add_case_arm_impl
  | ExpandWildcard => expand_wildcard_impl
  | AddParameter => add_param_impl
  | RemoveParameter => remove_param_impl
  | RenameFree(x, y) => rename_free_impl(x, y)
  | SwapParams(i) => swap_params_impl(i)
  | SwapArms(i) => swap_arms_impl(i)
  | SwapTuplePat(i) => swap_tuple_pat_impl(i)
  | HoistLet => hoist_let_impl
  | SinkLet => sink_let_impl
  | IfToCase => if_to_case_impl
  | CaseToIf => case_to_if_impl
  | ExtractLet => extract_let_impl
  | EtaReduce => eta_reduce_impl
  | BindArgument => ap_to_let_impl
  | BetaReduce => beta_reduce_impl
  | SplitLet => split_let_impl
  | ReduceCase => reduce_case_impl
  | BindArm => bind_arm_impl
  | ReduceIf => reduce_if_impl
  | NegateIf => negate_if_impl;

let all: list(Action.refactor) = [
  InlineLet,
  FeedLet,
  RemoveUnusedLet,
  AddTypeAnnotation,
  EtaExpand,
  EvaluateInPlace,
  BetaReduce,
  BindArgument,
  SplitLet,
  ReduceCase,
  BindArm,
  ReduceIf,
  AddCaseArm,
  ExpandWildcard,
  AddParameter,
  RemoveParameter,
  HoistLet,
  SinkLet,
  ExtractLet,
  EtaReduce,
  IfToCase,
  CaseToIf,
  NegateIf,
];

/* Cheap applicability for menu gating: shape/statics checks only.
 * menu_items runs on EVERY render (the command palette rebuilds with
 * the cursor), so nothing here may print, parse, or run the reparse
 * oracle — that work happens in prepare, only on invocation. Keep in
 * sync with each impl's matching; drift is benign (a stale offer
 * no-ops via Cant_refactor) but gating is tested via offers(). */
let let_applies =
    (~pred: (Pat.t, Exp.t, Exp.t) => bool, target: Id.t, program: Exp.t): bool =>
  switch (find_hit(~hit=hit_let(target), program)) {
  | Some(e) =>
    switch (IdTagged.term_of(e)) {
    | Let(p, def, body) => pred(p, def, body)
    | _ => false
    }
  | None => false
  };

let applies =
    (
      kind: Action.refactor,
      ~info_map: Statics.Map.t,
      ~target: Id.t,
      program: Exp.t,
    )
    : bool =>
  switch (kind) {
  | InlineLet =>
    let at = let_applies(~pred=inline_matches);
    at(target, program)
    || (
      switch (binder_of_occurrence(~info_map, ~target, program)) {
      | Some(binder) => at(binder, program)
      | None => false
      }
    );
  | FeedLet => Option.is_some(feed_plan(~info_map, ~target, program))
  | RemoveUnusedLet =>
    let_applies(
      ~pred=
        (p, _, _) =>
          switch (head_var_pat(p)) {
          | Some(hv) => pat_unused(~info_map, hv)
          | None => false
          },
      target,
      program,
    )
  | AddTypeAnnotation =>
    let_applies(
      ~pred=
        (p, def, _) =>
          var_pat_name(p) != None
          && (
            switch (exp_ty(~info_map, def)) {
            | Some(ty) => typ_known(ty)
            | None => false
            }
          ),
      target,
      program,
    )
  | AddParameter =>
    switch (find_hit(~hit=hit_add_param(target), program)) {
    | Some(e) => Option.is_some(add_param_rewrite(~program, e))
    | None => false
    }
  | RemoveParameter =>
    switch (
      find_hit(~hit=e => remove_param_target(~target, e) != None, program)
    ) {
    | Some(l) =>
      switch (remove_param_target(~target, l)) {
      | Some(t) => Option.is_some(remove_param_rewrite(~target=t, l))
      | None => false
      }
    | None => false
    }
  | RenameFree(x, y) =>
    switch (find_hit(~hit=hit_rename(target), program)) {
    | Some(e) => rename_pairs(~info_map, ~target, e) |> List.mem((x, y))
    | None => false
    }
  | SwapParams(i) =>
    switch (swap_site(~info_map, ~target, program)) {
    | Some((l, _)) =>
      Option.is_some(swap_params_rewrite(~fixup=false, i, l))
    | None => false
    }
  | SwapArms(i) =>
    switch (find_hit(~hit=hit_arm(target), program)) {
    | Some(m) =>
      Option.is_some(swap_arms_rewrite(~fixup=false, ~target, i, m))
    | None => false
    }
  | SwapTuplePat(i) =>
    switch (find_hit(~hit=hit_tuple_swap(target), program)) {
    | Some(l) =>
      Option.is_some(swap_tuple_pat_rewrite(~fixup=false, ~target, i, l))
    | None => false
    }
  | HoistLet =>
    switch (find_path(~hit=hit_let(target), program)) {
    | Some(path) => Option.is_some(hoist_step(~fixup=false, path))
    | None => false
    }
  | SinkLet =>
    switch (find_hit(~hit=hit_let(target), program)) {
    | Some(l) => Option.is_some(sink_step(~fixup=false, l))
    | None => false
    }
  | EtaExpand =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Fun(_)
      | Parens(_) => false
      | _ =>
        switch (exp_ty(~info_map, e)) {
        | Some(ty) =>
          switch (IdTagged.term_of(ty)) {
          | Arrow(_) => true
          | _ => false
          }
        | None => false
        }
      }
    | None => false
    }
  | EvaluateInPlace =>
    /* cheap gate only: closed (empty co-ctx) and not already a value;
       divergence/stuckness discovered at invocation (stale offer
       no-ops) */
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      /* a lambda's value is a closure — never spliceable surface
         syntax, so evaluating AT a fun literal always refuses;
         don't offer (andrew hit the dead press) */
      let rec is_fun = (e: Exp.t) =>
        switch (IdTagged.term_of(e)) {
        | Parens(inner) => is_fun(inner)
        | Fun(_) => true
        | _ => false
        };
      !is_fun(e)
      && !is_value_literal(e)
      && (
        switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
        | Some(InfoExp({co_ctx, _})) => co_ctx == []
        | _ => false
        }
      );
    | None => false
    }
  | AddCaseArm =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Match(_, [_, ..._]) => Option.is_some(match_witness(~info_map, e))
      | _ => false
      }
    | None => false
    }
  | ExpandWildcard =>
    switch (find_hit(~hit=hit_match_pat(target), program)) {
    | Some(e) => Option.is_some(wildcard_expansion(~info_map, ~target, e))
    | None => false
    }
  | ExtractLet => Option.is_some(extract_path(~target, program))
  | EtaReduce =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Fun(p, body, _, _) =>
        switch (var_pat_name(p), IdTagged.term_of(body)) {
        | (Some(x), Ap(Forward, f, arg)) =>
          switch (IdTagged.term_of(arg)) {
          | Var(y) => y == x && !mentions(x, f)
          | _ => false
          }
        | _ => false
        }
      | _ => false
      }
    | None => false
    }
  | IfToCase
  | NegateIf =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | If(_) => true
      | _ => false
      }
    | None => false
    }
  | CaseToIf =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Match(_, [(p1, _), (p2, _)]) =>
        switch (bool_pat(p1), bool_pat(p2)) {
        | (Some(a), Some(b)) => a != b
        | _ => false
        }
      | _ => false
      }
    | None => false
    }
  | BindArgument => Option.is_some(find_hit(~hit=hit_beta(target), program))
  | BetaReduce =>
    switch (find_hit(~hit=hit_beta(target), program)) {
    | Some(e) =>
      switch (beta_parts(e)) {
      | Some((p, _, _)) => let_head_name(p) != None
      | None => false
      }
    | None => false
    }
  | SplitLet =>
    switch (find_hit(~hit=hit_let(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Let(p, def, _) when let_head_name(p) == None =>
        switch (match_value(p, def)) {
        | Matched(_) => true
        | _ => false
        }
      | _ => false
      }
    | None => false
    }
  | ReduceIf =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | If(c, _, _) => lit_bool(c) != None
      | _ => false
      }
    | None => false
    }
  | ReduceCase =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Match(scrut, rules) =>
        is_value_literal(scrut) && pick_arm(scrut, rules) != None
      | _ => false
      }
    | None => false
    }
  | BindArm =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Match(scrut, rules) =>
        is_value_literal(scrut)
        && (
          switch (pick_arm(scrut, rules)) {
          | Some(([_, ..._], _)) => true
          | _ => false
          }
        )
      | _ => false
      }
    | None => false
    }
  };

/* Program-dependent labels for static kinds (rename already
 * enumerates its own). A one-pat print is bounded — unlike the
 * whole-program prints the gating rule bans — keep it that way. */
let label_override =
    (
      kind: Action.refactor,
      ~info_map: Statics.Map.t,
      ~target: Id.t,
      term: Exp.t,
    )
    : option(string) =>
  switch (kind) {
  | AddCaseArm =>
    switch (find_hit(~hit=hit_node(target), term)) {
    | Some(e) =>
      match_witness(~info_map, e)
      |> Option.map(w => {
           let text =
             Printer.of_segment(
               ~holes="?",
               ~refractors=[],
               ExpToSegment.pat_to_segment(
                 ~settings=roundtrip_settings,
                 wildify(w),
               ),
             );
           "Add arm | " ++ text;
         })
    | None => None
    }
  | RemoveParameter =>
    switch (find_hit(~hit=hit_param(target), term)) {
    | Some(l) =>
      param_items(l)
      |> List.find_opt((it: Pat.t) => List.mem(target, IdTagged.ids(it)))
      |> Option.map((it: Pat.t) =>
           "Remove Parameter"
           ++ (
             switch (var_pat_name(it)) {
             | Some(n) => " " ++ n
             | None => ""
             }
           )
         )
    | None => None
    }
  | AddParameter =>
    switch (find_hit(~hit=hit_let(target), term)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Let(p, _, _) =>
        (
          switch (sugar_fn_name(p)) {
          | Some(f) => Some(f)
          | None => let_head_name(p)
          }
        )
        |> Option.map(f => "Add param to " ++ f)
      | _ => None
      }
    | None => None
    }
  | _ => None
  };

/* Menu support: which refactorings apply at the current indication */
/* Payload kinds can't come from filtering the static `all` list: the
 * entries themselves depend on the program (one per candidate free
 * name), with labels naming names */
let rename_items =
    (~info_map: Statics.Map.t, ~target: Id.t, term: Exp.t)
    : list((Action.refactor, string, string)) =>
  switch (find_hit(~hit=hit_rename(target), term)) {
  | Some(e) =>
    rename_pairs(~info_map, ~target, e)
    |> List.map(((x, y)) =>
         (
           Action.RenameFree(x, y),
           "Rename " ++ x ++ " to " ++ y,
           "Bind free occurrences of " ++ x ++ " at this binding",
         )
       )
  | None => []
  };

let menu_items =
    (~info_map: Statics.Map.t, ~term: Exp.t, z: Zipper.t)
    : list((Action.refactor, string, string)) =>
  switch (Indicated.index(z)) {
  | None => []
  | Some(target) =>
    let static =
      all
      |> List.filter_map(kind => {
           let i = impl(kind);
           if (applies(kind, ~info_map, ~target, term)) {
             let label =
               label_override(kind, ~info_map, ~target, term)
               |> Option.value(~default=i.label);
             Some((kind, label, i.tooltip));
           } else {
             None;
           };
         });
    let swaps =
      switch (swap_site(~info_map, ~target, term)) {
      | Some((l, _)) =>
        let names = swap_param_names(l);
        List.init(max(List.length(names) - 1, 0), i => i)
        |> List.filter(i =>
             Option.is_some(swap_params_rewrite(~fixup=false, i, l))
           )
        |> List.map(i =>
             (
               Action.SwapParams(i),
               "Swap "
               ++ List.nth(names, i)
               ++ " ↔ "
               ++ List.nth(names, i + 1),
               "Swap these parameters at the definition and all call sites",
             )
           );
      | None => []
      };
    let arms =
      switch (find_hit(~hit=hit_arm(target), term)) {
      | Some(m) =>
        switch (arm_index_at(target, m)) {
        | Some(j) =>
          let mk = (i, label) =>
            Option.is_some(swap_arms_rewrite(~fixup=false, ~target, i, m))
              ? [
                (
                  Action.SwapArms(i),
                  label,
                  "Swap this arm with its neighbor (order-safe: patterns are disjoint)",
                ),
              ]
              : [];
          mk(j - 1, "Move arm up") @ mk(j, "Move arm down");
        | None => []
        }
      | None => []
      };
    let tuple_swaps =
      switch (find_hit(~hit=hit_tuple_swap(target), term)) {
      | Some(l) =>
        let items =
          switch (IdTagged.term_of(l)) {
          | Let(p, _, _) => tuple_pat_items(p)
          | _ => []
          };
        let name = (j: int) =>
          switch (var_pat_name(List.nth(items, j))) {
          | Some(n) => n
          | None => "#" ++ string_of_int(j + 1)
          };
        List.init(max(List.length(items) - 1, 0), i => i)
        |> List.filter(i =>
             Option.is_some(
               swap_tuple_pat_rewrite(~fixup=false, ~target, i, l),
             )
           )
        |> List.map(i =>
             (
               Action.SwapTuplePat(i),
               "Swap " ++ name(i) ++ " ↔ " ++ name(i + 1),
               "Swap these tuple components in the pattern and the definition",
             )
           );
      | None => []
      };
    static
    @ rename_items(~info_map, ~target, term)
    @ swaps
    @ tuple_swaps
    @ arms;
  };

/* === Directional gestures ===
 * Resolve (caret target zone, direction) to a refactor, first match
 * wins; None = dead press (visibly inert, never plain caret motion).
 * Vertical = movement across line slots/scopes (the elevator:
 * extract/hoist/sink/inline); horizontal = movement across comma
 * siblings (params/args) plus wildcard expansion. A matched zone with
 * a gated transform is DEAD - no fall-through - so gestures stay
 * predictable. */
let gesture =
    (~info_map: Statics.Map.t, ~term: Exp.t, g: Action.Gesture.t, z: Zipper.t)
    : option(Action.refactor) =>
  switch (Indicated.index(z)) {
  | None => None
  | Some(target) =>
    let shard = Indicated.shard_index(z);
    let app = (k: Action.refactor) =>
      applies(k, ~info_map, ~target, term) ? Some(k) : None;
    /* arm reorder: delta -1 = up, +1 = down */
    let arm_swap = (delta: int) =>
      switch (find_hit(~hit=hit_arm(target), term)) {
      | Some(m) =>
        switch (arm_index_at(target, m)) {
        | Some(j) =>
          let i = delta < 0 ? j - 1 : j;
          Option.is_some(swap_arms_rewrite(~fixup=false, ~target, i, m))
            ? Some(Action.SwapArms(i)) : None;
        | None => None
        }
      | None => None
      };
    let in_arm_zone = Option.is_some(find_hit(~hit=hit_arm(target), term));
    let in_let_zone = Option.is_some(find_hit(~hit=hit_let(target), term));
    let node_is = (pred: Exp.t => bool) =>
      switch (find_hit(~hit=hit_node(target), term)) {
      | Some(e) => pred(e)
      | None => false
      };
    let is_if =
      node_is(e =>
        switch (IdTagged.term_of(e)) {
        | If(_) => true
        | _ => false
        }
      );
    let is_case =
      node_is(e =>
        switch (IdTagged.term_of(e)) {
        | Match(_) => true
        | _ => false
        }
      );
    let param_swap = (delta: int) =>
      switch (swap_site(~info_map, ~target, term)) {
      | Some((l, Some(j))) =>
        let i = delta < 0 ? j - 1 : j;
        i >= 0 && Option.is_some(swap_params_rewrite(~fixup=false, i, l))
          ? Some(Action.SwapParams(i)) : None;
      | _ => None
      };
    /* the closing param paren: Right grows the sequence (append),
       Left sheds the last param; the opening paren stays dead
       (prepend, someday) */
    let at_closing_param_paren = (target: Id.t) =>
      shard == Some(1)
      && (
        switch (find_hit(~hit=hit_add_param(target), term)) {
        | Some(l) =>
          switch (param_paren(l)) {
          | Some(paren) => List.mem(target, IdTagged.ids(paren))
          | None => false
          }
        | None => false
        }
      );
    let tuple_swap = (delta: int) =>
      switch (find_hit(~hit=hit_tuple_swap(target), term)) {
      | Some(l) =>
        switch (tuple_pat_index_at(target, l)) {
        | Some(j) =>
          let i = delta < 0 ? j - 1 : j;
          i >= 0
          && Option.is_some(
               swap_tuple_pat_rewrite(~fixup=false, ~target, i, l),
             )
            ? Some(Action.SwapTuplePat(i)) : None;
        | None => None
        }
      | None => None
      };
    switch (g) {
    | Up =>
      if (in_arm_zone) {
        arm_swap(-1);
      } else if (in_let_zone) {
        app(HoistLet);
      } else if (is_if && shard == Some(2)) {
        app(
          NegateIf /* the else-arm moves up */
        );
      } else if (is_case && shard == Some(1)) {
        None;
            /* `end` has a case-specific vocation (add-arm below); a
               whole-case extract firing from it reads as an accident —
               extract stays on the `case` kw and in the menu */
      } else {
        app(ExtractLet);
      }
    | Down =>
      if (in_arm_zone) {
        arm_swap(1);
      } else if (in_let_zone) {
        /* movement rung if one exists, else the value flows: feed the
           nearest use (the last feed consumes the let) */
        switch (app(SinkLet)) {
        | Some(k) => Some(k)
        | None => app(FeedLet)
        };
      } else if (is_if && shard == Some(1)) {
        app(
          NegateIf /* the then-arm moves down */
        );
      } else if (is_case && shard == Some(1)) {
        app(
          AddCaseArm /* grow the sequence at its `end` */
        );
      } else {
        app(
          FeedLet /* at an occurrence: the definition feeds THIS use */
        );
      }
    | Left =>
      switch (param_swap(-1)) {
      | Some(k) => Some(k)
      | None =>
        switch (tuple_swap(-1)) {
        | Some(k) => Some(k)
        | None =>
          /* inverse of Right-at-`)` (append): shed the last param,
             gated on it being unused */
          at_closing_param_paren(target) ? app(RemoveParameter) : None
        }
      }
    | Right =>
      switch (param_swap(1)) {
      | Some(k) => Some(k)
      | None =>
        switch (tuple_swap(1)) {
        | Some(k) => Some(k)
        | None =>
          if (at_closing_param_paren(target)) {
            app(AddParameter);
          } else if (in_arm_zone) {
            app(ExpandWildcard);
          } else {
            None;
          }
        }
      }
    };
  };

/* the def subtree a FeedLet would clone: the emergeFrom source (D2's
   emergeMode=clone — the copy departs the source full-size at full
   opacity; a split, not a growth). Ids here are the LIVE def's;
   correlation with the commit's fresh clone ids happens positionally
   at flight time, because clone ids are minted per prepare run and
   are not stable across speculative/commit runs. */
let emerge_source =
    (~info_map, ~target, kind: Action.refactor, term): list(Id.t) =>
  switch (kind) {
  | FeedLet =>
    switch (feed_plan(~info_map, ~target, term)) {
    | Some(Feed(_, def, _)) => exp_subtree_ids(def)
    | _ => []
    }
  /* inline: every substituted copy emerges from the def (the first
     copy MOVES — same ids — the rest fly as fan-out clones) */
  | InlineLet =>
    switch (find_hit(~hit=hit_let(target), term)) {
    | Some(l) =>
      switch (IdTagged.term_of(l)) {
      | Let(_, def, _) => exp_subtree_ids(def)
      | _ => []
      }
    | None =>
      switch (binder_of_occurrence(~info_map, ~target, term)) {
      | Some(binder) =>
        switch (find_hit(~hit=hit_let(binder), term)) {
        | Some(l) =>
          switch (IdTagged.term_of(l)) {
          | Let(_, def, _) => exp_subtree_ids(def)
          | _ => []
          }
        | None => []
        }
      | None => []
      }
    }
  /* beta: the copies emerge from the ARGUMENT */
  | BetaReduce =>
    switch (find_hit(~hit=hit_beta(target), term)) {
    | Some(e) =>
      switch (beta_parts(e)) {
      | Some((_, arg, _)) => exp_subtree_ids(arg)
      | None => []
      }
    | None => []
    }
  | _ => []
  };

let gesture_emerge_source =
    (~info_map, ~term, g: Action.Gesture.t, z: Zipper.t): list(Id.t) =>
  switch (Indicated.index(z), gesture(~info_map, ~term, g, z)) {
  | (Some(target), Some(kind)) =>
    emerge_source(~info_map, ~target, kind, term)
  | _ => []
  };

let refactor_emerge_source =
    (~info_map, ~term, kind: Action.refactor, z: Zipper.t): list(Id.t) =>
  switch (Indicated.index(z)) {
  | Some(target) => emerge_source(~info_map, ~target, kind, term)
  | None => []
  };

/* === Drag candidates (pointer front-end to the gesture system) ===
 * For each direction, resolve the gesture at the caret, prepare it,
 * and measure the result — measures only, no statics/eval/view
 * (dragology's isTracking, done with Measured). Each candidate
 * defines a TRACK from the anchor's current position to its position
 * in the candidate. Anchors are (from, to) id pairs: the grabbed
 * construct for movement kinds; def -> fed occurrence for feeds (the
 * value travels, the binding doesn't). Degenerate tracks (anchor
 * doesn't move) are dropped — that transform stays arrows/menu-only.
 * Coincident targets keep the first in direction order (ambiguity
 * policy v1). */

module DragCandidate = {
  /* how the candidate layout maps onto the screen during the drag
     (the space-duality rule): candidate rows >= shift_from move by
     shift_rows; scroll_rows bumps the scroller at commit.
     - remove-kinds (feed): +N below the vacated line — bystanders
       and the target hold their LIVE positions; the blank persists
       until release (two-stage).
     - add-kinds (extract): global -N + a commit scroll bump — the
       origin line stays pinned while space opens above it. */
  type frame = {
    shift_from: int,
    shift_rows: int,
    scroll_rows: int,
  };
  let no_frame = {
    shift_from: 0,
    shift_rows: 0,
    scroll_rows: 0,
  };
  let frame_point = (f: frame, p: Measured.Point.t): Measured.Point.t =>
    p.row >= f.shift_from
      ? {
        ...p,
        row: p.row + f.shift_rows,
      }
      : p;

  type t = {
    dir: Action.Gesture.t,
    kind: Action.refactor,
    label: string,
    current: Measured.Point.t, /* track start (live layout) */
    target: Measured.Point.t, /* track end (screen frame) */
    frame,
    /* entering-token ORIGINS (dragology's emergeFrom): a transform
       that DUPLICATES content (feed with surviving uses) maps each
       fresh copy id to the live id it emerges from — the ghost
       travels from the source instead of growing in place */
    emerge: list((Id.t, Id.t)),
    term: Exp.t,
    focus: Id.t,
    segment: Segment.t,
    measured: Measured.t,
  };
};

let total_rows = (m: Measured.t): int =>
  switch (Measured.Rows.max_binding_opt(m.rows)) {
  | Some((r, _)) => r + 1
  | None => 0
  };

/* (from, to) anchor ids for a kind's track */
let drag_anchor =
    (
      ~feed_pref: bool=false,
      ~info_map,
      ~target: Id.t,
      kind: Action.refactor,
      term: Exp.t,
    )
    : option((Id.t, Id.t)) =>
  switch (kind) {
  | SwapArms(i) =>
    /* rule delimiters (| and =>) live in Match.ids, not the Measured
       maps — anchor at the MOVED arm's pattern so grabbing the bar
       works like grabbing the pattern */
    switch (find_hit(~hit=hit_arm(target), term)) {
    | Some(m) =>
      switch (IdTagged.term_of(m), arm_index_at(target, m)) {
      | (Match(_, rules), Some(j)) when j < List.length(rules) =>
        let (rp, _) = List.nth(rules, j);
        /* the grabbed arm is index j; it swaps with i/i+1 — anchor
           the arm the user grabbed either way */
        ignore(i);
        Some((Pat.rep_id(rp), Pat.rep_id(rp)));
      | _ => None
      }
    | None => None
    }
  | FeedLet =>
    switch (feed_site(~prefer_def_host=feed_pref, ~info_map, ~target, term)) {
    /* grabbed AT the use: a def->use track would start at its end
       (the pointer begins at t~1 and release commits instantly) —
       no track; the default (target, target) pair degenerates and
       the candidate drops. Feeds drag from the binding side. */
    | Some((_, _, Some(_))) => Some((target, target))
    | Some((l, x, None)) =>
      switch (IdTagged.term_of(l)) {
      | Let(_, def, body) =>
        first_occurrence(x, body)
        |> Option.map(o => (Exp.rep_id(def), Exp.rep_id(o)))
      | _ => None
      }
    | None => None
    }
  | _ => Some((target, target))
  };

let drag_candidates =
    (
      ~info_map: Statics.Map.t,
      ~term: Exp.t,
      ~measured: Measured.t,
      /* the LIVE projector shapes: projector ids survive transforms,
         so candidate layouts must reserve the same rendered widths —
         measuring with an empty map squeezed sliders to their token
         text and every tween target sat in the wrong geometry */
      ~shape_map: Id.Map.t(ProjectorCore.Shape.t)=Id.Map.empty,
      z: Zipper.t,
    )
    : list(DragCandidate.t) =>
  switch (Indicated.index(z)) {
  | None => []
  | Some(target) =>
    /* the grabbed SHARD anchors the track when known: a tile's
       delimiters don't move rigidly (case stays, end drops a row on
       add-arm), so tile-level lookup shows zero travel for real
       moves */
    let grab_shard = Indicated.shard_index(z);
    let shard_meas = (id: Id.t, m: Measured.t) =>
      switch (grab_shard) {
      | Some(k) when id == target =>
        switch (Id.Map.find_opt(id, m.tiles)) {
        | Some(shards) =>
          switch (List.assoc_opt(k, shards)) {
          | Some(meas) => Some(meas)
          | None => Measured.find_by_id(id, m)
          }
        | None => Measured.find_by_id(id, m)
        }
      | _ => Measured.find_by_id(id, m)
      };
    let mk =
        (~feed_pref: bool=false, dir: Action.Gesture.t)
        : option(DragCandidate.t) =>
      switch (gesture(~info_map, ~term, dir, z)) {
      | None => None
      | Some(kind) =>
        /* feeds drag from the BINDING side only (established rule):
           an at-use feed's clone lands where you grabbed — a
           meaningless sliver of a track (it used to drop via the
           zero-track guard; keep_ids shifted the geometry by a
           column and it survived). Killed explicitly; the Down
           retry then picks up the def-host reading when one exists. */
        let at_use_feed =
          kind == FeedLet
          && (
            switch (
              feed_site(~prefer_def_host=feed_pref, ~info_map, ~target, term)
            ) {
            | Some((_, _, Some(_))) => true
            | _ => false
            }
          );
        if (at_use_feed) {
          None;
        } else {
          let prepare =
            switch (kind) {
            | FeedLet => feed_prepare(~prefer_def_host=feed_pref)
            | _ => impl(kind).prepare
            };
          switch (prepare(~info_map, ~target, term)) {
          | None => None
          | Some((term', focus)) =>
            let segment =
              ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term')
              |> SpaceNormalize.go;
            let cand_measured =
              Measured.of_segment(segment, shape_map, Id.Map.empty);
            let (from_id, to_id) =
              drag_anchor(~feed_pref, ~info_map, ~target, kind, term)
              |> Option.value(~default=(target, target));
            let to_pos = (id: Id.t) =>
              shard_meas(id, cand_measured)
              |> Option.map((m: Measured.measurement) => m.origin);
            switch (
              shard_meas(from_id, measured),
              /* the grabbed id can vanish in a candidate (rare); the
                 focus is the moved content's id — try it second */
              switch (to_pos(to_id)) {
              | Some(p) => Some(p)
              | None => to_pos(focus)
              },
            ) {
            | (Some(cur), Some(tgt)) =>
              let live_rows = total_rows(measured);
              let cand_rows = total_rows(cand_measured);
              let frame =
                switch (kind) {
                | FeedLet when live_rows > cand_rows =>
                  /* two-stage: the vacated lines persist as blank
                     until release; everything at/below them holds its
                     live position */
                  {
                    DragCandidate.shift_from: cur.origin.row,
                    shift_rows: live_rows - cand_rows,
                    scroll_rows: 0,
                  }
                | ExtractLet when cand_rows > live_rows =>
                  /* two insertion geometries: a LINE-TAKEOVER extract
                     (the slot starts a line) opens space at-or-above
                     the origin — pin the origin, slide above-content
                     up, bump the scroll at commit. A SUB-SLOT extract
                     (inline fun/arm/chain body) lands the binding
                     WHERE THE DISPLACED BODY SITS — that content's
                     departure IS the target-space opening (duality
                     rule), so it moves WITH the pull: plain candidate
                     frame. (Pinning it overlapped the flyer with the
                     pinned body mid-drag — andrew.) */
                  let takeover =
                    switch (extract_path(~target, term)) {
                    | Some(path) =>
                      let line = lowest_line(path);
                      same_node(line, term)
                      || has_newline(sep_like(Slot.of_exp(line).lead));
                    | None => true
                    };
                  if (takeover) {
                    {
                      DragCandidate.shift_from: 0,
                      shift_rows: live_rows - cand_rows,
                      scroll_rows: cand_rows - live_rows,
                    };
                  } else {
                    DragCandidate.no_frame;
                  };
                | _ => DragCandidate.no_frame
                };
              let tgt = DragCandidate.frame_point(frame, tgt);
              /* emerge map (dragology's emergeFrom): the spawned clone's
                 ids are exactly the FRESH ids of the candidate; both
                 walks share traversal order, so they zip against the
                 def's ids positionally — no clone lookup needed */
              let emerge =
                switch (kind) {
                | FeedLet =>
                  switch (
                    feed_plan(
                      ~prefer_def_host=feed_pref,
                      ~info_map,
                      ~target,
                      term,
                    )
                  ) {
                  | Some(Feed(_, def, _)) =>
                    let live = exp_subtree_ids(term);
                    let fresh =
                      exp_subtree_ids(term')
                      |> List.filter(id => !List.mem(id, live));
                    let d = exp_subtree_ids(def);
                    /* combine raises on length mismatch — guard FIRST
                       (the eager-evaluation gotcha) */
                    let zip = ids =>
                      List.length(ids) == List.length(d)
                        ? List.combine(ids, d) : [];
                    switch (zip(fresh)) {
                    | [] =>
                      /* reparse demanded a paren wrapper: the parens
                         are genuinely NEW material (no source) — pair
                         the inner clone, found structurally */
                      let fresh_parens = (e: Exp.t) =>
                        switch (IdTagged.term_of(e)) {
                        | Parens(_) =>
                          IdTagged.ids(e)
                          |> List.exists(id => List.mem(id, fresh))
                        | _ => false
                        };
                      switch (find_hit(~hit=fresh_parens, term')) {
                      | Some(p) =>
                        switch (IdTagged.term_of(p)) {
                        | Parens(inner) => zip(exp_subtree_ids(inner))
                        | _ => []
                        }
                      | None => []
                      };
                    | pairs => pairs
                    };
                  | _ => []
                  }
                | _ => []
                };
              /* a spawned clone's track ends at the CLONE (the
                 occurrence's ids no longer exist in the candidate) */
              let tgt =
                switch (emerge |> List.find_opt(((_, d)) => d == from_id)) {
                | Some((clone_id, _)) =>
                  switch (to_pos(clone_id)) {
                  | Some(p) => DragCandidate.frame_point(frame, p)
                  | None => tgt
                  }
                | None => tgt
                };
              cur.origin != tgt
                ? Some({
                    DragCandidate.dir,
                    kind,
                    label: impl(kind).label,
                    current: cur.origin,
                    target: tgt,
                    frame,
                    emerge,
                    term: term',
                    focus,
                    segment,
                    measured: cand_measured,
                  })
                : None;
            | _ => None
            };
          };
        };
      };
    /* NO def-host retry (reverted): the commit dispatches the plain
       RefactorGesture, which RE-RESOLVES the position with default
       preferences — a candidate prepared with ~prefer_def_host
       previews a transform the commit then contradicts (andrew hit
       it: preview moved y's def, release fed a's binding).
       INVARIANT: enumeration must stay within what the gesture
       dispatch re-derives. The occurrence-inside-def spot is dead
       for drag until the commit path can carry a resolution. */
    [Action.Gesture.Up, Down, Left, Right]
    |> List.filter_map(dir => mk(dir))
    |> List.fold_left(
         (acc, c: DragCandidate.t) =>
           List.exists((c': DragCandidate.t) => c'.target == c.target, acc)
             ? acc : acc @ [c],
         [],
       );
  };

let go =
    (
      ~info_map: Statics.Map.t,
      ~term: Exp.t,
      kind: Action.refactor,
      z: Zipper.t,
    )
    : option(Zipper.t) =>
  switch (Indicated.index(z)) {
  | None => None
  | Some(target) =>
    switch (impl(kind).prepare(~info_map, ~target, term)) {
    | None => None
    | Some((term', focus)) =>
      let seg =
        ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term')
        |> SpaceNormalize.go;
      let mk = (zp: Zipper.t) => {
        ...zp,
        refractors: z.refractors,
      };
      /* caret fallback chain: the transform's focus id, else where
         the user was (target), else its statics ancestors — a
         vanished focus must not dump the caret at the document end */
      let ancestors =
        switch (Id.Map.find_opt(target, info_map)) {
        | Some(InfoExp({ancestors, _}))
        | Some(InfoPat({ancestors, _})) => ancestors
        | _ => []
        };
      /* structural caret placement (O(depth) splits); Move's token-walk
         jump costs ~90ms on a few-page buffer, so it's only the last
         resort */
      let place = (id: Id.t): option(Zipper.t) => {
        let try_side = side =>
          switch (Zipper.unzip_to_id(~side, id, seg)) {
          | Some(zp) =>
            let zp = mk(zp);
            Indicated.index(zp) == Some(id) ? Some(zp) : None;
          | None => None
          };
        switch (try_side(Util.Direction.Left)) {
        | Some(zp) => Some(zp)
        | None => try_side(Util.Direction.Right)
        };
      };
      let candidates = [focus, target] @ ancestors;
      switch (List.find_map(place, candidates)) {
      | Some(z'') => Some(z'')
      | None =>
        let z' = mk(Zipper.unzip(seg));
        Some(
          switch (Move.jump_to_first_indicated(z', candidates)) {
          | Some(z'') => z''
          | None => z'
          },
        );
      };
    }
  };
