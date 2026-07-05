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

let space = (): list(Secondary.t) => [
  {
    id: Id.mk(),
    content: Whitespace(" "),
  },
];

/* single spaces at a synthesized node's edges (can't be globalized:
 * user-authored tight junctions like `(a=1)` are legitimate) */
let pad = (e: IdTagged.t('a)): IdTagged.t('a) => {
  ...e,
  annotation: {
    ...e.annotation,
    secondary: (space(), space()),
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

/* The inserted copy takes over the replaced occurrence's stored
 * whitespace (its slot in the line); the definition keeps its own
 * interior spacing */
let inserted = (~parens: bool, def: Exp.t, at: Exp.t): Exp.t => {
  let secondary = at.annotation.secondary;
  let def = strip_boundaries(def);
  if (parens) {
    {
      annotation: {
        ...IdTagged.IdTag.mk_internal([Id.mk()]),
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
        secondary: (def_before @ before, def_after @ after),
      },
    };
  };
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

/* Shadow-aware substitution of def for x, preserving each occurrence's
 * whitespace slot. Does not rename binders: a free variable of def
 * that is rebound between the let and a use site can be captured
 * (matching the editor-wide Substitution limitation). */
let rec subst = (~bare: bool, x: string, def: Exp.t, e: Exp.t): Exp.t => {
  let go = subst(~bare, x, def);
  let (term, rewrap) = Exp.unwrap(e);
  switch (term) {
  | Var(y) when y == x =>
    inserted(~parens=!bare && needs_parens(def), def, e)
  | Let(p, d, body) =>
    rewrap(Let(p, go(d), binds(x, p) ? body : go(body)))
  | Fun(p, body, t, n) when binds(x, p) => rewrap(Fun(p, body, t, n))
  | FixF(p, body, env) when binds(x, p) => rewrap(FixF(p, body, env))
  | Match(scrut, rules) =>
    rewrap(
      Match(
        go(scrut),
        rules
        |> List.map(((p, body)) => (p, binds(x, p) ? body : go(body))),
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

/* Every substituted occurrence carries the def's ids; re-id all but
 * one so the buffer never contains duplicates (one copy keeps the
 * originals, so probes on the definition follow it somewhere) */
/* Fresh ids for every node AND secondary piece, keeping whitespace
 * content and lexemes (Exp.replace_all_ids drops secondary, which
 * would strip a duplicated copy's spacing) */
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

let dedupe_ids = (e: Exp.t): Exp.t => {
  let seen = ref(Id.Map.empty);
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let ids = IdTagged.ids(e);
        if (List.exists(id => Id.Map.mem(id, seen^), ids)) {
          refresh_ids(e);
        } else {
          List.iter(id => seen := Id.Map.add(id, (), seen^), ids);
          cont(e);
        };
      },
    e,
  );
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
          Some((strip_leading(result), f));
        | _ => None
        },
    program,
  );

let inline_let_impl: impl = {
  label: "Inline Let",
  tooltip: "Replace this let by substituting its definition",
  /* also offered at occurrences of the bound var */
  prepare: (~info_map, ~target, program) => {
    let attempt_with = (~bare, target) =>
      rewrite_let(
        ~target,
        ~matches=(p, _, _) => let_head_name(p) != None,
        ~rewrite=
          (p, def, body) => {
            let x = Option.get(let_head_name(p));
            (subst(~bare, x, def, body), Exp.rep_id(def));
          },
        program,
      );
    /* parens only where the reparse oracle proves them necessary */
    let attempt = target =>
      switch (attempt_with(~bare=true, target)) {
      | Some((cand, f)) when reparses_same(cand) => Some((cand, f))
      | Some(_) => attempt_with(~bare=false, target)
      | None => None
      };
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
  label: "Remove Unused Let",
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

/* fresh ids for a statics-derived type before it enters the buffer */
let refresh_typ_ids = (t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=
      (cont, t: Typ.t) =>
        cont({
          ...t,
          annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
        }),
    t,
  );

let fresh_typ = (term): Typ.t => {
  annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
  term,
};

let with_secondary_typ =
    (secondary: (list(Secondary.t), list(Secondary.t)), t: Typ.t): Typ.t => {
  ...t,
  annotation: {
    ...t.annotation,
    secondary,
  },
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

let exp_ty = (~info_map: Statics.Map.t, e: Exp.t): option(Typ.t) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(InfoExp({ty, _})) => Some(ty)
  | _ => None
  };

let add_annotation_impl: impl = {
  label: "Add Type Annotation",
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
                      ? fresh_typ(Parens(with_secondary_typ(([], []), bare)))
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
    switch (attempt(~parens=false)) {
    | Some((bare, f)) when reparses_same(bare) => Some((bare, f))
    | Some(_) => attempt(~parens=true)
    | None => None
    };
  },
};

let bool_pat = (p: Pat.t): option(bool) =>
  switch (IdTagged.term_of(p)) {
  | Atom(Bool(b)) => Some(b)
  | _ => None
  };

let if_to_case_impl: impl = {
  label: "Convert to Case",
  tooltip: "Rewrite this if/then/else as a case on true and false",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_node(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | If(c, t, alt) =>
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
            ))
          | _ => None
          },
      program,
    ),
};

let case_to_if_impl: impl = {
  label: "Convert to If",
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
                fresh(If(scrut, e1, strip_trailing(e2))),
                Exp.rep_id(scrut),
              ))
            | (Some(false), Some(true)) =>
              Some((
                fresh(If(scrut, e2, strip_trailing(e1))),
                Exp.rep_id(scrut),
              ))
            | _ => None
            }
          | _ => None
          },
      program,
    ),
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

let extractable = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(_)
  | EmptyHole
  | Let(_)
  | Seq(_)
  | Filter(_) => false
  | _ => true
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

/* === Lines ===
 * A node is in LINE position when it occupies a slot in a block: a
 * let's body, a fun body, a case-arm or if-branch body, or the root.
 * Extract binds at the nearest enclosing line; every binder-
 * introducing construct's body is itself a line, so the climb never
 * escapes a binder (the recursive-let def is the one exception,
 * checked explicitly). */

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

/* root-to-node path to the first node matched by ~hit */
let rec find_path = (~hit: Exp.t => bool, e: Exp.t): option(list(Exp.t)) =>
  if (hit(e)) {
    Some([e]);
  } else {
    children_of(e)
    |> List.find_map(c => find_path(~hit, c))
    |> Option.map(rest => [e, ...rest]);
  };

let same_node = (a: Exp.t, b: Exp.t): bool => Exp.rep_id(a) == Exp.rep_id(b);

let line_child = (parent: Exp.t, child: Exp.t): bool =>
  switch (IdTagged.term_of(parent)) {
  | Let(_, _, body) => same_node(body, child)
  | Fun(_, body, _, _) => same_node(body, child)
  | FixF(_, body, _) => same_node(body, child)
  | Match(_, rules) => rules |> List.exists(((_, b)) => same_node(b, child))
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

let extract_let_impl: impl = {
  label: "Extract to Let",
  tooltip: "Bind this expression to a fresh variable at the enclosing line",
  prepare: (~info_map as _, ~target, program) => {
    let x = fresh_name(program);
    /* oracle chain shared by both placements: `let ... in` extends
       maximally rightward, so a bare let can swallow what follows on
       reparse (nothing to do with variable capture — fresh_name
       avoids collisions). Parenthesize only when the reparse oracle
       says the bare form changes the program. */
    let via_oracle = (build: (~parens: bool) => option((Exp.t, Id.t))) =>
      switch (build(~parens=false)) {
      | Some((bare, f)) when reparses_same(bare) => Some((bare, f))
      | Some(_) => build(~parens=true)
      | None => None
      };
    /* fallback, and the degenerate case of extracting a whole line */
    let in_place = (~parens: bool) =>
      rewrite_node(
        ~hit=hit_node(target),
        ~rewrite=
          e =>
            extractable(e)
              ? {
                let def = pad(e |> strip_leading |> strip_trailing);
                let let_node =
                  fresh(Let(pad(fresh_pat(Var(x))), def, fresh(Var(x))));
                Some((
                  parens ? fresh(Parens(let_node)) : let_node,
                  Exp.rep_id(def),
                ));
              }
              : None,
        program,
      );
    /* the new binding lands at the nearest enclosing line; the use
       site takes over the extracted node's whitespace slot, and the
       line keeps its layout via a fresh copy of its leading run */
    let to_block = (line: Exp.t, t: Exp.t) => {
      let s = Slot.of_exp(t);
      let def = pad(Slot.drop(s, t));
      let use = Slot.give(s, fresh(Var(x)));
      let sep =
        switch (Slot.of_exp(line).lead) {
        | [] => space()
        | runs => copy_runs(runs)
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
              let let_node =
                fresh(Let(pad(fresh_pat(Var(x))), def, body));
              Some((
                parens ? fresh(Parens(let_node)) : let_node,
                Exp.rep_id(def),
              ));
            },
          program,
        );
      via_oracle(build);
    };
    switch (find_path(~hit=hit_node(target), program)) {
    | None => None
    | Some(path) =>
      let t = List.nth(path, List.length(path) - 1);
      if (extractable(t)) {
        let line = lowest_line(path);
        let blocked =
          crossed_rec_binders(line, path)
          |> List.exists(n => mentions(n, t));
        !blocked && !same_node(line, t)
          ? to_block(line, t) : via_oracle(in_place);
      } else {
        None;
      };
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
  label: "Negate & Swap Branches",
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
              let c = c |> strip_leading |> strip_trailing;
              let c = parens ? fresh(Parens(c)) : c;
              let cond = {
                ...fresh(UnOp(Bool(Not), c)),
                annotation: {
                  ...IdTagged.IdTag.mk_internal([Id.mk()]),
                  secondary: (space(), []),
                },
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
                Exp.rep_id(c),
              ));
            | _ => None
            },
        program,
      );
    switch (attempt(~parens=false)) {
    | Some((bare, f)) when reparses_same(bare) => Some((bare, f))
    | Some(_) => attempt(~parens=true)
    | None => None
    };
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
  label: "Add Missing Case Arm",
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
              let sep =
                switch (Slot.trail_of(last_body).trail) {
                | [] => space()
                | runs => copy_runs(runs)
                };
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
let rec patch_calls = (~bare_use: ref(bool), x: string, e: Exp.t): Exp.t => {
  let go = patch_calls(~bare_use, x);
  let (term, rewrap) = Exp.unwrap(e);
  switch (term) {
  | Ap(Forward, fn, arg) when is_var_named(x, fn) =>
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
    rewrap(Ap(Forward, fn, arg'));
  | Var(y) when y == x =>
    bare_use := true;
    e;
  | Let(p, d, body) =>
    rewrap(Let(p, go(d), binds(x, p) ? body : go(body)))
  | Fun(p, body, t, n) when binds(x, p) => rewrap(Fun(p, body, t, n))
  | FixF(p, body, env) when binds(x, p) => rewrap(FixF(p, body, env))
  | Match(scrut, rules) =>
    rewrap(
      Match(
        go(scrut),
        rules
        |> List.map(((p, body)) => (p, binds(x, p) ? body : go(body))),
      ),
    )
  | _ =>
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
            term: Parens(fresh_typ(Prod([clear_typ(inner), typ_unknown()]))),
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
      |> Option.map((inner': Pat.t) =>
           (
             {
               ...p,
               term: Asc(inner', ann),
             }: Pat.t
           )
         )
    | _ => None
    };
  go(p) |> Option.map(p' => (p', focus));
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

/* Shared by applies and prepare (no printing/parsing here, so gating
 * can afford the full build; rewrite_node adds the slot takeover on
 * invocation only). Shapes: let f = fun ...; let f : A -> B = fun ...
 * (annotation's arrow rewritten); let f(x) = ... (opt : Ret). */
let add_param_rewrite =
    (~program: Exp.t, e: Exp.t): option((Exp.t, Id.t)) =>
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
  label: "Add Parameter",
  tooltip: "Extend this function with a parameter; call sites get a hole",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_let(target),
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
    pat_var_names(p) |> List.sort_uniq(compare) |> List.map(y => (y, [body]))
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
let rename_sites =
    (~target: Id.t, e: Exp.t): list((string, list(Exp.t))) => {
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
    rename_sites_unfiltered(~target, e)
    |> List.filter(((y', _)) => y' == y)
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
let rec rename_free_in =
        (
          ~info_map: Statics.Map.t,
          ~count: ref(int),
          x: string,
          y: string,
          e: Exp.t,
        )
        : Exp.t => {
  let go = rename_free_in(~info_map, ~count, x, y);
  let (term, rewrap) = Exp.unwrap(e);
  switch (term) {
  | Var(z) when z == x && free_marked(~info_map, e) =>
    count := count^ + 1;
    {
      annotation: {
        ...e.annotation,
        lexeme: None,
      },
      term: Var(y),
    };
  | Let(p, _, _) when binds(y, p) => e
  | Let(p, d, body) => rewrap(Let(p, go(d), go(body)))
  | Fun(p, body, t, n) when binds(y, p) => rewrap(Fun(p, body, t, n))
  | FixF(p, body, env) when binds(y, p) => rewrap(FixF(p, body, env))
  | Match(scrut, rules) =>
    rewrap(
      Match(
        go(scrut),
        rules
        |> List.map(((p, body)) => (p, binds(y, p) ? body : go(body))),
      ),
    )
  | _ =>
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
                         replace_node(
                           ~at=Exp.rep_id(r),
                           ~with_=ren(r),
                           e,
                         ),
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
  names |> List.exists(n => mentions(n, e));

let disjoint_names = (a: list(string), b: list(string)): bool =>
  !(a |> List.exists(n => List.mem(n, b)));

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

let with_secondary =
    (secondary: (list(Secondary.t), list(Secondary.t)), e: Exp.t): Exp.t => {
  ...e,
  annotation: {
    ...e.annotation,
    secondary,
  },
};

/* one hoist step for the let at the end of ~path; returns the parent
 * node to rewrite, its replacement, and a focus id */
let hoist_step = (path: list(Exp.t)): option((Exp.t, Exp.t, Id.t)) => {
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
            ? {
              /* the def slot's occupant is now lbody, which brings its
                 own lead; l's trailing run (before the outer in) stays
                 with the def position */
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
          | [_, ..._] => copy_runs(p_lead)
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
      | Fun(fp, fbody, ft, fn)
          when
            same_node(fbody, c)
            && same_node(c, l)
            && disjoint_names(l_names, pat_var_names(fp))
            && !names_mentioned(pat_var_names(fp), ldef) =>
        /* out of a lambda: evaluates once instead of per call */
        let fun': Exp.t =
          with_secondary(
            l.annotation.secondary,
            {
              ...p,
              term: Fun(fp, lbody, ft, fn),
            },
          );
        let l': Exp.t =
          with_secondary(
            p.annotation.secondary,
            {
              ...l,
              term: Let(lp, ldef, fun'),
            },
          );
        Some((p, l', Exp.rep_id(l)));
      | _ => None
      };
    | _ => None
    };
  };
};

/* one sink step: push the let into its body's head construct */
let sink_step = (l: Exp.t): option((Exp.t, Id.t)) =>
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
    | Fun(fp, fbody, ft, fn)
        when
          disjoint_names(l_names, pat_var_names(fp))
          && !names_mentioned(pat_var_names(fp), ldef) =>
      /* into a lambda: evaluates per call */
      let l': Exp.t =
        with_secondary(
          fbody.annotation.secondary,
          {
            ...l,
            term: Let(lp, ldef, with_secondary(([], []), fbody)),
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
            && !names_mentioned(pat_var_names(rp), ldef) =>
        let l': Exp.t =
          with_secondary(
            rb.annotation.secondary,
            {
              ...l,
              term: Let(lp, ldef, with_secondary(([], []), rb)),
            },
          );
        let rules' =
          rules |> List.mapi((j, r) => j == i ? (rp, l') : r);
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
      switch (names_mentioned(l_names, t), names_mentioned(l_names, alt)) {
      | (true, false) =>
        let l': Exp.t =
          with_secondary(
            t.annotation.secondary,
            {
              ...l,
              term: Let(lp, ldef, with_secondary(([], []), t)),
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
      | (false, true) =>
        let l': Exp.t =
          with_secondary(
            alt.annotation.secondary,
            {
              ...l,
              term: Let(lp, ldef, with_secondary(([], []), alt)),
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

let hoist_let_impl: impl = {
  label: "Hoist Let",
  tooltip: "Move this binding up one level",
  prepare: (~info_map as _, ~target, program) =>
    switch (find_path(~hit=hit_let(target), program)) {
    | Some(path) =>
      switch (hoist_step(path)) {
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
  label: "Sink Let",
  tooltip: "Move this binding down into the scope that uses it",
  prepare: (~info_map as _, ~target, program) =>
    switch (
      find_path(~hit=hit_let(target), program)
      |> Option.map(path => List.nth(path, List.length(path) - 1))
    ) {
    | Some(l) =>
      switch (sink_step(l)) {
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
  | RemoveUnusedLet => remove_unused_let_impl
  | AddTypeAnnotation => add_annotation_impl
  | AddCaseArm => add_case_arm_impl
  | AddParameter => add_param_impl
  | RenameFree(x, y) => rename_free_impl(x, y)
  | HoistLet => hoist_let_impl
  | SinkLet => sink_let_impl
  | IfToCase => if_to_case_impl
  | CaseToIf => case_to_if_impl
  | ExtractLet => extract_let_impl
  | EtaReduce => eta_reduce_impl
  | NegateIf => negate_if_impl;

let all: list(Action.refactor) = [
  InlineLet,
  RemoveUnusedLet,
  AddTypeAnnotation,
  AddCaseArm,
  AddParameter,
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
    let at = let_applies(~pred=(p, _, _) => let_head_name(p) != None);
    at(target, program)
    || (
      switch (binder_of_occurrence(~info_map, ~target, program)) {
      | Some(binder) => at(binder, program)
      | None => false
      }
    );
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
    switch (find_hit(~hit=hit_let(target), program)) {
    | Some(e) => Option.is_some(add_param_rewrite(~program, e))
    | None => false
    }
  | RenameFree(x, y) =>
    switch (find_hit(~hit=hit_rename(target), program)) {
    | Some(e) =>
      rename_pairs(~info_map, ~target, e) |> List.mem((x, y))
    | None => false
    }
  | HoistLet =>
    switch (find_path(~hit=hit_let(target), program)) {
    | Some(path) => Option.is_some(hoist_step(path))
    | None => false
    }
  | SinkLet =>
    switch (find_hit(~hit=hit_let(target), program)) {
    | Some(l) => Option.is_some(sink_step(l))
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
  | ExtractLet =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) => extractable(e)
    | None => false
    }
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
           applies(kind, ~info_map, ~target, term)
             ? Some((kind, i.label, i.tooltip)) : None;
         });
    static @ rename_items(~info_map, ~target, term);
  };

let go =
    (~info_map: Statics.Map.t, kind: Action.refactor, z: Zipper.t)
    : option(Zipper.t) =>
  switch (Indicated.index(z)) {
  | None => None
  | Some(target) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    switch (impl(kind).prepare(~info_map, ~target, term)) {
    | None => None
    | Some((term', focus)) =>
      let seg =
        ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term')
        |> SpaceNormalize.go;
      let z' = {
        ...Zipper.unzip(seg),
        refractors: z.refractors,
      };
      Some(
        switch (Move.jump_to_id_indicated(z', focus)) {
        | Some(z'') => z''
        | None => z'
        },
      );
    };
  };
