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
    drop_secondary(
      List.map((w: Secondary.t) => w.id, s.lead @ s.trail),
      e,
    );
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
let refresh_ids = (e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) => {
        let refresh_sec = (ws: list(Secondary.t)) =>
          ws
          |> List.map((w: Secondary.t) =>
               {
                 ...w,
                 id: Id.mk(),
               }
             );
        let (before, after) = e.annotation.secondary;
        {
          ...e,
          annotation: {
            ids: [Id.mk()],
            secondary: (refresh_sec(before), refresh_sec(after)),
            incomplete: [],
            lexeme: e.annotation.lexeme,
          },
        }
        |> cont;
      },
    e,
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
        ~matches=(p, _, _) => var_pat_name(p) != None,
        ~rewrite=
          (p, def, body) => {
            let x = Option.get(var_pat_name(p));
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
        (p, _, _) => var_pat_name(p) != None && pat_unused(~info_map, p),
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
  !unknown^;
};

let exp_ty = (~info_map: Statics.Map.t, e: Exp.t): option(Typ.t) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(InfoExp({ty, _})) => Some(ty)
  | _ => None
  };

let add_annotation_impl: impl = {
  label: "Add Type Annotation",
  tooltip: "Annotate this binding with its inferred type",
  prepare: (~info_map, ~target, program) =>
    rewrite_node(
      ~hit=hit_let(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Let(p, def, body) when var_pat_name(p) != None =>
            switch (exp_ty(~info_map, def)) {
            | Some(ty) when typ_known(ty) =>
              let ty = pad(refresh_typ_ids(ty));
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
    ),
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

let extract_let_impl: impl = {
  label: "Extract to Let",
  tooltip: "Bind this expression to a fresh variable in place",
  prepare: (~info_map as _, ~target, program) => {
    let x = fresh_name(program);
    let attempt = (~parens: bool) =>
      rewrite_node(
        ~hit=hit_node(target),
        ~rewrite=
          e =>
            extractable(e)
              ? {
                let def = pad(e |> strip_leading |> strip_trailing);
                let let_node =
                  fresh(
                    Let(pad(fresh_pat(Var(x))), def, fresh(Var(x))),
                  );
                Some((
                  parens ? fresh(Parens(let_node)) : let_node,
                  Exp.rep_id(def),
                ));
              }
              : None,
        program,
      );
    /* `let ... in` extends maximally rightward, so a bare let in a
       tight position can swallow the rest of the enclosing expression
       on reparse (nothing to do with variable capture — fresh_name
       avoids collisions). Parenthesize only when the reparse oracle
       says the bare form changes the program. */
    switch (attempt(~parens=false)) {
    | Some((bare, f)) when reparses_same(bare) => Some((bare, f))
    | Some(_) => attempt(~parens=true)
    | None => None
    };
  },
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
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_node(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | If(c, t, alt) =>
            /* arms swap UNTOUCHED so their formatting (incl.
               multi-line layout) survives; the pre-`else` boundary
               run belongs to the SLOT, not the arm — the new then-arm
               takes it over. The condition sheds its post-`if` lead
               (it now sits after `!`). */
            let c = c |> strip_leading |> strip_trailing;
            let c = needs_parens(c) ? fresh(Parens(c)) : c;
            let cond = {
              ...fresh(UnOp(Bool(Not), c)),
              annotation: {
                ...IdTagged.IdTag.mk_internal([Id.mk()]),
                secondary: (space(), []),
              },
            };
            let boundary = Slot.trail_of(t);
            Some((
              fresh(
                If(cond, Slot.give(boundary, alt), Slot.drop(boundary, t)),
              ),
              Exp.rep_id(c),
            ));
          | _ => None
          },
      program,
    ),
};

let impl: Action.refactor => impl =
  fun
  | InlineLet => inline_let_impl
  | RemoveUnusedLet => remove_unused_let_impl
  | AddTypeAnnotation => add_annotation_impl
  | IfToCase => if_to_case_impl
  | CaseToIf => case_to_if_impl
  | ExtractLet => extract_let_impl
  | EtaReduce => eta_reduce_impl
  | NegateIf => negate_if_impl;

let all: list(Action.refactor) = [
  InlineLet,
  RemoveUnusedLet,
  AddTypeAnnotation,
  ExtractLet,
  EtaReduce,
  IfToCase,
  CaseToIf,
  NegateIf,
];

/* Menu support: which refactorings apply at the current indication */
let menu_items =
    (~info_map: Statics.Map.t, z: Zipper.t)
    : list((Action.refactor, string, string)) =>
  switch (Indicated.index(z)) {
  | None => []
  | Some(target) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    all
    |> List.filter_map(kind => {
         let i = impl(kind);
         Option.is_some(i.prepare(~info_map, ~target, term))
           ? Some((kind, i.label, i.tooltip)) : None;
       });
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
