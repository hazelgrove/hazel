open Language;
open RefactorBase;
open RefactorInline;

/* Re-exports for external call sites (web, tests) */
let roundtrip_settings = RefactorBase.roundtrip_settings;
let reparses_same = RefactorBase.reparses_same;
let dedupe_healed = RefactorBase.dedupe_healed;
let eq_defs = RefactorBase.eq_defs;
type impl =
  RefactorBase.impl = {
    label: string,
    tooltip: string,
    prepare:
      (~info_map: Statics.Map.t, ~target: Id.t, Exp.t) =>
      option((Exp.t, Id.t)),
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
let statement_crossable = (l: def_line, stmt: Exp.t): bool =>
  !names_mentioned(line_exp_names(l), stmt)
  && !mentions_typ_names(line_typ_names(l), stmt);

/* one hoist step for the let at the end of ~path; returns the parent
 * node to rewrite, its replacement, and a focus id. ~fixup as in
 * sink_step: invocation moves the released body's textual lead into
 * the vacated slot (prints; gating passes false). */
/* === Absorption ===
 * Two IDENTICAL definitions merge when one moves onto the other:
 * the STATIONARY binding survives (ids kept — P7), the moving one
 * dissolves, and its uses repoint to the survivor. Gates: simple
 * var heads, syntactically identical defs (modulo whitespace and
 * parens), and the survivor's name is never rebound in the
 * dissolved scope (capture guard, conservative). */
let absorb_lines =
    (~survivor: Exp.t, ~dissolved_head: Pat.t, scope: Exp.t)
    : option((Exp.t, Id.t)) =>
  switch (IdTagged.term_of(survivor)) {
  | Let(sp, _, _) =>
    switch (let_head_name(sp), let_head_name(dissolved_head)) {
    | (Some(sn), Some(dn)) when !binds_somewhere(sn, scope) =>
      let scope' = sn == dn ? scope : rename_syntactic(dn, sn, scope);
      Some((scope', Pat.rep_id(sp)));
    | _ => None
    }
  | _ => None
  };

let absorbable = (upper: Exp.t, lower: Exp.t): bool =>
  switch (IdTagged.term_of(upper), IdTagged.term_of(lower)) {
  | (Let(up, ud, _), Let(lp, ld, _)) =>
    Option.is_some(let_head_name(up))
    && Option.is_some(let_head_name(lp))
    && eq_defs(ud, ld)
  | _ => false
  };

/* 4th component: rep ids of the definition lines whose attached doc
   blocks should be re-homed after the rewrite (carry_attached_docs) */
let hoist_step =
    (~fixup: bool, path: list(Exp.t))
    : option((Exp.t, Exp.t, Id.t, list(Id.t))) => {
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
    /* def-line general moves first: absorption of an identical
       neighbor, chain swap with another definition line, or one
       step up past a statement */
    /* absorption of twins is its OWN refactoring (MergeUp) — the
       gesture ladder discriminates; movement stays a pure swap */
    let chain_swap =
      switch (def_line_of(l), def_line_of(p)) {
      | (Some((bl, lbody)), Some((bp, pbody)))
          when
            same_node(pbody, c)
            && same_node(c, l)
            && lines_swappable(bp, bl) =>
        /* chain swap; the two lines exchange line slots (attached
           doc blocks are re-homed by carry_attached_docs, from the
           textual pre-image — holders of a line's lead vary) */
        let m': Exp.t =
          with_secondary(l.annotation.secondary, def_line_rebuild(p, lbody));
        let l': Exp.t =
          with_secondary(p.annotation.secondary, def_line_rebuild(l, m'));
        Some((p, l', Exp.rep_id(l), [Exp.rep_id(l), Exp.rep_id(p)]));
      | _ => None
      };
    let seq_hoist =
      switch (IdTagged.term_of(p), def_line_of(l)) {
      | (Seq(s1, tail), Some((bl, lbody)))
          when
            same_node(tail, c)
            && same_node(c, l)
            && statement_crossable(bl, s1) =>
        /* Seq(s1, line(rest)) -> line(Seq(s1, rest)): up past one
           statement. Statement leads live on leaves, so the slot
           exchange is textual (P3) — fixup only (gating can't
           print). */
        if (fixup) {
          let sl = Slot.lead_of(l);
          let ss = Slot.lead_of(s1);
          let s1' = Slot.give(sl, Slot.drop(ss, s1));
          let inner: Exp.t = {
            ...p,
            term: Seq(s1', lbody),
          };
          let result =
            Slot.give(ss, def_line_rebuild(Slot.drop(sl, l), inner));
          Some((
            p,
            result,
            Exp.rep_id(l),
            [Exp.rep_id(l), Exp.rep_id(s1)],
          ));
        } else {
          let inner: Exp.t = {
            ...p,
            term: Seq(s1, lbody),
          };
          Some((p, def_line_rebuild(l, inner), Exp.rep_id(l), []));
        }
      | _ => None
      };
    switch (chain_swap, seq_hoist) {
    | (Some(r), _)
    | (_, Some(r)) => Some(r)
    | (None, None) =>
      switch (IdTagged.term_of(l)) {
      | Let(lp, ldef, lbody) =>
        let l_names = pat_var_names(lp);
        switch (IdTagged.term_of(p)) {
        | Let(mp, mdef, mbody)
            when
              same_node(mdef, c)
              && disjoint_names(l_names, pat_var_names(mp))
              && !names_mentioned(l_names, mbody)
              /* the def region is scoped by mp's vars — the binder
                 (recursion) and, for function sugar `let f(a, b) =`,
                 the params: a line mentioning them cannot leave
                 (hoisting span out of a sugar def unbound the args —
                 andrew) */
              && !names_mentioned(pat_var_names(mp), ldef) =>
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
          Some((p, l', Exp.rep_id(l), [Exp.rep_id(l)]));
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
          Some((p, l', Exp.rep_id(l), [Exp.rep_id(l)]));
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
          Some((p, l', Exp.rep_id(l), [Exp.rep_id(l)]));
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
          Some((p, l', Exp.rep_id(l), [Exp.rep_id(l)]));
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
              Some((p, l', Exp.rep_id(l), [Exp.rep_id(l)]));
            }
            : None
        };
      | _ => None
      }
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

let sink_step = (~fixup: bool, l: Exp.t): option((Exp.t, Id.t, list(Id.t))) => {
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
  /* def-line general moves first: absorption of an identical
     neighbor, chain swap with the next definition line, or one step
     down past a statement */
  /* absorption = MergeDown, its own refactoring; the ladder
     discriminates — movement stays a pure swap */
  let chain_sink =
    switch (def_line_of(l)) {
    | Some((bl, lbody)) =>
      switch (def_line_of(lbody)) {
      | Some((bm, mbody)) when lines_swappable(bl, bm) =>
        let l': Exp.t =
          with_secondary(
            lbody.annotation.secondary,
            def_line_rebuild(l, mbody),
          );
        let m': Exp.t =
          with_secondary(
            l.annotation.secondary,
            def_line_rebuild(lbody, l'),
          );
        Some((m', Exp.rep_id(l), [Exp.rep_id(l), Exp.rep_id(lbody)]));
      | _ => None
      }
    | None => None
    };
  let seq_sink =
    switch (def_line_of(l)) {
    | Some((bl, lbody)) =>
      switch (IdTagged.term_of(lbody)) {
      | Seq(s1, rest) when statement_crossable(bl, s1) =>
        /* line(Seq(s1, rest)) -> Seq(s1, line(rest)): down past one
           statement; textual slot exchange, fixup only */
        if (fixup) {
          let sl = Slot.lead_of(l);
          let ss = Slot.lead_of(s1);
          let s1' = Slot.give(sl, Slot.drop(ss, s1));
          let inner =
            Slot.give(ss, def_line_rebuild(Slot.drop(sl, l), rest));
          let result: Exp.t = {
            ...lbody,
            term: Seq(s1', inner),
          };
          Some((result, Exp.rep_id(l), [Exp.rep_id(l), Exp.rep_id(s1)]));
        } else {
          let result: Exp.t = {
            ...lbody,
            term: Seq(s1, def_line_rebuild(l, rest)),
          };
          Some((result, Exp.rep_id(l), []));
        }
      | _ => None
      }
    | None => None
    };
  switch (chain_sink, seq_sink) {
  | (Some(r), _)
  | (_, Some(r)) => Some(r)
  | (None, None) =>
    switch (IdTagged.term_of(l)) {
    | Let(lp, ldef, lbody) =>
      let l_names = pat_var_names(lp);
      switch (IdTagged.term_of(lbody)) {
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
        let nesting =
          fixup && !has_newline(d_lead) && has_newline(host_sep());
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
        Some((m', Exp.rep_id(l), [Exp.rep_id(l)]));
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
        Some((fun', Exp.rep_id(l), [Exp.rep_id(l)]));
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
          Some((match', Exp.rep_id(l), [Exp.rep_id(l)]));
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
            [Exp.rep_id(l)],
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
            [Exp.rep_id(l)],
          ));
        | _ => None
        }
      | _ => None
      };
    | _ => None
    }
  };
};

/* the flat Secondary run printed immediately before the piece with
   this id, searched recursively (tiles nest) */

/* === Merge (absorption as a named refactoring) ===
 * The menu is the vocabulary: one entry, one contract. MergeUp
 * dissolves this definition into its identical twin ABOVE (the twin
 * survives, uses repoint); MergeDown into the twin BELOW. Gestures
 * discriminate (Up = merge-when-applicable, else hoist — the same
 * ladder shape as Down's sink-else-feed). */
let survivor_name_free = (survivor: Exp.t, scope: Exp.t): bool =>
  switch (IdTagged.term_of(survivor)) {
  | Let(sp, _, _) =>
    switch (let_head_name(sp)) {
    | Some(sn) => !binds_somewhere(sn, scope)
    | None => false
    }
  | _ => false
  };

let merge_site_up = (~target, program): option((Exp.t, Exp.t)) =>
  /* (parent twin = survivor, this line) — the parent's body is l */
  switch (find_path(~hit=hit_def_line(target), program)) {
  | Some(path) when List.length(path) >= 2 =>
    let n = List.length(path);
    let l = List.nth(path, n - 1);
    let p = List.nth(path, n - 2);
    switch (def_line_of(p), def_line_of(l)) {
    | (Some((_, pbody)), Some((_, lbody)))
        when
          same_node(pbody, l)
          && absorbable(p, l)
          && survivor_name_free(p, lbody) =>
      Some((p, l))
    | _ => None
    };
  | _ => None
  };

let merge_site_down = (~target, program): option(Exp.t) =>
  switch (find_hit(~hit=hit_def_line(target), program)) {
  | Some(l) =>
    switch (def_line_of(l)) {
    | Some((_, lbody)) when absorbable(l, lbody) =>
      switch (IdTagged.term_of(lbody)) {
      | Let(_, _, mbody) when survivor_name_free(lbody, mbody) => Some(l)
      | _ => None
      }
    | _ => None
    }
  | None => None
  };

let merge_up_impl: impl = {
  label: "Merge up",
  tooltip: "Dissolve this definition into its identical twin above",
  prepare: (~info_map as _, ~target, program) =>
    switch (merge_site_up(~target, program)) {
    | Some((p, l)) =>
      switch (IdTagged.term_of(l)) {
      | Let(lp, _, lbody) =>
        switch (absorb_lines(~survivor=p, ~dissolved_head=lp, lbody)) {
        | Some((scope', focus)) =>
          rewrite_node(
            ~hit=same_node(p),
            ~rewrite=_ => Some((def_line_rebuild(p, scope'), focus)),
            program,
          )
        | None => None
        }
      | _ => None
      }
    | None => None
    },
};

let merge_down_impl: impl = {
  label: "Merge down",
  tooltip: "Dissolve this definition into its identical twin below",
  prepare: (~info_map as _, ~target, program) =>
    switch (merge_site_down(~target, program)) {
    | Some(l) =>
      switch (IdTagged.term_of(l)) {
      | Let(lp, _, lbody) =>
        switch (IdTagged.term_of(lbody)) {
        | Let(_, _, mbody) =>
          switch (absorb_lines(~survivor=lbody, ~dissolved_head=lp, mbody)) {
          | Some((mbody', focus)) =>
            /* the dissolved upper line's break dies with it; the
               surviving line moves up a slot — its docs ride along */
            rewrite_node(
              ~hit=same_node(l),
              ~rewrite=
                _ =>
                  Some((
                    strip_leading_ws(def_line_rebuild(lbody, mbody')),
                    focus,
                  )),
              program,
            )
            |> Option.map(((prog', f)) =>
                 (
                   carry_attached_docs(
                     ~line_ids=[Exp.rep_id(lbody)],
                     ~pre=program,
                     prog',
                   ),
                   f,
                 )
               )
          | None => None
          }
        | _ => None
        }
      | _ => None
      }
    | None => None
    },
};

let hoist_let_impl: impl = {
  label: "Hoist",
  tooltip: "Move this binding up one level",
  prepare: (~info_map as _, ~target, program) =>
    switch (find_path(~hit=hit_def_line(target), program)) {
    | Some(path) =>
      switch (hoist_step(~fixup=true, path)) {
      /* movement never parenthesizes, so no invocation oracle: the
         whole-program reparse cost ~0.5s per press on a few-page
         buffer. Reparse-safety is covered by the movement reparse
         tests in Test_Refactor instead. */
      | Some((pnode, result, focus, carry)) =>
        rewrite_node(
          ~hit=same_node(pnode),
          ~rewrite=_ => Some((result, focus)),
          program,
        )
        |> Option.map(((prog', f)) =>
             (carry_attached_docs(~line_ids=carry, ~pre=program, prog'), f)
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
      find_path(~hit=hit_def_line(target), program)
      |> Option.map(path => List.nth(path, List.length(path) - 1))
    ) {
    | Some(l) =>
      switch (sink_step(~fixup=true, l)) {
      | Some((result, focus, carry)) =>
        rewrite_node(
          ~hit=same_node(l),
          ~rewrite=_ => Some((result, focus)),
          program,
        )
        |> Option.map(((prog', f)) =>
             (carry_attached_docs(~line_ids=carry, ~pre=program, prog'), f)
           )
      | None => None
      }
    | None => None
    },
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
    let parens =
      switch (build(e)) {
      | Some((bare, _)) =>
        splice_parens_needed(~program, ~at=Exp.rep_id(e), bare)
      | None => true
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
let hoist_carry_site = (~target: Id.t, program: Exp.t): option(list(Exp.t)) =>
  switch (find_path(~hit=hit_def_line(target), program)) {
  | None => None
  | Some(path) =>
    let n = List.length(path);
    let c = List.nth(path, n - 1);
    switch (def_line_of(c)) {
    | None => None
    | Some((c_dl, _)) =>
      let rec walk = (i, block: list(def_line), block_nodes: list(Exp.t)) =>
        if (i < 0) {
          None;
        } else {
          let anc = List.nth(path, i);
          let child = List.nth(path, i + 1);
          switch (def_line_of(anc)) {
          | Some((dl, body)) when same_node(body, child) =>
            if (block |> List.exists(m => line_depends_on(m, dl))) {
              walk(i - 1, [dl, ...block], [anc, ...block_nodes]);
            } else {
              /* anc = X. Only offered when something is carried
                 (plain hoist owns the no-dependency case), and every
                 block member can legally cross X */
              List.length(block_nodes) >= 2
              && block
              |> List.for_all(m => lines_swappable(dl, m))
                ? Some([anc, ...block_nodes]) : None;
            }
          | _ => None /* ceiling: no line above in this chain */
          };
        };
      walk(n - 2, [c_dl], [c]);
    };
  };

let hoist_carry_impl: impl = {
  label: "Hoist with dependencies",
  tooltip: "Move this binding up, carrying the definitions it depends on",
  prepare: (~info_map as _, ~target, program) =>
    switch (hoist_carry_site(~target, program)) {
    | None => None
    | Some(nodes) =>
      let c = List.nth(nodes, List.length(nodes) - 1);
      let cbody =
        switch (def_line_of(c)) {
        | Some((_, body)) => body
        | None => c /* unreachable: site checked */
        };
      /* the lines rotate one slot: [X, T1..Tm, C] -> [T1..Tm, C, X];
         each line takes the SECONDARY of the old occupant of its new
         position (chain_swap generalized — every run used once, no
         piece duplication) */
      let secs = nodes |> List.map((nd: Exp.t) => nd.annotation.secondary);
      let new_order = List.tl(nodes) @ [List.hd(nodes)];
      let result =
        List.fold_left(
          (inner, (node, sec)) =>
            with_secondary(sec, def_line_rebuild(node, inner)),
          cbody,
          List.rev(List.combine(new_order, secs)),
        );
      let carry = nodes |> List.map(Exp.rep_id);
      rewrite_node(
        ~hit=same_node(List.hd(nodes)),
        ~rewrite=_ => Some((result, Exp.rep_id(c))),
        program,
      )
      |> Option.map(((prog', f)) =>
           (carry_attached_docs(~line_ids=carry, ~pre=program, prog'), f)
         );
    },
};

/* === Lift to helper (abstract over the enclosing binders) ===
 * The grabbed definition — plus the contiguous block of dependency
 * lines above it (same walk as the convoy) — leaves the enclosing
 * function as a helper when the block reaches the function ceiling:
 * carried deps become lets INSIDE the helper body, and the helper
 * abstracts over whichever params the block mentions (andrew: the
 * stuck block resolves by splitting the whole thing into a helper).
 * Covers both enclosure shapes: an explicit `fun p ->` in a let def,
 * and function sugar `let f(p) = ...`; the helper is emitted in the
 * matching style. Params PREPEND (helper(args) == the old def), so
 * every use rewrites uniformly u -> u(args). */

type lift_enclosure =
  | LiftFun(Exp.t, option(Exp.t)) /* fun node, def-position parens */
  | LiftSugar; /* outer let IS the sugar binder */

type lift_site_t = {
  ls_outer: Exp.t,
  ls_enc: lift_enclosure,
  ls_block: list(Exp.t), /* T1..C top-to-bottom; C last */
  ls_crossed: list(string),
};

/* ids of free VAR USES of any of `names` in a region (the pointing
   half of a refusal: these tokens are why) */
let blocked_uses_out: ref(list(Id.t)) = ref([]);

let lift_site = (~target: Id.t, program: Exp.t): option(lift_site_t) =>
  switch (find_path(~hit=hit_def_line(target), program)) {
  | None => None
  | Some(path) =>
    let n = List.length(path);
    let c = List.nth(path, n - 1);
    switch (IdTagged.term_of(c)) {
    | Let(cp, _, cbody) when let_head_name(cp) != None =>
      let g = Option.get(let_head_name(cp));
      /* the convoy walk: contiguous dependency run above C */
      let rec walk = (i, block_dls: list(def_line), block: list(Exp.t)) =>
        if (i < 0) {
          None;
        } else {
          let anc = List.nth(path, i);
          let child = List.nth(path, i + 1);
          switch (def_line_of(anc)) {
          | Some((dl, body)) when same_node(body, child) =>
            block_dls |> List.exists(m => line_depends_on(m, dl))
              ? walk(i - 1, [dl, ...block_dls], [anc, ...block])
              /* a non-dependency line above: convoy territory */
              : None
          | _ =>
            /* ceiling: the enclosure decides */
            let top = List.hd(block);
            switch (IdTagged.term_of(anc)) {
            | Fun(fp, fbody, _, _) when same_node(fbody, top) && i >= 1 =>
              let (outer, parens) =
                switch (IdTagged.term_of(List.nth(path, i - 1))) {
                | Parens(_) when i >= 2 => (
                    List.nth(path, i - 2),
                    Some(List.nth(path, i - 1)),
                  )
                | _ => (List.nth(path, i - 1), None)
                };
              switch (IdTagged.term_of(outer)) {
              | Let(_, odef, _)
                  when
                    same_node(
                      odef,
                      switch (parens) {
                      | Some(pn) => pn
                      | None => anc
                      },
                    ) =>
                Some((
                  outer,
                  LiftFun(anc, parens),
                  block,
                  pat_var_names(fp) |> List.rev,
                ))
              | _ => None
              };
            | Let(op, odef, _) when same_node(odef, top) =>
              switch (FunctionSugar.detect(op)) {
              | Some((_, args, _)) =>
                Some((
                  anc,
                  LiftSugar,
                  block,
                  pat_var_names(args) |> List.rev,
                ))
              | None => None
              }
            | _ => None
            };
          };
        };
      let c_dl =
        switch (def_line_of(c)) {
        | Some((dl, _)) => dl
        | None => LetLine(cp, fresh(EmptyHole)) /* unreachable */
        };
      switch (walk(n - 2, [c_dl], [c])) {
      | None => None
      | Some((outer, enc, block, params)) =>
        let defs = block |> List.filter_map(nd => def_line_of(nd));
        let crossed =
          params
          |> List.filter(x =>
               defs
               |> List.exists(((dl, _)) => free_in(x, line_material(dl)))
             );
        let carried = block |> List.filter(nd => !same_node(nd, c));
        let wall_names =
          switch (IdTagged.term_of(outer), enc) {
          | (Let(op, _, _), LiftFun(_)) => pat_var_names(op)
          | (Let(op, _, _), LiftSugar) =>
            switch (FunctionSugar.detect(op)) {
            | Some((fn, _, _)) => pat_var_names(fn)
            | None => pat_var_names(op)
            }
          | _ => []
          };
        let outer_minus =
          replace_node(
            ~at=Exp.rep_id(List.hd(block)),
            ~with_=fresh(EmptyHole),
            outer,
          );
        /* each wall reports its culprit tokens (collectors run only
           on the failing path; a passing site pays booleans only) */
        blocked_uses_out := [];
        let wall = (ids: list(Id.t)): bool => {
          blocked_uses_out := blocked_uses_out^ @ ids;
          ids == [];
        };
        let carried_names =
          carried
          |> List.concat_map(nd =>
               switch (def_line_of(nd)) {
               | Some((dl, _)) => line_exp_names(dl)
               | None => []
               }
             );
        /* carried deps are absorbed into the helper: uses of their
           bindings below the block would unbind */
        let ok_dep_below =
          wall(
            var_use_ids(
              carried_names |> List.filter(x => free_in(x, cbody)),
              cbody,
            ),
          );
        /* the helper name / crossed params rebound below: the args
           at those use sites would mean the wrong thing */
        let ok_shadow =
          wall(
            [g, ...crossed]
            |> List.filter(x => binds_somewhere(x, cbody))
            |> (names => names == [] ? [] : binder_ids_in(names, cbody)),
          );
        /* the enclosure's own binder mentioned in the block:
           recursion would unbind above it */
        let ok_recursion =
          wall(
            defs
            |> List.concat_map(((dl, _)) =>
                 wall_names
                 |> List.exists(o => free_in(o, line_material(dl)))
                   ? var_use_ids(wall_names, line_material(dl)) : []
               ),
          );
        /* an existing free g in outer would be captured by the new
           helper */
        let ok_capture =
          wall(
            free_in(g, outer_minus) ? var_use_ids([g], outer_minus) : [],
          );
        let ok_typ =
          carried
          |> List.for_all(nd =>
               switch (def_line_of(nd)) {
               | Some((dl, _)) =>
                 !mentions_typ_names(line_typ_names(dl), cbody)
               | None => false
               }
             );
        crossed != []
        && ok_dep_below
        && ok_shadow
        && ok_recursion
        && ok_capture
        && ok_typ
        && !List.mem(g, wall_names)
          ? Some({
              ls_outer: outer,
              ls_enc: enc,
              ls_block: block,
              ls_crossed: crossed,
            })
          : None;
      };
    | _ => None
    };
  };

/* the culprit tokens when a lift refuses (dead-press only) */
let lift_wall_blockers = (~target: Id.t, program: Exp.t): list(Id.t) => {
  blocked_uses_out := [];
  switch (lift_site(~target, program)) {
  | Some(_) => []
  | None => blocked_uses_out^
  };
};

/* culprits for refused UPWARD movement: dispatched on the parent
   form, mirroring hoist_step's arms (dead-press only) */
let hoist_blockers = (~target: Id.t, program: Exp.t): list(Id.t) =>
  switch (find_path(~hit=hit_def_line(target), program)) {
  | None => []
  | Some(path) =>
    let n = List.length(path);
    if (n < 2) {
      [];
    } else {
      let l = List.nth(path, n - 1);
      switch (def_line_of(l)) {
      | None => []
      | Some((l_dl, _)) =>
        let l_names = line_exp_names(l_dl);
        /* 1. the convoy walk's X refuses: a same-name line in the
           way (the shadow wall) — shake ITS binder */
        let shadow_x = {
          let rec walk = (i, block_dls: list(def_line)) =>
            if (i < 0) {
              [];
            } else {
              let anc = List.nth(path, i);
              let child = List.nth(path, i + 1);
              switch (def_line_of(anc)) {
              | Some((dl, body)) when same_node(body, child) =>
                if (block_dls |> List.exists(m => line_depends_on(m, dl))) {
                  walk(i - 1, [dl, ...block_dls]);
                } else if (block_dls
                           |> List.for_all(m => lines_swappable(dl, m))) {
                  [];
                } else {
                  let block_names =
                    block_dls |> List.concat_map(line_exp_names);
                  let colliding =
                    line_exp_names(dl)
                    |> List.filter(x => List.mem(x, block_names));
                  switch (dl) {
                  | LetLine(xp, _) => pat_binder_ids(colliding, xp)
                  | TypeLine(_) => []
                  };
                }
              | _ => []
              };
            };
          walk(n - 2, [l_dl]);
        };
        /* 2. parent-form walls */
        let direct = List.nth(path, n - 2);
        let (par, c) =
          switch (IdTagged.term_of(direct)) {
          | Parens(_) when n >= 3 => (List.nth(path, n - 3), direct)
          | _ => (direct, l)
          };
        let parent_wall =
          switch (IdTagged.term_of(par), IdTagged.term_of(l)) {
          | (Let(mp, mdef, _), Let(_, ldef, _)) when same_node(mdef, c) =>
            /* escaping a def region that scopes over the line: sugar
               params / the recursive binder */
            names_mentioned(pat_var_names(mp), ldef)
              ? var_use_ids(pat_var_names(mp), ldef) : []
          | (Seq(s1, tail), _) when same_node(tail, c) =>
            /* crossing a statement that references the line's names */
            names_mentioned(l_names, s1) ? var_use_ids(l_names, s1) : []
          | (Match(_) | If(_), _) when same_node(c, l) =>
            /* exiting an arm/branch: same-name uses elsewhere in the
               parent would be captured by the widened scope */

            let p_minus =
              replace_node(~at=Exp.rep_id(l), ~with_=fresh(EmptyHole), par);
            l_names |> List.exists(x => free_in(x, p_minus))
              ? var_use_ids(l_names, p_minus) : [];
          | (Fun(fp, fbody, _, _), _)
              when same_node(fbody, c) && same_node(c, l) =>
            /* the line's name collides with a lambda param */

            let colliding =
              l_names |> List.filter(x => List.mem(x, pat_var_names(fp)));
            colliding == [] ? [] : pat_binder_ids(colliding, fp);
          | _ => []
          };
        shadow_x @ parent_wall;
      };
    };
  };

/* culprits for refused DOWNWARD movement (dead-press only) */
let sink_blockers = (~target: Id.t, program: Exp.t): list(Id.t) =>
  switch (find_path(~hit=hit_def_line(target), program)) {
  | None => []
  | Some(path) =>
    let l = List.nth(path, List.length(path) - 1);
    switch (def_line_of(l)) {
    | None => []
    | Some((l_dl, lbody)) =>
      let l_names = line_exp_names(l_dl);
      switch (def_line_of(lbody), IdTagged.term_of(lbody)) {
      | (Some((m_dl, _)), _) when !lines_swappable(l_dl, m_dl) =>
        /* the line below depends on us (its uses pin us above it)
           or shadows us (its binder) */
        let dep_uses = var_use_ids(l_names, line_material(m_dl));
        let colliding =
          line_exp_names(m_dl) |> List.filter(x => List.mem(x, l_names));
        let shadow =
          switch (m_dl) {
          | LetLine(xp, _) => pat_binder_ids(colliding, xp)
          | TypeLine(_) => []
          };
        dep_uses @ shadow;
      | (_, Seq(s1, _)) when names_mentioned(l_names, s1) =>
        var_use_ids(l_names, s1)
      | _ => []
      };
    };
  };

/* the pointing half of a dead gesture press: which tokens are why.
   Consulted by the web layer only after gesture AND insist both
   decline (zero hot-path cost). */
let gesture_blockers =
    (~term: Exp.t, g: Action.Gesture.t, z: Zipper.t): list(Id.t) =>
  switch (Indicated.index(z)) {
  | None => []
  | Some(target) =>
    switch (g) {
    | Up =>
      lift_wall_blockers(~target, term)
      @ hoist_blockers(~target, term)
      |> List.sort_uniq(compare)
    | Down => sink_blockers(~target, term) |> List.sort_uniq(compare)
    | _ => []
    }
  };

let lift_function_impl: impl = {
  label: "Lift to helper",
  tooltip: "Move this definition (and the ones it depends on) out of the function, taking the parameters it uses as arguments",
  prepare: (~info_map as _, ~target, program) =>
    switch (lift_site(~target, program)) {
    | None => None
    | Some(site) =>
      let c = List.nth(site.ls_block, List.length(site.ls_block) - 1);
      switch (IdTagged.term_of(c)) {
      | Let(cp, cdef, cbody) =>
        let g = Option.get(let_head_name(cp));
        let carried = site.ls_block |> List.filter(nd => !same_node(nd, c));
        let sep_lead = i => i == 0 ? ([], []) : (space(), []);
        /* the helper INTERIOR: carried dep lines wrap C's def */
        /* multiline helper (andrew): with carried lets inside, the
           helper body breaks — each let line on its own line, the
           result trailing, all indented one step past the helper's
           line; the closing `in` returns to the helper's level.
           Fresh seps per line (block-top's ORIGINAL pieces are
           donated to the vacated slot — sharing would duplicate
           ids). Singleton helpers stay one-liners. */
        let (o_lead_for_sep, _) = site.ls_outer.annotation.secondary;
        let base_sep = () =>
          switch (o_lead_for_sep) {
          | [_, ..._] => sep_like(o_lead_for_sep)
          | [] => newline()
          };
        let inner_sep = () => base_sep() @ space() @ space();
        let carried' =
          carried |> List.map(nd => with_secondary((inner_sep(), []), nd));
        let interior =
          List.fold_left(
            (inner, nd) => def_line_rebuild(nd, inner),
            carried == []
              ? with_secondary(
                  (space(), []),
                  cdef |> strip_leading |> strip_trailing,
                )
              : with_secondary(
                  (inner_sep(), []),
                  cdef |> strip_leading |> strip_trailing,
                ),
            List.rev(carried'),
          );
        /* the closing `in` on its own line for multiline helpers */
        let interior =
          carried == []
            ? interior
            : with_secondary(
                (fst(interior.annotation.secondary), base_sep()),
                interior,
              );
        let mk_arg_exps = () =>
          switch (site.ls_crossed) {
          | [x] => fresh(Var(x))
          | xs =>
            fresh(
              Tuple(
                xs
                |> List.mapi((i, x) =>
                     {
                       ...fresh(Var(x)),
                       annotation: {
                         ...IdTagged.IdTag.mk_internal([Id.mk()]),
                         secondary: sep_lead(i),
                       },
                     }
                   ),
              ),
            )
          };
        let var_pats = () =>
          site.ls_crossed
          |> List.mapi((i, x) =>
               {
                 ...fresh_pat(Var(x)),
                 annotation: {
                   ...IdTagged.IdTag.mk_internal([Id.mk()]),
                   secondary: sep_lead(i),
                 },
               }
             );
        /* uses of g below the block become calls passing the params */
        let cbody' =
          Exp.map_term(
            ~f_exp=
              (cont, e: Exp.t) =>
                switch (IdTagged.term_of(e)) {
                | Var(x) when x == g =>
                  let u = with_secondary(([], []), e);
                  {
                    ...fresh(Ap(Forward, u, mk_arg_exps())),
                    annotation: {
                      ...IdTagged.IdTag.mk_internal([Id.mk()]),
                      secondary: e.annotation.secondary,
                    },
                  };
                | _ => cont(e)
                },
            cbody,
          );
        /* the vacated region: C's body takes the BLOCK TOP's slot */
        let top = List.hd(site.ls_block);
        let cbody'' = {
          let sl = Slot.lead_of(cbody');
          let dropped = Slot.drop(sl, cbody');
          let (b, a) = dropped.annotation.secondary;
          let (t_lead, t_after) = top.annotation.secondary;
          with_secondary((t_lead @ b, a @ t_after), dropped);
        };
        let (helper_pat, helper_def) =
          switch (site.ls_enc) {
          | LiftFun(_) =>
            let param: Pat.t =
              switch (var_pats()) {
              | [v] => pad(v)
              | vs => pad(fresh_pat(Parens(fresh_pat(Tuple(vs)))))
              };
            let fun_helper = fresh(Fun(param, interior, None, None));
            (
              cp,
              carried == []
                ? pad(fun_helper)
                : with_secondary((space(), []), fun_helper),
            );
          | LiftSugar =>
            /* the source used sugar; the helper does too:
               let g(args) = interior */
            let args: Pat.t =
              switch (var_pats()) {
              | [v] => v
              | vs => fresh_pat(Tuple(vs))
              };
            let sugar_pat = {
              let bare =
                fresh_pat(Ap(with_secondary_pat(([], []), cp), args));
              with_secondary_pat(cp.annotation.secondary, bare);
            };
            (sugar_pat, interior);
          };
        let rebuild_outer = (odef_new: Exp.t): option((Exp.t, Id.t)) =>
          switch (IdTagged.term_of(site.ls_outer)) {
          | Let(op, _, obody) =>
            let (o_lead, o_after) = site.ls_outer.annotation.secondary;
            let sep =
              switch (o_lead) {
              | [_, ..._] => sep_like(o_lead)
              | [] => newline()
              };
            let outer_rebuilt: Exp.t = {
              ...site.ls_outer,
              term: Let(op, odef_new, obody),
            };
            let outer' = with_secondary((sep, o_after), outer_rebuilt);
            let new_let =
              with_secondary(
                (o_lead, []),
                fresh(Let(helper_pat, helper_def, outer')),
              );
            Some((new_let, Pat.rep_id(cp)));
          | _ => None
          };
        let odef_new =
          switch (site.ls_enc) {
          | LiftFun(f, parens) =>
            switch (IdTagged.term_of(f)) {
            | Fun(fp, _, ft, fname) =>
              let fun': Exp.t = {
                ...f,
                term: Fun(fp, cbody'', ft, fname),
              };
              switch (parens) {
              | Some(pn) =>
                Some(
                  {
                    ...pn,
                    term: Parens(fun'),
                  }: Exp.t,
                )
              | None => Some(fun')
              };
            | _ => None
            }
          | LiftSugar => Some(cbody'')
          };
        switch (Option.bind(odef_new, rebuild_outer)) {
        | None => None
        | Some((new_let, focus)) =>
          rewrite_node(
            ~hit=same_node(site.ls_outer),
            ~rewrite=_ => Some((new_let, focus)),
            program,
          )
          |> Option.map(((prog', f)) =>
               (
                 carry_attached_docs(
                   ~line_ids=
                     (site.ls_block |> List.map(Exp.rep_id))
                     @ [Exp.rep_id(site.ls_outer)],
                   ~pre=program,
                   prog',
                 ),
                 f,
               )
             )
        };
      | _ => None
      };
    },
};

let evaluate_in_place_impl: impl = {
  label: "Evaluate closed",
  tooltip: "Replace this self-contained expression with its value",
  prepare: (~info_map, ~target, program) => {
    let hit = (e: Exp.t) => hit_node(target, e) && !is_value_literal(e);
    let build = (e: Exp.t) => {
      /* elaborate in the LOCAL TYPE context — constructors keep
         their ADT membership (ctor equality needs a definite
         elaborated type; standalone they are free and poly_equal
         goes indet). Closedness still holds: evaluation runs in
         the builtin env only, so context VARS have types but no
         values — any use evaluates indeterminate and fails the
         value gate. */
      let ctx =
        Id.Map.find_opt(Exp.rep_id(e), info_map) |> Option.map(Info.ctx_of);
      let elab =
        CachedStatics.init_from_term(
          ~settings=CoreSettings.on,
          ~is_dynamic_term=false,
          ~ctx?,
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

let ap_to_let_prepare = (~landing=true, ~info_map as _, ~target, program) => {
  let build = (e: Exp.t): option((Exp.t, Id.t)) =>
    beta_parts(e)
    |> Option.map(((p, arg, body)) => {
         let p = pad(p);
         let def = pad(arg |> strip_leading |> strip_trailing);
         /* landing block: a multiline construct breaks after the
            in; a body with its own break keeps it; inline stays
            inline */
         let body: Exp.t = body;
         let body =
           switch (
             landing ? Some(intro_sep(~program, ~at=Exp.rep_id(e))) : None
           ) {
           | Some(sep) when !has_newline(Slot.of_exp(body).lead) =>
             let body = strip_leading(body);
             with_secondary(
               (sep_copy(sep), snd(body.annotation.secondary)),
               body,
             );
           | _ => body
           };
         (fresh(Let(p, def, body)), Pat.rep_id(p));
       });
  /* parens per the feed policy: region-scoped reparse oracle;
     conservative parens when no bounded region (a root-level ap
     can have right siblings the bare let would absorb) */
  switch (find_hit(~hit=hit_beta(target), program)) {
  | None => None
  | Some(e) =>
    let parens =
      switch (build(e)) {
      | Some((bare, _)) =>
        splice_parens_needed(~program, ~at=Exp.rep_id(e), bare)
      | None => true
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
};

let ap_to_let_impl: impl = {
  label: "Bind argument",
  tooltip: "Rewrite this application of a function literal as a let binding its argument",
  prepare: (~info_map, ~target, program) =>
    ap_to_let_prepare(~info_map, ~target, program),
};

/* === Unfold call ===
 * At f(args) where f is a let-bound function: substitute f's
 * definition INTO this call (a one-use feed — the binding survives
 * for other uses) and rotate the resulting lambda application to a
 * let. Skips beta's two menu steps when stepping through calls of
 * named functions; deliberately stops at the let (staged, not
 * substituted — andrew: it's already skipping a step). */
let unfold_site = (~info_map, ~target, program): option(Id.t) =>
  switch (find_hit(~hit=hit_node(target), program)) {
  | Some(e) =>
    let ap_of = (e: Exp.t) =>
      switch (IdTagged.term_of(e)) {
      | Ap(Forward, f, _) =>
        switch (IdTagged.term_of(f)) {
        | Var(_) =>
          switch (
            binder_of_occurrence(~info_map, ~target=Exp.rep_id(f), program)
          ) {
          | Some(binder) =>
            switch (find_hit(~hit=hit_let(binder), program)) {
            | Some(l) =>
              switch (IdTagged.term_of(l)) {
              | Let(_, def, _) =>
                switch (IdTagged.term_of(def)) {
                | Fun(_) => Some(Exp.rep_id(f))
                | _ => None
                }
              | _ => None
              }
            | None => None
            }
          | None => None
          }
        | _ => None
        }
      | _ => None
      };
    /* the caret may sit on the ap tile or the fn var itself */
    switch (ap_of(e)) {
    | Some(fid) => Some(fid)
    | None =>
      switch (find_path(~hit=hit_node(target), program)) {
      | Some(path) when List.length(path) >= 2 =>
        ap_of(List.nth(path, List.length(path) - 2))
      | _ => None
      }
    };
  | None => None
  };

let unfold_call_impl: impl = {
  label: "Unfold call",
  tooltip: "Substitute the called function here and bind its argument",
  prepare: (~info_map, ~target, program) =>
    switch (unfold_site(~info_map, ~target, program)) {
    | Some(f_use) =>
      /* feed f's def into THIS use (occurrence-targeted feed) */
      switch (feed_let_impl.prepare(~info_map, ~target=f_use, program)) {
      | Some((prog', fed)) =>
        /* the lambda now sits where the var was — rotate its
           application. Chain on feed's focus id, not f_use: the
           copy's root id is branch policy (drag clones fresh). */
        ap_to_let_impl.prepare(~info_map, ~target=fed, prog')
      | None => None
      }
    | None => None
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
let rec wrap_bindings =
        (
          ~sep: option(list(Secondary.t))=None,
          bs: list((Pat.t, Exp.t)),
          body: Exp.t,
        )
        : Exp.t =>
  switch (bs) {
  | [] => body
  | [(x, v), ...rest] =>
    let inner = wrap_bindings(~sep, rest, body);
    let inner =
      switch (sep) {
      /* landing block: each binding takes its own line; a body
         that already breaks keeps its own lead */
      | Some(sp) =>
        rest == [] && has_newline(Slot.of_exp(inner).lead)
          ? inner
          /* strip the aggregated lead first — the old inline space
             can live on a descendant (see the Slot trap note) */
          : {
            let inner = strip_leading(inner);
            with_secondary(
              (sep_copy(sp), snd(inner.annotation.secondary)),
              inner,
            );
          }
      /* lead only: the nested let follows the outer `in`; its right
         edge is the body's own end */
      | None => rest == [] ? inner : with_secondary((space(), []), inner)
      };
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
let case_to_lets = (~landing: bool, ~target, program): option((Exp.t, Id.t)) => {
  let build = (e: Exp.t) =>
    switch (IdTagged.term_of(e)) {
    | Match(scrut, rules) =>
      pick_arm(scrut, rules)
      |> Option.map(((bs, body)) => {
           let sep =
             landing ? Some(intro_sep(~program, ~at=Exp.rep_id(e))) : None;
           let body = body |> strip_leading |> strip_trailing;
           let built = wrap_bindings(~sep, bs, body);
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
        | Some(([_, ..._], _)) =>
          case_to_lets(~landing=true, ~target, program)
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
               case_to_lets(~landing=false, ~target, program),
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

let split_let_prepare = (~landing=true, ~info_map as _, ~target, program) =>
  rewrite_node(
    ~hit=hit_let(target),
    ~rewrite=
      e =>
        switch (IdTagged.term_of(e)) {
        | Let(p, def, body) when let_head_name(p) == None =>
          switch (match_value(p, def)) {
          | Matched(bs) =>
            let sep =
              landing ? Some(intro_sep(~program, ~at=Exp.rep_id(e))) : None;
            let built = wrap_bindings(~sep, bs, body);
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
  );

let split_let_impl: impl = {
  label: "Split let",
  tooltip: "Destructure this pattern binding into one let per variable",
  prepare: (~info_map, ~target, program) =>
    split_let_prepare(~info_map, ~target, program),
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
        switch (
          ap_to_let_prepare(~landing=false, ~info_map, ~target, program)
        ) {
        | Some((prog', binder_id)) =>
          /* info_map is stale for prog' — inline at the binder is
             syntactic (name, occurrences, subst), so it doesn't
             matter */
          inline_let_impl.prepare(~info_map, ~target=binder_id, prog')
        | None => None
        }
      | Some((_p, _arg, _)) =>
        /* tuple/pattern parameter: bind, destructure the pattern
           over the argument (split-let's matcher gates), then
           inline each var binder — Step is never dead at an applied
           lambda just because its parameter is a tuple */
        switch (
          ap_to_let_prepare(~landing=false, ~info_map, ~target, program)
        ) {
        | Some((prog', binder_id)) =>
          let hit_binder = (e: Exp.t) =>
            switch (IdTagged.term_of(e)) {
            | Let(p'', _, _) => Pat.rep_id(p'') == binder_id
            | _ => false
            };
          switch (find_hit(~hit=hit_binder, prog')) {
          | Some(l) =>
            switch (IdTagged.term_of(l)) {
            | Let(p', _, _) =>
              /* the split keeps the pattern's var nodes as the new
                 binders (ids travel), so their ids are known up
                 front; wildcards bind nothing and drop */
              let binder_ids = {
                let acc = ref([]);
                let _ =
                  Pat.map_term(
                    ~f_pat=
                      (cont, q: Pat.t) => {
                        switch (IdTagged.term_of(q)) {
                        | Var(_) => acc := [Pat.rep_id(q), ...acc^]
                        | _ => ()
                        };
                        cont(q);
                      },
                    p',
                  );
                List.rev(acc^);
              };
              binder_ids
              |> List.fold_left(
                   (acc, bid) =>
                     Option.bind(acc, ((prog, _)) =>
                       inline_let_impl.prepare(~info_map, ~target=bid, prog)
                     ),
                   split_let_prepare(
                     ~landing=false,
                     ~info_map,
                     ~target=binder_id,
                     prog',
                   ),
                 );
            | _ => None
            }
          | None => None
          };
        | None => None
        }
      | None => None
      }
    },
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

/* free type names in the alias's scope: rename x to this alias
   (the typo-repair mirror of RenameFree; binder = the affordance) */

let impl: Action.refactor => impl =
  fun
  | InlineLet => inline_let_impl
  | FeedLet => feed_let_impl
  | RemoveUnusedLet => remove_unused_let_impl
  | InlineAlias => inline_alias_impl
  | ExtractAlias => extract_alias_impl
  | AddTypeAnnotation => add_annotation_impl
  | EtaExpand => eta_expand_impl
  | EvaluateInPlace => evaluate_in_place_impl
  | AddCaseArm => add_case_arm_impl
  | ExpandWildcard => expand_wildcard_impl
  | AddParameter => add_param_impl
  | RemoveParameter => remove_param_impl
  | RenameFree(x, y) => rename_free_impl(x, y)
  | RenameTypFree(x, t) => rename_typ_free_impl(x, t)
  | SwapParams(i) => swap_params_impl(i)
  | SwapArms(i) => swap_arms_impl(i)
  | SwapTuplePat(i) => swap_tuple_pat_impl(i)
  | HoistLet => hoist_let_impl
  | SinkLet => sink_let_impl
  | MergeUp => merge_up_impl
  | MergeDown => merge_down_impl
  | IfToCase => if_to_case_impl
  | CaseToIf => case_to_if_impl
  | ExtractLet => extract_let_impl
  | Explode => explode_impl
  | Implode => implode_impl
  | EtaReduce => eta_reduce_impl
  | BindArgument => ap_to_let_impl
  | BetaReduce => beta_reduce_impl
  | UnfoldCall => unfold_call_impl
  | HoistCarry => hoist_carry_impl
  | LiftFunction => lift_function_impl
  | SplitLet => split_let_impl
  | ReduceCase => reduce_case_impl
  | BindArm => bind_arm_impl
  | ReduceIf => reduce_if_impl
  | NegateIf => negate_if_impl;

let all: list(Action.refactor) = [
  InlineLet,
  FeedLet,
  RemoveUnusedLet,
  InlineAlias,
  AddTypeAnnotation,
  EtaExpand,
  EvaluateInPlace,
  BetaReduce,
  BindArgument,
  UnfoldCall,
  HoistCarry,
  LiftFunction,
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
  MergeUp,
  MergeDown,
  ExtractLet,
  Explode,
  Implode,
  ExtractAlias,
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
  | InlineAlias =>
    switch (find_hit(~hit=hit_tyalias(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | TyAlias(tp, ty, body) =>
        switch (tpat_names(tp)) {
        | [t] => !typ_mentions([t], ty) && !contains_use(body)
        | _ => false
        }
      | _ => false
      }
    | None => false
    }
  | InlineLet =>
    let at = let_applies(~pred=inline_matches);
    at(target, program)
    || (
      switch (binder_of_occurrence(~info_map, ~target, program)) {
      | Some(binder) => at(binder, program)
      | None => false
      }
    );
  | FeedLet =>
    Option.is_some(feed_plan(~info_map, ~target, program))
    || (
      switch (find_hit(~hit=hit_tyalias(target), program)) {
      | Some(e) =>
        switch (IdTagged.term_of(e)) {
        | TyAlias(tp, ty, body) =>
          switch (tpat_names(tp)) {
          | [t] =>
            !typ_mentions([t], ty)
            && !contains_use(body)
            && count_typ_uses(t, body) > 0
          | _ => false
          }
        | _ => false
        }
      | None => false
      }
    )
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
    || (
      switch (find_hit(~hit=hit_tyalias(target), program)) {
      | Some(e) =>
        switch (IdTagged.term_of(e)) {
        | TyAlias(tp, _, body) =>
          tpat_names(tp) != [] && !mentions_typ_names(tpat_names(tp), body)
        | _ => false
        }
      | None => false
      }
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
  | RenameTypFree(x, t) =>
    rename_typ_pairs(~target, program) |> List.mem((x, t))
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
    switch (find_path(~hit=hit_def_line(target), program)) {
    | Some(path) => Option.is_some(hoist_step(~fixup=false, path))
    | None => false
    }
  | SinkLet =>
    switch (find_hit(~hit=hit_def_line(target), program)) {
    | Some(l) => Option.is_some(sink_step(~fixup=false, l))
    | None => false
    }
  | MergeUp => Option.is_some(merge_site_up(~target, program))
  | MergeDown => Option.is_some(merge_site_down(~target, program))
  | Explode =>
    switch (find_hit(~hit=hit_let(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Let(p, def, _) =>
        let_head_name(p) != None
        && sugar_fn_name(p) == None
        && !x_reduced(def)
      | _ => false
      }
    | None => false
    }
  | Implode =>
    switch (find_hit(~hit=hit_let(target), program)) {
    | Some(y) =>
      Option.is_some(implode_parent(~y_id=Exp.rep_id(y), program))
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
  | ExtractAlias =>
    switch (typ_extract_site(~target, program)) {
    | Some((path, ty)) when alias_extractable(path, ty) =>
      let line = lowest_line(path);
      !(
        crossed_typ_binders(line, path)
        |> List.exists(n => typ_mentions([n], ty))
      );
    | _ => false
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
  | BindArgument => Option.is_some(find_hit(~hit=hit_beta(target), program))
  | UnfoldCall => Option.is_some(unfold_site(~info_map, ~target, program))
  | HoistCarry => Option.is_some(hoist_carry_site(~target, program))
  | LiftFunction => Option.is_some(lift_site(~target, program))
  | BetaReduce =>
    switch (find_hit(~hit=hit_beta(target), program)) {
    | Some(e) =>
      switch (beta_parts(e)) {
      | Some((p, arg, _)) =>
        let_head_name(p) != None
        || (
          switch (match_value(p, arg)) {
          | Matched(_) => true
          | _ => false
          }
        )
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
  | MergeUp =>
    switch (merge_site_up(~target, term)) {
    | Some((p, _)) =>
      switch (IdTagged.term_of(p)) {
      | Let(sp, _, _) =>
        let_head_name(sp) |> Option.map(n => "Merge into " ++ n)
      | _ => None
      }
    | None => None
    }
  | MergeDown =>
    switch (merge_site_down(~target, term)) {
    | Some(l) =>
      switch (IdTagged.term_of(l)) {
      | Let(_, _, lbody) =>
        switch (IdTagged.term_of(lbody)) {
        | Let(sp, _, _) =>
          let_head_name(sp) |> Option.map(n => "Merge into " ++ n)
        | _ => None
        }
      | _ => None
      }
    | None => None
    }
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
    : list((Action.refactor, string, string)) => {
  let exp_items =
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
  let typ_items =
    rename_typ_pairs(~target, term)
    |> List.map(((x, t)) =>
         (
           Action.RenameTypFree(x, t),
           "Rename " ++ x ++ " to " ++ t,
           "Bind free type mentions of " ++ x ++ " at this alias",
         )
       );
  exp_items @ typ_items;
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
    let in_def_zone =
      Option.is_some(find_hit(~hit=hit_def_line(target), term));
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
    | Step =>
      /* take a step of evaluation here: the reduce family, context-
         resolved like the spatial arrows; a closed non-value with no
         syntactic step falls through to full evaluation (reduce
         takes priority when both apply) */
      switch (app(BetaReduce)) {
      | Some(k) => Some(k)
      | None =>
        switch (app(ReduceCase)) {
        | Some(k) => Some(k)
        | None =>
          switch (app(ReduceIf)) {
          | Some(k) => Some(k)
          | None => app(EvaluateInPlace)
          }
        }
      }
    | Bind =>
      /* stage the step: introduce the binding without substituting */
      switch (app(BindArgument)) {
      | Some(k) => Some(k)
      | None => app(BindArm)
      }
    | Up =>
      if (in_arm_zone) {
        arm_swap(-1);
      } else if (in_def_zone) {
        /* absorption preempts the swap rung when a twin is above
           (menu keeps both; the gesture picks the smart one) */
        switch (app(MergeUp)) {
        | Some(k) => Some(k)
        | None => app(HoistLet)
        };
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
        switch (app(ExtractLet)) {
        | Some(k) => Some(k)
        | None => app(ExtractAlias)
        };
      }
    | Down =>
      if (in_arm_zone) {
        arm_swap(1);
      } else if (in_let_zone) {
        /* movement rung if one exists, else the value flows: feed the
           nearest use (the last feed consumes the let) */
        switch (app(MergeDown)) {
        | Some(k) => Some(k)
        | None =>
          switch (app(SinkLet)) {
          | Some(k) => Some(k)
          | None => app(FeedLet)
          }
        };
      } else if (in_def_zone) {
        /* type line: movement rung, else the alias flows one use
           at a time (the last feed consumes — let-feed parity) */
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
        /* a fn-position use unfolds — feed composed with the
           rotation, the smarter feed there (andrew); other
           occurrences: the definition feeds THIS use */
        switch (app(UnfoldCall)) {
        | Some(k) => Some(k)
        | None => app(FeedLet)
        };
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

/* mergeInto targets (D2 emerge reversed): when this invocation will
   ABSORB (identical definitions merging) or GLOM (extract reusing an
   identical existing def), the dissolved window's ids + the
   surviving window's ids — staged for the convergence flight. */
let merge_target =
    (~info_map as _, ~target, kind: Action.refactor, term)
    : (list(Id.t), list(Id.t)) => {
  let line_ids = (e: Exp.t): list(Id.t) =>
    switch (IdTagged.term_of(e)) {
    | Let(p, d, _) =>
      IdTagged.ids(e) @ pat_subtree_ids(p) @ exp_subtree_ids(d)
    | _ => IdTagged.ids(e)
    };
  switch (kind) {
  | MergeUp =>
    switch (merge_site_up(~target, term)) {
    | Some((p, l)) => (line_ids(l), line_ids(p))
    | None => ([], [])
    }
  | MergeDown =>
    switch (merge_site_down(~target, term)) {
    | Some(l) =>
      switch (def_line_of(l)) {
      | Some((_, lbody)) => (line_ids(l), line_ids(lbody))
      | None => ([], [])
      }
    | None => ([], [])
    }
  | ExtractLet =>
    switch (extract_path(~target, term)) {
    | Some(path) =>
      let t = List.nth(path, List.length(path) - 1);
      let line = lowest_line(path);
      let blocked =
        crossed_rec_binders(line, path) |> List.exists(n => mentions(n, t));
      let rec host = (path: list(Exp.t)) =>
        switch (path) {
        | [parent, child, ..._] when same_node(child, line) => Some(parent)
        | [_, ...rest] => host(rest)
        | [] => None
        };
      switch (blocked ? None : host(path)) {
      | Some(h) =>
        switch (IdTagged.term_of(h)) {
        | Let(lp, ldef, lbody)
            when same_node(lbody, line) && eq_defs(ldef, t) =>
          switch (let_head_name(lp)) {
          | Some(nm) when !List.mem(nm, binders_over(Exp.rep_id(t), lbody)) => (
              exp_subtree_ids(t),
              exp_subtree_ids(ldef),
            )
          | _ => ([], [])
          }
        | _ => ([], [])
        }
      | None => ([], [])
      };
    | None => ([], [])
    }
  | _ => ([], [])
  };
};

let gesture_merge_target =
    (~info_map, ~term, g: Action.Gesture.t, z: Zipper.t)
    : (list(Id.t), list(Id.t)) =>
  switch (Indicated.index(z), gesture(~info_map, ~term, g, z)) {
  | (Some(target), Some(kind)) =>
    merge_target(~info_map, ~target, kind, term)
  | _ => ([], [])
  };

let refactor_merge_target =
    (~info_map, ~term, kind: Action.refactor, z: Zipper.t)
    : (list(Id.t), list(Id.t)) =>
  switch (Indicated.index(z)) {
  | Some(target) => merge_target(~info_map, ~target, kind, term)
  | None => ([], [])
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

/* the INSIST tier: remedied moves a dead press escalates to on a
   second press (web tracks the pending state; menu lists these as
   their own entries per P10) */
let gesture_insist =
    (~info_map: Statics.Map.t, ~term: Exp.t, g: Action.Gesture.t, z: Zipper.t)
    : option(Action.refactor) =>
  switch (Indicated.index(z)) {
  | None => None
  | Some(target) =>
    let app = (k: Action.refactor) =>
      applies(k, ~info_map, ~target, term) ? Some(k) : None;
    switch (g) {
    | Up =>
      switch (app(HoistCarry)) {
      | Some(k) => Some(k)
      | None => app(LiftFunction)
      }
    | _ => None
    };
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
