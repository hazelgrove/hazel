/* Movement refactorings: hoist/sink, chain swaps, statement
 * crossings, merges, convoy carry, swaps, param add/remove. */
open Language;
open RefactorBase;

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
