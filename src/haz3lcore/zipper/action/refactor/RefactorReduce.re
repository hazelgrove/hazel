/* Reduction refactorings: beta, ap-to-let, case/if reduction,
 * eta, unfold, evaluate-in-place. */
open Language;
open RefactorBase;
open RefactorParens;
open RefactorInline;

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
      | LimitedCompleted((v, _)) when is_value_literal(v) =>
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
  reduce_prepare(~hit=hit_beta(target), ~build, program);
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
