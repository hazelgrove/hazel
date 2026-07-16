open Language;
open RefactorBase;
open RefactorInline;
open RefactorMove;

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
