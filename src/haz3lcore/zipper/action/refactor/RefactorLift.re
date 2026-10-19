/* Function lifting and gesture diagnostics (blockers). */
open Language;
open RefactorBase;

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
      /* the convoy walk; at the ceiling the enclosure decides */
      let site =
        switch (dep_run_walk(path)) {
        | Some({block, stop: StopCeiling(i, anc), _}) =>
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
              Some((anc, LiftSugar, block, pat_var_names(args) |> List.rev))
            | None => None
            }
          | _ => None
          };
        | _ => None
        };
      switch (site) {
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

let lift_function_impl: impl = {
  label: "Lift to helper",
  tooltip: "Move this definition (and the ones it depends on) out of the function, taking the parameters it uses as arguments",
  prepare: (~info_map, ~target, program) =>
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
        /* a helper param is a copy of the outer binder it abstracts
           (what statics says x means at the lifted def), so the check
           sees references rebinding to it as preserved */
        let outer_binder = x =>
          Id.Map.find_opt(Exp.rep_id(cdef), info_map)
          |> Option.bind(_, i => Ctx.lookup_var(Info.ctx_of(i), x))
          |> Option.map((e: Ctx.var_entry) => e.id);
        let var_pats = () =>
          site.ls_crossed
          |> List.mapi((i, x) =>
               {
                 ...fresh_pat(Var(x)),
                 annotation: {
                   ...
                     IdTagged.IdTag.mk_internal([
                       switch (outer_binder(x)) {
                       | Some(b) => fresh_from(b)
                       | None => Id.mk()
                       },
                     ]),
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
        let cbody'' = Slot.occupy(top.annotation.secondary, cbody');
        /* the helper is written with definition sugar, let g(args) =
           interior (andrew), whatever form the source function had */
        let (helper_pat, helper_def) = {
          let args: Pat.t =
            switch (var_pats()) {
            | [v] => v
            | vs => fresh_pat(Tuple(vs))
            };
          let bare = fresh_pat(Ap(with_secondary_pat(([], []), cp), args));
          (with_secondary_pat(cp.annotation.secondary, bare), interior);
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
            /* the helper IS the lifted line, moved: it keeps that
               line's let (ids), so it travels rather than vanishing
               and reappearing */
            let new_let =
              with_secondary(
                (o_lead, []),
                {
                  ...c,
                  term: Let(helper_pat, helper_def, outer'),
                },
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
        };
      | _ => None
      };
    },
};

/* === Move into caller (the inverse of Lift to helper) ===
 * `let g(xs) = d in let o = fun ps -> b in r` (o's def possibly
 * parenthesized, or o itself sugar `let o(ps) = b`): when every use of
 * g is a call g(xs) inside b, with xs among ps, g moves back into o as
 * `let g = d in` and each call becomes plain g. The helper keeps its
 * let (ids): a drag holds it as it moves in. References to the
 * helper's parameters now mean o's parameters (the calls passed
 * exactly those): binder_redirect tells the scope check so. */

/* the variable names a parameter pattern binds, in order: a variable
   or a (parenthesized) tuple of variables; with their binder ids */
let rec param_vars = (p: Pat.t): option(list((string, Id.t))) =>
  switch (IdTagged.term_of(p)) {
  | Var(x) => Some([(x, Pat.rep_id(p))])
  | Parens(inner) => param_vars(inner)
  | Tuple(items) =>
    List.fold_right(
      (it: Pat.t, acc) =>
        switch (IdTagged.term_of(it), acc) {
        | (Var(x), Some(rest)) => Some([(x, Pat.rep_id(it)), ...rest])
        | _ => None
        },
      items,
      Some([]),
    )
  | _ => None
  };

/* the argument variables of a call g(args) */
let rec arg_vars = (a: Exp.t): option(list(string)) =>
  switch (IdTagged.term_of(a)) {
  | Var(x) => Some([x])
  | Parens(inner) => arg_vars(inner)
  | Tuple(items) =>
    List.fold_right(
      (it: Exp.t, acc) =>
        switch (IdTagged.term_of(it), acc) {
        | (Var(x), Some(rest)) => Some([x, ...rest])
        | _ => None
        },
      items,
      Some([]),
    )
  | _ => None
  };

type unlift_site = {
  helper: Exp.t, /* the let g(xs) = d line */
  name_pat: Pat.t, /* g's own binder, reused by the local let */
  helper_params: list((string, Id.t)),
  body: Exp.t, /* d */
  caller: Exp.t, /* the let o = fun ps -> b line */
  caller_params: list((string, Id.t)),
  fun_body: Exp.t, /* b */
  rebuild: Exp.t => Exp.t /* o's def with a new b */,
};

let unlift_site = (~target: Id.t, program: Exp.t): option(unlift_site) =>
  switch (find_hit(~hit=hit_def_line(target), program)) {
  | None => None
  | Some(h) =>
    switch (IdTagged.term_of(h)) {
    | Let(hp, hd, o) =>
      let helper =
        switch (IdTagged.term_of(hp), IdTagged.term_of(hd)) {
        | (Ap(fv, argp), _) =>
          switch (var_pat_name(fv), param_vars(argp)) {
          | (Some(g), Some(xs)) => Some((g, fv, xs, hd))
          | _ => None
          }
        | (Var(g), Fun(fp, fbody, _, _)) =>
          param_vars(fp) |> Option.map(xs => (g, hp, xs, fbody))
        | _ => None
        };
      let caller =
        switch (IdTagged.term_of(o)) {
        | Let(op, odef, orest) =>
          let rec in_fun = (d: Exp.t) =>
            switch (IdTagged.term_of(d)) {
            | Parens(inner) =>
              in_fun(inner)
              |> Option.map(((ps, b, rb)) =>
                   (
                     ps,
                     b,
                     (
                       (b') => (
                         {
                           ...d,
                           term: Parens(rb(b')),
                         }: Exp.t
                       )
                     ),
                   )
                 )
            | Fun(fp, fb, ft, fname) =>
              param_vars(fp)
              |> Option.map(ps =>
                   (
                     ps,
                     fb,
                     (
                       (b') => (
                         {
                           ...d,
                           term: Fun(fp, b', ft, fname),
                         }: Exp.t
                       )
                     ),
                   )
                 )
            | _ => None
            };
          let shape =
            switch (IdTagged.term_of(op)) {
            | Ap(_, argp) =>
              param_vars(argp) |> Option.map(ps => (ps, odef, (b' => b')))
            | _ => in_fun(odef)
            };
          shape |> Option.map(((ps, b, rb)) => (o, ps, b, rb, orest));
        | _ => None
        };
      switch (helper, caller) {
      | (Some((g, name_pat, xs, d)), Some((o, ps, b, rebuild, orest))) =>
        let names = List.map(fst, xs);
        let calls = ref(0);
        let _ =
          Exp.map_term(
            ~f_exp=
              (cont, e: Exp.t) => {
                switch (IdTagged.term_of(e)) {
                | Ap(_, f, a)
                    when
                      IdTagged.term_of(f) == Var(g)
                      && arg_vars(a) == Some(names) =>
                  incr(calls)
                | _ => ()
                };
                cont(e);
              },
            b,
          );
        names != []
        && List.for_all(x => List.mem_assoc(x, ps), names)
        && !free_in(g, orest)
        && !free_in(g, d)
        && calls^ >= 1
        && calls^ == List.length(occurrences_of(g, b))
          ? Some({
              helper: h,
              name_pat,
              helper_params: xs,
              body: d,
              caller: o,
              caller_params: ps,
              fun_body: b,
              rebuild,
            })
          : None;
      | _ => None
      };
    | _ => None
    }
  };

let unlift_rewrite = (site: unlift_site): option((Exp.t, Id.t)) => {
  let g = var_pat_name(site.name_pat) |> Option.value(~default="");
  let names = List.map(fst, site.helper_params);
  /* each call g(xs) becomes g, taking the call's place */
  let b =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) =>
          switch (IdTagged.term_of(e)) {
          | Ap(_, f, a)
              when
                IdTagged.term_of(f) == Var(g) && arg_vars(a) == Some(names) => {
              ...f,
              annotation: {
                ...f.annotation,
                secondary: e.annotation.secondary,
              },
            }
          | _ => cont(e)
          },
      site.fun_body,
    );
  /* the local let opens the body on its own line: it takes the body's
     line break, the body gets a fresh one */
  let b_lead = Slot.lead_of(b).lead;
  let sep =
    has_newline(b_lead)
      ? sep_like(b_lead)
      : {
        let h_lead = sep_like(fst(site.helper.annotation.secondary));
        (has_newline(h_lead) ? h_lead : newline()) @ space() @ space();
      };
  let b = Slot.drop(Slot.lead_of(b), b);
  let local: Exp.t = {
    annotation: {
      ...site.helper.annotation,
      secondary: (has_newline(b_lead) ? b_lead : sep, []),
    },
    term:
      Let(
        pad(with_secondary_pat(([], []), site.name_pat)),
        pad(site.body |> strip_leading |> strip_trailing),
        with_secondary((sep_copy(sep), snd(b.annotation.secondary)), b),
      ),
  };
  switch (IdTagged.term_of(site.caller)) {
  | Let(op, _, orest) =>
    List.iter(
      ((x, hb)) =>
        switch (List.assoc_opt(x, site.caller_params)) {
        | Some(pb) => binder_redirect := Id.Map.add(hb, pb, binder_redirect^)
        | None => ()
        },
      site.helper_params,
    );
    let caller': Exp.t = {
      ...site.caller,
      term: Let(op, site.rebuild(local), orest),
    };
    Some((strip_leading_ws(caller'), Exp.rep_id(site.helper)));
  | _ => None
  };
};

let unlift_function_impl: impl = {
  label: "Move into caller",
  tooltip: "Move this helper back into the one function that calls it, as a local definition (the inverse of Lift to helper)",
  prepare: (~info_map as _, ~target, program) =>
    switch (unlift_site(~target, program)) {
    | Some(site) =>
      switch (unlift_rewrite(site)) {
      | Some((caller', focus)) =>
        rewrite_node(
          ~hit=same_node(site.helper),
          ~rewrite=_ => Some((caller', focus)),
          program,
        )
      | None => None
      }
    | None => None
    },
};
