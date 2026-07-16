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
           way (the shadow wall) — shake ITS binder. Carry owns the
           all-swappable case, so only a genuine collision blocks. */
        let shadow_x =
          switch (dep_run_walk(path)) {
          | Some({block_dls, stop: StopLine(_, dl), _})
              when !(block_dls |> List.for_all(m => lines_swappable(dl, m))) =>
            let block_names = block_dls |> List.concat_map(line_exp_names);
            let colliding =
              line_exp_names(dl)
              |> List.filter(x => List.mem(x, block_names));
            switch (dl) {
            | LetLine(xp, _) => pat_binder_ids(colliding, xp)
            | TypeLine(_) => []
            };
          | _ => []
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
