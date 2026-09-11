/* Keyboard-gesture front-end: resolve a directional press to a
 * refactoring (gesture / two-press insist), and on a dead press
 * report which tokens blocked it (gesture_blockers). */
open Language;
open RefactorBase;
open RefactorMove;
open RefactorLift;
open RefactorRegistry;

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
