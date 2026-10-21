/* The refactoring registry and how to run one: the kind->impl
 * table, the static applicability gate, and go (splice a prepared
 * transform back into the zipper). Modality-agnostic; the menu,
 * gesture, and drag front-ends all sit on top of this. */
open Language;
open RefactorBase;
open RefactorInline;
open RefactorMove;
open RefactorReduce;
open RefactorLift;

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
  | SinkCarry => sink_carry_impl
  | LiftFunction => lift_function_impl
  | UnliftFunction => unlift_function_impl
  | SplitComponent => split_component_impl
  | JoinLet => join_let_impl
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
  SinkCarry,
  LiftFunction,
  UnliftFunction,
  SplitComponent,
  JoinLet,
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
            | Some(ty) =>
              typ_known(ty) && annotation_unambiguous(~info_map, def, ty)
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
    rename_typ_pairs(~info_map, ~target, program) |> List.mem((x, t))
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
      Option.is_some(
        swap_arms_rewrite(~info_map, ~fixup=false, ~target, i, m),
      )
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
    | Some(path) =>
      comment_rung_applies(~up=true, List.nth(path, List.length(path) - 1))
      || Option.is_some(hoist_step(~fixup=false, path))
    | None => false
    }
  | SinkLet =>
    switch (find_hit(~hit=hit_def_line(target), program)) {
    | Some(l) =>
      comment_rung_applies(~up=false, l)
      || Option.is_some(sink_step(~fixup=false, l))
    | None => false
    }
  | MergeUp => Option.is_some(merge_site_up(~info_map, ~target, program))
  | MergeDown => Option.is_some(merge_site_down(~info_map, ~target, program))
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
        | Fun(_)
        | TypFun(_) => true
        | _ => false
        };
      /* binding forms read as programs, not expressions: evaluating at
         a let would replace everything below it; tests would vanish */
      let is_form =
        switch (IdTagged.term_of(e)) {
        | Let(_)
        | TyAlias(_)
        | Module(_)
        | Seq(_)
        | Use(_) => true
        | _ => false
        };
      let has_test =
        e
        |> collect_exp(e' =>
             switch (IdTagged.term_of(e')) {
             | Test(_)
             | HintedTest(_) => [()]
             | _ => []
             }
           )
        != [];
      /* closed up to builtins: every free name statics resolves to the
         same entry it has at the program root (the builtin env the
         evaluation runs in) */
      let root_ctx =
        Id.Map.find_opt(Exp.rep_id(program), info_map)
        |> Option.map(Info.ctx_of);
      let builtin_only = (ctx, co_ctx: CoCtx.t) =>
        Util.VarMap.to_list(co_ctx)
        |> List.for_all(((x, _)) =>
             switch (Ctx.lookup_var(ctx, x), root_ctx) {
             | (Some(here), Some(root)) =>
               switch (Ctx.lookup_var(root, x)) {
               | Some(top) => here.id == top.id
               | None => false
               }
             | _ => false
             }
           );
      /* a label isn't an expression; a value containing a function
         can't be spelled back as a literal */
      let is_label =
        switch (IdTagged.term_of(e)) {
        | Label(_)
        | TupLabel(_) => true
        | _ => false
        };
      let rec has_arrow = (t: Typ.t) =>
        switch (IdTagged.term_of(t)) {
        | Arrow(_)
        | Poly(_) => true
        | Prod(ts) => List.exists(has_arrow, ts)
        | List(t)
        | Parens(t)
        | TupLabel(_, t) => has_arrow(t)
        | _ => false
        };
      let fn_valued =
        switch (exp_ty(~info_map, e)) {
        | Some(ty) => has_arrow(ty)
        | None => false
        };
      !is_fun(e)
      && !is_form
      && !is_label
      && !fn_valued
      && !has_test
      && !is_value_literal(e)
      && (
        switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
        | Some(InfoExp({co_ctx, ctx, _})) => builtin_only(ctx, co_ctx)
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
    | Some(e) => Option.is_some(eta_reducible(e))
    | None => false
    }
  | IfToCase
  | NegateIf =>
    switch (find_hit(~hit=hit_node(target), program)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | If(_) => true
      /* a branch's root flips its if (drag holds the branch) */
      | _ => Option.is_some(if_of_branch(~target, program))
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
  | UnfoldCall =>
    /* unfolding is a feed of the callee: offered only where that feed
       can happen (not at a recursive self-call, not when capture-gated) */
    switch (unfold_site(~info_map, ~target, program)) {
    | Some(f_use) =>
      Option.is_some(feed_plan(~info_map, ~target=f_use, program))
    | None => false
    }
  | HoistCarry => Option.is_some(hoist_carry_site(~target, program))
  | SinkCarry => Option.is_some(sink_carry_site(~target, program))
  | LiftFunction => Option.is_some(lift_site(~target, program))
  | UnliftFunction => Option.is_some(unlift_site(~target, program))
  | SplitComponent =>
    switch (split_component_site(~target, program)) {
    | Some((l, i)) =>
      Option.is_some(split_component_rewrite(~program, l, i))
    | None => false
    }
  | JoinLet => Option.is_some(join_site(~target, program))
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

/* statics for a hypothetical program (a ladder rung, a path step's
   intermediate state): the root context is the live program's */
let statics_of =
    (
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~root: Exp.t,
      term: Exp.t,
    )
    : Statics.Map.t =>
  switch (Id.Map.find_opt(Exp.rep_id(root), info_map)) {
  | Some(r) => fst(Statics.mk(settings, Info.ctx_of(r), term))
  | None => info_map
  };

/* put a prepared result in the buffer: judge what lands (the spliced
   segment, read back the way the editor reads it — a structure the
   print doesn't keep, a let extending over a comma, is refused — then
   statics checks the read-back against the program as it was before
   whatever produced the result), and place the caret */
let land_result =
    /* a drag lands the caret on what it held, when that survives */
    (
      ~hand: option(Id.t)=?,
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~term: Exp.t,
      ~target: Id.t,
      z: Zipper.t,
      (term': Exp.t, focus: Id.t),
    )
    : option(Zipper.t) => {
  let seg =
    ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term')
    |> SpaceNormalize.go;
  let lands =
    ! RefactorCheck.enabled^
    || {
      let read = MakeTerm.go(seg).term;
      Exp.fast_equal(read, term')
      && RefactorCheck.preserves(~settings, ~info_map, ~before=term, read);
    };
  if (!lands) {
    None;
  } else {
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
    let candidates = Option.to_list(hand) @ [focus, target] @ ancestors;
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
  };
};

let go =
    (
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~term: Exp.t,
      kind: Action.refactor,
      z: Zipper.t,
    )
    : option(Zipper.t) =>
  switch (Indicated.index(z)) {
  | None => None
  | Some(target) =>
    id_origin := Id.Map.empty;
    binder_redirect := Id.Map.empty;
    impl(kind).prepare(~info_map, ~target, term)
    |> Option.bind(_, land_result(~settings, ~info_map, ~term, ~target, z));
  };

/* a drag ladder's commit: refactors in sequence, each at its own
   target in the state the previous one left (statics recomputed
   between steps), landed ONCE — one undo step, judged against the
   program before the first */
let moves_line = (k: Action.refactor): bool =>
  switch (k) {
  | HoistLet
  | SinkLet
  | HoistCarry
  | SinkCarry
  | LiftFunction
  | UnliftFunction
  | MergeUp
  | MergeDown
  | JoinLet => true
  | _ => false
  };

/* a grabbed VALUE: the let whose definition's root is the grab (not
   itself a line handle, as a let-headed definition's inner let is) */
let value_line = (~target: Id.t, term: Exp.t): option(Exp.t) =>
  find_hit(~hit=hit_def_line(target), term) != None
    ? None
    : find_hit(
        ~hit=
          e =>
            switch (IdTagged.term_of(e)) {
            | Let(_, def, _) => IdTagged.ids(def) |> List.mem(target)
            | _ => false
            },
        term,
      );

/* mergeInto targets (D2 emerge reversed): when this invocation will
   ABSORB (identical definitions merging) or GLOM (extract reusing an
   identical existing def), the dissolved window's ids + the
   surviving window's ids — staged for the convergence flight. */
let merge_target =
    (~info_map, ~target, kind: Action.refactor, term)
    : (list(Id.t), list(Id.t)) => {
  let line_ids = (e: Exp.t): list(Id.t) =>
    switch (IdTagged.term_of(e)) {
    | Let(p, d, _) =>
      IdTagged.ids(e) @ pat_subtree_ids(p) @ exp_subtree_ids(d)
    | _ => IdTagged.ids(e)
    };
  switch (kind) {
  | MergeUp =>
    switch (merge_site_up(~info_map, ~target, term)) {
    | Some((p, l)) => (line_ids(l), line_ids(p))
    | None => ([], [])
    }
  | MergeDown =>
    switch (merge_site_down(~info_map, ~target, term)) {
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

/* a feed's copies: each fresh id of the result paired with the id of
   the definition it was copied from (dragology's emergeFrom). Both walks
   share traversal order, so they zip positionally. */
let feed_copies =
    (~feed_pref=false, ~info_map, ~target, term: Exp.t, term': Exp.t)
    : list((Id.t, Id.t)) =>
  switch (feed_plan(~prefer_def_host=feed_pref, ~info_map, ~target, term)) {
  | Some(Feed(_, def, _)) =>
    let live = exp_subtree_ids(term);
    let fresh =
      exp_subtree_ids(term') |> List.filter(id => !List.mem(id, live));
    let d = exp_subtree_ids(def);
    /* combine raises on length mismatch — guard FIRST
       (the eager-evaluation gotcha) */
    let zip = ids =>
      List.length(ids) == List.length(d) ? List.combine(ids, d) : [];
    switch (zip(fresh)) {
    | [] =>
      /* reparse demanded a paren wrapper: the parens
         are genuinely NEW material (no source) — pair
         the inner clone, found structurally */
      let fresh_parens = (e: Exp.t) =>
        switch (IdTagged.term_of(e)) {
        | Parens(_) =>
          IdTagged.ids(e) |> List.exists(id => List.mem(id, fresh))
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
  };

/* a line move invoked on a grabbed value acts on the value's line:
   how a composite drag moves the line of the value it holds. The
   value's ids survive extract and lift (a fresh let's would not, so a
   re-derived path could not find it). */
let step_target = (kind: Action.refactor, target: Id.t, term: Exp.t): Id.t =>
  moves_line(kind)
    ? value_line(~target, term)
      |> Option.map(Exp.rep_id)
      |> Option.value(~default=target)
    : target;

let go_path =
    (
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~term: Exp.t,
      steps: list((Action.refactor, Id.t)),
      z: Zipper.t,
    )
    : option(Zipper.t) => {
  id_origin := Id.Map.empty;
  binder_redirect := Id.Map.empty;
  /* what each step acts on (the held piece) and where it puts it (the
     hand). The held piece survives in place or dissolves into an
     identical survivor (merge, extract onto a twin), and the next step
     acts on that. The hand is the held piece's place, or the copy a step
     made of it (feed: the track follows the copy while the definition
     stays held). The track ends at the hand; the caret lands there. */
  let held = ref(None);
  let hand = ref(None);
  let step = (t, im, (k, at)) => {
    let target = step_target(k, at, t);
    let (d, v) = merge_target(~info_map=im, ~target, k, t);
    let dissolved =
      List.length(d) == List.length(v) ? List.combine(d, v) : [];
    let result = impl(k).prepare(~info_map=im, ~target, t);
    switch (held^, result) {
    | (Some(h), Some((t', _))) =>
      let copies =
        k == FeedLet ? feed_copies(~info_map=im, ~target, t, t') : [];
      let h' = List.assoc_opt(h, dissolved) |> Option.value(~default=h);
      held := Some(h');
      hand :=
        Some(
          switch (List.find_opt(((_, src)) => src == h, copies)) {
          | Some((copy, _)) => copy
          | None => h'
          },
        );
    | _ => ()
    };
    result;
  };
  let rec run = (t: Exp.t, im, steps) =>
    switch (steps) {
    | [] => None
    | [s] => step(t, im, s)
    | [s, ...rest] =>
      switch (step(t, im, s)) {
      | Some((t', _)) =>
        run(t', statics_of(~settings, ~info_map, ~root=term, t'), rest)
      | None => None
      }
    };
  switch (steps) {
  | [] => None
  | [(_, target), ..._] =>
    held := Some(target);
    run(term, info_map, steps)
    |> Option.bind(
         _,
         land_result(~hand=?hand^, ~settings, ~info_map, ~term, ~target, z),
       );
  };
};
