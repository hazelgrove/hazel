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
