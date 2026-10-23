/* Theorem-backed source rewrites. This layer has no gesture or proof-step
   UI: it resolves a rule at an expression, matches it, and prepares the
   replacement. A future proof gesture can use the same match while
   recording a justification instead of editing source. */
open Language;
open RefactorBase;
open RefactorParens;
open Util.OptUtil.Syntax;

type rule = {
  id: Id.t,
  name: string,
  statement: Exp.t,
  rule: ProofRule.t,
};

/* Context order is newest first. A value binding shadows a theorem of
   the same name too. Only equality rules are source refactors here;
   conditional/propositional rules need obligation support first. Like
   the existing proof-rule lookup, availability means declared in scope,
   not a claim that the theorem's separate proof UI is complete. */
let rules_at = (~info_map, ~target): list(rule) =>
  switch (Statics.Map.ctx_of(target, info_map)) {
  | None => []
  | Some(ctx) =>
    ctx.entries
    |> List.fold_left(
         ((seen, rules), entry: Ctx.entry) =>
           switch (entry) {
           | VarEntry(v) when !List.mem(v.name, seen) =>
             let rules =
               switch (IdTagged.term_of(v.typ)) {
               | ProofOf(statement) =>
                 let rule = ProofRule.exp_to_rule(statement);
                 switch (rule.conclusion) {
                 | Equality(_) =>
                   rules
                   @ [
                     {
                       id: v.id,
                       name: v.name,
                       statement,
                       rule,
                     },
                   ]
                 | Other(_) => rules
                 };
               | _ => rules
               };
             ([v.name, ...seen], rules);
           | _ => (seen, rules)
           },
         ([], []),
       )
    |> snd
  };

/* A rule's free names must retain their declaration-site meanings. A
   matched argument must also be usable at the replacement's outer scope:
   matching a metavariable under a lambda must not let its binder escape. */
let usable_at = (~info_map, ~ctx: Ctx.t, e: Exp.t): bool =>
  switch (Statics.Map.lookup_exp(Exp.rep_id(e), info_map)) {
  | None => false
  | Some(info) =>
    info.co_ctx
    |> List.for_all(((name, _)) => {
         let binder = c =>
           Ctx.lookup_var(c, name) |> Option.map((v: Ctx.var_entry) => v.id);
         binder(info.ctx) == binder(ctx);
       })
  };

/* Simultaneous, capture-avoiding substitution with the source refactor's
   id/whitespace discipline. Stage through fresh private names so replacing
   x by an expression containing y cannot be affected by y's substitution.
   Unmatched rule variables become a fresh hole at EACH occurrence. */
let instantiate = (~program, sub: MatchExp.match_ctx, template: Exp.t): Exp.t => {
  let used = ref(used_names(program) @ used_names(template));
  let reserve = () => {
    let rec pick = i => {
      let name = "theorem_arg" ++ string_of_int(i);
      List.mem(name, used^) ? pick(i + 1) : name;
    };
    let name = pick(1);
    used := [name, ...used^];
    name;
  };
  let staged = sub |> List.map(((x, (_, value))) => (x, reserve(), value));
  /* Declaration indentation and comments stay at the declaration. New
     syntax gets normal spacing; matched source arguments retain theirs. */
  let template =
    ExpToSegment.exp_to_segment(
      ~settings={
        ...roundtrip_settings,
        secondary: AutoFormat,
      },
      refresh_ids(template),
    )
    |> SpaceNormalize.go
    |> (seg => MakeTerm.go(seg).term);
  let template =
    List.fold_left(
      (t, (x, private_name, _)) =>
        subst(
          ~parens_for=_ => false,
          ~avoid=[private_name],
          ~used,
          ~first=ref(true),
          x,
          fresh(Var(private_name)),
          t,
        ),
      template,
      staged,
    );
  List.fold_left(
    (t, (_, private_name, value)) => {
      let value = Option.value(~default=fresh(EmptyHole), value);
      subst(
        ~parens_for=
          occ =>
            splice_parens_needed(~program=t, ~at=Exp.rep_id(occ), value),
        ~avoid=vars_of(value) |> List.filter(v => free_in(v, value)),
        ~used,
        ~first=ref(true),
        private_name,
        strip_boundaries(value),
        t,
      );
    },
    template,
    staged,
  );
};

let replacement = (~info_map, ~target, ~program, id, direction) => {
  let* e = find_hit(~hit=hit_node(target), program);
  let* ctx = Statics.Map.ctx_of(target, info_map);
  let* r =
    rules_at(~info_map, ~target) |> List.find_opt((r: rule) => r.id == id);
  let statement_ids = exp_subtree_ids(r.statement);
  let* () =
    Statics.Map.error_ids(info_map)
    |> List.exists(id => List.mem(id, statement_ids))
      ? None : Some();
  let* () = usable_at(~info_map, ~ctx, r.statement) ? Some() : None;
  let* (sub, template) =
    ProofRule.match_equality(
      ~info_map,
      ~env=Environment.empty,
      ~direction,
      r.rule,
      e,
    );
  let* () =
    sub
    |> List.for_all(((_, (_, v))) =>
         v
         |> Option.map(usable_at(~info_map, ~ctx))
         |> Option.value(~default=true)
       )
      ? Some() : None;
  let result = instantiate(~program, sub, template);
  eq_defs(e, result) ? None : Some((r, e, result, sub));
};

let comments = e => {
  let rec collect = seg =>
    seg
    |> List.concat_map((p: Piece.t) =>
         switch (p) {
         | Secondary(w) when is_comment_piece(w) => [w]
         | Tile(t) => List.concat_map(collect, t.children)
         | _ => []
         }
       );
  ExpToSegment.exp_to_segment(~settings=roundtrip_settings, e) |> collect;
};

/* A visual correspondence must not overwrite the semantic provenance that
   RefactorCheck uses. Equal-looking literals may have different numeric
   modes, and equal-looking names may resolve to different binders. */
let same_identity = (~info_map, a, b) => {
  let info = id =>
    Statics.Map.lookup_exp(RefactorCheck.origin(id), info_map);
  switch (info(a), info(b)) {
  | (Some(a), Some(b)) =>
    let number = (info: Info.exp) =>
      switch (IdTagged.term_of(info.user_term)) {
      | Atom(_) =>
        switch (IdTagged.term_of(Typ.normalize(info.ctx, info.ty))) {
        | Atom((Int | SInt | Nat | Float) as k) => Some(k)
        | _ => None
        }
      | _ => None
      };
    let numbers_agree =
      switch (number(a), number(b)) {
      | (Some(x), Some(y)) => x == y
      | _ => true
      };
    let names_agree =
      switch (IdTagged.term_of(a.user_term), IdTagged.term_of(b.user_term)) {
      | (Var(x), Var(y)) =>
        Option.map((v: Ctx.var_entry) => v.id, Ctx.lookup_var(a.ctx, x))
        == Option.map((v: Ctx.var_entry) => v.id, Ctx.lookup_var(b.ctx, y))
      | _ => true
      };
    numbers_agree && names_agree;
  | _ => true
  };
};

let grip_is_passenger = (~target, sub: MatchExp.match_ctx) =>
  sub
  |> List.exists(((_, (_, value))) =>
       switch (value) {
       | Some(v) =>
         List.mem(target, exp_subtree_ids(v))
         && !List.mem(target, IdTagged.ids(v))
       | None => false
       }
     );

/* Tile containment is not expression containment: a call's function can
   be a sibling of its argument tile, and an open lambda body can remain in
   its enclosing tile slot. Compare semantic parent/child roles as well, so
   an argument becoming a callee (map fusion's g) is a genuine mover. */
let expression_role = (~target, e) => {
  let rec slots = path =>
    switch (path) {
    | [parent, child, ...rest] =>
      let seat =
        children_of(parent)
        |> List.mapi((i, e) => same_node(e, child) ? Some(i) : None)
        |> List.find_map(Fun.id);
      [(Exp.rep_id(parent), seat), ...slots([child, ...rest])];
    | _ => []
    };
  find_path(~hit=hit_node(target), e) |> Option.map(slots);
};

/* A grip can be a participant of an enclosing rewrite. A token strictly
   inside a substituted argument is a passenger, not a handle for that rule. */
let prepare_details = (~info_map, ~target, ~site, id, direction, program) => {
  let* (_, e, result, sub) =
    replacement(~info_map, ~target=site, ~program, id, direction);
  let* () = grip_is_passenger(~target, sub) ? None : Some();
  let result =
    splice_parens_needed(~program, ~at=Exp.rep_id(e), result)
      ? fresh(Parens(result)) : result;
  /* Keep comments from the replaced expression even when the rule erases
     their operand. Boundary comments already stay in the original slot. */
  let kept =
    comments(result)
    @ boundary_comments(e)
    |> List.map((w: Secondary.t) => w.id);
  let missing =
    comments(e) |> List.filter((w: Secondary.t) => !List.mem(w.id, kept));
  let result =
    missing == []
      ? result
      : Slot.give(
          {
            lead: missing @ space(),
            trail: [],
          },
          result,
        );
  let correspondence =
    SyntaxCorrespondence.infer(
      ~can_match=same_identity(~info_map),
      ~grab=target,
      ~old_seg=ExpToSegment.exp_to_segment(~settings=roundtrip_settings, e),
      ~new_seg=
        ExpToSegment.exp_to_segment(~settings=roundtrip_settings, result),
      (),
    );
  let result = MakeTerm.go(correspondence.segment).term;
  let correspondence =
    switch (
      correspondence.destination,
      expression_role(~target, e),
      expression_role(~target, result),
    ) {
    | (None, Some(before), Some(after)) when before != after => {
        ...correspondence,
        destination: Some(Result(target)),
      }
    | _ => correspondence
    };
  let* (term, focus) =
    rewrite_node(
      ~hit=hit_node(site),
      ~rewrite=_ => Some((result, Exp.rep_id(result))),
      program,
    );
  Some((term, focus, correspondence));
};

let prepare = (~info_map, ~target, ~site, id, direction, program) =>
  prepare_details(~info_map, ~target, ~site, id, direction, program)
  |> Option.map(((term, focus, _)) => (term, focus));

let impl = (id, direction: Util.Direction.t, site): impl => {
  label: direction == Left ? "Rewrite theorem ←" : "Rewrite theorem →",
  tooltip: "Rewrite this expression using an equality in scope",
  prepare: (~info_map, ~target, program) =>
    prepare(~info_map, ~target, ~site, id, direction, program),
};
