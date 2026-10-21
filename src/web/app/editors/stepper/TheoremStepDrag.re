/* Proof-side adapter for theorem drag previews. The stepper remains the
   owner of persistence and validation; this module never edits its source. */
open Util;
open Language;
open Haz3lcore;
open OptUtil.Syntax;

type application = {
  name: string,
  at_idx: int,
  at_exp: Exp.t,
  direction: Direction.t,
};

let environment_id = (env: Environment.t(Exp.t)): option(Id.t) =>
  switch (env) {
  | Empty => None
  | E(e) => Some(e.id)
  };

/* Leave named functions available for theorem matching before the normal
   hidden lookup/fixpoint steps unfold them. Other stepper modes are unchanged. */
let for_drag = (settings: CoreSettings.t): CoreSettings.t =>
  settings.drag_refactor && settings.evaluation.enable_proof
    ? {
      ...settings,
      evaluation: {
        ...settings.evaluation,
        show_lookup_steps: true,
        show_fixpoints: true,
      },
    }
    : settings;

let fold_mode = (settings: CoreSettings.t) =>
  settings.drag_refactor
  && settings.evaluation.enable_proof
  && !settings.evaluation.show_fn_bodies
    ? Some(`Named) : None;

/* Result views can omit type annotations and add fold projectors. These
   are display differences, not changes to the expression being previewed. */
let same_projection =
  Equality.equality({
    ...Equality.semantic_settings,
    ignore_ascriptions: true,
    ignore_function_types: true,
    ignore_constructor_types: true,
  }).
    exp;

let rules =
    (~info_map, ~term, ~env: Environment.t(Exp.t))
    : list(RefactorTheorem.rule) => {
  let ctx =
    Statics.Map.ctx_of(Exp.rep_id(term), info_map)
    |> Option.value(~default=Ctx.empty);
  ProofCtx.of_env(~builtins=[], ~ctx, env)
  |> List.filter((r: ProofCtx.entry) =>
       !r.is_captured
       && (
         switch (Environment.lookup(env, r.name)) {
         | Some({term: ProofObject(e), _}) =>
           Exp.rep_id(e) == Exp.rep_id(r.exp)
         | Some(_) => false
         | None => false
         }
       )
     )
  |> List.fold_left(
       (acc, r: ProofCtx.entry) =>
         List.exists((r': RefactorTheorem.rule) => r'.name == r.name, acc)
           ? acc
           : acc
             @ [
               {
                 RefactorTheorem.id: Exp.rep_id(r.exp),
                 name: r.name,
                 statement: r.exp,
                 rule: r.rule,
               },
             ],
       [],
     );
};

let matched = (~info_map, ~env, ~term, rules, id, direction, site) => {
  let* r = List.find_opt((r: RefactorTheorem.rule) => r.id == id, rules);
  let* e = RefactorBase.find_hit(~hit=RefactorBase.hit_node(site), term);
  let* (sub, template) =
    ProofRule.match_equality(~info_map, ~env, ~direction, r.rule, e);
  /* Existing axiom steps do not persist user-supplied witnesses for an
     unmatched variable. Do not preview holes that release cannot replay. */
  let* () = List.exists(((_, (_, v))) => v == None, sub) ? None : Some();
  let* ctx = Statics.Map.ctx_of(site, info_map);
  let* () =
    sub
    |> List.for_all(((_, (_, v))) =>
         switch (v) {
         | None => false
         | Some(v) =>
           let free =
             RefactorBase.vars_of(v)
             |> List.filter(name => RefactorBase.free_in(name, v));
           free == [] || RefactorTheorem.usable_at(~info_map, ~ctx, v);
         }
       )
      ? Some() : None;
  let replay = MatchExp.substitute_exp(sub, template);
  let template =
    ExpToSegment.parenthesize(
      ~parenthesization=Defensive,
      ~show_ascriptions=true,
      ~show_filters=true,
      template,
    );
  /* Keep argument ids as anchors for the spatial correspondence. The
     legacy proof instantiator freshens those ids; check semantic agreement
     before offering a preview, then let the recorded axiom step replay it. */
  let result =
    RefactorTheorem.instantiate(
      ~program=term,
      ~normalize_template=false,
      sub,
      template,
    );
  let* () = same_projection(replay, result) ? Some() : None;
  let* () = RefactorBase.eq_defs(e, result) ? None : Some();
  Some((r, e, result, sub));
};

let candidates =
    (
      ~settings,
      ~info_map,
      ~env,
      ~term,
      ~measured,
      ~shape_map=Id.Map.empty,
      ~target,
      ~shard,
      (),
    ) => {
  let settings = for_drag(settings);
  let rules = rules(~info_map, ~term, ~env);
  let render = e =>
    ExpToSegment.exp_to_segment(
      ~settings=
        ExpToSegment.Settings.of_core(
          ~inline=false,
          ~fold_fn_bodies=?fold_mode(settings),
          settings,
        ),
      e,
    )
    |> PrettySegment.prettify;
  let render_correspondence = e =>
    ExpToSegment.exp_to_segment(
      ~settings=
        ExpToSegment.Settings.of_core(
          ~inline=false,
          ~fold_fn_bodies=`NoFold,
          settings,
        ),
      e,
    )
    |> PrettySegment.prettify;
  let theorems: RefactorDrag.theorem_source = {
    rules,
    env,
    render,
    /* A proof's next expression is freshly identified on replay. Source
       binder-id preservation is not a validity criterion for this visual
       projection; require it to depict the matched proof result faithfully. */
    accepts: (~before as _, ~after, segment) => {
      /* Native functions render as names (e.g. int_plus). Resolve those
         names in the proof environment before comparing the projection. */
      let read = MakeTerm.go(segment).term |> Substitution.in_exp(env);
      same_projection(read, Substitution.in_exp(env, after));
    },
    prepare: (~target, ~site, id, direction, program) => {
      let* (_, e, result, sub) =
        matched(~info_map, ~env, ~term=program, rules, id, direction, site);
      RefactorTheorem.prepare_matched_details(
        ~info_map,
        ~target,
        ~site,
        ~e,
        ~result,
        ~sub,
        ~render=render_correspondence,
        ~retain_semantics=true,
        program,
      );
    },
  };
  Refactor.drag_candidates(
    ~settings,
    ~info_map,
    ~term,
    ~measured,
    ~shape_map,
    ~target,
    ~shard,
    ~push=false,
    ~theorems,
    ~only_theorems=true,
    (),
  )
  |> List.map((c: Refactor.DragCandidate.t) =>
       switch (c.kind) {
       | RewriteTheorem(id, direction, _) =>
         let r = List.find((r: RefactorTheorem.rule) => r.id == id, rules);
         {
           ...c,
           label: r.name ++ (direction == Left ? " ←" : " →"),
         };
       | _ => c
       }
     );
};

/* Re-enumeration at release checks the current context, exact site and grip.
   It also rejects non-theorem paths and multi-rung source edits. */
let release =
    (~settings, ~editor: CodeSelectable.Model.t, ~env, path)
    : option(application) => {
  let* (id, direction, site, target, kind) =
    switch (path) {
    | [(Action.RewriteTheorem(id, direction, site) as kind, target)] =>
      Some((id, direction, site, target, kind))
    | _ => None
    };
  let term = editor.statics.term;
  let info_map = editor.statics.info_map;
  let* () =
    candidates(
      ~settings,
      ~info_map,
      ~env,
      ~term,
      ~measured=editor.editor.syntax.measured,
      ~shape_map=editor.editor.syntax.shape_map,
      ~target,
      ~shard=
        Indicated.index(editor.editor.state.zipper) == Some(target)
          ? Indicated.shard_index(editor.editor.state.zipper) : None,
      (),
    )
    |> List.exists((c: Refactor.DragCandidate.t) => c.kind == kind)
      ? Some() : None;
  let* (r, e, _, _) =
    matched(
      ~info_map,
      ~env,
      ~term,
      rules(~info_map, ~term, ~env),
      id,
      direction,
      site,
    );
  Some({
    name: r.name,
    at_idx: ProofHacks.exp_idx(e, term),
    at_exp: e,
    direction,
  });
};
