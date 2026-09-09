open Util;

/* User-defined livelits. A definition is a module declaring three types
   and four members:

     let ^name = {
       type Model = ...;                the livelit's internal state
       type Action = ...;               what the GUI emits
       type Expansion = ...;            what a use MEANS to the program
       let init : Model = ...;          initial model, inserted on ^name<space>
       let update = fun (m, a) -> ...;  (Model, Action) => Model
       let view = fun m -> ...;         Model => HTML, handlers emit Actions
       let expand = fun m -> ...        Model => Expansion
     } in ...

   The three type members are the livelit's interface, and all three are
   required. `Expansion` in particular is what clients type against: a use
   of ^name synthesizes Expansion whatever the expansion turns out to be,
   which is the abstract reasoning principle of the livelits paper. The
   obligation that buys it is discharged at each use, where statics types
   the expansion and marks the use with BadLivelitExpansion if its type is
   inconsistent with the declared one (the LivelitName case of Statics.re).
   Checking per use rather than once per definition is the paper's own
   strategy (PLDI 2021, S3.2.5), not an approximation of it: the expansion
   is validated at each invocation site, with errors reported to the client.

   Splices are the part of the paper still absent. When they arrive as a
   SpliceRef type with operations over it, `expand` extends to return a
   pair whose second component is the list of SpliceRefs, and the check
   here becomes a check of that pair's parameterized first component. With
   the splice list empty it degenerates to what this file does.

   Optional member `shape = Inline(w) | Block(w, h) | Tab(w, h)` (a
   LivelitShape) sets the projector's footprint in character cells. Helpers
   are ordinary additional members.

   Expansion and view instrumentation are built syntactically here — no
   evaluation during statics. A projected use's view runs in the main
   evaluation (instrument_view below) and the projector renders the sampled
   HTML; `update` runs at event time in the builtin environment via
   `user_def`, so definitions should be closed, with helpers among their
   members. */

let is_livelit_name = (name: string): bool =>
  String.length(name) > 1 && name.[0] == '^';

/* The livelit bound by this let pattern, if any (bare name, no caret) */
let rec binder_name = (p: TermBase.Pat.t): option(string) =>
  switch (p.term) {
  | Parens(p)
  | Asc(p, _) => binder_name(p)
  | Var(name) when is_livelit_name(name) =>
    Some(String.sub(name, 1, String.length(name) - 1))
  | _ => None
  };

let rec strip_parens = (e: TermBase.Exp.t): TermBase.Exp.t =>
  switch (e.term) {
  | Parens(e) => strip_parens(e)
  | _ => e
  };

let rec pat_name = (p: TermBase.Pat.t): option(string) =>
  switch (p.term) {
  | Parens(p)
  | Asc(p, _) => pat_name(p)
  /* funlet member (`let view(m) = ...`): the head var names the member;
     constructor heads fall through to None */
  | Ap(fn, _) => pat_name(fn)
  | Var(name) => Some(name)
  | _ => None
  };

/* A well-formed definition: the four required members (plus any helpers and
   the optional `shape`) and the three declared interface types. */
[@deriving show({with_path: false})]
type def = {
  members: list((string, TermBase.Exp.t)), /* member -> bound syntax */
  model_t: TermBase.Typ.t,
  action_t: TermBase.Typ.t,
  expansion_t: TermBase.Typ.t,
};

let required_members = ["init", "update", "view", "expand"];
let required_types = ["Model", "Action", "Expansion"];

/* Module members, in order; a repeated name keeps the LAST binding, matching
   module shadowing semantics. */
let module_members =
    (items: list(TermBase.Mod.t)): list((string, TermBase.Exp.t)) =>
  List.fold_left(
    (acc, item: TermBase.Mod.t) =>
      switch (item.term) {
      | ModLet(p, e) =>
        switch (pat_name(p)) {
        | Some(name) => [(name, e), ...List.remove_assoc(name, acc)]
        | None => acc
        }
      | _ => acc
      },
    [],
    items,
  );

let missing = (required: list(string), have: list((string, 'a))) =>
  List.filter(r => !List.mem_assoc(r, have), required);

/* The definition is the trailing module, looking through helper bindings:
   `let helper = ... in {...}`. A helper type alias is brought into scope on
   the way down, so a member type may be stated in terms of it. */
let rec detect =
        (~ctx: Ctx.t, def: TermBase.Exp.t)
        : result(def, Mark.livelit_def_error) =>
  switch (strip_parens(def).term) {
  | Let(_, _, body) => detect(~ctx, body)
  | TyAlias(tp, ty, body) =>
    let ctx =
      switch (tp.term) {
      | Var(name) => Ctx.extend_alias(ctx, name, TPat.rep_id(tp), ty)
      | _ => ctx
      };
    detect(~ctx, body);
  | Module(items) =>
    let members = module_members(items);
    /* the same resolution the module's own type members get, so a member
       type may name an earlier one (`type Expansion = Model`) */
    let types = ModuleHelpers.collect_type_exports(ctx, items);
    switch (
      missing(required_members, members),
      missing(required_types, types),
    ) {
    | ([_, ..._] as ms, _) => Error(DefMissingMembers(ms))
    | ([], [_, ..._] as ts) => Error(DefMissingTypes(ts))
    | ([], []) =>
      Ok({
        members,
        model_t: List.assoc("Model", types),
        action_t: List.assoc("Action", types),
        expansion_t: List.assoc("Expansion", types),
      })
    };
  | _ => Error(DefNotModule)
  };

let unknown = () => IdTagged.FreshGrammar.Typ.unknown(Internal);

/* The `shape` member: a LivelitShape constructor. Inline(w) is one
   line; Block(w, h) / Tab(w, h) are h LINES tall (the internal
   vertical counts linebreaks, hence h - 1). */
let shape_of = (e: TermBase.Exp.t): option(ProjectorShape.t) => {
  let int_of = w =>
    switch (strip_parens(w).term) {
    | Atom(Int(n)) => Bigint.to_int(n)
    | _ => None
    };
  let pair_of = arg =>
    switch (strip_parens(arg).term) {
    | Tuple([w, h]) =>
      switch (int_of(w), int_of(h)) {
      | (Some(w), Some(h)) => Some((w, h))
      | _ => None
      }
    | _ => None
    };
  switch (strip_parens(e).term) {
  | Ap(_, ctr, arg) =>
    switch (strip_parens(ctr).term) {
    | Constructor("Inline", _) =>
      int_of(arg)
      |> Option.map(w =>
           {
             ProjectorShape.horizontal: w,
             vertical: Inline,
           }
         )
    | Constructor("Block", _) =>
      pair_of(arg)
      |> Option.map(((w, h)) =>
           {
             ProjectorShape.horizontal: w,
             vertical: h <= 1 ? Inline : Block(h - 1),
           }
         )
    | Constructor("Tab", _) =>
      pair_of(arg)
      |> Option.map(((w, h)) =>
           {
             ProjectorShape.horizontal: w,
             vertical: h <= 1 ? Inline : Tab(h - 1),
           }
         )
    | _ => None
    }
  | _ => None
  };
};

let default_shape: ProjectorShape.t = {
  horizontal: 24,
  vertical: Inline,
};

/* The expansion of `^name(model)`: fetch the expand member from the runtime
   binding and apply it to the model. Scoping comes for free: `^name`
   resolves to the nearest enclosing livelit let. Note that this reaches the
   member through the ordinary `Var` binding, not the `^name.expand` surface
   form, so typing it consults the definition's ACTUAL expand member rather
   than the interface `member_ty` advertises — which is what makes the
   use-site expansion check below non-vacuous. */
let mk_expand_dot = (~name: string, model: TermBase.Exp.t) => {
  IdTagged.FreshGrammar.(
    Some(
      Exp.ap(
        Operators.Forward,
        Exp.dot(Exp.var("^" ++ name), Exp.label("expand")),
        model,
      ),
    )
  );
};

let is_user_livelit = (ctx: Ctx.t, name: string): bool =>
  switch (Ctx.lookup_livelit(ctx, name)) {
  | Some({user_def: Some(_), _}) => true
  | _ => false
  };

/* Surface member access (^name.member) types against the livelit's DECLARED
   interface (Model, Action, Expansion), not against the definition's actual
   members — the same abstraction a use of ^name gets. */
let member_ty = (ctx: Ctx.t, name: string, member: string): TermBase.Typ.t =>
  switch (Ctx.lookup_livelit(ctx, name)) {
  | Some({model_t, action_t, expansion_t, _}) =>
    IdTagged.FreshGrammar.(
      switch (member) {
      | "update" => Typ.arrow(Typ.prod([model_t, action_t]), model_t)
      | "expand" => Typ.arrow(model_t, expansion_t)
      | "init" => model_t
      | _ => unknown()
      }
    )
  | None => unknown()
  };

/* The transition an interaction commits as the new model argument:
   ^name.update(prev_model, action). Living in the text, the last
   transition stays where probes and the stepper can reach it; the next
   commit collapses it to its value first, so depth stays constant. */
let mk_update_redex =
    (~name: string, ~model_value: TermBase.Exp.t, ~action: TermBase.Exp.t)
    : TermBase.Exp.t => {
  let model_value = Exp.replace_all_ids(model_value);
  let action = Exp.replace_all_ids(action);
  IdTagged.FreshGrammar.(
    Exp.ap(
      Operators.Forward,
      Exp.dot(Exp.var("^" ++ name), Exp.label("update")),
      Exp.tuple([model_value, action]),
    )
  );
};

/* A projected use of a user-defined livelit: (bare name, model term) */
let use_parts =
    (ctx: Ctx.t, use: TermBase.Exp.t): option((string, TermBase.Exp.t)) =>
  switch (strip_parens(use).term) {
  | Ap(_, {term: LivelitName(name), _}, model) =>
    switch (Ctx.lookup_livelit(ctx, name)) {
    | Some({user_def: Some(_), _}) => Some((name, model))
    | _ => None
    }
  | _ => None
  };

/* View fold-in: a projected use also computes view(model) in the main run,
   discarded by the program but sampled at the projector's id — the same id
   the projector's dynamics probe watches — so the projector can render the
   live HTML without evaluating anything itself. The model is bound once
   (`%model`, not a lexable token) and shared between the view call and the
   expansion, so a committed ^name.update(m, a) transition runs — and its
   probes fire — exactly once. The model keeps its surface ids as the
   binding's definition, so its value samples at the model's own id. */
let instrument_view =
    (
      ~projector_id: Id.t,
      ~name: string,
      ~model: TermBase.Exp.t,
      body: TermBase.Exp.t,
    )
    : TermBase.Exp.t => {
  let model_id = Exp.rep_id(model);
  let m_var = "%model";
  let m_ref = () => IdTagged.FreshGrammar.Exp.var(m_var);
  let body =
    Exp.map_term(
      ~f_exp=
        (continue, e) => Exp.rep_id(e) == model_id ? m_ref() : continue(e),
      body,
    );
  IdTagged.FreshGrammar.(
    {
      let view_ap =
        IdTagged.mk_internal(
          [projector_id],
          Grammar.Ap(
            Operators.Forward,
            Exp.dot(Exp.var("^" ++ name), Exp.label("view")),
            m_ref(),
          ): TermBase.Exp.term,
        );
      Exp.let_(Pat.var(m_var), model, Exp.let_(Pat.wild(), view_ap, body));
    }
  );
};

let mk =
    (
      ~ctx: Ctx.t,
      ~name: string,
      ~id: Id.t,
      ~def_user: TermBase.Exp.t,
      ~def_elab: TermBase.Exp.t,
    )
    : result(LivelitCtx.raw_livelit, Mark.livelit_def_error) =>
  switch (detect(~ctx, def_user)) {
  | Error(e) => Error(e)
  | Ok({members, model_t, action_t, expansion_t}) =>
    Ok({
      LivelitCtx.name,
      id,
      model_t,
      model_default: Exp.replace_all_ids(List.assoc("init", members)),
      expansion_t,
      expand: mk_expand_dot(~name),
      action_t,
      update: (_action, model) => model,
      view: (_model, _send) =>
        Virtual_dom.Vdom.Node.text("user-defined livelit"),
      shape:
        switch (Option.bind(List.assoc_opt("shape", members), shape_of)) {
        | Some(shape) => shape
        | None => default_shape
        },
      user_def: Some(def_elab),
    })
  };

/* The use-site expansion obligation: a use of ^name synthesizes the DECLARED
   expansion type, so statics owes a check that the expansion actually has
   that type. `actual` is the type the expansion synthesizes on its own; a
   mark is due when the two are inconsistent. Consistency, not equality, is
   the test: an expansion that synthesizes Unknown (an unannotated `expand`,
   or a builtin livelit generating a hole) stays gradual, exactly as it would
   anywhere else in the language. */
let expansion_mark =
    (ctx: Ctx.t, ~declared: TermBase.Typ.t, ~actual: TermBase.Typ.t)
    : list(Mark.t) =>
  switch (Typ.meet(ctx, declared, actual)) {
  | Some(_) => []
  | None => [
      Mark.BadLivelitExpansion({
        declared,
        actual,
      }),
    ]
  };
