open Util;

/* User-defined livelits. The canonical definition form is a module:

     let ^name = {
       type Model = ...;
       type Action = ...;
       let init : Model = ...;          initial model, inserted on ^name<space>
       let update = fun (m, a) -> ...;  (Model, Action) => Model
       let view = fun m -> ...;         Model => HTML, handlers emit Actions
       let expand = fun m -> ...        Model => Expansion
     } in ...

   Optional member `size = (width, height)` sets the projector size in
   character cells. Type members are accepted (and encouraged) but not yet
   semantically load-bearing. Helpers are ordinary additional members.
   Since modules are sugar for labeled tuples, a positional 4/5-tuple
   (init, update, view, expand[, size]) is accepted as the equivalent form.

   Expansion and view instrumentation are built syntactically here — no
   evaluation during statics. A projected use's view runs in the main
   evaluation (instrument_view below) and the projector renders the sampled
   HTML; `update` runs at event time in the builtin environment via
   `user_def`, so definitions should be closed, with helpers among their
   members. */

let expand_slot = 3;

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

[@deriving show({with_path: false})]
type shape =
  | ModuleDef(list((string, TermBase.Exp.t))) /* member -> bound syntax */
  | TupleDef(list(TermBase.Exp.t)); /* 4/5 positional or labeled fields */

let required_members = ["init", "update", "view", "expand"];

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

/* The definition record is the trailing module or tuple, looking through
   helper bindings: `let helper = ... in {...}`. */
let rec detect = (def: TermBase.Exp.t): result(shape, Mark.livelit_def_error) =>
  switch (strip_parens(def).term) {
  | Let(_, _, body)
  | TyAlias(_, _, body) => detect(body)
  | Module(items) =>
    let members = module_members(items);
    switch (List.filter(r => !List.mem_assoc(r, members), required_members)) {
    | [] => Ok(ModuleDef(members))
    | missing => Error(DefMissingMembers(missing))
    };
  | Tuple([_, _, _, _] as fs)
  | Tuple([_, _, _, _, _] as fs) => Ok(TupleDef(fs))
  | Tuple(fs) => Error(DefBadArity(List.length(fs)))
  | _ => Error(DefNotTuple)
  };

let field_label = (e: TermBase.Exp.t): option(string) =>
  switch (strip_parens(e).term) {
  | TupLabel({term: Label(l), _}, _) => Some(l)
  | _ => None
  };

let field_payload = (e: TermBase.Exp.t): TermBase.Exp.t =>
  switch (strip_parens(e).term) {
  | TupLabel(_, v) => v
  | _ => e
  };

/* Select a tuple field by label when labeled, positionally otherwise */
let slot_index = (fs: list(TermBase.Exp.t), ~label: string, ~index: int): int => {
  let rec find = (i, fs) =>
    switch (fs) {
    | [] => index
    | [f, ..._] when field_label(f) == Some(label) => i
    | [_, ...fs] => find(i + 1, fs)
    };
  find(0, fs);
};

let slot = (fs: list(TermBase.Exp.t), ~label: string, ~index: int) =>
  List.nth(fs, slot_index(fs, ~label, ~index));

let unknown = () => IdTagged.FreshGrammar.Typ.unknown(Internal);

let arrow_of = (ty: option(TermBase.Typ.t)) =>
  switch (Option.map(Typ.term_of, ty)) {
  | Some(Arrow(a, b)) => Some((a, b))
  | _ => None
  };

/* Loose typing: model/expansion types from the expand member's arrow type,
   action type from update's second argument, Unknown where unannotated. */
let types_of =
    (~update_ty: option(TermBase.Typ.t), ~expand_ty: option(TermBase.Typ.t))
    : (TermBase.Typ.t, TermBase.Typ.t, TermBase.Typ.t) => {
  let (model_t, expansion_t) =
    switch (arrow_of(expand_ty)) {
    | Some((m, e)) => (m, e)
    | None => (unknown(), unknown())
    };
  let action_t =
    switch (arrow_of(update_ty)) {
    | Some((args, _)) =>
      switch (Typ.term_of(args)) {
      | Prod([_, a]) => a
      | _ => unknown()
      }
    | None => unknown()
    };
  (model_t, expansion_t, action_t);
};

/* The def's type is a (labeled) product; find a member's type by label,
   falling back to position for unlabeled tuples. */
let ty_member =
    (def_ty: TermBase.Typ.t, ~label: string, ~index: option(int))
    : option(TermBase.Typ.t) => {
  let strip_ty = (ty: TermBase.Typ.t) =>
    switch (Typ.term_of(ty)) {
    | TupLabel(_, ty) => ty
    | _ => ty
    };
  switch (Typ.term_of(def_ty)) {
  | Prod(tys) =>
    let by_label =
      List.find_map(
        ty =>
          switch (Typ.term_of(ty)) {
          | TupLabel({term: Label(l), _}, t) when l == label => Some(t)
          | _ => None
          },
        tys,
      );
    switch (by_label, index) {
    | (Some(t), _) => Some(t)
    | (None, Some(i)) when List.length(tys) > i =>
      Some(strip_ty(List.nth(tys, i)))
    | _ => None
    };
  | _ => None
  };
};

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
  switch (strip_parens(field_payload(e)).term) {
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

let default_size: ProjectorShape.t = {
  horizontal: 24,
  vertical: Inline,
};

/* The expansion of `^name(model)`: fetch the expand member from the runtime
   binding and apply it to the model. Scoping comes for free: `^name`
   resolves to the nearest enclosing livelit let. Module and labeled-tuple
   definitions use member access; unlabeled tuples destructure positionally
   (`%expand` is not a lexable token, so user code cannot capture it). */
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

let mk_expand_positional =
    (~name: string, ~n_fields: int, ~expand_i: int, model: TermBase.Exp.t) => {
  open IdTagged.FreshGrammar;
  let hidden = "%expand";
  let pats =
    List.init(n_fields, i => i == expand_i ? Pat.var(hidden) : Pat.wild());
  Some(
    Exp.let_(
      Pat.tuple(pats),
      Exp.var("^" ++ name),
      Exp.ap(Operators.Forward, Exp.var(hidden), model),
    ),
  );
};

let is_user_livelit = (ctx: Ctx.t, name: string): bool =>
  switch (Ctx.lookup_livelit(ctx, name)) {
  | Some({user_def: Some(_), _}) => true
  | _ => false
  };

/* Surface member access (^name.member) types loosely, from the member
   shapes the record convention implies */
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
      ~name: string,
      ~id: Id.t,
      ~def_user: TermBase.Exp.t,
      ~def_elab: TermBase.Exp.t,
      ~def_ty: TermBase.Typ.t,
    )
    : result(LivelitCtx.raw_livelit, Mark.livelit_def_error) => {
  let build = (~init, ~size, ~expand, ~update_ty, ~expand_ty) => {
    let (model_t, expansion_t, action_t) = types_of(~update_ty, ~expand_ty);
    {
      LivelitCtx.name,
      id,
      model_t,
      model_default: Exp.replace_all_ids(field_payload(init)),
      expansion_t,
      expand,
      action_t,
      update: (_action, model) => model,
      view: (_model, _send) =>
        Virtual_dom.Vdom.Node.text("user-defined livelit"),
      size:
        switch (Option.bind(size, shape_of)) {
        | Some(size) => size
        | None => default_size
        },
      user_def: Some(def_elab),
    };
  };
  switch (detect(def_user)) {
  | Error(e) => Error(e)
  | Ok(ModuleDef(members)) =>
    Ok(
      build(
        ~init=List.assoc("init", members),
        ~size=List.assoc_opt("shape", members),
        ~expand=mk_expand_dot(~name),
        ~update_ty=ty_member(def_ty, ~label="update", ~index=None),
        ~expand_ty=ty_member(def_ty, ~label="expand", ~index=None),
      ),
    )
  | Ok(TupleDef(fs)) =>
    let update_i = slot_index(fs, ~label="update", ~index=1);
    let expand_i = slot_index(fs, ~label="expand", ~index=expand_slot);
    let expand =
      field_label(List.nth(fs, expand_i)) != None
        ? mk_expand_dot(~name)
        : mk_expand_positional(~name, ~n_fields=List.length(fs), ~expand_i);
    Ok(
      build(
        ~init=slot(fs, ~label="init", ~index=0),
        ~size=
          List.length(fs) == 5
            ? Some(slot(fs, ~label="shape", ~index=4)) : None,
        ~expand,
        ~update_ty=ty_member(def_ty, ~label="update", ~index=Some(update_i)),
        ~expand_ty=ty_member(def_ty, ~label="expand", ~index=Some(expand_i)),
      ),
    );
  };
};
