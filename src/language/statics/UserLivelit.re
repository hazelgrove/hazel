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

   Only `expand` participates in statics, and it is built syntactically here
   — no evaluation. The record's `update`/`view` run at render time in
   LivelitProj via `user_def`, evaluated in the builtin environment: the
   definition must be closed, with helpers among its members. */

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

let size_of = (e: TermBase.Exp.t): option(ProjectorShape.t) =>
  switch (strip_parens(field_payload(e)).term) {
  | Tuple([w, h]) =>
    switch (strip_parens(w).term, strip_parens(h).term) {
    | (Atom(Int(w)), Atom(Int(h))) =>
      switch (Bigint.to_int(w), Bigint.to_int(h)) {
      | (Some(w), Some(h)) =>
        Some({
          ProjectorShape.horizontal: w,
          vertical: h <= 1 ? Inline : Block(h),
        })
      | _ => None
      }
    | _ => None
    }
  | _ => None
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
        switch (Option.bind(size, size_of)) {
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
        ~size=List.assoc_opt("size", members),
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
            ? Some(slot(fs, ~label="size", ~index=4)) : None,
        ~expand,
        ~update_ty=ty_member(def_ty, ~label="update", ~index=Some(update_i)),
        ~expand_ty=ty_member(def_ty, ~label="expand", ~index=Some(expand_i)),
      ),
    );
  };
};
