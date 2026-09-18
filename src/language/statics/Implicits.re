/* Modular implicits: resolving the implicit components of a function's
   parameter at an application.

   A parameter tuple may contain `implicit S : SIG` components (Pat.Implicit,
   Typ.Implicit). At a call `f(args)` whose domain has p components, k of them
   implicit and n = p - k explicit, the argument's arity m decides how the
   implicit ones are supplied:
   - m = n: every implicit component is resolved;
   - m = p: every implicit component is passed explicitly, positionally;
   - n < m < p: walking the domain left to right, an implicit component takes
     the current argument when extra arguments remain and the argument's type
     is a signature consistent with the component's; otherwise it is resolved.
   Resolution searches the implicit instances in scope (Ctx.implicit_instances)
   for those whose type fits the component's signature; if several fit, the
   explicit arguments' types and then the expected type narrow them. Exactly
   one left is the instance. None, or several, is a mark on the application
   and a hole is passed in the instance's place, so evaluation proceeds around
   the error. Binders are instantiated left to right: a resolved or passed
   binder is substituted (by its instance's path) into the later components
   and the codomain; an unresolved one, or one passed a non-path expression,
   is erased there (Typ.avoid). The chosen instances are spliced into the
   elaborated argument, so the runtime sees an ordinary application and
   incremental evaluation re-keys on the elaboration. */
open Util;

/* How a component of the parameter is supplied at a call. */
type supply =
  | Given(int) /* the argument component at this index */
  | Resolve; /* an implicit component to resolve */

type outcome =
  | Instance(Var.t)
  | Unresolved;

type t = {
  ty_in: Typ.t, /* what the argument is analyzed against */
  ty_out: Typ.t, /* the instantiated codomain */
  slots: list(option(outcome)), /* per component: Some for a resolved one */
  resolved: list((Var.t, Var.t)), /* binder, instance */
  marks: list(Mark.t),
  literal_tuple: bool /* the argument is a literal tuple */
};

let rec strip_parens = (e: Exp.t): Exp.t =>
  switch (IdTagged.term_of(e)) {
  | Parens(e) => strip_parens(e)
  | _ => e
  };

/* A literal tuple argument's items. */
let literal_items = (arg: Exp.t): option(list(Exp.t)) =>
  switch (IdTagged.term_of(strip_parens(arg))) {
  | Tuple(es) => Some(es)
  | _ => None
  };

let whnf_term = (ctx, ty) => Typ.term_of(Typ.weak_head_normalize(ctx, ty));

let fits = (ctx, ~ana, ~syn) =>
  Option.is_some(Typ.coercion(ctx, ~from=syn, ~to_=ana));

/* A module whose type fits [sig_]. */
let is_module_of = (ctx, ~sig_, ty) =>
  switch (whnf_term(ctx, ty)) {
  | Sig(_) => fits(ctx, ~ana=sig_, ~syn=ty)
  | _ => false
  };

/* The member chain of a type that is a path rooted at [s]: `S.Inner.T`
   gives ["Inner", "T"], `S` itself gives []. None for anything else. */
let path_members = (~s: Var.t, ty: Typ.t): option(list(string)) => {
  let rec go = (ty, acc) =>
    switch (Typ.term_of(Typ.strip_parens(ty))) {
    | Var(x) => x == s ? Some(acc) : None
    | ProdProjection(p, {term: Label(l), _}) => go(p, [l, ...acc])
    | _ => None
    };
  go(ty, []);
};

let uninformative = (ty: Typ.t) =>
  switch (Typ.term_of(ty)) {
  | Unknown(_) => true
  | _ => false
  };

/* A supplied type as it is worth reporting: without the wrappers that say
   nothing about the member. */
let rec reportable = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(Typ.strip_parens(ty))) {
  | TupLabel(_, ty) => reportable(ty)
  | _ => Typ.strip_parens(ty)
  };

/* The type members the call requires of the binder [s]: matching a
   component's declared type, which mentions `s`, against the type the call
   actually supplies there reads off `T = String`. These requirements, not
   just the signature, are what makes a resolution fail, so they are what a
   failure reports (as Haskell reports `No instance for (Show String)` and
   OCaml's implicits report `SHOW with type T = string`). */
let rec member_eqs =
        (ctx: Ctx.t, ~s: Var.t, ~want: Typ.t, ~got: Typ.t)
        : list((string, Typ.t)) => {
  /* [want] is read before normalization: the binder is not in the caller's
     context, so normalizing its paths away would lose them. */
  let want = Typ.strip_parens(want);
  let got = Typ.strip_parens(got);
  let recur = (want, got) => member_eqs(ctx, ~s, ~want, ~got);
  switch (path_members(~s, want)) {
  | Some([_, ..._] as members) =>
    let got = reportable(got);
    uninformative(got) ? [] : [(String.concat(".", members), got)];
  | Some([])
  | None =>
    switch (
      Typ.term_of(Typ.weak_head_normalize(ctx, want)),
      Typ.term_of(Typ.weak_head_normalize(ctx, got)),
    ) {
    | (Arrow(w_in, w_out), Arrow(g_in, g_out)) =>
      recur(w_in, g_in) @ recur(w_out, g_out)
    | (List(w), List(g)) => recur(w, g)
    | (Prod(ws), Prod(gs)) when List.length(ws) == List.length(gs) =>
      List.concat(List.map2(recur, ws, gs))
    /* A label, not a position, decides where a component goes, so only
       components under the same label are matched. */
    | (
        TupLabel({term: Label(l_want), _}, w),
        TupLabel({term: Label(l_got), _}, g),
      )
        when l_want == l_got =>
      recur(w, g)
    | _ => []
    }
  };
};

/* Keep the first requirement recorded for each member. */
let dedup_eqs = (eqs: list((string, Typ.t))) =>
  List.fold_left(
    (acc, (name, ty)) =>
      List.mem_assoc(name, acc) ? acc : acc @ [(name, ty)],
    [],
    eqs,
  );

/* Instantiate the binders decided so far in [ty]: substituted ones by their
   path, erased ones by avoidance in a context where they are bound. */
let instantiate =
    (
      ctx: Ctx.t,
      ~subst: list((Var.t, Typ.t)),
      ~erased: list((Var.t, Typ.t)),
      ty,
    )
    : Typ.t => {
  let ty =
    List.fold_left(
      (ty, (from, to_)) => Typ.subst_path_root(~from, ~to_, ty),
      ty,
      subst,
    );
  switch (erased) {
  | [] => ty
  | _ =>
    /* A binder erased here is one resolution could not supply, and that
       failure is already marked, so this stays unknown rather than becoming
       an escaped abstract type that would cascade. */
    Typ.avoid(
      Typ.bind_implicits(ctx, erased),
      ~escape_to=ErasesToUnknown,
      ~escaping=Typ.binder_names(erased),
      ty,
    )
  };
};

/* Rename the implicit binders of an expected arrow type (domain [dom],
   codomain [cod]) to a pattern's [binders], position by position, so the
   pattern's components and the body are analyzed against paths in the
   pattern's own names. */
let rename_expected = (~binders: list(Var.t), dom: Typ.t, cod: Typ.t) => {
  let expected = Typ.binder_names(Typ.implicit_binders(dom));
  if (List.length(expected) == List.length(binders)) {
    List.fold_left2(
      ((dom, cod), from, to_) =>
        if (from == to_) {
          (dom, cod);
        } else {
          let rename = Typ.subst_path_root(~from, ~to_=Var(to_) |> Typ.temp);
          (rename(dom), rename(cod));
        },
      (dom, cod),
      expected,
      binders,
    );
  } else {
    (dom, cod);
  };
};

/* Plan the application of a function whose domain has the implicit
   components [comps] (MatchedTyp.implicit_components) and codomain [cod] to
   [arg], whose synthesized type is [arg_ty]; [ana] is the application's own
   expectation. None when the argument's arity fits no reading: the ordinary
   check then reports the shape mismatch. */
let plan =
    (
      ctx: Ctx.t,
      ~ana: Typ.t,
      ~comps: list((option((Var.t, Typ.t)), Typ.t)),
      ~cod: Typ.t,
      ~arg: Exp.t,
      ~arg_ty: Typ.t,
    )
    : option(t) => {
  let p = List.length(comps);
  let k = List.length(List.filter(((b, _)) => b != None, comps));
  let n = p - k;
  let literal = literal_items(arg);
  let instances = Ctx.implicit_instances(ctx);
  let instance_names = List.map((v: Ctx.var_entry) => v.name, instances);
  let strengthened = (v: Ctx.var_entry) =>
    Typ.strengthen(ctx, v.typ, ~path=Var(v.name) |> Typ.temp);
  /* One reading of the argument: [m] components with types [arg_tys] and
     expressions [arg_exps]; None when the arity fits no assignment. */
  let attempt = (~literal, ~m, ~arg_tys, ~arg_exps): option(t) =>
    if (m < n || m > p) {
      None;
    } else {
      /* Which argument component, if any, supplies each component: an
         implicit one takes the current argument only when extra arguments
         remain and that argument is a module fitting its signature. */
      let (_, _, supplies) =
        List.fold_left(
          ((j, extras, acc), (binder, _)) =>
            switch (binder) {
            | None => (j + 1, extras, [Given(j), ...acc])
            | Some((_, sig_)) =>
              extras > 0
              && j < m
              && is_module_of(ctx, ~sig_, List.nth(arg_tys, j))
                ? (j + 1, extras - 1, [Given(j), ...acc])
                : (j, extras, [Resolve, ...acc])
            },
          (0, m - n, []),
          comps,
        );
      let supplies = List.rev(supplies);
      let consumed =
        List.length(
          List.filter(
            fun
            | Given(_) => true
            | Resolve => false,
            supplies,
          ),
        );
      if (consumed < m) {
        None;
      } else {
        /* Candidates for binder [s] after the later components are taken
           into account: with s := candidate (and the later binders erased),
           every later given component must accept its argument and, under
           an expectation, the codomain must fit it. */
        let narrow = (~subst, ~erased, s, rest, cands) =>
          List.filter(
            (v: Ctx.var_entry) => {
              let subst = subst @ [(s, Var(v.name) |> Typ.temp)];
              let erased =
                erased @ List.filter_map((((b, _), _)) => b, rest);
              let inst = instantiate(ctx, ~subst, ~erased);
              List.for_all(
                (((binder, ty), supply)) =>
                  switch (supply) {
                  | Given(j) =>
                    let want =
                      switch (binder) {
                      | Some((_, sig_)) => sig_
                      | None => ty
                      };
                    fits(ctx, ~ana=inst(want), ~syn=List.nth(arg_tys, j));
                  | Resolve => true
                  },
                rest,
              )
              && (Typ.is_syn(ana) || fits(ctx, ~ana, ~syn=inst(cod)));
            },
            cands,
          );
        /* What the call requires of binder [s]: the later explicit
           components' arguments and, under an expectation, the codomain. */
        let requirements = (~inst, s, rest) =>
          List.concat_map(
            (((binder, ty), supply)) =>
              switch (binder, supply) {
              | (None, Given(j)) =>
                member_eqs(
                  ctx,
                  ~s,
                  ~want=inst(ty),
                  ~got=List.nth(arg_tys, j),
                )
              | (Some(_), _)
              | (None, Resolve) => []
              },
            rest,
          )
          @ (
            Typ.is_syn(ana)
              ? [] : member_eqs(ctx, ~s, ~want=inst(cod), ~got=ana)
          )
          |> dedup_eqs;
        let rec go = (subst, erased, slots, resolved, marks, items, pairs) =>
          switch (pairs) {
          | [] => (
              subst,
              erased,
              List.rev(slots),
              resolved,
              marks,
              List.rev(items),
            )
          | [((binder, ty), supply), ...rest] =>
            let inst = instantiate(ctx, ~subst, ~erased);
            switch (binder, supply) {
            | (None, _) =>
              go(
                subst,
                erased,
                [None, ...slots],
                resolved,
                marks,
                [inst(ty), ...items],
                rest,
              )
            | (Some((s, sig_)), Given(j)) =>
              /* Passed explicitly: the binder is the argument's path, or is
                 erased when the argument is not a path. */
              let sig_ = inst(sig_);
              let (subst, erased) =
                switch (
                  Option.bind(
                    List.nth(arg_exps, j),
                    ModuleHelpers.path_of_exp(ctx),
                  )
                ) {
                | Some(path) => (subst @ [(s, path)], erased)
                | None => (subst, erased @ [(s, sig_)])
                };
              go(
                subst,
                erased,
                [None, ...slots],
                resolved,
                marks,
                [sig_, ...items],
                rest,
              );
            | (Some((s, sig_)), Resolve) =>
              let sig_ = inst(sig_);
              let cands =
                List.filter(
                  (v: Ctx.var_entry) =>
                    is_module_of(ctx, ~sig_, strengthened(v)),
                  instances,
                );
              let cands =
                List.length(cands) > 1
                  ? narrow(~subst, ~erased, s, rest, cands) : cands;
              switch (cands) {
              | [v] =>
                go(
                  subst @ [(s, Var(v.name) |> Typ.temp)],
                  erased,
                  [Some(Instance(v.name)), ...slots],
                  resolved @ [(s, v.name)],
                  marks,
                  items,
                  rest,
                )
              | [] =>
                go(
                  subst,
                  erased @ [(s, sig_)],
                  [Some(Unresolved), ...slots],
                  resolved,
                  marks
                  @ [
                    Mark.ImplicitNotFound({
                      binder: s,
                      signature: sig_,
                      constraints: requirements(~inst, s, rest),
                      candidates: instance_names,
                    }),
                  ],
                  items,
                  rest,
                )
              | _ =>
                go(
                  subst,
                  erased @ [(s, sig_)],
                  [Some(Unresolved), ...slots],
                  resolved,
                  marks
                  @ [
                    Mark.ImplicitAmbiguous({
                      binder: s,
                      signature: sig_,
                      candidates:
                        List.map((v: Ctx.var_entry) => v.name, cands),
                    }),
                  ],
                  items,
                  rest,
                )
              };
            };
          };
        let (subst, erased, slots, resolved, marks, ty_items) =
          go([], [], [], [], [], [], List.combine(comps, supplies));
        /* A single remaining component is the argument itself unless the
           argument is a (one-item) literal tuple. */
        let ty_in =
          switch (literal, ty_items) {
          | (None, [t]) => t
          | _ => Prod(ty_items) |> Typ.temp
          };
        let ty_out = instantiate(ctx, ~subst, ~erased, cod);
        Some({
          ty_in,
          ty_out,
          slots,
          resolved,
          marks,
          literal_tuple: Option.is_some(literal),
        });
      };
    };
  let whole_arg = (): option(t) =>
    attempt(~literal=None, ~m=1, ~arg_tys=[arg_ty], ~arg_exps=[Some(arg)]);
  switch (literal) {
  | Some(es) =>
    /* A literal tuple's items are the components. If that leaves items
       unconsumed and there is one explicit component, the tuple is that
       component's argument. */
    let m = List.length(es);
    let arg_tys =
      switch (whnf_term(ctx, arg_ty)) {
      | Prod(ts) when List.length(ts) == m => ts
      | _ => List.init(m, _ => Unknown(Internal) |> Typ.temp)
      };
    switch (
      attempt(~literal, ~m, ~arg_tys, ~arg_exps=List.map(Option.some, es))
    ) {
    | Some(_) as r => r
    | None when n == 1 => whole_arg()
    | None => None
    };
  | None when n == 1 => whole_arg()
  | None =>
    /* A tuple-typed argument that is not a literal: its arity is its type's. */
    let m =
      switch (whnf_term(ctx, arg_ty)) {
      | Prod(ts) => List.length(ts)
      | _ => 1
      };
    let arg_tys =
      switch (whnf_term(ctx, arg_ty)) {
      | Prod(ts) when List.length(ts) == m => ts
      | _ => List.init(m, _ => Unknown(Internal) |> Typ.temp)
      };
    attempt(~literal, ~m, ~arg_tys, ~arg_exps=List.init(m, _ => None));
  };
};

/* Splice the resolved instances into the elaborated argument at their
   positions among the given components; an unresolved one becomes a hole.
   Synthetic nodes get ids derived from the application's, so no registered
   id carries a term its recorded info does not describe. */
let splice = (~site: Id.t, plan: t, arg_elab: Exp.t): Exp.t =>
  if (List.for_all(Option.is_none, plan.slots)) {
    arg_elab;
  } else {
    let next = ref(site);
    let fresh = term => {
      next := Id.next(next^);
      IdTagged.mk_internal([next^], term);
    };
    let node =
      fun
      | Instance(x) => fresh(Var(x): Exp.term)
      | Unresolved => fresh(EmptyHole: Exp.term);
    let fill = (given: list(Exp.t)): list(Exp.t) => {
      let (_, rev) =
        List.fold_left(
          ((given, acc), slot) =>
            switch (slot, given) {
            | (Some(o), _) => (given, [node(o), ...acc])
            | (None, [e, ...given]) => (given, [e, ...acc])
            | (None, []) => (given, [fresh(EmptyHole: Exp.term), ...acc])
            },
          (given, []),
          plan.slots,
        );
      List.rev(rev);
    };
    switch (plan.slots, literal_items(arg_elab)) {
    | ([Some(o)], _) => node(o)
    | (_, Some(es)) when plan.literal_tuple =>
      fresh(Tuple(fill(es)): Exp.term)
    | _ =>
      let given = List.length(List.filter(Option.is_none, plan.slots));
      if (given == 1) {
        fresh(Tuple(fill([arg_elab])): Exp.term);
      } else {
        /* A tuple-typed argument that is not a literal: destructure it. */
        let names = List.init(given, i => "%a" ++ string_of_int(i));
        let vars = List.map(x => fresh(Var(x): Pat.term), names);
        let refs = List.map(x => fresh(Var(x): Exp.term), names);
        fresh(
          Let(
            fresh(Tuple(vars): Pat.term),
            arg_elab,
            fresh(Tuple(fill(refs)): Exp.term),
          ): Exp.term,
        );
      };
    };
  };

/* The resolved instances count as uses at the application. */
let co_ctx = (~site: Id.t, plan: t): CoCtx.t =>
  plan.resolved
  |> List.map(((_, inst)) =>
       CoCtx.singleton(inst, site, Unknown(Internal) |> Typ.temp)
     )
  |> CoCtx.union;
