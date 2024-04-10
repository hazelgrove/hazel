/*


 A nice property would be that elaboration is idempotent...
 */

open Util;
open OptUtil.Syntax;

exception MissingTypeInfo;

module Elaboration = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    d: DHExp.t,
    info_map: Statics.Map.t,
  };
};

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

let fixed_exp_typ = (m: Statics.Map.t, e: UExp.t): option(Typ.t) =>
  switch (Id.Map.find_opt(UExp.rep_id(e), m)) {
  | Some(InfoExp({ty, _})) => Some(ty)
  | _ => None
  };

let fixed_pat_typ = (m: Statics.Map.t, p: UPat.t): option(Typ.t) =>
  switch (Id.Map.find_opt(UPat.rep_id(p), m)) {
  | Some(InfoPat({ty, _})) => Some(ty)
  | _ => None
  };

/* This function gets rid of casts that do nothing, and casts to a
   type with a SynSwitch. */
let rec get_cast = (t1: Typ.t, t2: Typ.t): option((Typ.t, Typ.t)) => {
  switch (Typ.term_of(t1), Typ.term_of(t2)) {
  // SynSwitch should only appear on right.
  | (_, Unknown(SynSwitch))
  | (Unknown(SynSwitch), _) => None
  | (Parens(t1), _) => get_cast(t1, t2)
  | (_, Parens(t2)) => get_cast(t1, t2)

  | (Int, Int)
  | (Bool, Bool)
  | (Float, Float)
  | (String, String)
  | (Unknown(_) | Ap(_), Unknown(_) | Ap(_)) => None
  | (Var(x), Var(y)) when x == y => None

  | (List(t1), List(t2)) =>
    let+ (t1', t2') = get_cast(t1, t2);
    (List(t1') |> Typ.fresh, List(t2') |> Typ.fresh);
  | (Arrow(t1, t2), Arrow(t3, t4)) =>
    let cast1 = get_cast(t1, t3);
    let cast2 = get_cast(t2, t4);
    switch (cast1, cast2) {
    | (None, None) => None
    | _ =>
      let (t1', t2') = cast1 |> Option.value(~default=(t1, t1));
      let (t3', t4') = cast2 |> Option.value(~default=(t2, t2));
      Some((Arrow(t1', t2') |> Typ.fresh, Arrow(t3', t4') |> Typ.fresh));
    };
  | (Prod(ts1), Prod(ts2)) =>
    let casts = List.map2(get_cast, ts1, ts2);
    if (List.for_all(Option.is_none, casts)) {
      None;
    } else {
      let casts =
        List.map2(
          (cst, dft) => Option.value(~default=(dft, dft), cst),
          casts,
          ts1,
        );
      Some((
        Prod(casts |> List.map(fst)) |> Typ.fresh,
        Prod(casts |> List.map(snd)) |> Typ.fresh,
      ));
    };
  | (Sum(m1), Sum(m2)) =>
    let+ (m1', m2') = ConstructorMap.get_cast(Typ.eq, get_cast, m1, m2);
    (Sum(m1') |> Typ.fresh, Sum(m2') |> Typ.fresh);
  | (Rec({term: Var(x1), _}, t1), Rec({term: Var(x2), _}, t2))
      when x1 == x2 =>
    get_cast(t1, t2)
  | (Rec({term: Var(x1), _}, t1), Rec({term: Var(x2), _}, t2)) =>
    get_cast(t1, Typ.subst(Typ.fresh(Var(x1)), x2, t2))

  | (Int, _)
  | (Bool, _)
  | (Float, _)
  | (String, _)
  | (Unknown(_) | Ap(_), _)
  | (List(_), _)
  | (Arrow(_), _)
  | (Prod(_), _)
  | (Sum(_), _)
  | (Var(_), _)
  | (Rec(_), _) => Some((t1, t2))
  };
};

let fresh_cast = (d: DHExp.t, t1: Typ.t, t2: Typ.t): DHExp.t => {
  switch (get_cast(t1, t2)) {
  | Some((t1', t2')) => DHExp.Cast(d, t1', t2') |> DHExp.fresh
  | None => d
  };
};

/* Adds casts if required.
   When adding a new construct, [TODO: write something helpful here] */
let cast = (ctx: Ctx.t, mode: Mode.t, self_ty: Typ.t, d: DHExp.t) =>
  switch (mode) {
  | Syn => d
  | SynFun =>
    switch (Typ.term_of(self_ty)) {
    | Unknown(_) =>
      fresh_cast(d, self_ty, Arrow(self_ty, self_ty) |> Typ.fresh)
    | Arrow(_) => d
    | _ => failwith("Elaborator.wrap: SynFun non-arrow-type")
    }
  | Ana(ana_ty) =>
    let ana_ty = Typ.normalize(ctx, ana_ty);
    /* Forms with special ana rules get cast from their appropriate Matched types */
    switch (DHExp.term_of(d)) {
    | ListLit(_)
    | ListConcat(_)
    | Cons(_) =>
      switch (Typ.term_of(ana_ty)) {
      | Unknown(_) => fresh_cast(d, List(ana_ty) |> Typ.fresh, ana_ty)
      | _ => d
      }
    | Fun(_) =>
      /* See regression tests in Documentation/Dynamics */
      let (_, ana_out) = Typ.matched_arrow(ctx, ana_ty);
      let (self_in, _) = Typ.matched_arrow(ctx, self_ty);
      fresh_cast(d, Arrow(self_in, ana_out) |> Typ.fresh, ana_ty);
    | Tuple(ds) =>
      switch (Typ.term_of(ana_ty)) {
      | Unknown(prov) =>
        let us =
          List.init(List.length(ds), _ => Typ.Unknown(prov) |> Typ.fresh);
        fresh_cast(d, Prod(us) |> Typ.fresh, ana_ty);
      | _ => d
      }
    | Constructor(_) =>
      switch (ana_ty |> Typ.term_of, self_ty |> Typ.term_of) {
      | (Unknown(_), Rec(_, {term: Sum(_), _}))
      | (Unknown(_), Sum(_)) => fresh_cast(d, self_ty, ana_ty)
      | _ => d
      }
    | Ap(_, f, _) =>
      switch (DHExp.term_of(f)) {
      | Constructor(_) =>
        switch (ana_ty |> Typ.term_of, self_ty |> Typ.term_of) {
        | (Unknown(_), Rec(_, {term: Sum(_), _}))
        | (Unknown(_), Sum(_)) => fresh_cast(d, self_ty, ana_ty)
        | _ => d
        }
      | StaticErrorHole(_, g) =>
        switch (DHExp.term_of(g)) {
        | Constructor(_) =>
          switch (ana_ty |> Typ.term_of, self_ty |> Typ.term_of) {
          | (Unknown(_), Rec(_, {term: Sum(_), _}))
          | (Unknown(_), Sum(_)) => fresh_cast(d, self_ty, ana_ty)
          | _ => d
          }
        | _ => fresh_cast(d, self_ty, ana_ty)
        }
      | _ => fresh_cast(d, self_ty, ana_ty)
      }
    /* Forms with special ana rules but no particular typing requirements */
    | Match(_)
    | If(_)
    | Seq(_)
    | Let(_)
    | FixF(_) => d
    /* Hole-like forms: Don't cast */
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | StaticErrorHole(_) => d
    /* DHExp-specific forms: Don't cast */
    | Cast(_)
    | Closure(_)
    | Filter(_)
    | FailedCast(_)
    | DynamicErrorHole(_) => d
    /* Forms that are currently desugared in elaboration */
    | Deferral(_)
    | DeferredAp(_) => d
    /* Normal cases: wrap */
    | Var(_)
    | BuiltinFun(_)
    | Parens(_)
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | UnOp(_)
    | BinOp(_)
    | TyAlias(_)
    | Test(_) => fresh_cast(d, self_ty, ana_ty)
    };
  };

/* Handles cast insertion and non-empty-hole wrapping
   for elaborated expressions */
let wrap = (m, exp: Exp.t): DHExp.t => {
  let (mode, self, ctx) =
    switch (Id.Map.find_opt(Exp.rep_id(exp), m)) {
    | Some(Info.InfoExp({mode, self, ctx, _})) => (mode, self, ctx)
    | _ => raise(MissingTypeInfo)
    };
  switch (Info.status_exp(ctx, mode, self)) {
  | NotInHole(_) =>
    let self_ty =
      switch (Self.typ_of_exp(ctx, self)) {
      | Some(self_ty) => Typ.normalize(ctx, self_ty)
      | None => Unknown(Internal) |> Typ.fresh
      };
    cast(ctx, mode, self_ty, exp);
  | InHole(
      FreeVariable(_) | Common(NoType(_)) | UnusedDeferral | BadPartialAp(_) |
      Common(Inconsistent(Internal(_))),
    ) => exp
  | InHole(Common(Inconsistent(Expectation(_) | WithArrow(_)))) =>
    DHExp.fresh(StaticErrorHole(Exp.rep_id(exp), exp))
  };
};

/*
  This function converts user-expressions (UExp.t) to dynamic expressions (DHExp.t). They
  have the same datatype but there are some small differences so that UExp.t can be edited
  and DHExp.t can be evaluated.

 Currently, Elaboration does the following things:

   - Insert casts
   - Insert non-empty hole wrappers
   - Annotate functions with names
   - Insert implicit fixpoints
   - Remove parentheses [not strictly necessary]
   - Remove TyAlias [not strictly necessary]

 When adding a new construct you can probably just add it to the default cases.
  */
let rec dexp_of_uexp = (m, uexp, ~in_filter) => {
  Exp.map_term(
    ~f_exp=
      (continue, exp) => {
        let (term, rewrap) = Exp.unwrap(exp);
        switch (term) {
        // Default cases: do not need to change at elaboration
        | Closure(_)
        | Cast(_)
        | Invalid(_)
        | EmptyHole
        | MultiHole(_)
        | StaticErrorHole(_)
        | DynamicErrorHole(_)
        | FailedCast(_)
        | Bool(_)
        | Int(_)
        | Float(_)
        | String(_)
        | ListLit(_)
        | Tuple(_)
        | Cons(_)
        | ListConcat(_)
        | UnOp(Int(_) | Bool(_), _)
        | BinOp(_)
        | BuiltinFun(_)
        | Seq(_)
        | Test(_)
        | Filter(Residue(_), _)
        | Var(_)
        | Constructor(_)
        | Ap(_)
        | If(_)
        | Fun(_)
        | FixF(_)
        | Match(_)
        | Deferral(_) => continue(exp) |> wrap(m)

        /* DeferredAp - TODO: this is currently desugared, but it should ideally persist
           through to evaluation. Changing `dexp_of_uexp` will be easy (add it to default cases),
           but someone will need to work out what `cast` should do. */
        | DeferredAp(fn, args) =>
          let (mode, self, ctx, ancestors) =
            switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
            | Some(Info.InfoExp({mode, self, ctx, ancestors, _})) => (
                mode,
                self,
                ctx,
                ancestors,
              )
            | _ => failwith("DeferredAp missing info")
            };
          let err_status = Info.status_exp(ctx, mode, self);
          switch (err_status) {
          | InHole(BadPartialAp(NoDeferredArgs)) =>
            dexp_of_uexp(~in_filter, m, fn)
          | InHole(BadPartialAp(ArityMismatch(_))) =>
            DHExp.Invalid("<invalid partial ap>") |> DHExp.fresh
          | _ =>
            let mk_tuple = (ctor, xs) =>
              List.length(xs) == 1 ? List.hd(xs) : ctor(xs);
            let ty_fn = fixed_exp_typ(m, fn) |> Option.get;
            let (ty_arg, ty_ret) = Typ.matched_arrow(ctx, ty_fn);
            let ty_ins = Typ.matched_args(ctx, List.length(args), ty_arg);
            /* Substitute all deferrals for new variables */
            let (pats, ty_args, ap_args, ap_ctx) =
              List.combine(args, ty_ins)
              |> List.fold_left(
                   ((pats, ty_args, ap_args, ap_ctx), (e: Exp.t, ty)) =>
                     if (Exp.is_deferral(e)) {
                       // Internal variable name for deferrals
                       let name =
                         "__deferred__" ++ string_of_int(List.length(pats));
                       let var: Exp.t = {
                         ids: e.ids,
                         copied: false,
                         term: Var(name),
                       };
                       let var_entry =
                         Ctx.VarEntry({name, id: Exp.rep_id(e), typ: ty});
                       (
                         pats @ [Var(name) |> DHPat.fresh],
                         ty_args @ [ty],
                         ap_args @ [var],
                         Ctx.extend(ap_ctx, var_entry),
                       );
                     } else {
                       (pats, ty_args, ap_args @ [e], ap_ctx);
                     },
                   ([], [], [], ctx),
                 );
            let (pat, _) = (
              mk_tuple(x => DHPat.Tuple(x) |> DHPat.fresh, pats),
              mk_tuple(x => Typ.Prod(x) |> Typ.fresh, ty_args),
            );
            let arg: Exp.t = {
              ids: [Id.mk()],
              copied: false,
              term: Tuple(ap_args),
            };
            let body: Exp.t = {
              ids: [Id.mk()],
              copied: false,
              term: Ap(Forward, fn, arg),
            };
            let (_info, m) =
              Statics.uexp_to_info_map(
                ~ctx=ap_ctx,
                ~mode=Ana(ty_ret),
                ~ancestors,
                body,
                m,
              );
            let dbody = dexp_of_uexp(~in_filter, m, body);
            Fun(pat, dbody, None, None) |> DHExp.fresh;
          };

        // Unquote operator: should be turned into constructor if inside filter body.
        | UnOp(Meta(Unquote), e) =>
          switch (e.term) {
          | Var("e") when in_filter =>
            Constructor("$e") |> DHExp.fresh |> wrap(m)
          | Var("v") when in_filter =>
            Constructor("$v") |> DHExp.fresh |> wrap(m)
          | _ => DHExp.EmptyHole |> DHExp.fresh |> wrap(m)
          }
        | Filter(Filter({act, pat}), body) =>
          Filter(
            Filter({act, pat: dexp_of_uexp(m, pat, ~in_filter=true)}),
            dexp_of_uexp(m, body, ~in_filter),
          )
          |> rewrap
          |> wrap(m)

        // Let bindings: insert implicit fixpoints and label functions with their names.
        | Let(p, def, body) =>
          let add_name: (option(string), DHExp.t) => DHExp.t = (
            (name, d) => {
              let (term, rewrap) = DHExp.unwrap(d);
              switch (term) {
              | Fun(p, e, ctx, _) => DHExp.Fun(p, e, ctx, name) |> rewrap
              | _ => d
              };
            }
          );
          let ddef = dexp_of_uexp(m, def, ~in_filter);
          let dbody = dexp_of_uexp(m, body, ~in_filter);
          switch (UPat.get_recursive_bindings(p)) {
          | None =>
            /* not recursive */
            DHExp.Let(
              p,
              add_name(Option.map(x => x ++ "", UPat.get_var(p)), ddef),
              dbody,
            )
            |> rewrap
            |> wrap(m)
          | Some(b) =>
            DHExp.Let(
              p,
              FixF(
                p,
                add_name(
                  Some(String.concat(",", List.map(x => x ++ "+", b))),
                  ddef,
                ),
                None,
              )
              |> DHExp.fresh,
              dbody,
            )
            |> rewrap
            |> wrap(m)
          };

        // type alias and parentheses: remove during elaboration
        | TyAlias(_, _, e)
        | Parens(e) => dexp_of_uexp(m, e, ~in_filter)
        };
      },
    uexp,
  );
};

//let dhexp_of_uexp = Core.Memo.general(~cache_size_bound=1000, dhexp_of_uexp);

let uexp_elab = (m: Statics.Map.t, uexp: UExp.t): ElaborationResult.t =>
  switch (dexp_of_uexp(m, uexp, ~in_filter=false)) {
  | exception MissingTypeInfo => DoesNotElaborate
  | d =>
    //let d = uexp_elab_wrap_builtins(d);
    let ty =
      switch (fixed_exp_typ(m, uexp)) {
      | Some(ty) => ty
      | None => Typ.Unknown(Internal) |> Typ.fresh
      };
    Elaborates(d, ty, Delta.empty);
  };
