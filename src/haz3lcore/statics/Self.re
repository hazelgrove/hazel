open Util;

/* SELF.re

   This module defines the SELF data structure, which represents
   the synthetic type information derivable from a term independent
   of the type expectation (i.e. MODE) of its syntactic context. This
   synethetic information is not entirely independent, in that it still
   uses the typing context passed down from the syntactic context.

   A term which from which a type can be derived in isolation, that is,
   that has a valid synthetic typing judgement, will generally have a SELF
   of Just(some_type). (The one current exception are the constructors of labelled
   sum types, which are handled specially as their synthetic type
   may be 'overwritten' by the analytic expectation)

   The other cases all represent states for which no single type can be
   derived, such as syntactic errors, or branching constructs which may
   have inconsistent types.

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type join_type =
  | Id
  | List;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Just(Typ.t) /* Just a regular type */
  | NoJoin(join_type, list(Typ.source)) /* Inconsistent types for e.g match, listlits */
  | Duplicate(LabeledTuple.label, t) /* Duplicate label, marked as duplicate */
  | BadToken(Token.t) /* Invalid expression token, continues with undefined behavior */
  | BadOperator(string) /* Invalid operator, continues with undefined behavior */
  | BadTrivAp(Typ.t) /* Trivial (nullary) ap on function that doesn't take triv */
  | BadLabel(Any.t) /* TupLabel label component is not a valid Label*/
  | InvalidLabel(LabeledTuple.label) /* Invalid label in a labeled tuple */
  | TupleLabelError({
      malformed_labels: list(Any.t), // Labels that are not of the right syntactic form
      duplicate_labels: list(LabeledTuple.label),
      invalid_labels: list(LabeledTuple.label), // Labels that are present but aren't present in the analyzed type
      typ: Typ.t,
    }) /* Tuple/TupLabel contains malformed labels, duplicate labels, and/or invalid labels */
  | IsMulti /* Multihole, treated as hole */
  | FreeConstructor(Constructor.t) /* Constructor not bound in context or ana type */
  | WantTuple /* Want a Tuple, found not-tuple */
  | LabelNotFound(LabeledTuple.label, list(LabeledTuple.label))
  | InvalidUseMode({
      bad_typ: Typ.t,
      inner_typ: Typ.t,
    }); /* Currently used by the dot operator for a label not found */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_partial_ap =
  | NoDeferredArgs
  | ArityMismatch({
      expected: int,
      actual: int,
    });

/* Expressions can also be free variables */
[@deriving (show({with_path: false}), sexp, yojson)]
type exp =
  | Free(Var.t)
  | InexhaustiveMatch(exp)
  | IsDeferral(Exp.deferral_position)
  | IsBadPartialAp(error_partial_ap)
  | Common(t);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat =
  | Redundant(pat)
  | ExpectedConstructor(pat)
  | Common(t);

let join_of = (j: join_type, ty: Typ.t): Typ.t =>
  switch (j) {
  | Id => ty
  | List => List(ty) |> Typ.fresh_empty
  };

/* What the type would be if the position had been
   synthetic, so no hole fixing. Returns none if
   there's no applicable synthetic rule. */
// TOOD: Slicing unknown types derived from errors
let typ_of: (Ctx.t, t) => option(Typ.t) =
  _ctx =>
    fun
    | Just(typ)
    | Duplicate(_, Just(typ))
    | TupleLabelError({typ, _}) => Some(typ)
    | FreeConstructor(name) =>
      Some(
        Sum([
          ConstructorMap.Variant(name, [Id.invalid], None),
          ConstructorMap.BadEntry(Unknown(Internal) |> Typ.temp_empty),
        ])
        |> Typ.temp_empty,
      )
    | InvalidUseMode({inner_typ, _}) => Some(inner_typ)
    | BadToken(_)
    | BadOperator(_)
    | BadTrivAp(_)
    | IsMulti
    | Duplicate(_)
    | WantTuple
    | LabelNotFound(_)
    | BadLabel(_)
    | InvalidLabel(_)
    | NoJoin(_) => None;

let typ_of_exp: (Ctx.t, exp) => option(Typ.t) =
  ctx =>
    fun
    | Free(_)
    | InexhaustiveMatch(_)
    | IsDeferral(_)
    | IsBadPartialAp(_) => None
    | Common(self) => typ_of(ctx, self);

let rec typ_of_pat: (Ctx.t, pat) => option(Typ.t) =
  ctx =>
    fun
    | Redundant(pat) => typ_of_pat(ctx, pat)
    | ExpectedConstructor(pat) => typ_of_pat(ctx, pat)
    | Common(self) => typ_of(ctx, self);

/* The self of a var depends on the ctx; if the
   lookup fails, it is a free variable */
// Slice of a var is the ids of the term + the var_entry in ctx_used + the slice from the context
let of_exp_var = (ids: list(Id.t), ctx: Ctx.t, name: Var.t): exp =>
  switch (Ctx.lookup_var(ctx, name)) {
  | None => Free(name)
  | Some(var) =>
    Common(
      Just(
        Typ.(
          var.typ
          |> wrap_ana_slice(CodeSlice.of_ids_ctx(ids, [Var(var.name)]))
        ),
      ),
    )
  };

let ctr_ana_typ =
    (ctx: Ctx.t, ty_ana: Typ.t, ctr: Constructor.t): option(Typ.t) => {
  let slice_ana = Typ.ana_code_slice_of(ty_ana);
  /* If a ctr is being analyzed against (an arrow type returning)
     a sum type having that ctr as a variant, we consider the
     ctr's type to be determined by the sum type */
  OptUtil.Syntax.(
    if (Typ.is_arrow(ty_ana)) {
      let (_, ty_out) = Typ.unarrow(ty_ana);
      let* ctrs = Typ.get_sum_constructors(ctx, ty_out);
      let* ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => None
      | Some(ty_in) =>
        Some(
          Arrow(ty_in, ty_out) |> Typ.from_ana_slice(slice_ana) |> Typ.temp,
        )
      };
    } else {
      let* ctrs = Typ.get_sum_constructors(ctx, ty_ana);
      let+ ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => ty_ana
      | Some(ty_in) =>
        Arrow(ty_in, ty_ana) |> Typ.from_ana_slice(slice_ana) |> Typ.temp
      };
    }
  );
};

let of_ctr =
    (
      _ids, // TODO: Track ids of ctr error
      ctx: Ctx.t,
      name: Constructor.t,
      ana: Typ.t,
      ty: option(option(Typ.t)),
    )
    : t => {
  // (1) check to see if type already assigned (e.g. if we are doing statics for results)
  switch (ty) {
  | Some(Some(ty)) => Just(ty)
  | Some(None) => FreeConstructor(name)
  | None =>
    // (2) check to see if constructor appears in ana type
    switch (ctr_ana_typ(ctx, ana, name)) {
    | Some(ty) => Just(ty)
    | None =>
      // (3) check to see if constructor appears in ctx
      switch (Ctx.lookup_ctr(ctx, name)) {
      | Some({typ, _}) => Just(typ)
      | None => FreeConstructor(name)
      }
    }
  };
};

let of_deferred_ap = (args, ty_ins: list(Typ.t), ty_out: Typ.t): exp => {
  let expected = List.length(ty_ins);
  let actual = List.length(args);
  if (expected != actual) {
    IsBadPartialAp(
      ArityMismatch({
        expected,
        actual,
      }),
    );
  } else if (List.for_all(Exp.is_deferral, args)) {
    IsBadPartialAp(NoDeferredArgs);
  } else {
    let ty_ins =
      List.combine(args, ty_ins)
      |> List.filter(((arg, _ty)) => Exp.is_deferral(arg))
      |> List.map(snd);
    let ty_in =
      List.length(ty_ins) == 1
        ? List.hd(ty_ins) : Prod(ty_ins) |> Typ.fresh_empty; // TODO: slicing
    Common(Just(Arrow(ty_in, ty_out) |> Typ.fresh_empty));
  };
};

let add_source =
  List.map2((id, ty) =>
    Typ.{
      id,
      ty,
    }
  );

let of_match =
    (ids: list(Id.t), ctx: Ctx.t, tys: list(Typ.t), c_ids: list(Id.t)): t =>
  switch (
    Typ.join_all(~empty=Unknown(Internal) |> Typ.fresh_empty, ctx, tys)
  ) {
  | None => NoJoin(Id, add_source(c_ids, tys))
  | Some(ty) => Just(ty |> Typ.(wrap_syn_slice(CodeSlice.of_ids(ids))))
  };

// Slices of list literals is just the join of the element slices + the ids of the listlit constructor
let of_list_lit =
    (
      ~empty,
      ids: list(Id.t),
      ctx: Ctx.t,
      tys: list(Typ.t),
      elem_ids: list(Id.t),
    )
    : t =>
  switch (Typ.join_all(~empty, ctx, tys)) {
  | None => NoJoin(List, add_source(elem_ids, tys))
  | Some(ty) =>
    Just(List(ty) |> Typ.from_syn_slice(CodeSlice.of_ids(ids)) |> Typ.temp)
  };

// Slice of a cons is just the slice of the head element + the ids of the cons constructor
let of_list_cons = (ids: list(Id.t), hd_ty: Typ.t): t =>
  Just(
    List(hd_ty) |> Typ.from_syn_slice(CodeSlice.of_ids(ids)) |> Typ.temp,
  );

// Slice of a concat is just the joined slice of the list arguments + the @ operator ids
let of_list_concat = (ids: list(Id.t), ctx: Ctx.t, tys: list(Typ.t)): t =>
  switch (Typ.join_all(~empty=Unknown(Internal) |> Typ.temp_empty, ctx, tys)) {
  | None => NoJoin(List, add_source(ids, tys))
  | Some(ty) => Just(Typ.(ty |> wrap_syn_slice(CodeSlice.of_ids(ids))))
  };

let of_prod = (ids: list(Id.t), tys: list(Typ.t)) =>
  Just(
    Prod(tys) |> Typ.from_syn_slice(ids |> CodeSlice.of_ids) |> Typ.temp,
  );

// Base types slices should all contain the term's ids
let of_base = (ids: list(Id.t), ty: Typ.term) =>
  Just(ty |> Typ.from_syn_slice(CodeSlice.of_ids(ids)) |> Typ.temp);
// Operation slices are similarly determined by their ids
let of_op = of_base;

let of_parens = (ids: list(Id.t), ty: Typ.t) =>
  Just(ty |> Typ.(wrap_syn_slice(CodeSlice.of_ids(ids))));
let of_seq = of_parens;
let of_filter = of_seq; // TODO: check
let of_fix = of_seq;
let of_ap_ok = of_seq;

// Holes should be omitted from slices. Note: likely useful in practice to eventually not omit these slices
let hole = Just(Unknown(Internal) |> Typ.temp_empty);

// Where arg has been analysed to check if nullary_args
let of_ap =
    (
      ids: list(Id.t),
      ctx: Ctx.t,
      arg_ids: list(Id.t),
      ty_in: Typ.t,
      ty_out: Typ.t,
    )
    : t =>
  Id.is_nullary_ap_flag(arg_ids)
  && !Typ.is_consistent(ctx, ty_in, Prod([]) |> Typ.temp_empty)
    ? BadTrivAp(ty_in) : ty_out |> of_ap_ok(ids);

let of_typap = (ids: list(Id.t), ctx: Ctx.t, typ_ap: Typ.t, ty: Typ.t) => {
  let (option_name, ty_body) = Typ.matched_forall(ctx, ty);
  switch (option_name) {
  | Some(name) =>
    Just(
      Typ.(
        subst(typ_ap, name, ty_body)
        |> wrap_syn_slice(CodeSlice.of_ids(ids))
      ),
    ) // TODO: Check slices here
  | None => Just(ty_body) /* invalid name matches with no free type variables. */
  };
};

let of_fun =
    (ids: list(Id.t), is_exhaustive: bool, ty_in: Typ.t, ty_out: Typ.t) => {
  let unwrapped_self: exp =
    Common(
      Just(
        Arrow(ty_in, ty_out)
        |> Typ.from_syn_slice(CodeSlice.of_ids(ids))
        |> Typ.temp,
      ),
    );
  is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
};

let of_typfun = (ids: list(Id.t), tpat, ty) =>
  Just(
    Forall(tpat, ty)
    |> Typ.from_syn_slice(CodeSlice.of_ids(ids))
    |> Typ.temp,
  );

let of_let = (is_exhaustive: bool, ty: Typ.t) => {
  let unwrapped_self: exp = Common(Just(ty));
  is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
};

let of_annot = (ids: list(Id.t), ty: Typ.t): t => {
  // The annotation should be global
  Just(
    /*create_slices*/ ty |> Typ.(wrap_ana_slice(CodeSlice.of_ids(ids))),
  );
};

let of_tuple =
    (ids, ~duplicate_labels, ~malformed_labels, ~invalid_labels, ty_list) => {
  let ty_list = Typ.remove_duplicate_labels(~duplicate_labels, ty_list);

  List.is_empty(malformed_labels)
  && List.is_empty(duplicate_labels)
  && List.is_empty(invalid_labels)
    ? Just(
        Prod(ty_list)
        |> Typ.from_syn_slice(CodeSlice.of_ids(ids))
        |> Typ.temp,
      )
    : TupleLabelError({
        malformed_labels,
        duplicate_labels,
        invalid_labels,
        typ:
          Prod(ty_list)
          |> Typ.from_syn_slice(CodeSlice.of_ids(ids))
          |> Typ.temp,
      });
};

let of_label = (ids, name, ~duplicates) => {
  let self =
    Just(
      Label(name) |> Typ.from_syn_slice(CodeSlice.of_ids(ids)) |> Typ.temp,
    );
  List.exists(l => name == l, duplicates) ? Duplicate(name, self) : self;
};
