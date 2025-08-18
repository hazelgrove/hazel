open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Atom(Atom.cls)
  | Invalid
  | EmptyHole
  | MultiHole
  | SynSwitch
  | Internal
  | Arrow
  | Prod
  | TupLabel
  | Label
  | Sum
  | List
  | Var
  | Constructor // Constructor does not exist on Typ.term it's being used here as a hack for the cursors inspector
  | Parens
  | Ap
  | Rec
  | Forall;

include TermBase.Typ;

// Utilities
let term_of: t => term = ty => IdTagged.term_of(ty).typ;
let slice_of: t => slice = IdTagged.term_of;
let unwrap_to_slice: t => (slice, slice => t) = IdTagged.unwrap;
let unwrap: t => (term, term => t) = Grammar.unwrap_typslice;
let unwrap_slice: slice => (term, term => slice) = Grammar.unwrap_typslice_term;
let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: slice => t = IdTagged.fresh;
/* fresh assigns a random id, whereas temp assigns Id.invalid, which
   is a lot faster, and since we so often make types and throw them away
   shortly after, it makes sense to use it. */
let temp: slice => t =
  term => {
    term,
    annotation: {
      ids: [Id.invalid],
    },
  };

// Create an empty type slice from a typ_term
let empty: term => slice =
  typ => {
    typ,
    syn_slice: CodeSlice.empty,
    ana_slice: CodeSlice.empty,
  };

let fresh_empty = Fun.compose(fresh, empty);
let temp_empty = Fun.compose(fresh, empty);

let from_code_slices: ((CodeSlice.t, CodeSlice.t), term) => slice =
  ((syn_slice, ana_slice), typ) => {
    typ,
    syn_slice,
    ana_slice,
  };

let from_syn_slice = code => from_code_slices((code, CodeSlice.empty));

let from_ana_slice = code => from_code_slices((CodeSlice.empty, code));

let wrap_syn_slice: (CodeSlice.t, t) => t =
  (code, {term: {syn_slice, _} as slice, _} as ty) => {
    ...ty,
    term: {
      ...slice,
      syn_slice: CodeSlice.union(syn_slice, code),
    },
  };
let wrap_ana_slice: (CodeSlice.t, t) => t =
  (code, {term: {ana_slice, _} as slice, _} as ty) => {
    ...ty,
    term: {
      ...slice,
      ana_slice: CodeSlice.union(ana_slice, code),
    },
  };
let wrap_slices = ((c1, c2), ty) =>
  ty |> wrap_syn_slice(c1) |> wrap_ana_slice(c2);

let code_slices_of: t => (CodeSlice.t, CodeSlice.t) =
  ty => (slice_of(ty).syn_slice, slice_of(ty).ana_slice);
let syn_code_slice_of = Fun.compose(fst, code_slices_of);
let ana_code_slice_of = Fun.compose(fst, code_slices_of);

let rec full_slice = ty =>
  (
    switch (term_of(ty)) {
    | List(ty)
    | Parens(ty)
    | Rec(_, ty)
    | Forall(_, ty) => full_slice(ty)
    | Arrow(ty1, ty2)
    | Ap(ty1, ty2)
    | TupLabel(ty1, ty2) =>
      TupleUtil.map2_bin(CodeSlice.union, full_slice(ty1), full_slice(ty2))
    | Prod(tys) =>
      List.fold_left(
        (acc, ty) =>
          TupleUtil.map2_bin(CodeSlice.union, full_slice(ty), acc),
        (CodeSlice.empty, CodeSlice.empty),
        tys,
      )
    | Sum(m) =>
      ConstructorMap.fold_vals(
        (acc, s) => TupleUtil.map2_bin(CodeSlice.union, full_slice(s), acc),
        (CodeSlice.empty, CodeSlice.empty),
        m,
      )
    | Atom(_)
    | Label(_)
    | Unknown(_)
    | Var(_) => (CodeSlice.empty, CodeSlice.empty)
    }
  )
  |> TupleUtil.(apply(map2(CodeSlice.union, code_slices_of(ty))));

let full_slice_combined =
  Fun.compose(TupleUtil.uncurry(CodeSlice.union), full_slice);

// Approximate slice size. Used to pick branches with smaller slices
let slice_size =
  Fun.compose(
    Fun.compose(TupleUtil.uncurry((+)), TupleUtil.map2(CodeSlice.size)),
    full_slice,
  );

let apply = (f, ty) => f(term_of(ty));

let map = (f, ty) => {
  let (term, rewrap) = unwrap(ty);
  term |> f |> rewrap;
};

// Applies a function to a inner type term and merges the top-level slice with the resulting typ
// Drops syn slices by default
let map_merge_t = (~drop_syn=true, f, ty) =>
  term_of(ty)
  |> f
  |> wrap_slices(
       drop_syn
         ? (CodeSlice.empty, ana_code_slice_of(ty)) : code_slices_of(ty),
     );
// Same, but retains original term id
let map_merge = (~drop_syn=true, f, ty) => {
  let (_, rewrap) = unwrap_to_slice(ty);
  term_of(ty)
  |> f
  |> rewrap
  |> wrap_slices(
       drop_syn
         ? (CodeSlice.empty, ana_code_slice_of(ty)) : code_slices_of(ty),
     );
};

let all_ids_temp = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) =>
      {
        term: exp.term,
        annotation: {
          ids: [Id.invalid],
        },
      }
      |> continue;
  map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f);
};

let (replace_temp, replace_temp_exp) = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) => IdTagged.replace_temp(exp) |> continue;
  (
    map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
    TermBase.Exp.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
  );
};

let hole = (tms: list(TermBase.Any.t)): TermBase.Typ.term =>
  switch (tms) {
  | [] => Unknown(Hole(EmptyHole))
  | [_, ..._] => Unknown(Hole(MultiHole(tms)))
  };

let cls_of_term: Grammar.typ_term('a) => cls =
  fun
  | Unknown(Hole(Invalid(_))) => Invalid
  | Unknown(Hole(EmptyHole)) => EmptyHole
  | Unknown(Hole(MultiHole(_))) => MultiHole
  | Unknown(SynSwitch) => SynSwitch
  | Unknown(Internal) => Internal
  | Atom(c) => Atom(c)
  | List(_) => List
  | Arrow(_) => Arrow
  | Var(_) => Var
  | Prod(_) => Prod
  | TupLabel(_) => TupLabel
  | Label(_) => Label
  | Parens(_) => Parens
  | Ap(_) => Ap
  | Sum(_) => Sum
  | Rec(_) => Rec
  | Forall(_) => Forall;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type"
  | MultiHole => "Broken type"
  | EmptyHole => "Empty type hole"
  | SynSwitch => "Synthetic type"
  | Internal => "Internal type"
  | Atom(_) => "Base type"
  | Var => "Type variable"
  | Constructor => "Sum constructor"
  | List => "List type"
  | Arrow => "Function type"
  | Prod => "Tuple type"
  | TupLabel => "Labeled tuple item type"
  | Label => "Label"
  | Sum => "Sum type"
  | Parens => "Parenthesized type"
  | Ap => "Constructor application"
  | Rec => "Recursive type"
  | Forall => "Forall type";

// Collects ids of variables in typ
let ids_of_var = (name, t) => {
  let ids = ref([]);
  let _ =
    map_term(
      ~f_typ=
        (cont, t) =>
          switch (term_of(t)) {
          | Var(x) when x == name =>
            ids := IdTagged.ids(t) @ ids^;
            t;
          | _ => cont(t)
          },
      t,
    );
  ids^;
};

let is_parens = (typ: t) => {
  switch (term_of(typ)) {
  | Parens(_) => true
  | Arrow(_)
  | Unknown(_)
  | Atom(_)
  | List(_)
  | Label(_)
  | Prod(_)
  | TupLabel(_)
  | Var(_)
  | Ap(_)
  | Sum(_)
  | Forall(_)
  | Rec(_) => false
  };
};

// ignore_parens=false can give a syntactic notion (e.g. for use in pattern matching).
let rec is_arrow = (~ignore_parens=true, typ: t) => {
  switch (term_of(typ)) {
  | Parens(typ)
  | TupLabel(_, typ) => ignore_parens ? is_arrow(typ) : false
  | Arrow(_) => true
  | Unknown(_)
  | Atom(_)
  | List(_)
  | Label(_)
  | Prod(_)
  | Var(_)
  | Ap(_)
  | Sum(_)
  | Forall(_)
  | Rec(_) => false
  };
};

[@ocaml.warning "-32"]
let rec is_unknown = (~ignore_parens=true, typ: t) => {
  switch (term_of(typ)) {
  | Parens(typ) => ignore_parens ? false : is_unknown(typ)
  | Unknown(_) => true
  | Arrow(_)
  | Atom(_)
  | List(_)
  | Prod(_)
  | Label(_)
  | TupLabel(_)
  | Var(_)
  | Ap(_)
  | Sum(_)
  | Forall(_)
  | Rec(_) => false
  };
};

let rec is_list = (~ignore_parens=true, typ: t) => {
  switch (term_of(typ)) {
  | Parens(typ) => ignore_parens ? false : is_list(typ)
  | List(_) => true
  | Unknown(_)
  | Atom(_)
  | Arrow(_)
  | Prod(_)
  | Label(_)
  | TupLabel(_)
  | Var(_)
  | Ap(_)
  | Sum(_)
  | Forall(_)
  | Rec(_) => false
  };
};

let rec is_forall = (~ignore_parens=true, typ: t) => {
  switch (term_of(typ)) {
  | Parens(typ)
  | TupLabel(_, typ) => ignore_parens ? false : is_forall(typ)
  | Forall(_) => true
  | Unknown(_)
  | Atom(_)
  | Arrow(_)
  | List(_)
  | Label(_)
  | Prod(_)
  | Var(_)
  | Ap(_)
  | Sum(_)
  | Rec(_) => false
  };
};

let is_void = (typ: t) =>
  switch (term_of(typ)) {
  | Sum(ctrs) => ConstructorMap.is_empty(ctrs)
  | Rec(_, {term: {typ: Sum(ctrs), _}, _}) =>
    ConstructorMap.is_empty(ctrs)
  | _ => false
  };

let rec is_sum = (~ignore_parens=true, typ: t) => {
  switch (term_of(typ)) {
  | Parens(typ)
  | TupLabel(_, typ) => ignore_parens ? false : is_sum(typ)
  | Sum(_) => true
  | Unknown(_)
  | Atom(_)
  | Arrow(_)
  | List(_)
  | Prod(_)
  | Label(_)
  | Var(_)
  | Ap(_)
  | Forall(_)
  | Rec(_) => false
  };
};

let rec is_tuplabel = (~ignore_parens=true, typ: t) => {
  switch (term_of(typ)) {
  | Parens(typ) => ignore_parens ? false : is_tuplabel(typ)
  | TupLabel(_) => true
  | Unknown(_)
  | Atom(_)
  | Arrow(_)
  | List(_)
  | Prod(_)
  | Label(_)
  | Sum(_)
  | Var(_)
  | Ap(_)
  | Forall(_)
  | Rec(_) => false
  };
};

let rec is_prod = (~ignore_parens=true, typ: t) => {
  switch (term_of(typ)) {
  | Parens(typ) => ignore_parens ? false : is_prod(typ)
  | Prod(_) => true
  | Unknown(_)
  | Atom(_)
  | Arrow(_)
  | List(_)
  | TupLabel(_)
  | Label(_)
  | Sum(_)
  | Var(_)
  | Ap(_)
  | Forall(_)
  | Rec(_) => false
  };
};

let rec is_label = (~ignore_parens=true, typ: t) => {
  switch (term_of(typ)) {
  | Parens(typ) => ignore_parens ? false : is_label(typ)
  | Label(_) => true
  | Unknown(_)
  | Atom(_)
  | Arrow(_)
  | List(_)
  | TupLabel(_)
  | Prod(_)
  | Sum(_)
  | Var(_)
  | Ap(_)
  | Forall(_)
  | Rec(_) => false
  };
};

// Destructuring constructs, retaining only ana slices by default
// Intended use: | ty when is_parens(ty) => let ty' = unparens(ty) in ...
let unparens = (~drop_syn=true) =>
  map_merge_t(
    ~drop_syn,
    fun
    | Parens(ty) => ty
    | _ => failwith("Not a parens"),
  );

let unparens = unparens;
let unlist = (~drop_syn=true) =>
  map_merge_t(
    ~drop_syn,
    fun
    | List(ty) => ty
    | _ => failwith("Not a list"),
  );
let unprod = (~drop_syn=true, ty: t) => {
  ty
  |> apply(
       fun
       | Prod(tys) =>
         tys
         |> List.map(
              wrap_slices(
                drop_syn
                  ? (CodeSlice.empty, ana_code_slice_of(ty))
                  : code_slices_of(ty),
              ),
            )
       | _ => failwith("Not a product"),
     );
};

let unarrow = (~drop_syn=true, ty: t) => {
  ty
  |> apply(
       fun
       | Arrow(ty1, ty2) =>
         (ty1, ty2)
         |> TupleUtil.map2(
              wrap_slices(
                drop_syn
                  ? (CodeSlice.empty, ana_code_slice_of(ty))
                  : code_slices_of(ty),
              ),
            )
       | _ => failwith("Not an arrow"),
     );
};

// get forall term
let unforall = (~drop_syn=true, ty: t) => {
  ty
  |> apply(
       fun
       | Forall(tpat, ty) => (
           tpat,
           ty
           |> wrap_slices(
                drop_syn
                  ? (CodeSlice.empty, ana_code_slice_of(ty))
                  : code_slices_of(ty),
              ),
         )
       | _ => failwith("Not a forall"),
     );
};

let unlabel = (ty: t) => {
  ty
  |> apply(
       fun
       | Label(name) => name
       | _ => failwith("Not a label"),
     );
};

let untuplabel = (~drop_syn=true, ty: t) => {
  ty
  |> apply(
       fun
       | TupLabel(label, ty) =>
         (label, ty)
         |> TupleUtil.map2(
              wrap_slices(
                drop_syn
                  ? (CodeSlice.empty, ana_code_slice_of(ty))
                  : code_slices_of(ty),
              ),
            )
       | _ => failwith("Not a tuplabel"),
     );
};

let get_sum = (~drop_syn=true, ty) =>
  ty
  |> apply(
       fun
       | Sum(m) =>
         m
         |> ConstructorMap.map_preserving(
              wrap_slices(
                drop_syn
                  ? (CodeSlice.empty, ana_code_slice_of(ty))
                  : code_slices_of(ty),
              ),
            )
       | _ => failwith("Not a sum"),
     );

/* Functions below this point assume that types have been through the to_typ function above */

[@deriving (show({with_path: false}), sexp, yojson)]
type source = {
  id: Id.t,
  ty: t,
};

/* Strip location information from a list of sources */
let of_source = List.map((source: source) => source.ty);

/* How type provenance information should be collated when
   joining unknown types. This probably requires more thought,
   but right now TypeHole strictly predominates over Internal
   which strictly predominates over SynSwitch. */
let join_type_provenance =
    (p1: TermBase.type_provenance, p2: TermBase.type_provenance)
    : TermBase.type_provenance =>
  switch (p1, p2) {
  | (Hole(h1), Hole(h2)) when h1 == h2 => Hole(h1)
  | (Hole(EmptyHole), Hole(EmptyHole) | SynSwitch)
  | (SynSwitch, Hole(EmptyHole)) => Hole(EmptyHole)
  | (SynSwitch, Internal)
  | (Internal, SynSwitch) => SynSwitch
  | (Internal | Hole(_), _)
  | (_, Hole(_)) => Internal
  | (SynSwitch, SynSwitch) => SynSwitch
  };

// TODO: What do do with slices here?
let rec match_tup_label = ty => {
  let ana_slice = ana_code_slice_of(ty);
  switch (term_of(ty)) {
  | Parens(ty) => match_tup_label(ty)
  | TupLabel(label, t') =>
    switch (term_of(label)) {
    | Label(name) => Some((name, t' |> wrap_ana_slice(ana_slice)))
    | _ => None
    }
  | _ => None
  };
};

let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
  switch (term_of(ty)) {
  | Unknown(_)
  | Atom(_)
  | Label(_) => []
  | Ap(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Var(v) => List.mem(v, bound) ? [] : [v]
  | Parens(ty) => free_vars(~bound, ty)
  | List(ty) => free_vars(~bound, ty)
  | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Sum(sm) => ConstructorMap.free_variables(free_vars(~bound), sm)
  | Prod(tys) => ListUtil.flat_map(free_vars(~bound), tys)
  | TupLabel(_, ty) => free_vars(~bound, ty)
  | Rec(x, ty)
  | Forall(x, ty) =>
    free_vars(~bound=(x |> TPat.tyvar_of_utpat |> Option.to_list) @ bound, ty)
  };

let var_count = ref(0);
let fresh_var = (var_name: string) => {
  let x = var_count^;
  var_count := x + 1;
  var_name ++ "_α" ++ string_of_int(x);
};

// TODO: Slice metatheory
let unroll = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec(tp, ty_body) =>
    subst(ty, tp, ty_body) |> wrap_slices(code_slices_of(ty))
  | _ => ty
  };

/* Type Equality: This coincides with alpha equivalence for normalized types.
   Other types may be equivalent but this will not detect so if they are not normalized. */
let equal = (t1: t, t2: t): bool => fast_equal(t1, t2);

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */

// Tracks if atomic type of either branch is used in the join type.
// Picks the branch with smallest (approx) slice when same typing info in both branches (and Right if equal)
// join used leaves from either: none, left branch, right branch, or both
// None here occurs only if both branches are Unknown.
// TODO: Have a version which retains slices more completely, producing valid terms (i.e. duplicating type constructor highlighting)
let rec join_using =
        (~resolve=false, ctx: Ctx.t, ty1: t, ty2: t): Joins.join(t, t) => {
  open Joins;
  let smallest_branch = slice_size(ty1) < slice_size(ty2) ? Left : Right;
  let c1 = code_slices_of(ty1);
  let c2 = code_slices_of(ty2);
  let add_slices = branch_used =>
    from_code_slices(choose_branch(c1, c2, branch_used));
  let join' = join_using(~resolve, ctx);
  switch (term_of(ty1), term_of(ty2)) {
  | (_, Parens(ty2)) => join'(ty1, ty2)
  | (Parens(ty1), _) => join'(ty1, ty2)
  | (Unknown(p1), Unknown(p2)) =>
    Join(Unknown(join_type_provenance(p1, p2)) |> temp_empty, None) // Don't slice holes. Note: In future do slice these
  | (Unknown(_), _) => Join(ty2, Right)
  | (_, Unknown(_)) => Join(ty1, Left)
  | (Var(n1), Var(n2)) =>
    if (n1 == n2) {
      Join(ty1, smallest_branch);
    } else {
      {
        let* ty1 = Ctx.lookup_alias(ctx, n1);
        let* ty2 = Ctx.lookup_alias(ctx, n2);
        Some(
          switch (join'(ty1, ty2)) {
          | Join(ty_join, branch_used) =>
            !resolve && equal(ty1, ty_join)
              ? Join(ty1, Left) : Join(ty_join, branch_used)
          | NoJoin(ts) => NoJoin([(ty1, ty2), ...ts])
          },
        );
      }
      |> (
        fun
        | Some(Join(t, b)) => Join(t, b)
        | Some(NoJoin(ts)) => NoJoin(ts)
        | None => NoJoin([(ty1, ty2)])
      );
    }
  | (Var(name), _) =>
    {
      let* ty_name = Ctx.lookup_alias(ctx, name);
      Some(
        switch (join'(ty_name, ty2)) {
        | Join(ty_join, branch_used) =>
          !resolve && equal(ty_name, ty_join)
            ? Join(ty1, Left) : Join(ty_join, branch_used)
        | NoJoin(ts) => NoJoin([(ty1, ty2), ...ts])
        },
      );
    }
    |> (
      fun
      | Some(Join(t, b)) => Join(t, b)
      | Some(NoJoin(ts)) => NoJoin(ts)
      | None => NoJoin([(ty1, ty2)])
    )
  | (_, Var(name)) =>
    {
      let* ty_name = Ctx.lookup_alias(ctx, name);
      Some(
        switch (join'(ty_name, ty1)) {
        | Join(ty_join, branch_used) =>
          !resolve && equal(ty_name, ty_join)
            ? Join(ty2, Right) : Join(ty_join, branch_used)
        | NoJoin(ts) => NoJoin([(ty1, ty2), ...ts])
        },
      );
    }
    |> (
      fun
      | Some(Join(t, b)) => Join(t, b)
      | Some(NoJoin(ts)) => NoJoin(ts)
      | None => NoJoin([(ty1, ty2)])
    )
  /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
  | (Rec(tp1, ty1), Rec(tp2, ty2)) =>
    let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
    let ty1' =
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(x2) =>
        subst(
          Var(x2)
          |> from_ana_slice(CodeSlice.of_ids([TPat.rep_id(tp2)]))
          |> temp,
          tp1,
          ty1,
        )
      | None => ty1
      };
    let. (ty_body, branch_used) = join_using(~resolve, ctx, ty1', ty2);
    Join(Rec(tp1, ty_body) |> add_slices(branch_used) |> temp, branch_used);
  | (Rec(_), _) => NoJoin([(ty1, ty2)])
  | (Forall(tp1, ty1), Forall(tp2, ty2)) =>
    let ty1' =
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(x2) =>
        subst(
          Var(x2)
          |> from_ana_slice(CodeSlice.of_ids([TPat.rep_id(tp2)]))
          |> temp,
          tp1,
          ty1,
        )
      | None => ty1
      };
    let ctx = Ctx.extend_dummy_tvar(ctx, tp2);
    let+ (ty_body, branch_used) = join_using(~resolve, ctx, ty1', ty2);
    (Forall(tp2, ty_body) |> add_slices(branch_used) |> temp, branch_used);
  /* Note for above: there is no danger of free variable capture as
     subst itself performs capture avoiding substitution. However this
     may generate internal type variable names that in corner cases can
     be exposed to the user. We preserve the variable name of the
     second type to preserve synthesized type variable names, which
     come from user annotations. */
  | (Forall(_), _) => NoJoin([(ty1, ty2)])
  | (Atom(a1), Atom(a2)) when a1 == a2 => Join(ty1, Right)
  | (Atom(_), _) => NoJoin([(ty1, ty2)])
  | (Label(_), Label("")) => Join(ty1, Left)
  | (Label(""), Label(_)) => Join(ty2, Right)
  | (Label(name1), Label(name2))
      when LabeledTuple.match_labels(name1, name2) =>
    Join(ty1, Right)
  | (Label(_), _) => NoJoin([(ty1, ty2)])
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let+ ty1 = join'(ty1, ty1')
    and+ ty2 = join'(ty2, ty2')
    and! branch_used = ();
    (Arrow(ty1, ty2) |> add_slices(branch_used) |> temp, branch_used);
  | (Arrow(_), _) => NoJoin([(ty1, ty2)])
  | (TupLabel(lab1, ty1'), TupLabel(lab2, ty2')) =>
    let+ lab = join'(lab1, lab2)
    and+ ty = join'(ty1', ty2')
    and! branch_used = ();
    (TupLabel(lab, ty) |> add_slices(branch_used) |> temp, branch_used);
  | (TupLabel(_), _) => NoJoin([(ty1, ty2)])
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      NoJoin
        ([(ty1, ty2)]); // TODO: Could only return the extra parts of tys1/tys2
    } else {
      let joins = List.map2(join', tys1, tys2);
      let joins =
        List.fold_right(
          (j, acc) =>
            switch (acc, j) {
            | (Ok((tys, branches_used)), Join(ty_join, branch_used)) =>
              Ok(([ty_join, ...tys], [branch_used, ...branches_used]))
            | (Ok(_), NoJoin(ts))
            | (Error(ts), Join(_)) => Error(ts)
            | (Error(ts_acc), NoJoin(ts)) => Error(ts_acc @ ts)
            },
          joins,
          Ok(([], [])),
        );
      switch (joins) {
      | Ok((tys, branches_used)) =>
        let branch_used =
          List.fold_left(combine_branches_used, None, branches_used);
        Join(Prod(tys) |> add_slices(branch_used) |> temp, branch_used);
      | Error(ts) => NoJoin(ts)
      };
    }
  | (Prod(_), _) => NoJoin([(ty1, ty2)])
  | (Sum(sm1), Sum(sm2)) =>
    switch (
      ConstructorMap.join(
        equal,
        (x, y) => join_using(~resolve, ctx, x, y),
        sm1,
        sm2,
      )
    ) {
    | Join(sm', branch_used) =>
      Join(Sum(sm') |> add_slices(branch_used) |> temp, branch_used)
    | NoJoin(sms) =>
      NoJoin(
        List.map(
          ((s1, s2)) => (Sum(s1) |> temp_empty, Sum(s2) |> temp_empty),
          sms,
        ) // TODO: Slices here?
      )
    }
  | (Sum(_), _) => NoJoin([(ty1, ty2)])
  | (List(ty1), List(ty2)) =>
    let+ (ty, branch_used) = join'(ty1, ty2);
    (List(ty) |> add_slices(branch_used) |> temp, branch_used);
  | (List(_), _) => NoJoin([(ty1, ty2)])
  | (Ap(_), _) => failwith("Type join of ap")
  };
};

let join = (~resolve=false, ctx: Ctx.t, ty1: t, ty2: t): option(t) =>
  join_using(~resolve, ctx, ty1, ty2)
  |> (
    fun
    | Join(t, _) => Some(t)
    | NoJoin(_) => None
  );

let join_inconsistency =
    (~resolve=false, ctx: Ctx.t, ty1: t, ty2: t): list((t, t)) =>
  join_using(~resolve, ctx, ty1, ty2)
  |> (
    fun
    | Join(_, _) => []
    | NoJoin(ts) => ts
  );

/* REQUIRES NORMALIZED TYPES
   Remove synswitches from t1 by matching against t2
   Left slices being used (except for synswitch) */
let rec match_synswitch = (t1: t, t2: t) => {
  let (term1, rewrap1) = unwrap(t1);
  switch (term1, term_of(t2)) {
  | (Parens(t1), _) => Parens(match_synswitch(t1, t2)) |> rewrap1
  | (Unknown(SynSwitch), _) => t2
  // These cases can't have a synswitch inside
  | (Unknown(_), _)
  | (Atom(_), _)
  | (Label(_), _)
  | (Var(_), _)
  | (Ap(_), _)
  | (Rec(_), _) => t1
  // These might
  | (List(ty1), List(ty2)) => List(match_synswitch(ty1, ty2)) |> rewrap1
  | (List(_), _) => t1
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    Arrow(match_synswitch(ty1, ty1'), match_synswitch(ty2, ty2')) |> rewrap1
  | (Arrow(_), _) => t1
  | (Prod(tys1), Prod(tys2)) when List.length(tys1) == List.length(tys2) =>
    let tys = List.map2(match_synswitch, tys1, tys2);
    Prod(tys) |> rewrap1;
  | (Prod(_), _) => t1
  | (TupLabel(label1, ty1), TupLabel(label2, ty2)) =>
    TupLabel(match_synswitch(label1, label2), match_synswitch(ty1, ty2))
    |> rewrap1
  | (TupLabel(_, _), _) => t1
  | (Sum(sm1), Sum(sm2)) =>
    let sm' =
      ConstructorMap.match_synswitch(match_synswitch, equal, sm1, sm2);
    Sum(sm') |> rewrap1;
  | (Sum(_), _) => t1
  // HACK[Matt]: The only possible forall is `Forall Syn -> Syn`
  | (Forall(_), Forall(_)) => t2
  | (Forall(_), _) => t1
  };
};

let join_all = (~empty: t, ctx: Ctx.t, ts: list(t)): option(t) =>
  List.fold_left(
    (acc, ty) => OptUtil.and_then(join(ctx, ty), acc),
    Some(empty),
    ts,
  );

let join_inconsistency_all = (~empty: t, ctx, ts) =>
  List.fold_left(
    fun
    | Ok(acc) => (
        t =>
          switch (join_using(ctx, acc, t)) {
          | Join(acc', _) => Ok(acc')
          | NoJoin(ts) => Error(ts)
          }
      )
    | Error(ts) => (_ => Error(ts)),
    Ok(empty),
    ts,
  )
  |> (
    fun
    | Ok(_) => []
    | Error(ts) => ts
  );

let is_consistent = (ctx: Ctx.t, ty1: t, ty2: t): bool =>
  join(ctx, ty1, ty2) != None;

let rec weak_head_normalize = (~rec_counter=0, ctx: Ctx.t, ty: t): t => {
  if (rec_counter > 1000) {
    failwith("weak_head_normalize exceeded 1000 recursive calls");
  };
  switch (term_of(ty)) {
  | Parens(t) => weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t)
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty)
    | None => ty
    }
  | _ => ty
  };
};

let rec normalize = (~rec_counter=0, ctx: Ctx.t, ty: t): t => {
  if (rec_counter > 1000) {
    failwith("normalize exceeded 1000 recursive calls");
  };
  let normalize = normalize(~rec_counter=rec_counter + 1);
  let (term, rewrap) = unwrap(ty);
  switch (term) {
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => normalize(ctx, ty)
    | None => ty
    }
  | Unknown(_)
  | Atom(_)
  | Label(_) => ty
  | Parens(t) => normalize(ctx, t)
  | List(t) => List(normalize(ctx, t)) |> rewrap
  | Ap(t1, t2) => Ap(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Arrow(t1, t2) =>
    Arrow(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(normalize(ctx), ts)) |> rewrap
  | TupLabel(label, ty) =>
    TupLabel(normalize(ctx, label), normalize(ctx, ty)) |> rewrap
  | Sum(ts) =>
    Sum(ConstructorMap.map(Option.map(normalize(ctx)), ts)) |> rewrap
  | Rec(tpat, ty) =>
    /* NOTE: Dummy tvar added has fake id but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec(tpat, normalize(Ctx.extend_dummy_tvar(ctx, tpat), ty)) |> rewrap
  | Forall(name, ty) =>
    Forall(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty)) |> rewrap
  };
};

// ids are used to assigning part of an ana slice. i.e. moving Mode.re slicing logic into Typ.re
// Use the ids that 'explain' why an arrow was required
let rec matched_arrow_strict = (~ids=[], ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_arrow_strict(ctx, ty)
  | Arrow(ty_in, ty_out) =>
    Some((
      ty_in |> wrap_ana_slice(ana_code_slice_of(ty_in)),
      ty_out |> wrap_ana_slice(ana_code_slice_of(ty_out)),
    ))
  | Unknown(SynSwitch) =>
    Some((
      Unknown(SynSwitch) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
      Unknown(SynSwitch) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
    ))
  | _ => None
  };

let matched_arrow = (~ids=[], ctx, ty) =>
  matched_arrow_strict(~ids, ctx, ty)
  |> Option.value(
       ~default=(
         Unknown(Internal) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
         Unknown(Internal) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
       ),
     );

let rec matched_forall_strict = (~ids=[], ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_forall_strict(ctx, ty)
  | Forall(t, ty) =>
    Some((Some(t), ty |> wrap_ana_slice(ana_code_slice_of(ty))))
  | Unknown(SynSwitch) =>
    Some((
      None,
      Unknown(SynSwitch) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
    ))
  | _ => None
  };

let matched_forall = (~ids=[], ctx, ty) =>
  matched_forall_strict(~ids, ctx, ty)
  |> Option.value(
       ~default=(
         Option.None,
         Unknown(Internal) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
       ),
     );

let rec get_labels = (ctx, ty): list(option(string)) => {
  let ty = weak_head_normalize(ctx, ty);
  switch (term_of(ty)) {
  | Parens(ty) => get_labels(ctx, ty)
  | Prod(tys) => List.map(x => Option.map(fst, match_tup_label(x)), tys)
  | _ => []
  };
};

let rec matched_prod_strict =
        (~ids=[], ctx: Ctx.t, es, get_label_es, ty: t, constructor) => {
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_prod_strict(ctx, es, get_label_es, ty, constructor)
  | Prod(tys: list(t)) =>
    if (List.length(es) != List.length(tys)) {
      (es, None);
    } else {
      (
        LabeledTuple.rearrange(
          match_tup_label,
          get_label_es,
          tys |> List.map(wrap_ana_slice(ana_code_slice_of(ty))),
          es,
          constructor,
        ),
        Some(tys),
      );
    }
  | Unknown(SynSwitch) => (
      es,
      Some(
        List.init(List.length(es), _ =>
          Unknown(SynSwitch)
          |> from_ana_slice(CodeSlice.of_ids(ids))
          |> temp
        ),
      ),
    )
  | _ => (es, None)
  };
};

let matched_prod = (~ids=[], ctx, es, get_label_es, ty, constructor) => {
  let (es, tys_opt) =
    matched_prod_strict(~ids, ctx, es, get_label_es, ty, constructor);
  (
    es,
    tys_opt
    |> Option.value(
         ~default=
           List.init(List.length(es), _ =>
             Unknown(Internal)
             |> from_ana_slice(CodeSlice.of_ids(ids))
             |> temp
           ),
       ),
  );
};

let rec matched_list_strict = (~ids=[], ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_list_strict(ctx, ty)
  | List(ty) => Some(ty |> wrap_ana_slice(ana_code_slice_of(ty)))
  | Unknown(SynSwitch) =>
    Some(
      Unknown(SynSwitch) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
    )
  | _ => None
  };

let matched_list = (~ids=[], ctx, ty) =>
  matched_list_strict(~ids, ctx, ty)
  |> Option.value(
       ~default=
         Unknown(Internal) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
     );

let rec matched_args_strict = (~ids=[], ctx, ty, arity): Either.t('a, int) => {
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_args_strict(ctx, ty, arity)
  | Prod(tys) when List.length(tys) == arity =>
    L(tys |> List.map(wrap_ana_slice(ana_code_slice_of(ty))))
  | Prod(tys) => R(List.length(tys))
  | _ when arity == 1 => L([ty])
  | Unknown((SynSwitch | Internal) as p) =>
    L(
      List.init(arity, _ =>
        Unknown(p) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp
      ),
    )
  | _ => R(1)
  };
};

let matched_args = (~ids=[], ctx, ty, arity) =>
  switch (matched_args_strict(~ids, ctx, ty, arity)) {
  | L(tys) => tys
  | R(_) =>
    List.init(arity, _ =>
      Unknown(Internal) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp
    )
  };

let matched_label = (~ids=[], ctx, ty): option((t, t)) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | TupLabel({term: {typ: Label(ml), _}, _}, ty') =>
    Some((
      Label(ml) |> from_ana_slice(ana_code_slice_of(ty)) |> temp,
      ty' |> wrap_ana_slice(ana_code_slice_of(ty)),
    ))
  | Unknown(SynSwitch) =>
    Some((
      Unknown(SynSwitch) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
      Unknown(SynSwitch) |> from_ana_slice(CodeSlice.of_ids(ids)) |> temp,
    ))
  | _ => None
  };

// TODO: Slicing here?
let rec get_sum_constructors = (ctx: Ctx.t, ty: t): option(sum_map) => {
  let ty = weak_head_normalize(ctx, ty);
  switch (term_of(ty)) {
  | Parens(ty) => get_sum_constructors(ctx, ty)
  | Sum(sm) => Some(sm)
  | Rec(_) =>
    /* Note: We must unroll here to get right ctr types;
       otherwise the rec parameter will leak. However, seeing
       as substitution is too expensive to be used here, we
       currently making the optimization that, since all
       recursive types are type alises which use the alias name
       as the recursive parameter, and type aliases cannot be
       shadowed, it is safe to simply remove the Rec constructor,
       provided we haven't escaped the context in which the alias
       is bound. If either of the above assumptions become invalid,
       the below code will be incorrect! */
    let ty =
      switch (ty |> term_of) {
      | Rec({term: Var(x), _}, _ty_body) =>
        switch (Ctx.lookup_alias(ctx, x)) {
        | None => unroll(ty)
        | Some(_) => unroll(ty)
        }
      | _ => ty
      };
    switch (ty |> term_of) {
    | Sum(sm) => Some(sm)
    | _ => None
    };
  | _ => None
  };
};

let rec is_unknown = (~ignore_parens=true, ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => ignore_parens ? is_unknown(x) : false
  | Unknown(_) => true
  | _ => false
  };

let rec is_syn = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_syn(x)
  | Unknown(SynSwitch) => true
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | Var(_)
  | Ap(_)
  | Rec(_)
  | Forall(_)
  | List(_)
  | Arrow(_)
  | Prod(_)
  | Sum(_) => false
  };

let rec is_ana_atom = (ty: t) =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_ana_atom(x)
  | Atom(a) => Some(a)
  | Unknown(_)
  | Label(_)
  | Var(_)
  | Ap(_)
  | Rec(_)
  | Forall(_)
  | List(_)
  | Arrow(_)
  | Prod(_)
  | Sum(_) => None
  };

let rec is_syn_fun = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_syn_fun(x)
  | Arrow(t1, t2) => is_syn(t1) && is_syn_fun(t2)
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | Var(_)
  | Ap(_)
  | Rec(_)
  | Forall(_)
  | List(_)
  | Prod(_)
  | Sum(_) => false
  };

let rec is_syn_plus = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_syn_plus(x)
  | Unknown(SynSwitch) => true
  | Arrow(t1, t2) => is_syn(t1) && is_syn_plus(t2)
  | Forall(_, t) => is_syn(t)
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | Var(_)
  | Ap(_)
  | Rec(_)
  | List(_)
  | Prod(_)
  | Sum(_) => false
  };

/* Does the type require parentheses when on the left of an arrow for printing? */
let rec needs_parens = (ty: t): bool =>
  switch (term_of(ty)) {
  | Parens(ty) => needs_parens(ty)
  | Ap(_)
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | TupLabel(_, _)
  | List(_) /* is already wrapped in [] */
  | Var(_) => false
  | Rec(_, _)
  | Forall(_, _)
  | Arrow(_, _)
  | Prod(_)
  | Sum(_) => true /* disambiguate between (A + B) -> C and A + (B -> C) */
  };

let pretty_print_tvar = (tv: TPat.t): string =>
  switch (IdTagged.term_of(tv)) {
  | Var(x) => x
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => "?"
  };

/* Essentially recreates haz3lweb/view/Type.re's view_ty but with string output */
let rec pretty_print = (ty: t): string =>
  switch (term_of(ty)) {
  | Parens(ty) => pretty_print(ty)
  | Ap(_)
  | Unknown(_) => "?"
  | Atom(Int) => "Int"
  | Atom(Float) => "Float"
  | Atom(Bool) => "Bool"
  | Atom(String) => "String"
  | Atom(Nat) => "Nat"
  | Atom(SInt) => "SInt"
  | Var(tvar) => tvar
  | List(t) => "[" ++ pretty_print(t) ++ "]"
  | Arrow(t1, t2) => paren_pretty_print(t1) ++ " -> " ++ pretty_print(t2)
  | Sum(sm) =>
    switch (sm) {
    | [] => "+?"
    | [t0] => "+" ++ ctr_pretty_print(t0)
    | [t0, ...ts] =>
      List.fold_left(
        (acc, t) => acc ++ " + " ++ ctr_pretty_print(t),
        ctr_pretty_print(t0),
        ts,
      )
    }
  | Prod([]) => "()"
  | Prod([t0, ...ts]) =>
    "("
    ++ List.fold_left(
         (acc, t) => acc ++ ", " ++ pretty_print(t),
         pretty_print(t0),
         ts,
       )
    ++ ")"
  | Label(name) => name
  | TupLabel(label, t) => pretty_print(label) ++ "=" ++ pretty_print(t)
  | Rec(tv, t) =>
    "rec " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  | Forall(tv, t) =>
    "forall " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  }
and ctr_pretty_print =
  fun
  | ConstructorMap.Variant(ctr, _, None) => ctr
  | ConstructorMap.Variant(ctr, _, Some(t)) =>
    ctr ++ "(" ++ pretty_print(t) ++ ")"
  | ConstructorMap.BadEntry(_) => "?"
and paren_pretty_print = typ =>
  if (needs_parens(typ)) {
    "(" ++ pretty_print(typ) ++ ")";
  } else {
    pretty_print(typ);
  };

/**
 * Removes duplicate labels from a given list of types inside a tuple.
 *
 * This function takes a list of types and returns a new list with all
 * duplicate labels replaced with their first occurence and the unknown type.
 *
 * @param duplicate_labels - The list of duplicate labels.
 * @param tys - The list of types to remove duplicates from.
 * @return A new list of types with duplicates removed.
 */
let remove_duplicate_labels =
    (~duplicate_labels: list(LabeledTuple.label), tys: list(t)): list(t) => {
  snd(
    List.fold_left(
      ((seen_duplicates, deduplicated_types), ty) => {
        let tup_label = match_tup_label(ty);
        switch (tup_label) {
        | Some((l, _))
            when
              List.mem(l, duplicate_labels) && List.mem(l, seen_duplicates) => (
            seen_duplicates,
            deduplicated_types,
          )
        | Some((l, _)) when List.mem(l, duplicate_labels) => (
            [l] @ seen_duplicates,
            deduplicated_types
            @ [
              TupLabel(
                Label(l) |> temp_empty,
                Unknown(Internal) |> temp_empty,
              )
              |> temp_empty,
            ],
          )
        | Some(_) => (seen_duplicates, deduplicated_types @ [ty])
        | None => (seen_duplicates, deduplicated_types @ [ty])
        };
      },
      ([], []),
      tys,
    ),
  );
};
