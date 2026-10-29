open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Atom(Atom.cls)
  | DrvQuoteTy
  | Invalid
  | EmptyHole
  | MultiHole
  | SynSwitch
  | Internal
  | Arrow
  | Prod
  | TupLabel
  | Label
  | ExplicitNonlabel
  | Sum
  | List
  | Var
  | Constructor // Constructor does not exist on Typ.term it's being used here as a hack for the cursors inspector
  | Parens
  | Projector
  | Rec
  | Poly
  | ProofOf
  | ProdProjection
  | ProdExtension
  | Sig;

include TermBase.Typ;

let term_of: t => term = IdTagged.term_of;
let unwrap: t => (term, term => t) = IdTagged.unwrap;
let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;
let fresh_atom: Atom.cls => t = cls => fresh(Atom(cls));
/* fresh assigns a random id, whereas temp assigns Id.invalid, which
   is a lot faster, and since we so often make types and throw them away
   shortly after, it makes sense to use it. */
let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

let all_ids_temp = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) =>
      {
        term: exp.term,
        annotation: IdTagged.IdTag.temp(),
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
  | DrvQuoteTy(_) => DrvQuoteTy
  | List(_) => List
  | Arrow(_) => Arrow
  | Var(_) => Var
  | Prod(_) => Prod
  | TupLabel(_) => TupLabel
  | Label(_) => Label
  | ExplicitNonlabel => ExplicitNonlabel
  | Parens(_) => Parens
  | Projector(_) => Projector
  | Sum(_) => Sum
  | Rec(_) => Rec
  | Poly(_) => Poly
  | ProofOf(_) => ProofOf
  | ProdProjection(_) => ProdProjection
  | ProdExtension(_) => ProdExtension
  | Sig(_) => Sig;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type"
  | MultiHole => "Broken type"
  | EmptyHole => "Type hole"
  | SynSwitch => "Synthetic type"
  | Internal => "Internal type"
  | Atom(_) => "Base type"
  | DrvQuoteTy => "Derivation-Mode Quotation Type"
  | Var => "Type variable"
  | Constructor => "Sum constructor"
  | List => "List type"
  | Arrow => "Function type"
  | Prod => "Tuple type"
  | TupLabel => "Tuple item type"
  | Label => "Label"
  | ExplicitNonlabel => "Explicitly unlabeled tuple item type"
  | Sum => "Sum type"
  | Parens => "Parenthesized type"
  | Projector => "Projector type"
  | Rec => "Recursive type"
  | Poly => "Type quantifier"
  | ProofOf => "Proof type"
  | ProdProjection => "Tuple projection"
  | ProdExtension => "Tuple extension"
  | Sig => "Signature type";

let rec is_arrow = (typ: t) => {
  switch (typ.term) {
  | Parens(typ)
  | Projector(_, typ)
  | TupLabel(_, typ) => is_arrow(typ)
  | Arrow(_) => true
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | List(_)
  | Label(_)
  | ExplicitNonlabel
  | Prod(_)
  | Var(_)
  | Sum(_)
  | Poly(_)
  | ProofOf(_)
  | Rec(_)
  | ProdProjection(_)
  | ProdExtension(_)
  | Sig(_) => false
  };
};

let rec has_fun = (typ: t) =>
  switch (typ.term) {
  | Parens(typ)
  | Projector(_, typ)
  | TupLabel(_, typ)
  | ProdProjection(typ, _) => has_fun(typ)
  | Arrow(_)
  | Poly(_)
  | ProofOf(_) => true
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Label(_)
  | ExplicitNonlabel
  | Var(_) => false
  | List(t) => has_fun(t)
  | Rec(_, t) => has_fun(t)
  | Sum(sm) =>
    List.exists(
      fun
      | ConstructorMap.Variant(_, _, Some(t)) => has_fun(t)
      | _ => false,
      sm,
    )
  | Prod(tys) => List.exists(has_fun, tys)
  | ProdExtension(t1, t2) => has_fun(t1) || has_fun(t2)
  | Sig(s) =>
    List.exists(
      (m: Sig.member) =>
        switch (m) {
        | Val(_, ty) => has_fun(ty)
        | TypeManifest(_) => false
        },
      Sig.closed_members(s),
    )
  };

let is_void = (typ: t) =>
  switch (typ.term) {
  | Sum(ctrs) => ConstructorMap.is_empty(ctrs)
  | Rec(_, {term: Sum(ctrs), _}) => ConstructorMap.is_empty(ctrs)
  | _ => false
  };

/* Functions below this point assume that types have been through the to_typ function above */

[@deriving (show({with_path: false}), sexp, yojson)]
type source = {
  id: Id.t,
  ty: t,
};

let add_source =
  List.map2((id, ty) =>
    {
      id,
      ty,
    }
  );

/* Strip location information from a list of sources */
let of_source = List.map((source: source) => source.ty);

/* How type provenance information should be collated when
   meeting unknown types. This probably requires more thought,
   but right now TypeHole strictly predominates over Internal
   which strictly predominates over SynSwitch. */
let meet_type_provenance =
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

let rec match_tup_optional_label = (ty: t) =>
  switch (term_of(ty)) {
  | Parens(ty) => match_tup_optional_label(ty)
  | TupLabel({term: Label(name), _}, t') => Some((Some(name), t'))
  | TupLabel({term: Unknown(_), _}, t') => Some((None, t'))
  | Unknown(_) => Some((None, ty))
  | _ => None
  };
let match_tup_label = ty => {
  switch (match_tup_optional_label(ty)) {
  | Some((Some(name), t')) => Some((name, t'))
  | _ => None
  };
};

/* What a substituted name is bound as: a type variable, replaced wherever it
   occurs, or a module, replaced only where it roots a path (`m.T`). */
type binder = [
  | `TVar
  | `Var
];

/* The free variables of [ty] that substituting for a [binder] would
   replace: every one for a type variable, only path roots for a module.
   A closed signature's members bind nothing: see [close]. */
let rec free_vars =
        (~binder: binder=`TVar, ~bound=[], ~bound_roots=[], ty: t)
        : list(Var.t) => {
  let go = free_vars(~binder, ~bound, ~bound_roots);
  switch (term_of(ty)) {
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Label(_)
  | ExplicitNonlabel => []
  | Var(v) =>
    switch (binder) {
    | `TVar when !List.mem(v, bound) => [v]
    | `TVar
    | `Var => []
    }
  | Parens(ty)
  | Projector(_, ty) => go(ty)
  | List(ty) => go(ty)
  | ProdExtension(t1, t2)
  | Arrow(t1, t2) => go(t1) @ go(t2)
  | Sum(sm) => ConstructorMap.free_variables(go, sm)
  | Prod(tys) => List.concat_map(go, tys)
  | ProdProjection(t1, _) =>
    switch (term_of(t1)) {
    | Var(v) => List.mem(v, bound) || List.mem(v, bound_roots) ? [] : [v]
    | _ => go(t1)
    }
  | TupLabel(_, ty) => go(ty)
  | Rec(x, ty)
  | Poly(x, ty) =>
    free_vars(
      ~binder,
      ~bound=(x |> TPat.tyvar_of_utpat |> Option.to_list) @ bound,
      ~bound_roots,
      ty,
    )
  | ProofOf(_) => []
  | Sig(Closed(items)) =>
    List.concat_map(
      (m: Sig.member) =>
        switch (m) {
        | Val(_, ty)
        | TypeManifest(_, ty) => go(ty)
        },
      Sig.members(ClosedSig.items(items)),
    )
  /* Written items scope sequentially: a later item may mention an earlier
     type member or root a path at an earlier value member, and a type
     member's name is in scope in its own definition. */
  | Sig(Open(items)) =>
    let (_, _, vs) =
      List.fold_left(
        ((bound, bound_roots, acc), item: Sig.t) => {
          let member = Sig.member_of_item(item);
          let bound =
            switch (member) {
            | Some(TypeManifest(name, _)) => [name, ...bound]
            | Some(Val(_))
            | None => bound
            };
          let vs =
            switch (Sig.typ_of_item(item)) {
            | Some(ty) => free_vars(~binder, ~bound, ~bound_roots, ty)
            | None => []
            };
          let bound_roots =
            switch (member) {
            | Some(Val(name, _)) => [name, ...bound_roots]
            | Some(TypeManifest(_))
            | None => bound_roots
            };
          (bound, bound_roots, acc @ vs);
        },
        (bound, bound_roots, []),
        items,
      );
    vs;
  };
};

let var_count = ref(0);
let fresh_var = (var_name: string) => {
  let x = var_count^;
  var_count := x + 1;
  var_name ++ "_α" ++ string_of_int(x);
};

/* Number of Unknown constructors in type AST */
let rec count_unknowns = (ty: t): int =>
  switch (ty.term) {
  | Unknown(_) => 1
  | Atom(_)
  | DrvQuoteTy(_)
  | Var(_) => 0
  | Arrow(t1, t2) => count_unknowns(t1) + count_unknowns(t2)
  | Prod(tys) =>
    List.fold_left((acc, ty) => acc + count_unknowns(ty), 0, tys)
  | Sum(sm) =>
    List.fold_left(
      (acc, variant) =>
        switch (variant) {
        | ConstructorMap.BadEntry(_) => acc
        | Variant(_, _, ty) =>
          acc + Util.OptUtil.get(() => 0, Option.map(count_unknowns, ty))
        },
      0,
      sm,
    )
  | Rec(_, ty) => count_unknowns(ty)
  | List(ty) => count_unknowns(ty)
  | Parens(ty)
  | Projector(_, ty) => count_unknowns(ty)
  | Poly(_, ty) => count_unknowns(ty)
  | ProofOf(_) => 0
  | ExplicitNonlabel
  | Label(_) => 0
  | TupLabel(_, ty) => count_unknowns(ty)
  | ProdProjection(ty1, _) => count_unknowns(ty1)
  | ProdExtension(ty1, ty2) => count_unknowns(ty1) + count_unknowns(ty2)
  | Sig(s) =>
    List.fold_left(
      (acc, m: Sig.member) =>
        switch (m) {
        | Val(_, ty)
        | TypeManifest(_, ty) => acc + count_unknowns(ty)
        },
      0,
      Sig.closed_members(s),
    )
  };

let contains_unknown = (ty: t): bool => count_unknowns(ty) > 0;

/* Capture-avoiding substitution of `s` for `x` in `ty`.

   When recursing under a type binder `Poly(tp2, body)` or `Rec(tp2, body)`
   whose name occurs free in `s`, naive substitution would let occurrences of
   that name introduced by substituting `s` be captured by the binder. To
   avoid this, we alpha-rename the clashing binder to a fresh name (via
   `fresh_var`) before recursing. The inner subst used for renaming is itself
   capture-avoiding, so repeated collisions are handled naturally.

   A signature's members bind nothing (see [close]), so its items are
   substituted into independently. */
let rec subst = (~binder: binder=`TVar, s: t, x: TPat.t, ty: t): t =>
  switch (TPat.tyvar_of_utpat(x)) {
  | None => ty
  | Some(str) =>
    let avoid_capture = (s: t, tp2: TPat.t, body: t): (TPat.t, t) =>
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(name) when List.mem(name, free_vars(s)) =>
        let fresh = fresh_var(name);
        let tp2': TPat.t = Var(fresh) |> TPat.fresh;
        (tp2', subst(Var(fresh) |> temp, tp2, body));
      | _ => (tp2, body)
      };
    let rec go = (ty: t): t => {
      let (term, rewrap) = unwrap(ty);
      switch (term) {
      | Atom(_)
      | Label(_)
      | ExplicitNonlabel
      | Unknown(_)
      | ProofOf(_)
      | DrvQuoteTy(_) => ty
      | Arrow(ty1, ty2) => Arrow(go(ty1), go(ty2)) |> rewrap
      | Prod(tys) => Prod(List.map(go, tys)) |> rewrap
      | TupLabel(label, ty) => TupLabel(label, go(ty)) |> rewrap
      | Sum(sm) => Sum(ConstructorMap.map(Option.map(go), sm)) |> rewrap
      | Poly(tp2, _)
      | Rec(tp2, _)
          when
            Option.equal(
              String.equal,
              TPat.tyvar_of_utpat(x),
              TPat.tyvar_of_utpat(tp2),
            ) => ty
      | Poly(tp2, ty) =>
        let (tp2', ty') = avoid_capture(s, tp2, ty);
        Poly(tp2', go(ty')) |> rewrap;
      | Rec(tp2, ty) =>
        let (tp2', ty') = avoid_capture(s, tp2, ty);
        Rec(tp2', go(ty')) |> rewrap;
      | List(ty) => List(go(ty)) |> rewrap
      | Var(y) =>
        switch (binder) {
        | `TVar when String.equal(str, y) => s
        | `TVar
        | `Var => ty
        }
      | Parens(ty) => Parens(go(ty)) |> rewrap
      | Projector(data, ty) => Projector(data, go(ty)) |> rewrap
      | ProdProjection(t1, t2) =>
        let t1 =
          switch (binder, term_of(t1)) {
          | (`Var, Var(y)) when String.equal(str, y) => s
          | _ => go(t1)
          };
        ProdProjection(t1, go(t2)) |> rewrap;
      | ProdExtension(t1, t2) => ProdExtension(go(t1), go(t2)) |> rewrap
      /* Closed members are independent, so a substitution reaches each. */
      | Sig(s) =>
        Sig(Closed(ClosedSig.map(Sig.map_typ(go), Sig.closed(s))))
        |> rewrap
      };
    };
    go(ty);
  };

let unroll = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec(tp, ty_body) =>
    switch (TPat.tyvar_of_utpat(tp)) {
    | None => ty_body
    | Some(_) => subst(ty, tp, ty_body)
    }
  | _ => ty
  };

/* Unroll a Rec type until its head is not a Rec. Returns None on self-loop
   types like `rec x -> x` where unrolling cannot make progress. Normalizes
   the body first so that vacuous inner Recs (e.g. `rec x -> (rec ? -> x)`)
   are recognized as the equivalent self-loop. See hazelgrove/hazel#2235,
   #1624. */
let rec unroll_to_non_rec = (ty: t): option(t) =>
  switch (term_of(ty)) {
  | Rec(tp, body) =>
    switch (unroll_to_non_rec(body)) {
    | None => None
    | Some(body') =>
      switch (TPat.tyvar_of_utpat(tp), term_of(body')) {
      | (Some(w), Var(v)) when v == w => None
      | _ =>
        let (_, rewrap) = Annotated.unwrap(ty);
        unroll_to_non_rec(unroll(Grammar.Rec(tp, body') |> rewrap));
      }
    }
  | _ => Some(ty)
  };

/* Substitute each [(binder, name, def)] for [name] where it is free in
   [ty], latest binder first. */
let apply_sig_subst = (sigma: list((binder, Var.t, t)), ty: t): t => {
  let fv = free_vars(ty);
  List.fold_left(
    (ty, (binder, name, def)) =>
      List.exists(String.equal(name), fv)
        ? subst(~binder, def, Var(name) |> TPat.fresh, ty) : ty,
    ty,
    sigma,
  );
};

/* Whether two signatures' members have the same value names and the same
   type names, in any order. */
let same_member_names = (a: list(Sig.member), b: list(Sig.member)): bool => {
  let same = (a, b) =>
    List.equal(
      String.equal,
      List.sort(String.compare, a),
      List.sort(String.compare, b),
    );
  same(Sig.value_names(a), Sig.value_names(b))
  && same(Sig.type_names(a), Sig.type_names(b));
};

/* Type Equality: This coincides with alpha equivalence for normalized types.
   Other types may be equivalent but this will not detect so if they are not normalized. */
let fast_equal = Equality.semantic.typ;
let equal = (t1: t, t2: t): bool => Equality.syntactic.typ(t1, t2);

let project_type = (tys: list(t), label: string): option(t) =>
  switch (LabeledTuple.find_label(match_tup_label, tys, label)) {
  | Some({term: TupLabel(_, ty), _}) => Some(ty)
  | _ => None
  };

let product_extension = (tys1: list(t), tys2: list(t)): term => {
  let get_lv = (t: t) => {
    switch (match_tup_label(t)) {
    | Some((l, t)) => (Some(l), t)
    | None => (None, t)
    };
  };

  let new_tys =
    LabeledTuple.extension(List.map(get_lv, tys1), List.map(get_lv, tys2))
    |> List.map(((l, ty)) =>
         switch (l) {
         | Some(l) => TupLabel(fresh(Label(l)), ty) |> temp
         | None => ty
         }
       );
  Prod(new_tys);
};

/**
 * Removes duplicate labels from a given list of types inside a tuple.
 *
 * For each label in the list of duplicate labels, keeps only the first occurrence,
 * replacing its type with Unknown(Internal), and removes all subsequent occurrences.
 *
 * @param duplicate_labels - The list of duplicate labels.
 * @param tys - The list of types to remove duplicates from.
 * @return A new list of types where, for each duplicate label, only the first occurrence
 *         is kept (with type Unknown(Internal)), and all subsequent occurrences are removed.
 */
let remove_duplicate_labels =
    (~duplicate_labels: list(LabeledTuple.label), tys: list(t)): list(t) => {
  let (_, rev_deduplicated) =
    List.fold_left(
      ((seen_duplicates, rev_deduplicated_types), ty) => {
        let tup_label = match_tup_label(ty);
        switch (tup_label) {
        | Some((l, _))
            when
              List.mem(l, duplicate_labels) && List.mem(l, seen_duplicates) => (
            seen_duplicates,
            rev_deduplicated_types,
          )
        | Some((l, _)) when List.mem(l, duplicate_labels) => (
            [l, ...seen_duplicates],
            [
              TupLabel(Label(l) |> temp, Unknown(Internal) |> temp) |> temp,
              ...rev_deduplicated_types,
            ],
          )
        | Some(_) => (seen_duplicates, [ty, ...rev_deduplicated_types])
        | None => (seen_duplicates, [ty, ...rev_deduplicated_types])
        };
      },
      ([], []),
      tys,
    );
  List.rev(rev_deduplicated);
};

/* Fuel bounds the walk, so a path with no weak head normal form, like `m.T`
   in `let m : { type T = m.T }` with an outer `m`, normalizes to `?` (the
   path never resolves) instead of crashing. */
let rec weak_head_normalize = (~rec_counter=0, ctx: Ctx.t, ty: t): t =>
  if (rec_counter > 1000) {
    ty;
  } else {
    switch (term_of(ty)) {
    | Parens(t)
    | Projector(_, t) =>
      weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t)
    | Var(x) =>
      switch (Ctx.lookup_alias(ctx, x)) {
      | Some(ty) => weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty)
      | None => ty
      }
    | TupLabel({term: ExplicitNonlabel, _}, ty) =>
      weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty)
    | ProdProjection(t, label) =>
      let (_, rewrap) = unwrap(ty);
      let default = Unknown(Internal) |> rewrap;
      switch (label.term, path_head(~rec_counter=rec_counter + 1, ctx, t)) {
      | (Label(l), Some({term: Sig(s), _})) =>
        /* `M.T`: type member of a module path or of a signature alias. */
        switch (Sig.find_type_def(Sig.closed_members(s), l)) {
        | Some(ty') =>
          weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty')
        | None => default
        }
      | (Label(l), Some({term: Prod(tys), _})) =>
        /* `P.x`: label of a labeled tuple type. */
        project_type(tys, l) |> Option.value(~default)
      | _ => default // It would be better to do this via a more direct error recovery mechanism in statics
      };
    | Prod(ts) =>
      let (_, rewrap) = unwrap(ty);
      let duplicate_labels =
        LabeledTuple.get_duplicate_labels(match_tup_label, ts);
      if (List.is_empty(duplicate_labels)) {
        ty;
      } else {
        let cleaned_ts = remove_duplicate_labels(~duplicate_labels, ts);
        Prod(cleaned_ts) |> rewrap;
      };
    | ProdExtension(t1, t2) =>
      let (_, rewrap) = unwrap(ty);

      let t1 = weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t1);
      let t2 = weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t2);
      switch (t1.term, t2.term) {
      | (Prod(tys1), Prod(tys2)) => product_extension(tys1, tys2) |> rewrap
      | _ =>
        // It would be better to do this via a more direct error recovery mechanism in statics
        Unknown(Internal) |> rewrap
      };
    | _ => ty
    };
  }
/* What the left of a projection denotes, weak-head normalized: a module
   path's signature, or the expansion of a type. A variable root is
   whichever of a type and a value of that name was bound later; a value
   root counts only if it is a module, so `m.x` is no type. A mid-path label
   (`M.P.T`) names a sub-module, else a type member or a tuple label. Never
   uses Ctx.lookup_alias: it returns an invalid-hole type for unbound
   names, which would shadow the value namespace. */
and path_head = (~rec_counter, ctx: Ctx.t, t: t): option(t) =>
  if (rec_counter > 1000) {
    None;
  } else {
    let module_type = ty => {
      let ty = head_type(~rec_counter, ctx, ty);
      switch (term_of(ty)) {
      | Sig(_) => Some(ty)
      | _ => None
      };
    };
    switch (term_of(t)) {
    | Parens(t)
    | Projector(_, t) => path_head(~rec_counter=rec_counter + 1, ctx, t)
    | Var(n) =>
      switch (Ctx.newest_tvar_or_var(ctx, n)) {
      | Some(`TVar(_)) => Some(head_type(~rec_counter, ctx, t))
      | Some(`Var({typ, _})) => module_type(typ)
      | None => None
      }
    | ProdProjection(p, {term: Label(l), _}) =>
      switch (path_head(~rec_counter=rec_counter + 1, ctx, p)) {
      | Some({term: Sig(s), _}) =>
        let members = Sig.closed_members(s);
        switch (Option.bind(Sig.find_value(members, l), module_type)) {
        | Some(ty) => Some(ty)
        | None =>
          Sig.find_type_def(members, l)
          |> Option.map(head_type(~rec_counter, ctx))
        };
      | Some({term: Prod(tys), _}) =>
        project_type(tys, l) |> Option.map(head_type(~rec_counter, ctx))
      | _ => None
      }
    | ProdProjection(_) => None
    | _ => Some(head_type(~rec_counter, ctx, t))
    };
  }
/* Weak head normal form, unrolling a recursive type once. */
and head_type = (~rec_counter, ctx: Ctx.t, ty: t): t => {
  let ty = weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty);
  switch (term_of(ty)) {
  | Rec(_) =>
    weak_head_normalize(~rec_counter=rec_counter + 1, ctx, unroll(ty))
  | _ => ty
  };
};

/* The signature items a module path denotes, if any. A type alias whose
   expansion is a signature also counts, so `S.T` resolves on
   `type S = { type T = Int }`. */
let path_sig = (ctx: Ctx.t, t: t): option(ClosedSig.t(Sig.t)) =>
  switch (path_head(~rec_counter=0, ctx, t)) {
  | Some({term: Sig(s), _}) => Some(Sig.closed(s))
  | _ => None
  };

let as_sig = (ctx: Ctx.t, ty: t): option(ClosedSig.t(Sig.t)) =>
  switch (term_of(head_type(~rec_counter=0, ctx, ty))) {
  | Sig(s) => Some(Sig.closed(s))
  | _ => None
  };

/* Bind [entry] without changing what the types already in [ctx] mean.
   Those types name aliases and modules that are looked up where the types
   are used, so a binding that shadows one would capture their mentions of
   it. Those mentions, and the entry's own, are first replaced by what the
   shadowed binding stood for: a path root (`N` in `N.T`) by the newest
   alias or module of that name, a type by the newest alias. An abstract
   type variable has nothing to stand for, so its mentions stay captured. */
let extend_ctx = (ctx: Ctx.t, entry: Ctx.entry): Ctx.t => {
  let root = name =>
    switch (Ctx.newest_tvar_or_var(ctx, name)) {
    | Some(`TVar(Ctx.Singleton(def))) => Some(def)
    | Some(`Var({typ, _})) when Option.is_some(as_sig(ctx, typ)) =>
      Some(typ)
    | Some(`TVar(Ctx.Abstract))
    | Some(`Var(_))
    | None => None
    };
  let bare = name =>
    switch (Ctx.lookup_tvar(ctx, name)) {
    | Some(Ctx.Singleton(def)) => Some(def)
    | Some(Ctx.Abstract)
    | None => None
    };
  let shadowed: list((binder, Var.t, option(t))) =
    switch (entry) {
    | VarEntry({name, _}) => [(`Var, name, root(name))]
    | TVarEntry({name, _}) => [
        (`Var, name, root(name)),
        (`TVar, name, bare(name)),
      ]
    | ConstructorEntry(_)
    | LivelitEntry(_) => []
    };
  switch (
    List.filter_map(
      ((binder, name, def)) => Option.map(def => (binder, name, def), def),
      shadowed,
    )
  ) {
  | [] => Ctx.extend(ctx, entry)
  | sigma =>
    let closed = apply_sig_subst(sigma);
    let close_entry = (entry: Ctx.entry): Ctx.entry =>
      switch (entry) {
      | VarEntry(v) =>
        VarEntry({
          ...v,
          typ: closed(v.typ),
        })
      | ConstructorEntry(c) =>
        ConstructorEntry({
          ...c,
          typ: closed(c.typ),
        })
      | TVarEntry({kind: Singleton(def), _} as v) =>
        TVarEntry({
          ...v,
          kind: Singleton(closed(def)),
        })
      | TVarEntry({kind: Abstract, _})
      | LivelitEntry(_) => entry
      };
    Ctx.extend(
      {
        ...ctx,
        entries: List.map(close_entry, ctx.entries),
      },
      close_entry(entry),
    );
  };
};

/* Bind the member declared by a closed signature item, if any. Written
   items scope sequentially: later items may mention earlier type members,
   and root paths at earlier modules (`m.T`). A member named like an outer
   alias or module leaves the earlier members' meaning alone. */
let extend_sig_item = (ctx: Ctx.t, item: Sig.t): Ctx.t =>
  switch (item.term) {
  | SigType({term: Var(name), _} as tp, ty) =>
    extend_ctx(
      ctx,
      TVarEntry({
        name,
        id: IdTagged.rep_id(tp),
        kind: Singleton(ty),
      }),
    )
  | SigLet(_)
  | SigModule(_) =>
    switch (Sig.member_of_item(item)) {
    | Some(Val(name, typ)) =>
      extend_ctx(
        ctx,
        VarEntry({
          name,
          id: IdTagged.rep_id(item),
          typ,
          custom_statics: None,
        }),
      )
    | Some(TypeManifest(_))
    | None => ctx
    }
  | SigType(_, _)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => ctx
  };

/* [tvar]'s variable bound at its own id, for the body of a binder read
   from the syntax; a hole binds nothing. */
let extend_tvar_of_utpat = (ctx: Ctx.t, tvar: TPat.t, kind: Ctx.kind): Ctx.t =>
  switch (TPat.tyvar_of_utpat(tvar)) {
  | Some(name) =>
    extend_ctx(
      ctx,
      TVarEntry({
        name,
        id: TPat.rep_id(tvar),
        kind,
      }),
    )
  | None => ctx
  };

/* Members of a signature a type-level path may continue through
   (`M.P.T`): value members whose type is a signature, and type members
   defined as one. */
let sig_module_member_names = (ctx: Ctx.t, items: list(Sig.t)): list(Var.t) => {
  let members = Sig.members(items);
  let is_module = ty => Option.is_some(as_sig(ctx, ty));
  let modules = (names, find) =>
    List.filter(
      x => Option.fold(~none=false, ~some=is_module, find(members, x)),
      names,
    );
  modules(Sig.value_names(members), Sig.find_value)
  @ modules(Sig.type_names(members), Sig.find_type_def);
};

/* ~expand restricts which alias names get expanded (default: all): a
   signature's expectations expand only the type names the module body
   declares, keeping outer aliases as written. */
let rec normalize = (~rec_counter=0, ~expand=_ => true, ctx: Ctx.t, ty: t): t => {
  if (rec_counter > 1000) {
    failwith("normalize exceeded 1000 recursive calls");
  };
  let normalize = normalize(~rec_counter=rec_counter + 1, ~expand);
  let (term, rewrap) = unwrap(ty);
  switch (term) {
  | Var(x) when expand(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => normalize(ctx, ty)
    | None => ty
    }
  | Var(_) => ty
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | ExplicitNonlabel
  | Label(_) => ty
  | Parens(t)
  | Projector(_, t) => normalize(ctx, t)
  | List(t) => List(normalize(ctx, t)) |> rewrap
  | Arrow(t1, t2) =>
    Arrow(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Prod(ts) =>
    let ts = List.map(normalize(ctx), ts);
    let duplicate_labels =
      LabeledTuple.get_duplicate_labels(match_tup_label, ts);
    let ts =
      List.is_empty(duplicate_labels)
        ? ts : remove_duplicate_labels(~duplicate_labels, ts);
    Prod(ts) |> rewrap;
  | ProdProjection(_) => weak_head_normalize(ctx, ty) |> normalize(ctx)
  | ProdExtension(_) => weak_head_normalize(ctx, ty) |> normalize(ctx)
  | TupLabel({term: ExplicitNonlabel, _}, ty) => normalize(ctx, ty) // Drop ExplicitNonlabel in normalization
  | TupLabel(label, ty) =>
    TupLabel(normalize(ctx, label), normalize(ctx, ty)) |> rewrap
  | Sum(ts) =>
    Sum(ConstructorMap.map(Option.map(normalize(ctx)), ts)) |> rewrap
  | Rec(tpat, ty) =>
    /* NOTE: Dummy tvar added has fake id but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec(tpat, normalize(extend_tvar_of_utpat(ctx, tpat, Abstract), ty))
    |> rewrap
  | Poly(name, ty) =>
    Poly(name, normalize(extend_tvar_of_utpat(ctx, name, Abstract), ty))
    |> rewrap
  | ProofOf(_) => ty // Todo: should we normalize this?
  | Sig(s) =>
    /* An item that declares no member is dropped from the normal form,
       except a hole, which could declare any. */
    Sig(
      Closed(
        Sig.closed(s)
        |> ClosedSig.filter(item =>
             Option.is_some(Sig.member_of_item(item)) || Sig.is_hole(item)
           )
        |> ClosedSig.map(Sig.map_typ(normalize(ctx))),
      ),
    )
    |> rewrap
  };
};

/* Lattice meet on types. This was called 'join' in the 2019 Hazelnut live paper,
   but we're now calling it 'meet' to clarify that Unknown represents the top
   (least precise) element in the precision ordering: specific types dominate Unknown. */

/* A function inside a type: its type, and the signature members it is
   found under, outermost first (none when the type itself is a function,
   or the function sits in a tuple, list or sum). */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type fun_in = {
  members: list(Var.t),
  ty: t,
};

/* The first function in [ty], resolving aliases on demand rather than
   normalizing the whole type. Rec binders shadow their name, so bound
   occurrences don't re-expand through the outer context. Functions have no
   equality, so a type with one in it cannot be compared. */
let fun_in = (ctx: Ctx.t, ty: t): option(fun_in) => {
  let rec go = (~depth, ~members, ctx: Ctx.t, ty: t): option(fun_in) =>
    depth > 256
      ? None
      : {
        let go = go(~depth=depth + 1);
        switch (term_of(ty)) {
        | Parens(t)
        | Projector(_, t)
        | TupLabel(_, t)
        | ProdProjection(t, _) => go(~members, ctx, t)
        | Arrow(_)
        | Poly(_)
        | ProofOf(_) =>
          Some({
            members,
            ty,
          })
        | Var(x) =>
          switch (Ctx.lookup_alias(ctx, x)) {
          | Some(t) => go(~members, ctx, t)
          | None => None
          }
        | Unknown(_)
        | Atom(_)
        | DrvQuoteTy(_)
        | Label(_)
        | ExplicitNonlabel => None
        | Sig(s) =>
          List.find_map(
            (m: Sig.member) =>
              switch (m) {
              | Val(x, t) => go(~members=members @ [x], ctx, t)
              | TypeManifest(_) => None
              },
            Sig.closed_members(s),
          )
        | List(t) => go(~members, ctx, t)
        | Rec(tp, t) =>
          go(~members, extend_tvar_of_utpat(ctx, tp, Abstract), t)
        | Sum(sm) =>
          List.find_map(
            fun
            | ConstructorMap.Variant(_, _, Some(t)) => go(~members, ctx, t)
            | _ => None,
            sm,
          )
        | Prod(tys) => List.find_map(go(~members, ctx), tys)
        | ProdExtension(t1, t2) =>
          switch (go(~members, ctx, t1)) {
          | Some(f) => Some(f)
          | None => go(~members, ctx, t2)
          }
        };
      };
  go(~depth=0, ~members=[], ctx, ty);
};

/* Equality up to alias expansion, WITHOUT deep normalization: the decision
   procedure for `fast_equal(normalize(ctx, a), normalize(ctx, b))` that
   expands alias heads lazily, only where the comparison actually reaches
   them (the OCaml/GHC discipline: peel one layer on demand; a compact
   alias meeting itself or its own expansion never unrolls the body).
   Heads are resolved with weak_head_normalize; Rec/Poly binders shadow
   their name via a dummy tvar exactly as normalize does.
   Two conservative divergences from the normalize-then-compare original,
   both returning false where it might have said true (callers use the
   result to decide ascription-wrapping/marks, where a false negative is
   safe): alpha-differing binders are not renamed, and comparisons deeper
   than the recursion cap report unequal rather than failing. */
let equal_up_to_aliases = (ctx: Ctx.t, a: t, b: t): bool => {
  let rec go = (~depth, ctx: Ctx.t, a: t, b: t): bool =>
    if (depth > 256) {
      false;
    } else if (a === b || fast_equal(a, b)) {
      true;
    } else {
      let go = go(~depth=depth + 1);
      let a = weak_head_normalize(ctx, a);
      let b = weak_head_normalize(ctx, b);
      switch (term_of(a), term_of(b)) {
      | (Var(n1), Var(n2)) => n1 == n2 /* both unresolvable in ctx */
      | (Sig(Open(_)), _)
      | (_, Sig(Open(_))) => raise(Sig.OpenSignature)
      | (Sig(sx), Sig(sy)) =>
        /* Order-insensitive, as in [meet]. */
        let mx = Sig.closed_members(sx) |> Sig.dedup_last;
        let my = Sig.closed_members(sy) |> Sig.dedup_last;
        same_member_names(mx, my)
        && List.for_all(
             (m: Sig.member) =>
               switch (m) {
               | Val(x, t1) =>
                 Sig.find_value(my, x)
                 |> Option.fold(~none=false, ~some=go(ctx, t1))
               | TypeManifest(n, t1) =>
                 Sig.find_type_def(my, n)
                 |> Option.fold(~none=false, ~some=go(ctx, t1))
               },
             mx,
           );
      | (List(x), List(y)) => go(ctx, x, y)
      | (Arrow(x1, y1), Arrow(x2, y2)) =>
        go(ctx, x1, x2) && go(ctx, y1, y2)
      | (Prod(xs), Prod(ys)) =>
        List.length(xs) == List.length(ys)
        && List.for_all2(go(ctx), xs, ys)
      | (TupLabel(l1, x), TupLabel(l2, y)) =>
        fast_equal(l1, l2) && go(ctx, x, y)
      | (Sum(xs), Sum(ys)) => ConstructorMap.equal(go(ctx), xs, ys)
      | (Rec(tp1, x), Rec(tp2, y))
      | (Poly(tp1, x), Poly(tp2, y)) =>
        switch (TPat.tyvar_of_utpat(tp1), TPat.tyvar_of_utpat(tp2)) {
        | (Some(n1), Some(n2)) when n1 == n2 =>
          go(extend_tvar_of_utpat(ctx, tp1, Abstract), x, y)
        | _ => fast_equal(a, b)
        }
      /* Atoms, Unknowns, Labels: compared again now that their heads are
         alias-resolved. */
      | _ => fast_equal(a, b)
      };
    };
  go(~depth=0, ctx, a, b);
};

/* Structural canonicalization WITHOUT alias expansion: desugars Sig,
   computes tuple projections/extensions, dedups labels, strips wrapper
   noise — but leaves every alias compact. This is the right form for
   types EMBEDDED into elaborations (ascriptions, recorded elab_syn_ty):
   they only need to be resolvable in ctx, and expanding them is what
   made large-sum programs quadratically slow downstream. */
let canonicalize = (ctx: Ctx.t, ty: t): t =>
  normalize(~expand=_ => false, ctx, ty);

/* An item of a signature as written, its closed form, and the context it is
   read in: the one outside the signature extended with the earlier items,
   closed. */
type sig_item_scope = {
  written: Sig.t,
  closed: Sig.t,
  ctx: Ctx.t,
};

/* [ty] with every signature in it closed: each member's type written in
   terms of the context outside its signature alone, so the members are
   independent and the signature is a record. Signature items are written
   sequentially: a later item may mention an earlier type member by name or
   root a path at an earlier module member, and a type member's name is in
   scope in its own definition. Closing substitutes the earlier members in,
   makes a self-mentioning type member recursive, and expands an outer alias
   named like one of the signature's type members, so no member type
   mentions a name a sibling binds. Every signature type statics works with
   is closed, here, when it is read from the syntax. */
let rec close = (ctx: Ctx.t, ty: t): t => {
  let (term, rewrap) = unwrap(ty);
  switch (term) {
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Label(_)
  | ExplicitNonlabel
  | Var(_)
  | ProofOf(_) => ty
  | Parens(t) => Parens(close(ctx, t)) |> rewrap
  | Projector(d, t) => Projector(d, close(ctx, t)) |> rewrap
  | List(t) => List(close(ctx, t)) |> rewrap
  | Arrow(t1, t2) => Arrow(close(ctx, t1), close(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(close(ctx), ts)) |> rewrap
  | TupLabel(l, t) => TupLabel(l, close(ctx, t)) |> rewrap
  | ProdProjection(t1, t2) =>
    ProdProjection(close(ctx, t1), close(ctx, t2)) |> rewrap
  | ProdExtension(t1, t2) =>
    ProdExtension(close(ctx, t1), close(ctx, t2)) |> rewrap
  | Sum(sm) =>
    Sum(ConstructorMap.map(Option.map(close(ctx)), sm)) |> rewrap
  | Rec(tp, t) =>
    Rec(tp, close(extend_tvar_of_utpat(ctx, tp, Abstract), t)) |> rewrap
  | Poly(tp, t) =>
    Poly(tp, close(extend_tvar_of_utpat(ctx, tp, Abstract), t)) |> rewrap
  | Sig(Open(items)) => Sig(Closed(close_items(ctx, items))) |> rewrap
  /* Already closed: its members are independent, so each is closed alone,
     never re-read as mentioning a sibling. */
  | Sig(Closed(items)) =>
    Sig(Closed(ClosedSig.map(Sig.map_typ(close(ctx)), items))) |> rewrap
  };
}
and close_items = (ctx: Ctx.t, items: list(Sig.t)): ClosedSig.t(Sig.t) =>
  sig_item_scopes(ctx, items)
  |> List.map(scope => scope.closed)
  |> ClosedSig.of_items
/* The type an alias [tp] defined as [def] stands for: [def] closed with
   the alias's own name abstract, recursive if it mentions itself. */
and alias_def = (ctx: Ctx.t, tp: TPat.t, def: t): t =>
  switch (TPat.tyvar_of_utpat(tp)) {
  | None => close(ctx, def)
  | Some(name) =>
    let def = close(extend_tvar_of_utpat(ctx, tp, Abstract), def);
    List.exists(String.equal(name), free_vars(def))
      ? Rec(Var(name) |> TPat.fresh, def) |> temp : def;
  }
and sig_item_scopes = (ctx: Ctx.t, items: list(Sig.t)): list(sig_item_scope) => {
  let type_members = Sig.type_names(Sig.members(items));
  let is_type_member = n => List.exists(String.equal(n), type_members);
  let close_item = (ctx, item: Sig.t) => {
    let closed =
      switch (item.term) {
      | SigType(tp, _) => alias_def(ctx, tp)
      | SigLet(_)
      | SigModule(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => close(ctx)
      };
    Sig.map_typ(
      ty => normalize(~expand=is_type_member, ctx, closed(ty)),
      item,
    );
  };
  let (_, rev) =
    List.fold_left(
      ((ctx, acc), written: Sig.t) => {
        let closed = close_item(ctx, written);
        let ctx_after = extend_sig_item(ctx, closed);
        /* A type member's name is in scope in its own definition; a value
           or module member's is not. */
        let read_in =
          switch (written.term) {
          | SigType(_) => ctx_after
          | SigLet(_)
          | SigModule(_)
          | Invalid(_)
          | EmptyHole
          | MultiHole(_) => ctx
          };
        (
          ctx_after,
          [
            {
              written,
              closed,
              ctx: read_in,
            },
            ...acc,
          ],
        );
      },
      (ctx, []),
      items,
    );
  List.rev(rev);
};

let rec meet = (ctx: Ctx.t, ty1: t, ty2: t): option(t) => {
  let meet' = meet(ctx);
  switch (term_of(ty1), term_of(ty2)) {
  | (_, Parens(ty2))
  | (_, Projector(_, ty2)) => meet'(ty1, ty2)
  | (Parens(ty1), _)
  | (Projector(_, ty1), _) => meet'(ty1, ty2)
  | (TupLabel({term: ExplicitNonlabel, _}, ty1'), _) => meet'(ty1', ty2)
  | (_, TupLabel({term: ExplicitNonlabel, _}, ty2')) => meet'(ty1, ty2')
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(meet_type_provenance(p1, p2)) |> temp)
  | (Unknown(_), _) => Some(ty2)
  | (_, Unknown(_)) => Some(ty1)
  | (Var(n1), Var(n2)) =>
    if (n1 == n2) {
      Some(ty1);
    } else {
      let ty1' = Ctx.lookup_alias(ctx, n1);
      let ty2' = Ctx.lookup_alias(ctx, n2);
      switch (ty1', ty2') {
      | (Some(ty1), Some(ty2)) => meet'(ty1, ty2)
      | (Some(ty1), None) => meet'(ty1, ty2)
      | (None, Some(ty2)) => meet'(ty1, ty2)
      | (None, None) => None
      };
    }
  | (Var(name), _) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ ty_meet = meet'(ty_name, ty2);
    equal(ty_name, ty_meet) ? ty1 : ty_meet;
  | (_, Var(name)) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ ty_meet = meet'(ty_name, ty1);
    equal(ty_name, ty_meet) ? ty2 : ty_meet;
  /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
  | (ProdProjection(_), _) => meet'(weak_head_normalize(ctx, ty1), ty2)
  | (_, ProdProjection(_)) => meet'(ty1, weak_head_normalize(ctx, ty2))
  | (ProdExtension(_), _) => meet'(weak_head_normalize(ctx, ty1), ty2)
  | (_, ProdExtension(_)) => meet'(ty1, weak_head_normalize(ctx, ty2))
  | (Rec(tp1, ty1), Rec(tp2, ty2)) =>
    let ctx = extend_tvar_of_utpat(ctx, tp1, Abstract);
    let ty1' =
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(x2) => subst(Var(x2) |> temp, tp1, ty1)
      | None => ty1
      };
    let+ ty_body = meet(ctx, ty1', ty2);
    Rec(tp1, ty_body) |> temp;
  /* A signature alias that mentions itself is a Rec; a self-loop unrolls
     to nothing. */
  | (Rec(_), Sig(_)) =>
    Option.bind(unroll_to_non_rec(ty1), ty1 => meet'(ty1, ty2))
  | (Sig(_), Rec(_)) =>
    Option.bind(unroll_to_non_rec(ty2), ty2 => meet'(ty1, ty2))
  | (Rec(_), _) => None
  | (Poly(x1, ty1), Poly(x2, ty2)) =>
    let ty1' =
      switch (TPat.tyvar_of_utpat(x2)) {
      | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
      | None => ty1
      };
    let ctx = extend_tvar_of_utpat(ctx, x2, Abstract);
    let+ ty_body = meet(ctx, ty1', ty2);
    Poly(x2, ty_body) |> temp;
  /* Note for above: `subst` is capture-avoiding (see its definition),
     so renaming `x1` to `x2` via substitution is safe. In rare cases
     where capture does trigger, `subst` generates fresh internal type
     variable names via `fresh_var` that may be exposed to the user. We
     preserve the variable name of the second type to preserve
     synthesized type variable names, which come from user annotations. */
  | (Poly(_), _) => None
  | (Atom(c1), Atom(c2)) when c1 == c2 => Some(ty1)
  | (Atom(_), _) => None
  | (DrvQuoteTy(d1), DrvQuoteTy(d2)) when d1 == d2 => Some(ty1)
  | (DrvQuoteTy(_), _) => None
  | (Label(_), Label("")) => Some(ty1)
  | (Label(""), Label(_)) => Some(ty2)
  | (Label(name1), Label(name2))
      when LabeledTuple.match_labels(name1, name2) =>
    Some(ty1)
  | (Label(_), _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* ty1 = meet'(ty1, ty1');
    let+ ty2 = meet'(ty2, ty2');
    Arrow(ty1, ty2) |> temp;
  | (Arrow(_), _) => None
  | (TupLabel(lab1, ty1'), TupLabel(lab2, ty2')) =>
    let* lab = meet'(lab1, lab2);
    let+ ty = meet'(ty1', ty2');
    TupLabel(lab, ty) |> temp;
  | (TupLabel(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    let tys1 =
      remove_duplicate_labels(
        ~duplicate_labels=
          LabeledTuple.get_duplicate_labels(match_tup_label, tys1),
        tys1,
      );
    let tys2 =
      remove_duplicate_labels(
        ~duplicate_labels=
          LabeledTuple.get_duplicate_labels(match_tup_label, tys2),
        tys2,
      );

    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      let* tys = ListUtil.map2_opt(meet', tys1, tys2);
      let+ tys = OptUtil.sequence(tys);
      Prod(tys) |> temp;
    };
  | (Prod(_), _) => None
  | (Sum(sm1), Sum(sm2)) =>
    let+ sm' = ConstructorMap.meet(equal, meet(ctx), sm1, sm2);
    Sum(sm') |> temp;
  | (Sum(_), _) => None
  | (List(ty1), List(ty2)) =>
    let+ ty = meet'(ty1, ty2);
    List(ty) |> temp;
  | (List(_), _) => None
  | (ProofOf(e1), ProofOf(e2)) =>
    Equality.semantic.exp(e1, e2) ? Some(ty1) : None
  | (ProofOf(_), _) => None
  // We would prefer for this to be a sort difference and never appear in a meet.
  // These get marked in statics but that does not remove them from the utyp's propagated on parents.
  | (ExplicitNonlabel, _) => None
  | (Sig(Closed(xs)), Sig(Closed(ys))) =>
    /* Exact consistency: the same value-member names and the same
       type-member names (order-insensitive), members pairwise consistent,
       except that a hole item on one side stands for the members the
       other side has beyond it. The result is built from the left
       operand's members, the last of each name, so each keeps its form,
       and its type where the meet refines nothing; then the right's
       members the left lacks; then a hole, if both sides keep one. */
    let xs = ClosedSig.items(xs);
    let ys = ClosedSig.items(ys);
    let x_items =
      xs
      |> List.filter(item => Option.is_some(Sig.member_of_item(item)))
      |> Sig.dedup_last_items;
    let mx = Sig.members(x_items);
    let my = Sig.members(ys) |> Sig.dedup_last;
    let hole_x = Sig.has_hole_item(xs);
    let hole_y = Sig.has_hole_item(ys);
    let within = (a: list(Sig.member), b: list(Sig.member)) =>
      List.for_all(
        x => Option.is_some(Sig.find_value(b, x)),
        Sig.value_names(a),
      )
      && List.for_all(
           t => Option.is_some(Sig.find_type_def(b, t)),
           Sig.type_names(a),
         );
    if (!(hole_y || within(mx, my)) || !(hole_x || within(my, mx))) {
      None;
    } else {
      let meet_item = (item: Sig.t) =>
        switch (Sig.member_of_item(item)) {
        | None => Some(item)
        | Some(m) =>
          let (t1, t2) =
            switch (m) {
            | Val(x, t1) => (t1, Sig.find_value(my, x))
            | TypeManifest(n, t1) => (t1, Sig.find_type_def(my, n))
            };
          switch (t2) {
          | None => Some(item)
          | Some(t2) =>
            let+ t = meet'(t1, t2);
            equal(t, t1) ? item : Sig.map_typ(_ => t, item);
          };
        };
      let+ items = OptUtil.traverse(meet_item, x_items);
      let extra =
        my
        |> List.filter((m: Sig.member) =>
             switch (m) {
             | Val(x, _) => Option.is_none(Sig.find_value(mx, x))
             | TypeManifest(n, _) =>
               Option.is_none(Sig.find_type_def(mx, n))
             }
           )
        |> List.map(Sig.item_of_member);
      let hole = hole_x && hole_y ? [TempGrammar.Sig.empty_hole()] : [];
      Sig(Closed(ClosedSig.of_items(items @ extra @ hole))) |> temp;
    };
  | (Sig(Open(_)), _)
  | (_, Sig(Open(_))) => raise(Sig.OpenSignature)
  | (Sig(_), _) => None
  };
};

/* REQUIRES NORMALIZED TYPES
   Remove synswitches from t1 by matching against t2 */
let rec match_synswitch = (t1: t, t2: t) => {
  let (term1, rewrap1) = unwrap(t1);
  switch (term1, term_of(t2)) {
  | (Parens(t1), _) => Parens(match_synswitch(t1, t2)) |> rewrap1
  | (Projector(data, t1), _) =>
    Projector(data, match_synswitch(t1, t2)) |> rewrap1
  | (Unknown(SynSwitch), _) => t2
  // These cases can't have a synswitch inside
  | (Unknown(_), _)
  | (Atom(_), _)
  | (DrvQuoteTy(_), _)
  | (Label(_), _)
  | (ExplicitNonlabel, _)
  | (Var(_), _)
  | (Rec(_), _)
  | (ProofOf(_), _)
  | (ProdProjection(_), _)
  | (ProdExtension(_), _) => t1
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
  // HACK[Matt]: The only possible poly is `Poly Syn -> Syn`
  | (Poly(_), Poly(_)) => t2
  | (Poly(_), _) => t1
  | (Sig(_), _) => t1
  };
};

let meet_all = (~empty: t, ctx: Ctx.t, ts: list(t)): option(t) =>
  List.fold_left(
    (acc, ty) => OptUtil.and_then(meet(ctx, ty), acc),
    Some(empty),
    ts,
  );

let is_consistent = (ctx: Ctx.t, ty1: t, ty2: t): bool =>
  meet(ctx, ty1, ty2) != None;

/**
   * Determines if one type (`ty1`) is more precise than another type (`ty2`) within a given context (`ctx`).
   *
   * @return - `true` if `ty1` is more precise than `ty2`, otherwise `false`.
   */
let is_more_precise = (ctx: Ctx.t, ty1: t, ty2: t): bool => {
  let met = meet(ctx, ty1, ty2);
  switch (met) {
  | None => false
  | Some(met) => Equality.semantic.typ(met, ty1)
  };
};

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

let rec is_syn = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x)
  | Projector(_, x) => is_syn(x)
  | Unknown(SynSwitch) => true
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Label(_)
  | Var(_)
  | Rec(_)
  | Poly(_)
  | ProofOf(_)
  | List(_)
  | Arrow(_)
  | Prod(_)
  | Sum(_)
  | ProdProjection(_)
  | ProdExtension(_)
  | ExplicitNonlabel
  | Sig(_) => false
  };

let rec is_ana_atom = (ty: t) =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x)
  | Projector(_, x) => is_ana_atom(x)
  | Atom(a) => Some(a)
  | DrvQuoteTy(_)
  | Unknown(_)
  | ExplicitNonlabel
  | Label(_)
  | Var(_)
  | Rec(_)
  | Poly(_)
  | ProofOf(_)
  | List(_)
  | Arrow(_)
  | Prod(_)
  | ProdProjection(_)
  | ProdExtension(_)
  | Sum(_)
  | Sig(_) => None
  };

let rec is_syn_plus = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x)
  | Projector(_, x) => is_syn_plus(x)
  | Unknown(SynSwitch) => true
  | Arrow(t1, t2) => is_syn(t1) && is_syn_plus(t2)
  | Poly(_, t) => is_syn(t)
  | ProofOf(_)
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | ExplicitNonlabel
  | Label(_)
  | Var(_)
  | Rec(_)
  | List(_)
  | Prod(_)
  | Sum(_)
  | ProdProjection(_)
  | ProdExtension(_)
  | Sig(_) => false
  };

let rec is_arrow_like = (ty: t): bool =>
  switch (term_of(ty)) {
  | Unknown(_) => true
  | Arrow(_, _) => true
  | Poly(_, t) => is_arrow_like(t)
  | Parens(t)
  | Projector(_, t) => is_arrow_like(t)
  | _ => false
  };

/* Does the type require parentheses when on the left of an arrow for printing? */
let rec needs_parens = (ty: t): bool =>
  switch (term_of(ty)) {
  | Parens(ty)
  | Projector(_, ty) => needs_parens(ty)
  | Unknown(_)
  | Atom(_)
  | ExplicitNonlabel
  | Label(_)
  | DrvQuoteTy(_)
  | List(_) /* is already wrapped in [] */
  | ProofOf(_)
  | Var(_) => false
  | ProdProjection(_, _)
  | ProdExtension(_, _)
  | TupLabel(_, _)
  | Rec(_, _)
  | Poly(_, _)
  | Arrow(_, _)
  | Prod(_)
  | Sum(_) => true /* disambiguate between (A + B) -> C and A + (B -> C) */
  | Sig(_) => false /* already wrapped in {} */
  };

let pretty_print_tvar = (tv: TPat.t): string =>
  switch (IdTagged.term_of(tv)) {
  | Var(x) => x
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => "?"
  };

/* Essentially recreates web/view/Type.re's view_ty but with string output */
let rec pretty_print = (ty: t): string =>
  switch (term_of(ty)) {
  | Parens(ty)
  | Projector(_, ty) => pretty_print(ty)
  | Unknown(_) => "?"
  | Atom(Int) => "Int"
  | Atom(Float) => "Float"
  | Atom(Bool) => "Bool"
  | Atom(String) => "String"
  | DrvQuoteTy(d) => DrvSort.to_string(d)
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
  | ProdProjection(t, label) =>
    pretty_print(t) ++ "." ++ pretty_print(label)
  | ProdExtension(t, label) =>
    pretty_print(t) ++ " + " ++ pretty_print(label)
  | Label(name) => name
  | ExplicitNonlabel => "_"
  | TupLabel(label, t) => pretty_print(label) ++ "=" ++ pretty_print(t)
  | Rec(tv, t) =>
    "rec " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  | Poly(tv, t) =>
    "poly " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  | ProofOf(_e) => "yes <e> indeed"
  | Sig(s) =>
    let sig_item_str = (item: Sig.t) =>
      switch (item.term) {
      | SigLet(p) =>
        "let "
        ++ (
          switch (IdTagged.term_of(p)) {
          | Var(x) => x
          | Asc(p', t) =>
            (
              switch (IdTagged.term_of(p')) {
              | Var(x) => x
              | _ => "?"
              }
            )
            ++ " : "
            ++ pretty_print(t)
          | _ => "?"
          }
        )
      | SigType(tp, t) =>
        "type " ++ pretty_print_tvar(tp) ++ " = " ++ pretty_print(t)
      | SigModule(mp) =>
        let rec mpat_str = (mp: TermBase.MPat.t) =>
          switch (IdTagged.term_of(mp)) {
          | Var(x) => x
          | Asc(inner, t) => mpat_str(inner) ++ " : " ++ pretty_print(t)
          | Invalid(_)
          | EmptyHole
          | MultiHole(_) => "?"
          };
        "module " ++ mpat_str(mp);
      | EmptyHole => "?"
      | Invalid(s) => s
      | MultiHole(_) => "?"
      };
    "{ "
    ++ String.concat("; ", List.map(sig_item_str, Sig.items(s)))
    ++ " }";
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
 * Converts a list of types (`tys`) into a product type.
 *
 * @param tys - A list of types to be combined into a product type.
 * @return A product type representing the combination of the input types
 */
let to_product = (tys: list(t)): t => TempGrammar.Typ.(prod(tys));

/* Every id in a type, including the variant_ann ids on Sum constructors. */
let all_ids = (ty: t): list(Id.t) => {
  let ids = ref([]);
  let _ =
    Grammar.map_typ_annotation(
      (t: IdTagged.IdTag.t) => {
        ids := t.ids @ ids^;
        t;
      },
      ty: t,
    );
  let rec collect_ann_ids = (ty: t) => {
    switch (term_of(ty)) {
    | Sum(variants) =>
      List.iter(
        fun
        | ConstructorMap.Variant(_, ann, opt) => {
            ids := ann.ids @ ids^;
            Option.iter(collect_ann_ids, opt);
          }
        | BadEntry(t) => collect_ann_ids(t),
        variants,
      )
    | Arrow(t1, t2)
    | TupLabel(t1, t2)
    | ProdExtension(t1, t2)
    | ProdProjection(t1, t2) =>
      collect_ann_ids(t1);
      collect_ann_ids(t2);
    | List(t)
    | Parens(t)
    | Projector(_, t)
    | Rec(_, t)
    | Poly(_, t) => collect_ann_ids(t)
    | Prod(ts) => List.iter(collect_ann_ids, ts)
    | Unknown(_)
    | Atom(_)
    | DrvQuoteTy(_)
    | Label(_)
    | ExplicitNonlabel
    | Var(_)
    | ProofOf(_) => ()
    /* Syntactic, not `Sig.members`: an item whose name is a hole still
       prints. */
    | Sig(s) =>
      List.iter(
        item => Option.iter(collect_ann_ids, Sig.typ_of_item(item)),
        Sig.items(s),
      )
    };
  };
  collect_ann_ids(ty);
  ids^;
};

/* Every id in one constructor variant. */
let variant_all_ids = (v: ConstructorMap.variant(t)): list(Id.t) =>
  switch (v) {
  | Variant(_, ann, Some(t)) => ann.ids @ all_ids(t)
  | Variant(_, ann, None) => ann.ids
  | BadEntry(t) => all_ids(t)
  };

/* The ids of the nodes of [ty'] that [ty] does not account for -- what has
   to be marked to show how [ty'] differs. Ids in either type must be
   distinct, or the result names the wrong nodes.

   [expanded_aliases] names the aliases already expanded on the way here, so
   a cycle through the context (`type A = (B) in type B = (A)`) stops rather
   than expanding forever. Only the arms that see the same position again --
   alias expansion and the nodes that carry no meaning of their own -- thread
   it; descending into a component starts over. */
let rec diff =
        (
          ~ctx: option(Ctx.t)=?,
          ~expanded_aliases: list(string)=[],
          ty: t,
          ty': t,
        )
        : list(Id.t) => {
  let get_ids = () => all_ids(ty');
  let expand = name =>
    if (List.exists(String.equal(name), expanded_aliases)) {
      None;
    } else {
      ctx |> Option.map(Ctx.lookup_alias(_, name)) |> Option.join;
    };
  switch (term_of(ty), term_of(ty')) {
  | (Parens(t1), _) => diff(~ctx?, ~expanded_aliases, t1, ty')
  | (_, Projector(_, t2)) => diff(~ctx?, ~expanded_aliases, ty, t2)
  | (Projector(_, t1), _) => diff(~ctx?, ~expanded_aliases, t1, ty')
  /* Parens carry no meaning of their own, so they take the verdict of the
     node they wrap: marked when that node is itself replaced, unmarked when
     it merely contains something that changed. `(Int, ?)` vs `(Int, String)`
     leaves the parens alone -- still the same tuple, one component differs --
     while `?` vs `(a=Int)` marks them, the tuple being wholly new. */
  | (_, Parens(t2)) =>
    let inner = diff(~ctx?, ~expanded_aliases, ty, t2);
    let wrapped_replaced =
      IdTagged.ids(t2) |> List.exists(id => List.mem(id, inner));
    wrapped_replaced ? IdTagged.ids(ty') @ inner : inner;
  /* Runtime knowing less than statics is not something runtime supplied:
     `?` on the right is unmarked whatever stands on the left. */
  | (_, Unknown(_)) => []
  | (Unknown(_), _) => get_ids()
  | (Atom(c1), Atom(c2)) when c1 == c2 => []
  | (Atom(_), _) => get_ids()
  | (DrvQuoteTy(d1), DrvQuoteTy(d2)) when d1 == d2 => []
  | (DrvQuoteTy(_), _) => get_ids()
  | (Label(l1), Label(l2)) when l1 == l2 => []
  | (Label(_), _) => get_ids()
  | (ExplicitNonlabel, ExplicitNonlabel) => []
  | (ExplicitNonlabel, _) => get_ids()
  | (Var(v1), Var(v2)) when v1 == v2 => []
  | (Var(name), _) =>
    switch (expand(name)) {
    | Some(expanded) =>
      diff(
        ~ctx?,
        ~expanded_aliases=[name, ...expanded_aliases],
        expanded,
        ty',
      )
    | None => get_ids()
    }
  /* An alias prints as one token carrying the Var node's own ids, so the
     expansion decides only WHETHER it differs; the ids returned are the
     Var's, and the verdict is all-or-nothing. */
  | (_, Var(name)) =>
    switch (expand(name)) {
    | Some(expanded) =>
      diff(
        ~ctx?,
        ~expanded_aliases=[name, ...expanded_aliases],
        ty,
        expanded,
      )
      == []
        ? [] : get_ids()
    | None => get_ids()
    }
  | (Rec(tp1, t1), Rec(tp2, t2)) when Equality.syntactic.tpat(tp1, tp2) =>
    diff(~ctx?, t1, t2)
  | (Rec(_), _) => get_ids()
  | (Poly(tp1, t1), Poly(tp2, t2)) when Equality.syntactic.tpat(tp1, tp2) =>
    diff(~ctx?, t1, t2)
  | (Poly(_), _) => get_ids()
  | (ProofOf(e1), ProofOf(e2)) =>
    Equality.syntactic.exp(e1, e2) ? [] : get_ids()
  | (ProofOf(_), _) => get_ids()
  | (Arrow(t1a, t1b), Arrow(t2a, t2b)) =>
    diff(~ctx?, t1a, t2a) @ diff(~ctx?, t1b, t2b)
  | (Arrow(_), _) => get_ids()
  | (Prod(tys1), Prod(tys2)) when List.length(tys1) == List.length(tys2) =>
    List.map2(diff(~ctx?), tys1, tys2) |> List.concat
  | (Prod(_), _) => get_ids()
  | (TupLabel(l1, t1), TupLabel(l2, t2)) =>
    diff(~ctx?, l1, l2) @ diff(~ctx?, t1, t2)
  | (TupLabel(_, _), _) => get_ids()
  | (List(t1), List(t2)) => diff(~ctx?, t1, t2)
  | (List(_), _) => get_ids()
  | (ProdProjection(t1, t2), ProdProjection(t1', t2')) =>
    diff(~ctx?, t1, t1') @ diff(~ctx?, t2, t2')
  | (ProdProjection(_, _), _) => get_ids()
  | (ProdExtension(t1, t2), ProdExtension(t1', t2')) =>
    diff(~ctx?, t1, t1') @ diff(~ctx?, t2, t2')
  | (ProdExtension(_, _), _) => get_ids()
  | (Sum(sm1), Sum(sm2)) =>
    let (inter, left, right) =
      ConstructorMap.venn_regions(
        ConstructorMap.same_constructor(fast_equal),
        sm1,
        sm2,
      );
    if (left != []) {
      /* A constructor missing on the right makes the whole Sum different. */
      get_ids();
    } else {
      let matched_ids =
        List.concat_map(
          ((v1, v2)) =>
            switch (v1, v2) {
            | (
                ConstructorMap.Variant(_, _, Some(t1)),
                ConstructorMap.Variant(_, _, Some(t2)),
              ) =>
              diff(~ctx?, t1, t2)
            | (
                ConstructorMap.Variant(_, _, None),
                ConstructorMap.Variant(_, _, None),
              ) =>
              []
            | (ConstructorMap.BadEntry(t1), ConstructorMap.BadEntry(t2)) =>
              diff(~ctx?, t1, t2)
            | (_, v2) => variant_all_ids(v2)
            },
          inter,
        );
      let right_ids = List.concat_map(variant_all_ids, right);
      matched_ids @ right_ids;
    };
  | (Sum(_), _) => get_ids()
  | (Sig(_), Sig(_)) when fast_equal(ty, ty') => []
  | (Sig(_), _) => get_ids()
  };
};
