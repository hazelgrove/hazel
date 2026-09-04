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
  | Sig(items) =>
    List.exists(
      (m: Sig.member) =>
        switch (m) {
        | Val(_, ty) => has_fun(ty)
        | TypeManifest(_) => false
        },
      Sig.members(items),
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

let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
  switch (term_of(ty)) {
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Label(_)
  | ExplicitNonlabel => []
  | Var(v) => List.mem(v, bound) ? [] : [v]
  | Parens(ty)
  | Projector(_, ty) => free_vars(~bound, ty)
  | List(ty) => free_vars(~bound, ty)
  | ProdExtension(t1, t2)
  | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Sum(sm) => ConstructorMap.free_variables(free_vars(~bound), sm)
  | Prod(tys) => List.concat_map(free_vars(~bound), tys)
  | ProdProjection(t1, _) => free_vars(~bound, t1)
  | TupLabel(_, ty) => free_vars(~bound, ty)
  | Rec(x, ty)
  | Poly(x, ty) =>
    free_vars(~bound=(x |> TPat.tyvar_of_utpat |> Option.to_list) @ bound, ty)
  | ProofOf(_) => []
  | Sig(items) =>
    /* Type members bind their name for the items that follow. */
    items
    |> List.fold_left(
         ((bound, acc), item) =>
           switch (Sig.member_of_item(item)) {
           | Some(Val(_, ty)) => (bound, acc @ free_vars(~bound, ty))
           | Some(TypeManifest(name, ty)) => (
               [name, ...bound],
               acc @ free_vars(~bound, ty),
             )
           | None => (bound, acc)
           },
         (bound, []),
       )
    |> snd
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
  | Sig(items) =>
    List.fold_left(
      (acc, m: Sig.member) =>
        switch (m) {
        | Val(_, ty)
        | TypeManifest(_, ty) => acc + count_unknowns(ty)
        },
      0,
      Sig.members(items),
    )
  };

let contains_unknown = (ty: t): bool => count_unknowns(ty) > 0;

/* Capture-avoiding substitution of `s` for `x` in `ty`.

   When recursing under a type binder `Poly(tp2, body)` or `Rec(tp2, body)`
   whose name occurs free in `s`, naive substitution would let occurrences of
   that name introduced by substituting `s` be captured by the binder. To
   avoid this, we alpha-rename the clashing binder to a fresh name (via
   `fresh_var`) before recursing. The inner subst used for renaming is itself
   capture-avoiding, so repeated collisions are handled naturally. */
let rec subst = (s: t, x: TPat.t, ty: t): t => {
  let avoid_capture = (tp2: TPat.t, body: t): (TPat.t, t) =>
    switch (TPat.tyvar_of_utpat(tp2)) {
    | Some(name) when List.mem(name, free_vars(s)) =>
      let fresh = fresh_var(name);
      let tp2': TPat.t = Var(fresh) |> TPat.fresh;
      let body' = subst(Var(fresh) |> temp, tp2, body);
      (tp2', body');
    | _ => (tp2, body)
    };
  switch (TPat.tyvar_of_utpat(x)) {
  | Some(str) =>
    let (term, rewrap) = Annotated.unwrap(ty);
    switch (term) {
    | Atom(_) => ty
    | Label(name) => Grammar.Label(name) |> rewrap
    | ExplicitNonlabel => ExplicitNonlabel |> rewrap
    | Unknown(prov) => Unknown(prov) |> rewrap
    | Arrow(ty1, ty2) =>
      Arrow(subst(s, x, ty1), subst(s, x, ty2)) |> rewrap
    | Prod(tys) => Prod(List.map(subst(s, x), tys)) |> rewrap
    | TupLabel(label, ty) => TupLabel(label, subst(s, x, ty)) |> rewrap
    | Sum(sm) =>
      Sum(ConstructorMap.map(Option.map(subst(s, x)), sm)) |> rewrap
    | Poly(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
      Poly(tp2, ty) |> rewrap
    | Poly(tp2, ty) =>
      let (tp2', ty') = avoid_capture(tp2, ty);
      Poly(tp2', subst(s, x, ty')) |> rewrap;
    | Rec(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
      Rec(tp2, ty) |> rewrap
    | Rec(tp2, ty) =>
      let (tp2', ty') = avoid_capture(tp2, ty);
      Rec(tp2', subst(s, x, ty')) |> rewrap;
    | List(ty) => List(subst(s, x, ty)) |> rewrap
    | Var(y) => str == y ? s : Var(y) |> rewrap
    | Parens(ty) => Parens(subst(s, x, ty)) |> rewrap
    | Projector(data, ty) => Projector(data, subst(s, x, ty)) |> rewrap
    | ProdProjection(t1, t2) =>
      ProdProjection(subst(s, x, t1), subst(s, x, t2)) |> rewrap
    | ProdExtension(t1, t2) =>
      ProdExtension(subst(s, x, t1), subst(s, x, t2)) |> rewrap
    | ProofOf(e) => ProofOf(e) |> rewrap
    | Sig(items) =>
      /* Type members bind their name for later items and cannot be renamed
         (member names are labels and `M.T` keys), so on capture we fall
         back to substituting Unknown into the remaining items. */
      let fv_s = free_vars(s);
      let rec go = (items: list(Sig.t)) =>
        switch (items) {
        | [] => []
        | [item, ...rest] =>
          let item' = Sig.map_typ(subst(s, x), item);
          switch (Sig.member_of_item(item)) {
          | Some(TypeManifest(n, _)) when n == str => [item', ...rest]
          | Some(TypeManifest(n, _)) when List.mem(n, fv_s) => [
              item',
              ...List.map(
                   Sig.map_typ(subst(Unknown(Internal) |> temp, x)),
                   rest,
                 ),
            ]
          | _ => [item', ...go(rest)]
          };
        };
      Sig(go(items)) |> rewrap;
    | DrvQuoteTy(_) => ty
    };
  | None => ty
  };
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

/* ==================== Signature member projection ====================
   Later signature items may mention earlier type members by name, so the
   type of a member is only meaningful outside the signature once those
   references are substituted away. The walk threads a substitution
   (latest binder first) over the items in order. */
let apply_sig_subst = (sigma: list((Var.t, t)), ty: t): t =>
  List.fold_left(
    (ty, (name, def)) => subst(def, Var(name) |> TPat.fresh, ty),
    ty,
    sigma,
  );

/* Each well-formed member paired with its type after substituting the
   type members declared before it. */
let sig_members_closed = (items: list(Sig.t)): list((Sig.member, t)) => {
  let (_, rev) =
    List.fold_left(
      ((sigma, acc), item) =>
        switch (Sig.member_of_item(item)) {
        | Some(Val(_, ty) as m) => (
            sigma,
            [(m, apply_sig_subst(sigma, ty)), ...acc],
          )
        | Some(TypeManifest(name, def) as m) =>
          let def = apply_sig_subst(sigma, def);
          ([(name, def), ...sigma], [(m, def), ...acc]);
        | None => (sigma, acc)
        },
      ([], []),
      items,
    );
  List.rev(rev);
};

/* The type of value member [name] (last declaration wins), closed with
   respect to the signature's own type members. */
let sig_project_value = (items: list(Sig.t), name: Var.t): option(t) =>
  sig_members_closed(items)
  |> List.fold_left(
       (acc, (m: Sig.member, ty)) =>
         switch (m) {
         | Val(x, _) when x == name => Some(ty)
         | _ => acc
         },
       None,
     );

/* The definition of type member [name] (last declaration wins). */
let sig_project_type = (items: list(Sig.t), name: Var.t): option(t) =>
  sig_members_closed(items)
  |> List.fold_left(
       (acc, (m: Sig.member, ty)) =>
         switch (m) {
         | TypeManifest(x, _) when x == name => Some(ty)
         | _ => acc
         },
       None,
     );

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

let rec weak_head_normalize = (~rec_counter=0, ctx: Ctx.t, ty: t): t => {
  if (rec_counter > 1000) {
    failwith("weak_head_normalize exceeded 1000 recursive calls");
  };
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
    switch (label.term) {
    | Label(l) =>
      switch (path_sig(~rec_counter=rec_counter + 1, ctx, t)) {
      | Some(items) =>
        /* `M.T`: type member of a module path or of a signature alias. */
        switch (sig_project_type(items, l)) {
        | Some(ty') =>
          weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty')
        | None => default
        }
      | None =>
        /* `P.x`: label of a labeled tuple type. */
        let normalized_t =
          weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t);
        switch (normalized_t.term) {
        | Prod(tys) => project_type(tys, l) |> Option.value(~default)
        | _ => default // It would be better to do this via a more direct error recovery mechanism in statics
        };
      }
    | _ => default
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
/* The signature items a module path denotes, if any. A path is a variable
   naming a module (looked up in the value namespace once it is not a type
   alias) or a projection of a value member out of another path. A type
   alias whose expansion is a signature also counts, so `S.T` resolves on
   `type S = { type T = Int }`. Never uses Ctx.lookup_alias: it returns an
   invalid-hole type for unbound names, which would shadow the value
   namespace. */
and path_sig = (~rec_counter=0, ctx: Ctx.t, t: t): option(list(Sig.t)) =>
  if (rec_counter > 1000) {
    None;
  } else {
    switch (term_of(t)) {
    | Parens(t)
    | Projector(_, t) => path_sig(~rec_counter=rec_counter + 1, ctx, t)
    | Var(n) =>
      switch (Ctx.lookup_tvar(ctx, n)) {
      | Some(Singleton(alias)) => as_sig(~rec_counter, ctx, alias)
      | Some(Abstract) => None
      | None =>
        switch (Ctx.lookup_var(ctx, n)) {
        | Some({typ, _}) => as_sig(~rec_counter, ctx, typ)
        | None => None
        }
      }
    | ProdProjection(p, {term: Label(l), _}) =>
      switch (path_sig(~rec_counter=rec_counter + 1, ctx, p)) {
      | Some(items) =>
        switch (sig_project_value(items, l)) {
        | Some(ty) => as_sig(~rec_counter, ctx, ty)
        | None => None
        }
      | None => None
      }
    | _ => None
    };
  }
and as_sig = (~rec_counter, ctx: Ctx.t, ty: t): option(list(Sig.t)) => {
  let ty = weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty);
  let ty =
    switch (term_of(ty)) {
    | Rec(_) =>
      weak_head_normalize(~rec_counter=rec_counter + 1, ctx, unroll(ty))
    | _ => ty
    };
  switch (term_of(ty)) {
  | Sig(items) => Some(items)
  | _ => None
  };
};

/* Value members of a signature whose own type is a signature: the
   sub-modules a type-level path may continue through (`M.P.T`). */
let sig_module_member_names = (ctx: Ctx.t, items: list(Sig.t)): list(Var.t) =>
  Sig.members(items)
  |> Sig.value_names
  |> List.filter(x =>
       switch (sig_project_value(items, x)) {
       | Some(ty) => as_sig(~rec_counter=0, ctx, ty) != None
       | None => false
       }
     );

/* ~expand restricts which alias names get expanded (default: all). Used
   by module lowering to expand only module-LOCAL aliases when a member
   type escapes its scope, keeping global/builtin aliases compact. */
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
    Rec(tpat, normalize(Ctx.extend_dummy_tvar(ctx, tpat), ty)) |> rewrap
  | Poly(name, ty) =>
    Poly(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty)) |> rewrap
  | ProofOf(_) => ty // Todo: should we normalize this?
  | Sig(items) =>
    /* Signatures are dependent records: normalize each member in a context
       extended with the type members declared before it. Malformed items
       (holes, non-variable patterns) are dropped from the normal form. */
    let (_, rev) =
      List.fold_left(
        ((ctx, acc), item: Sig.t) =>
          switch (Sig.member_of_item(item)) {
          | Some(Val(x, ty)) => (
              ctx,
              [Sig.item_of_member(Val(x, normalize(ctx, ty))), ...acc],
            )
          | Some(TypeManifest(name, def)) =>
            let item' =
              Sig.item_of_member(TypeManifest(name, normalize(ctx, def)));
            (Ctx.extend_sig_item(ctx, item'), [item', ...acc]);
          | None => (ctx, acc)
          },
        (ctx, []),
        items,
      );
    Sig(List.rev(rev)) |> rewrap;
  };
};

/* Lattice meet on types. This was called 'join' in the 2019 Hazelnut live paper,
   but we're now calling it 'meet' to clarify that Unknown represents the top
   (least precise) element in the precision ordering: specific types dominate Unknown. */

/* [has_fun] with lazy alias resolution: resolves Var heads on demand
   instead of pre-normalizing the whole type. Rec binders shadow their
   name, so bound occurrences don't re-expand through the outer context. */
let has_fun_up_to_aliases = (ctx: Ctx.t, ty: t): bool => {
  let rec go = (~depth, ctx: Ctx.t, ty: t): bool =>
    depth > 256
      ? false
      : (
        switch (term_of(ty)) {
        | Parens(t)
        | Projector(_, t)
        | TupLabel(_, t)
        | ProdProjection(t, _) => go(~depth=depth + 1, ctx, t)
        | Arrow(_)
        | Poly(_)
        | ProofOf(_) => true
        | Var(x) =>
          switch (Ctx.lookup_alias(ctx, x)) {
          | Some(t) => go(~depth=depth + 1, ctx, t)
          | None => false
          }
        | Unknown(_)
        | Atom(_)
        | DrvQuoteTy(_)
        | Label(_)
        | ExplicitNonlabel => false
        | Sig(items) =>
          items
          |> List.fold_left(
               ((ctx, found), item: Sig.t) =>
                 switch (Sig.member_of_item(item)) {
                 | Some(Val(_, t)) => (
                     ctx,
                     found || go(~depth=depth + 1, ctx, t),
                   )
                 | Some(TypeManifest(_)) => (
                     Ctx.extend_sig_item(ctx, item),
                     found,
                   )
                 | None => (ctx, found)
                 },
               (ctx, false),
             )
          |> snd
        | List(t) => go(~depth=depth + 1, ctx, t)
        | Rec(tp, t) =>
          go(~depth=depth + 1, Ctx.extend_dummy_tvar(ctx, tp), t)
        | Sum(sm) =>
          List.exists(
            fun
            | ConstructorMap.Variant(_, _, Some(t)) =>
              go(~depth=depth + 1, ctx, t)
            | _ => false,
            sm,
          )
        | Prod(tys) => List.exists(go(~depth=depth + 1, ctx), tys)
        | ProdExtension(t1, t2) =>
          go(~depth=depth + 1, ctx, t1) || go(~depth=depth + 1, ctx, t2)
        }
      );
  go(~depth=0, ctx, ty);
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
      | (Sig(xs), Sig(ys)) =>
        /* Positional: type members bind their name for later items. */
        let rec go_members = (ctx, xs: list(Sig.t), ys: list(Sig.t)) =>
          switch (xs, ys) {
          | ([], []) => true
          | ([x, ...xs], [y, ...ys]) =>
            switch (Sig.member_of_item(x), Sig.member_of_item(y)) {
            | (Some(Val(n1, t1)), Some(Val(n2, t2))) =>
              n1 == n2 && go(ctx, t1, t2) && go_members(ctx, xs, ys)
            | (Some(TypeManifest(n1, d1)), Some(TypeManifest(n2, d2))) =>
              n1 == n2
              && go(ctx, d1, d2)
              && go_members(
                   Ctx.extend_dummy_tvar(ctx, Var(n1) |> TPat.fresh),
                   xs,
                   ys,
                 )
            | (None, None) => go_members(ctx, xs, ys)
            | _ => false
            }
          | _ => false
          };
        go_members(ctx, xs, ys);
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
          go(Ctx.extend_dummy_tvar(ctx, tp1), x, y)
        | _ => fast_equal(a, b)
        }
      /* Atoms, Unknowns, Labels, and anything alias-free: fast_equal
         already said false above, and heads are now alias-resolved, so
         differing constructors are genuinely unequal. */
      | _ => false
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
    let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
    let ty1' =
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(x2) => subst(Var(x2) |> temp, tp1, ty1)
      | None => ty1
      };
    let+ ty_body = meet(ctx, ty1', ty2);
    Rec(tp1, ty_body) |> temp;
  | (Rec(_), _) => None
  | (Poly(x1, ty1), Poly(x2, ty2)) =>
    let ty1' =
      switch (TPat.tyvar_of_utpat(x2)) {
      | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
      | None => ty1
      };
    let ctx = Ctx.extend_dummy_tvar(ctx, x2);
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
  | (Sig(xs), Sig(ys)) =>
    /* Exact consistency: the same value-member names and the same
       type-member names (order-insensitive), members pairwise consistent.
       Subtyping between signatures lives in ana_meet, not here. */
    let mx = Sig.members(xs) |> Sig.dedup_last;
    let my = Sig.members(ys) |> Sig.dedup_last;
    let same_names = (a, b) =>
      List.sort_uniq(compare, a) == List.sort_uniq(compare, b);
    if (!same_names(Sig.value_names(mx), Sig.value_names(my))
        || !same_names(Sig.type_names(mx), Sig.type_names(my))) {
      None;
    } else {
      let rec go_members = (ctx, ms: list(Sig.member), acc) =>
        switch (ms) {
        | [] => Some(List.rev(acc))
        | [Sig.Val(x, t1), ...ms] =>
          let* t2 = Sig.find_value(my, x);
          let* t = meet(ctx, t1, t2);
          go_members(ctx, ms, [Sig.Val(x, t), ...acc]);
        | [Sig.TypeManifest(n, d1), ...ms] =>
          let* d2 = Sig.find_type_def(my, n);
          let* d = meet(ctx, d1, d2);
          go_members(
            Ctx.extend_alias(ctx, n, Id.invalid, d),
            ms,
            [Sig.TypeManifest(n, d), ...acc],
          );
        };
      let+ ms = go_members(ctx, mx, []);
      Sig(Sig.of_members(ms)) |> temp;
    };
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
  | Sig(items) =>
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
      | EmptyHole => "?"
      | Invalid(s) => s
      | MultiHole(_) => "?"
      };
    "{ " ++ String.concat("; ", List.map(sig_item_str, items)) ++ " }";
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
