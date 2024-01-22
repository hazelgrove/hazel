open Sexplib.Std;
open Util;
open OptUtil.Syntax;

let precedence_Prod = 1;
let precedence_Arrow = 2;
let precedence_Sum = 3;
let precedence_Const = 4;

module rec Typ: {
  /*  TYPE_PROVENANCE:
          From whence does an unknown type originate?
          Note: An unknown's provenance should be sufficient to uniquely identify it.
          If the unknown is one for which global inference results aren't useful, it can be safely treated as NoProvenance.

          We identify three cases:
              NoProvenance:   This unknown is either not unique to any specific term in the program, or it is not derived any such term.
                              Unknowns created outside of the Statics.re often have this provenance.
                              Eg: Any unknown in Evaluator.re or the unknown type associated with a wildcard.

              Term:           This unknown represents a specific term in the program (generally a UPat or UExp).
                              Such provenances are further distinguished by the kind of term they arise from (and potentially why).

              Matched:        This unknown is derived from a specific term in the program through some Matched function.
                              Eg: The unknowns resulting from an invocation of matched_arrow on another unknown
      */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | NoProvenance
    | TypeHole(Id.t)
    | ExpHole(hole_reason, Id.t)
    | Matched(matched_provenance, type_provenance)
  and matched_provenance =
    | Matched_Arrow_Left
    | Matched_Arrow_Right
    | Matched_Prod_N(int)
    | Matched_List
  and hole_reason =
    | EmptyHole
    | Internal
    | PatternVar(Id.t)
    | Error
    | Free(TypVar.t);

  /* TYP.T: Hazel types */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Unknown(type_provenance, is_synswitch)
    | Int
    | Float
    | Bool
    | String
    | Var(TypVar.t)
    | List(t)
    | Arrow(t, t)
    | Sum(sum_map)
    | Prod(list(t))
    | Rec(TypVar.t, t)
  and sum_map = ConstructorMap.t(option(t))
  and is_synswitch = bool;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sum_entry = ConstructorMap.binding(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type equivalence = (t, t)
  and constraints = list(equivalence);

  /* Hazel type annotated with a relevant source location.
     Currently used to track match branches for inconsistent
     branches errors, but could perhaps be used more broadly
     for type debugging UI. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: Id.t,
    ty: t,
  };

  let of_source: list(source) => list(t);
  let join_type_provenance:
    (type_provenance, type_provenance) => type_provenance;
  let matched_arrow: (Ctx.t, Id.t, t) => ((t, t), constraints);
  let matched_prod: (Ctx.t, int, Id.t, t) => (list(t), constraints);
  let matched_list: (Ctx.t, Id.t, t) => (t, constraints);
  let precedence: t => int;
  let subst: (t, TypVar.t, t) => t;
  let unroll: t => t;
  let eq: (t, t) => bool;
  let free_vars: (~bound: list(Var.t)=?, t) => list(Var.t);
  let join: (~resolve: bool=?, ~fix: bool, Ctx.t, t, t) => option(t);
  let join_fix: (~resolve: bool=?, Ctx.t, t, t) => option(t);
  let join_all: (~empty: t, Ctx.t, list(t)) => option(t);
  let is_consistent: (Ctx.t, t, t) => bool;
  let weak_head_normalize: (Ctx.t, t) => t;
  let normalize: (Ctx.t, t) => t;
  let sum_entry: (Constructor.t, sum_map) => option(sum_entry);
  let get_sum_constructors: (Ctx.t, t) => option(sum_map);
  let is_unknown: t => bool;
  let typ_to_string: (t, bool) => string;
  let typ_to_string_with_parens: (bool, t, bool) => string;
  let contains_hole: t => bool;
  let constraints_to_string: constraints => string;
  let equivalence_to_string: equivalence => string;
  let prov_to_string: type_provenance => string;
  let matched_prov_to_string: matched_provenance => string;
  let unknown_synswitch: Id.t => t;
} = {
  // We retain provenances to uniquely identify different unknowns during inference and to retain information on their sources.
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | NoProvenance
    | TypeHole(Id.t)
    | ExpHole(hole_reason, Id.t)
    | Matched(matched_provenance, type_provenance)
  and matched_provenance =
    | Matched_Arrow_Left
    | Matched_Arrow_Right
    | Matched_Prod_N(int)
    | Matched_List
  and hole_reason =
    | EmptyHole
    | Internal
    | PatternVar(Id.t)
    | Error
    | Free(TypVar.t);

  /* TYP.T: Hazel types */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Unknown(type_provenance, is_synswitch)
    | Int
    | Float
    | Bool
    | String
    | Var(TypVar.t)
    | List(t)
    | Arrow(t, t)
    | Sum(sum_map)
    | Prod(list(t))
    | Rec(TypVar.t, t)
  and sum_map = ConstructorMap.t(option(t))
  and is_synswitch = bool;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sum_entry = ConstructorMap.binding(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: Id.t,
    ty: t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type equivalence = (t, t)
  and constraints = list(equivalence);

  /* Strip location information from a list of sources */
  let of_source = List.map((source: source) => source.ty);

  /*  How type provenance information should be collated when joining unknown types.
          TypeHole(id) > ExpHole(reason, id) > Matched(_, prov) > NoProvenance

          Generally, we break ties by first favoring provenances more valuable in inference, and then favoring more informative provenances.
          - TypeHoles are favored over all other provenances. They can directly own displayable inference suggestions
            and are thus favored over others.
          - ExpHoles can be displayable suggestions only if they are constrained to some TypeHole and are thus next in precedence.
          - Matched provenances have precedence over NoProvenance, as NoProvenance unknowns are entirely ignored in inference.
      */
  let join_type_provenance =
      (p1: type_provenance, p2: type_provenance): type_provenance =>
    switch (p1, p2) {
    | (TypeHole(_) as p, _)
    | (_, TypeHole(_) as p) => p
    | (ExpHole(Free(tv1), _), ExpHole(Free(tv2), _))
        when TypVar.eq(tv1, tv2) => p1
    | (ExpHole(PatternVar(_), _) as p, ExpHole(_, _))
    | (ExpHole(_, _), ExpHole(PatternVar(_), _) as p) => p
    | (ExpHole(_, _) as p, _)
    | (_, ExpHole(_, _) as p) => p
    | (Matched(_, _) as p, _)
    | (_, Matched(_, _) as p) => p
    | (NoProvenance, NoProvenance) => p1
    };

  let unknown_synswitch = id => Unknown(ExpHole(Internal, id), true);

  let precedence = (ty: t): int =>
    switch (ty) {
    | Int
    | Float
    | Bool
    | String
    | Unknown(_)
    | Var(_)
    | Rec(_)
    | Sum(_) => precedence_Sum
    | List(_) => precedence_Const
    | Prod(_) => precedence_Prod
    | Arrow(_, _) => precedence_Arrow
    };

  let rec subst = (s: t, x: TypVar.t, ty: t) => {
    switch (ty) {
    | Int => Int
    | Float => Float
    | Bool => Bool
    | String => String
    | Unknown(prov, flags) => Unknown(prov, flags)
    | Arrow(ty1, ty2) => Arrow(subst(s, x, ty1), subst(s, x, ty2))
    | Prod(tys) => Prod(List.map(subst(s, x), tys))
    | Sum(sm) => Sum(ConstructorMap.map(Option.map(subst(s, x)), sm))
    | Rec(y, ty) when TypVar.eq(x, y) => Rec(y, ty)
    | Rec(y, ty) => Rec(y, subst(s, x, ty))
    | List(ty) => List(subst(s, x, ty))
    | Var(y) => TypVar.eq(x, y) ? s : Var(y)
    };
  };

  let unroll = (ty: t): t =>
    switch (ty) {
    | Rec(x, ty_body) => subst(ty, x, ty_body)
    | _ => ty
    };

  /* Type Equality: At the moment, this coincides with alpha equivalence,
     but this will change when polymorphic types are implemented */
  let rec eq = (t1: t, t2: t): bool => {
    switch (t1, t2) {
    | (Rec(x1, t1), Rec(x2, t2)) => eq(t1, subst(Var(x1), x2, t2))
    | (Rec(_), _) => false
    | (Int, Int) => true
    | (Int, _) => false
    | (Float, Float) => true
    | (Float, _) => false
    | (Bool, Bool) => true
    | (Bool, _) => false
    | (String, String) => true
    | (String, _) => false
    | (Unknown(_), Unknown(_)) => true
    | (Unknown(_), _) => false
    | (Arrow(t1, t2), Arrow(t1', t2')) => eq(t1, t1') && eq(t2, t2')
    | (Arrow(_), _) => false
    | (Prod(tys1), Prod(tys2)) => List.equal(eq, tys1, tys2)
    | (Prod(_), _) => false
    | (List(t1), List(t2)) => eq(t1, t2)
    | (List(_), _) => false
    | (Sum(sm1), Sum(sm2)) =>
      ConstructorMap.equal(Option.equal(eq), sm1, sm2)
    | (Sum(_), _) => false
    | (Var(n1), Var(n2)) => n1 == n2
    | (Var(_), _) => false
    };
  };

  let rec prov_to_string = (prov: type_provenance): string => {
    switch (prov) {
    | NoProvenance => ""
    | Matched(mprov, type_provenance) =>
      matched_prov_to_string(mprov)
      ++ "{"
      ++ prov_to_string(type_provenance)
      ++ "}"
    | ExpHole(Free(var), _) => var
    | TypeHole(id) => "TypeHole(" ++ Id.to_string(id) ++ ")"
    | ExpHole(reason, id) =>
      "ExpHole("
      ++ reason_to_string(reason)
      ++ ", "
      ++ Id.to_string(id)
      ++ ")"
    };
  }
  and matched_prov_to_string = (mprov: matched_provenance): string => {
    switch (mprov) {
    | Matched_Arrow_Left => "M->L"
    | Matched_Arrow_Right => "M->R"
    | Matched_Prod_N(n) => "M*#" ++ string_of_int(n)
    | Matched_List => "M[]"
    };
  }
  and reason_to_string = (reason: hole_reason): string => {
    switch (reason) {
    | EmptyHole => "EmptyHole"
    | Internal => "Internal"
    | PatternVar(_) => "PatternVar"
    | Error => "Error"
    | Free(_) => "Free"
    };
  };

  let rec typ_to_string = (ty: t, debug): string => {
    typ_to_string_with_parens(false, ty, debug);
  }
  and typ_to_string_with_parens = (is_left_child: bool, ty: t, debug): string => {
    let parenthesize_if_left_child = s => is_left_child ? "(" ++ s ++ ")" : s;
    switch (ty) {
    | Unknown(prov, _) => "?" ++ (debug ? prov_to_string(prov) : "")
    | Int => "Int"
    | Float => "Float"
    | String => "String"
    | Bool => "Bool"
    | Var(name) => name
    | List(t) => "[" ++ typ_to_string(t, debug) ++ "]"
    | Arrow(t1, t2) =>
      typ_to_string_with_parens(true, t1, debug)
      ++ " -> "
      ++ typ_to_string(t2, debug)
      |> parenthesize_if_left_child
    | Prod([]) => "()"
    | Prod([_]) => "BadProduct"
    | Prod([t0, ...ts]) =>
      "("
      ++ List.fold_left(
           (acc, t) => acc ++ ", " ++ typ_to_string(t, debug),
           typ_to_string(t0, debug),
           ts,
         )
      ++ ")"
    | Sum(ctr_map) =>
      (
        switch (ctr_map) {
        | [] => "Nullary Sum"
        | [t0] => "+" ++ ctr_to_string(is_left_child, t0, debug)
        | [t0, ...ts] =>
          List.fold_left(
            (acc, hd) =>
              acc ++ " + " ++ ctr_to_string(is_left_child, hd, debug),
            ctr_to_string(is_left_child, t0, debug),
            ts,
          )
        }
      )
      |> parenthesize_if_left_child
    | Rec(var, body) =>
      "Rec "
      ++ var
      ++ ". "
      ++ typ_to_string_with_parens(is_left_child, body, debug)
    };
  }
  and ctr_to_string = (is_left_child, (ctr, typ), debug): string => {
    switch (typ) {
    | None => ctr
    | Some(typ) =>
      ctr
      ++ "("
      ++ typ_to_string_with_parens(is_left_child, typ, debug)
      ++ ")"
    };
  };

  let rec constraints_to_string = (constraints: constraints) => {
    String.concat("\n", List.map(equivalence_to_string, constraints));
  }
  and equivalence_to_string = (equivalence: equivalence) => {
    let (a, b) = equivalence;
    String.concat(
      "",
      [
        "(",
        Typ.typ_to_string(a, true),
        ", ",
        Typ.typ_to_string(b, true),
        ")",
      ],
    );
  };

  let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
    switch (ty) {
    | Unknown(_)
    | Int
    | Float
    | Bool
    | String => []
    | Var(v) => List.mem(v, bound) ? [] : [v]
    | List(ty) => free_vars(~bound, ty)
    | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
    | Sum(sm) =>
      ListUtil.flat_map(
        fun
        | None => []
        | Some(typ) => free_vars(~bound, typ),
        List.map(snd, sm),
      )
    | Prod(tys) => ListUtil.flat_map(free_vars(~bound), tys)
    | Rec(x, ty) => free_vars(~bound=[x, ...bound], ty)
    };

  /* Lattice join on types. This is a LUB join in the hazel2
     sense in that any type dominates Unknown. The optional
     resolve parameter specifies whether, in the case of a type
     variable and a succesful join, to return the resolved join type,
     or to return the (first) type variable for readability */
  let rec join =
          (~resolve=false, ~fix, ctx: Ctx.t, ty1: t, ty2: t): option(t) => {
    let join' = join(~resolve, ~fix, ctx);
    switch (ty1, ty2) {
    | (_, Unknown(ExpHole(Free(_), _) | TypeHole(_), _) as ty) when fix =>
      /* NOTE(andrew): This is load bearing
         for ensuring that function literals get appropriate
         casts. Examples/Dynamics has regression tests */
      Some(ty)
    | (Unknown(p1, s1), Unknown(p2, s2)) =>
      Some(Unknown(join_type_provenance(p1, p2), s1 && s2))
    | (Unknown(_), ty)
    | (ty, Unknown(_)) => Some(ty)
    | (Var(n1), Var(n2)) =>
      if (n1 == n2) {
        Some(Var(n1));
      } else {
        let* ty1 = Ctx.lookup_alias(ctx, n1);
        let* ty2 = Ctx.lookup_alias(ctx, n2);
        let+ ty_join = join'(ty1, ty2);
        !resolve && eq(ty1, ty_join) ? Var(n1) : ty_join;
      }
    | (Var(name), ty)
    | (ty, Var(name)) =>
      let* ty_name = Ctx.lookup_alias(ctx, name);
      let+ ty_join = join'(ty_name, ty);
      !resolve && eq(ty_name, ty_join) ? Var(name) : ty_join;
    /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
    | (Rec(x1, ty1), Rec(x2, ty2)) =>
      /* TODO:
           This code isn't fully correct, as we may be doing
           substitution on open terms; if x1 occurs in ty2,
           we should be substituting x1 for a fresh variable
           in ty2. This is annoying, and should be obviated
           by the forthcoming debruijn index implementation
         */
      let ctx = Ctx.extend_dummy_tvar(ctx, x1);
      let+ ty_body =
        join(~resolve, ~fix, ctx, ty1, subst(Var(x1), x2, ty2));
      Rec(x1, ty_body);
    | (Rec(_), _) => None
    | (Int, Int) => Some(Int)
    | (Int, _) => None
    | (Float, Float) => Some(Float)
    | (Float, _) => None
    | (Bool, Bool) => Some(Bool)
    | (Bool, _) => None
    | (String, String) => Some(String)
    | (String, _) => None
    | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
      let* ty1 = join'(ty1, ty1');
      let+ ty2 = join'(ty2, ty2');
      Arrow(ty1, ty2);
    | (Arrow(_), _) => None
    | (Prod(tys1), Prod(tys2)) =>
      let* tys = ListUtil.map2_opt(join', tys1, tys2);
      let+ tys = OptUtil.sequence(tys);
      Prod(tys);
    | (Prod(_), _) => None
    | (Sum(sm1), Sum(sm2)) =>
      let (sorted1, sorted2) =
        /* If same order, retain order for UI */
        ConstructorMap.same_constructors_same_order(sm1, sm2)
          ? (sm1, sm2)
          : (ConstructorMap.sort(sm1), ConstructorMap.sort(sm2));
      let* ty =
        ListUtil.map2_opt(
          join_sum_entries(~resolve, ~fix, ctx),
          sorted1,
          sorted2,
        );
      let+ ty = OptUtil.sequence(ty);
      Sum(ty);
    | (Sum(_), _) => None
    | (List(ty1), List(ty2)) =>
      let+ ty = join'(ty1, ty2);
      List(ty);
    | (List(_), _) => None
    };
  }
  and join_sum_entries =
      (
        ~resolve,
        ~fix,
        ctx: Ctx.t,
        (ctr1, ty1): sum_entry,
        (ctr2, ty2): sum_entry,
      )
      : option(sum_entry) =>
    switch (ty1, ty2) {
    | (None, None) when ctr1 == ctr2 => Some((ctr1, None))
    | (Some(ty1), Some(ty2)) when ctr1 == ctr2 =>
      let+ ty_join = join(~resolve, ~fix, ctx, ty1, ty2);
      (ctr1, Some(ty_join));
    | _ => None
    };

  let join_fix = join(~fix=true);

  let join_all = (~empty: t, ctx: Ctx.t, ts: list(t)): option(t) =>
    List.fold_left(
      (acc, ty) => OptUtil.and_then(join(~fix=false, ctx, ty), acc),
      Some(empty),
      ts,
    );

  let rec contains_hole = (ty: t): bool =>
    switch (ty) {
    | Unknown(_) => true
    | Arrow(ty1, ty2) => contains_hole(ty1) || contains_hole(ty2)
    | Sum(tys) =>
      tys |> List.filter_map(((_, b)) => b) |> List.exists(contains_hole)
    | Prod(tys) => List.exists(contains_hole, tys)
    | _ => false
    };

  let is_consistent = (ctx: Ctx.t, ty1: t, ty2: t): bool =>
    join(~fix=false, ctx, ty1, ty2) != None;

  let rec weak_head_normalize = (ctx: Ctx.t, ty: t): t =>
    switch (ty) {
    | Var(x) =>
      switch (Ctx.lookup_alias(ctx, x)) {
      | Some(ty) => weak_head_normalize(ctx, ty)
      | None => ty
      }
    | _ => ty
    };

  let rec normalize = (ctx: Ctx.t, ty: t): t => {
    switch (ty) {
    | Var(x) =>
      switch (Ctx.lookup_alias(ctx, x)) {
      | Some(ty) => normalize(ctx, ty)
      | None => ty
      }
    | Unknown(_)
    | Int
    | Float
    | Bool
    | String => ty
    | List(t) => List(normalize(ctx, t))
    | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
    | Prod(ts) => Prod(List.map(normalize(ctx), ts))
    | Sum(ts) => Sum(ConstructorMap.map(Option.map(normalize(ctx)), ts))
    | Rec(name, ty) =>
      /* NOTE: Dummy tvar added has fake id but shouldn't matter
         as in current implementation Recs do not occur in the
         surface syntax, so we won't try to jump to them. */
      Rec(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty))
    };
  };

  let matched_arrow =
      (ctx: Ctx.t, termId: Id.t, ty: t): ((t, t), Typ.constraints) => {
    let matched_arrow_of_prov = (prov, flags) => {
      let (arrow_lhs, arrow_rhs) = (
        Unknown(Matched(Matched_Arrow_Left, prov), flags),
        Unknown(Matched(Matched_Arrow_Right, prov), flags),
      );
      (
        (arrow_lhs, arrow_rhs),
        [(Unknown(prov, flags), Arrow(arrow_lhs, arrow_rhs))],
      );
    };
    switch (weak_head_normalize(ctx, ty)) {
    | Arrow(ty_in, ty_out) => ((ty_in, ty_out), [])
    | Unknown(prov, flags) => matched_arrow_of_prov(prov, flags)
    | _ => matched_arrow_of_prov(ExpHole(Error, termId), false)
    };
  };

  let matched_prod = (ctx: Ctx.t, length, termId: Id.t, ty: t) => {
    let matched_prod_of_prov = (prov, flags) => {
      let matched_prod_typs =
        List.init(length, n =>
          Unknown(Matched(Matched_Prod_N(n), prov), flags)
        );
      (
        matched_prod_typs,
        [(Unknown(prov, flags), Prod(matched_prod_typs))],
      );
    };
    switch (weak_head_normalize(ctx, ty)) {
    | Prod(tys) when List.length(tys) == length => (tys, [])
    | Unknown(prov, flags) => matched_prod_of_prov(prov, flags)
    | _ => matched_prod_of_prov(ExpHole(Error, termId), false)
    };
  };

  let matched_list = (_ctx: Ctx.t, termId: Id.t, ty: t) => {
    let matched_list_of_prov = (prov, flags) => {
      let list_elts_typ = Unknown(Matched(Matched_List, prov), flags);
      (list_elts_typ, [(Unknown(prov, flags), List(list_elts_typ))]);
    };
    switch (ty) {
    | List(ty) => (ty, [])
    | Unknown(prov, flags) => matched_list_of_prov(prov, flags)
    | _ => matched_list_of_prov(ExpHole(Error, termId), false)
    };
  };

  let sum_entry = (ctr: Constructor.t, ctrs: sum_map): option(sum_entry) =>
    List.find_map(
      fun
      | (t, typ) when Constructor.equal(t, ctr) => Some((t, typ))
      | _ => None,
      ctrs,
    );

  let get_sum_constructors = (ctx: Ctx.t, ty: t): option(sum_map) => {
    let ty = weak_head_normalize(ctx, ty);
    switch (ty) {
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
        switch (ty) {
        | Rec(x, ty_body) =>
          switch (Ctx.lookup_alias(ctx, x)) {
          | None => unroll(ty)
          | Some(_) => ty_body
          }
        | _ => ty
        };
      switch (ty) {
      | Sum(sm) => Some(sm)
      | _ => None
      };
    | _ => None
    };
  };

  let is_unknown = (ty: t): bool =>
    switch (ty) {
    | Unknown(_) => true
    | _ => false
    };
}
and Ctx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Var.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tvar_entry = {
    name: TypVar.t,
    id: Id.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | ConstructorEntry(var_entry)
    | TVarEntry(tvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  let extend: (t, entry) => t;
  let extend_tvar: (t, tvar_entry) => t;
  let extend_alias: (t, TypVar.t, Id.t, Typ.t) => t;
  let extend_dummy_tvar: (t, TypVar.t) => t;
  let lookup_tvar: (t, TypVar.t) => option(tvar_entry);
  let lookup_alias: (t, TypVar.t) => option(Typ.t);
  let get_id: entry => Id.t;
  let lookup_var: (t, string) => option(var_entry);
  let lookup_ctr: (t, string) => option(var_entry);
  let is_alias: (t, TypVar.t) => bool;
  let add_ctrs: (t, TypVar.t, Id.t, Typ.sum_map) => t;
  let subtract_prefix: (t, t) => option(t);
  let added_bindings: (t, t) => t;
  let filter_duplicates: t => t;
  let shadows_typ: (t, TypVar.t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Var.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tvar_entry = {
    name: TypVar.t,
    id: Id.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | ConstructorEntry(var_entry)
    | TVarEntry(tvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  let extend = (ctx, entry) => List.cons(entry, ctx);

  let extend_tvar = (ctx: t, tvar_entry: tvar_entry): t =>
    extend(ctx, TVarEntry(tvar_entry));

  let extend_alias = (ctx: t, name: TypVar.t, id: Id.t, ty: Typ.t): t =>
    extend_tvar(ctx, {name, id, kind: Singleton(ty)});

  let extend_dummy_tvar = (ctx: t, name: TypVar.t) =>
    extend_tvar(ctx, {kind: Abstract, name, id: Id.invalid});

  let lookup_tvar = (ctx: t, name: TypVar.t): option(tvar_entry) =>
    List.find_map(
      fun
      | TVarEntry(v) when v.name == name => Some(v)
      | _ => None,
      ctx,
    );

  let lookup_alias = (ctx: t, t: TypVar.t): option(Typ.t) =>
    switch (lookup_tvar(ctx, t)) {
    | Some({kind: Singleton(ty), _}) => Some(ty)
    | Some({kind: Abstract, _})
    | None => None
    };

  let get_id: entry => Id.t =
    fun
    | VarEntry({id, _})
    | ConstructorEntry({id, _})
    | TVarEntry({id, _}) => id;

  let lookup_var = (ctx: t, name: string): option(var_entry) =>
    List.find_map(
      fun
      | VarEntry(v) when v.name == name => Some(v)
      | _ => None,
      ctx,
    );

  let lookup_ctr = (ctx: t, name: string): option(var_entry) =>
    List.find_map(
      fun
      | ConstructorEntry(t) when t.name == name => Some(t)
      | _ => None,
      ctx,
    );

  let is_alias = (ctx: t, name: TypVar.t): bool =>
    switch (lookup_alias(ctx, name)) {
    | Some(_) => true
    | None => false
    };

  let add_ctrs = (ctx: t, name: TypVar.t, id: Id.t, ctrs: Typ.sum_map): t =>
    List.map(
      ((ctr, typ)) =>
        ConstructorEntry({
          name: ctr,
          id,
          typ:
            switch (typ) {
            | None => Var(name)
            | Some(typ) => Arrow(typ, Var(name))
            },
        }),
      ctrs,
    )
    @ ctx;

  let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
    // NOTE: does not check that the prefix is an actual prefix
    let prefix_length = List.length(prefix_ctx);
    let ctx_length = List.length(ctx);
    if (prefix_length > ctx_length) {
      None;
    } else {
      Some(
        List.rev(
          ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx)),
        ),
      );
    };
  };

  let added_bindings = (ctx_after: t, ctx_before: t): t => {
    /* Precondition: new_ctx is old_ctx plus some new bindings */
    let new_count = List.length(ctx_after) - List.length(ctx_before);
    switch (ListUtil.split_n_opt(new_count, ctx_after)) {
    | Some((ctx, _)) => ctx
    | _ => []
    };
  };

  module VarSet = Set.Make(Var);

  // Note: filter out duplicates when rendering
  let filter_duplicates = (ctx: t): t =>
    ctx
    |> List.fold_left(
         ((ctx, term_set, typ_set), entry) => {
           switch (entry) {
           | VarEntry({name, _})
           | ConstructorEntry({name, _}) =>
             VarSet.mem(name, term_set)
               ? (ctx, term_set, typ_set)
               : ([entry, ...ctx], VarSet.add(name, term_set), typ_set)
           | TVarEntry({name, _}) =>
             VarSet.mem(name, typ_set)
               ? (ctx, term_set, typ_set)
               : ([entry, ...ctx], term_set, VarSet.add(name, typ_set))
           }
         },
         ([], VarSet.empty, VarSet.empty),
       )
    |> (((ctx, _, _)) => List.rev(ctx));

  let shadows_typ = (ctx: t, name: TypVar.t): bool =>
    Form.is_base_typ(name) || lookup_alias(ctx, name) != None;
}
and Kind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;
};
