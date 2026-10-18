open Util;

/* INFERENCE.re — type hole inference for implicit polymorphism.

   Statics.mk runs the bidirectional statics twice. The inference pass gives
   identities to *implicit* type holes, the unknown types nobody wrote:

   - binder holes: the type of an unannotated binder whose expected type
     carries no information (`fun x -> …`);
   - instantiation holes: the type arguments of a `poly`-typed expression
     used where a non-`poly` type is expected (`id(1)` is `id@<?>(1)`);
   - derived holes: the domain and codomain of a hole-typed callee.

   At each `let x = def` it solves the constraints recorded inside `def` and
   generalizes over the holes nothing constrains (marking paper §4.4: a hole
   an expression hole could still constrain, `etc`, is not generalized). One
   solve over the whole program then fills every hole. The elaboration pass
   re-runs statics with the fills morally inserted: binders read their fill,
   instantiations elaborate to `TypAp`, generalized lets to `TypFun`. Holes
   whose constraints conflict stay `?`, and the conflict is marked at the
   hole's site rather than at whichever use happened to come last. */

[@deriving (show({with_path: false}), sexp, yojson)]
type derived_tag =
  | ArrowL
  | ArrowR;

[@deriving (show({with_path: false}), sexp, yojson)]
type hole_kind =
  | Binder
  | Inst(int)
  | Derived(Id.t, derived_tag)
  | Opaque;

type hole = {
  site: Id.t,
  kind: hole_kind,
};

/* A generalized let: its type variables in binder order, and the holes
   each one replaces. */
[@deriving (show({with_path: false}), sexp, yojson)]
type gen = {
  tvars: list(string),
  holes: list((Id.t, string)),
};

type constraint_ = (Ctx.t, Typ.t, Typ.t);

type decision =
  | Decided(Typ.t)
  | Conflicted(list(Typ.t));

type infer_state = {
  holes: Hashtbl.t(Id.t, hole),
  extras: Hashtbl.t(Id.t, list(constraint_)),
  gens: Hashtbl.t(Id.t, gen),
  instantiated: Hashtbl.t(Id.t, unit),
  /* Holes something consumed the way only `?` allows (see gradual_use). */
  gradual: Hashtbl.t(Id.t, unit),
  /* What a let's definition alone decided for the holes it owns. */
  decided: Hashtbl.t(Id.t, decision),
};

type apply_state = {
  fills: Hashtbl.t(Id.t, (Typ.t, Typ.t)) /* as solved, and alias-normalized */,
  conflicts: Hashtbl.t(Id.t, list(Typ.t)) /* keyed by site */,
  gens: Hashtbl.t(Id.t, gen),
  instantiated: Hashtbl.t(Id.t, unit),
};

type mode =
  | Off
  | Infer(infer_state)
  | Apply(apply_state);

let mode: ref(mode) = ref(Off);

/* For comparing against plain bidirectional statics, and for timing the
   passes, from the CLI. */
let disabled = Sys.getenv_opt("HAZEL_INFER_OFF") != None;
let timing = Sys.getenv_opt("HAZEL_INFER_TIMING") != None;
let now = () => timing ? Sys.time() : 0.;
let let_time = ref(0.);

/* Hole ids are derived from their site so both passes agree on them. The
   cache spans runs (sites are mostly stable across edits), within a bound. */
let id_cache: Hashtbl.t((Id.t, string), Id.t) = Hashtbl.create(256);
let derive = (salt: string, id: Id.t): Id.t =>
  switch (Hashtbl.find_opt(id_cache, (id, salt))) {
  | Some(d) => d
  | None =>
    if (Hashtbl.length(id_cache) > 100_000) {
      Hashtbl.reset(id_cache);
    };
    let d = Id.derive(~salt, id);
    Hashtbl.add(id_cache, (id, salt), d);
    d;
  };

/* Ids for the nodes the elaboration adds (see Id.derive_implicit). */
let derive_implicit = (salt: string, id: Id.t): Id.t =>
  switch (Hashtbl.find_opt(id_cache, (id, "!" ++ salt))) {
  | Some(d) => d
  | None =>
    let d = Id.derive_implicit(~salt, id);
    Hashtbl.add(id_cache, (id, "!" ++ salt), d);
    d;
  };

let salt_of_kind: hole_kind => string =
  fun
  | Binder => "implicit-binder"
  | Inst(i) => "implicit-inst-" ++ string_of_int(i)
  | Derived(_, ArrowL) => "implicit-arrow-l"
  | Derived(_, ArrowR) => "implicit-arrow-r"
  | Opaque => "implicit-opaque";

let hole_id = (site: Id.t, kind: hole_kind): Id.t =>
  switch (kind) {
  | Derived(parent, _) => derive(salt_of_kind(kind), parent)
  | Binder
  | Inst(_)
  | Opaque => derive(salt_of_kind(kind), site)
  };

let hole_typ = (id: Id.t): Typ.t =>
  IdTagged.mk_internal([id], Unknown(Hole(EmptyHole)): Typ.term);

let unknown = () => Typ.temp(Unknown(Internal));

let new_hole = (st: infer_state, site: Id.t, kind: hole_kind): Typ.t => {
  let id = hole_id(site, kind);
  Hashtbl.replace(
    st.holes,
    id,
    {
      site,
      kind,
    },
  );
  hole_typ(id);
};

let rec strip = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Parens(t)
  | Projector(_, t)
  | TupLabel({term: ExplicitNonlabel, _}, t) => strip(t)
  | _ => ty
  };

let hole_of = (st: infer_state, ty: Typ.t): option(Id.t) => {
  let ty = strip(ty);
  switch (Typ.term_of(ty)) {
  | Unknown(Hole(EmptyHole)) when Hashtbl.mem(st.holes, Typ.rep_id(ty)) =>
    Some(Typ.rep_id(ty))
  | _ => None
  };
};

/* The site a hole ultimately belongs to: derived holes inherit theirs. */
let rec root_site = (st: infer_state, id: Id.t): option(Id.t) =>
  switch (Hashtbl.find_opt(st.holes, id)) {
  | Some({kind: Derived(parent, _), _}) => root_site(st, parent)
  | Some({site, _}) => Some(site)
  | None => None
  };

/* An expected type with no information for a binder to take on. A type
   hole the user wrote is information: it says "gradual here". */
let uninformative = (ctx: Ctx.t, ana: Typ.t): bool =>
  switch (Typ.term_of(Typ.weak_head_normalize(ctx, strip(ana)))) {
  | Unknown(SynSwitch | Internal) => true
  | _ => false
  };

/* ===== Hooks called by Statics ===== */

let free_in_scope = (ctx: Ctx.t, ty: Typ.t): bool =>
  List.for_all(v => Ctx.lookup_tvar(ctx, v) != None, Typ.free_vars(ty));

/* A fill that mentions an alias out of scope at the use site falls back to
   its normalized form; failing that, to `?`. */
let fill_at = (ctx: Ctx.t, (fill, fill_norm): (Typ.t, Typ.t)): Typ.t =>
  if (free_in_scope(ctx, fill)) {
    fill;
  } else if (free_in_scope(ctx, fill_norm)) {
    fill_norm;
  } else {
    unknown();
  };

/* The type of an unannotated binder, when inference has one to give. */
let binder_type =
    (~ctx: Ctx.t, ~is_synswitch: bool, ~site: Id.t, ~ana: Typ.t)
    : option(Typ.t) =>
  switch (mode^) {
  | Off => None
  | _ when is_synswitch => None
  | Infer(st) =>
    switch (hole_of(st, ana)) {
    | Some(_) => Some(strip(ana)) /* already a hole; the binder shares it */
    | None when uninformative(ctx, ana) => Some(new_hole(st, site, Binder))
    | None => None
    }
  | Apply(_) when !uninformative(ctx, ana) => None
  | Apply(ap) =>
    Hashtbl.find_opt(ap.fills, hole_id(site, Binder))
    |> Option.map(fill_at(ctx))
  };

/* Only where an instance is needed: a known, non-`poly` type is expected.
   A `poly` is consistent with `?`, so an unknown expectation keeps it. */
let wants_instance = (ctx: Ctx.t, ana: Typ.t): bool =>
  switch (Typ.term_of(Typ.weak_head_normalize(ctx, strip(ana)))) {
  | Unknown(_)
  | Poly(_) => false
  | _ => true
  };

let rec poly_binders = (ctx: Ctx.t, ty: Typ.t): option((TPat.t, Typ.t)) =>
  switch (Typ.term_of(strip(ty))) {
  /* Only these heads can normalize to a `poly`; skip the lookup for the
     rest, as this runs for every expression. */
  | Poly(_)
  | Var(_)
  | ProdProjection(_) =>
    switch (Typ.term_of(Typ.weak_head_normalize(ctx, strip(ty)))) {
    | Poly(tp, body) => Some((tp, body))
    | Parens(t) => poly_binders(ctx, t)
    | _ => None
    }
  | _ => None
  };

/* A `poly`-typed expression used where a non-`poly` type is expected is
   implicitly applied to one type argument per leading binder. */
let instantiate =
    (~ctx: Ctx.t, ~site: Id.t, ~ana: Typ.t, elab: Exp.t, syn: Typ.t)
    : (Exp.t, Typ.t, option(Message.instance)) =>
  switch (mode^) {
  | Off => (elab, syn, None)
  | _ when poly_binders(ctx, syn) == None => (elab, syn, None)
  /* Expected to be an inference hole: instantiate as ML would, so the
     hole is solved to the instance (the elaboration pass replays this). */
  | Infer(st) when !wants_instance(ctx, ana) && hole_of(st, ana) == None => (
      elab,
      syn,
      None,
    )
  | Apply(ap)
      when !wants_instance(ctx, ana) && !Hashtbl.mem(ap.instantiated, site) => (
      elab,
      syn,
      None,
    )
  | m =>
    switch (m) {
    | Infer(st) => Hashtbl.replace(st.instantiated, site, ())
    | _ => ()
    };
    let arg = i =>
      switch (m) {
      | Infer(st) => new_hole(st, site, Inst(i))
      | Apply(ap) =>
        switch (Hashtbl.find_opt(ap.fills, hole_id(site, Inst(i)))) {
        | Some(fill) => fill_at(ctx, fill)
        | None => unknown()
        }
      | Off => unknown()
      };
    let rec go = (i, elab, ty, args) =>
      switch (poly_binders(ctx, ty)) {
      | Some((tp, body)) =>
        let a = arg(i);
        let elab =
          IdTagged.mk_internal(
            [derive_implicit("implicit-typap-" ++ string_of_int(i), site)],
            TypAp(elab, Typ.normalize(ctx, a)): Exp.term,
          );
        go(i + 1, elab, Typ.subst(a, tp, body), [a, ...args]);
      | None => (elab, ty, List.rev(args))
      };
    let (elab, ty, args) = go(0, elab, syn, []);
    (
      elab,
      ty,
      Some({
        poly: syn,
        args,
      }),
    );
  };

/* The domain and codomain of an applied callee; a hole-typed callee gets
   derived holes so what flows in and out of it stays connected. */
let matched_arrow =
    (ctx: Ctx.t, ~fn_syn: Typ.t, ~fn_ty: Typ.t): (Typ.t, Typ.t) =>
  switch (mode^) {
  | Infer(st) =>
    switch (hole_of(st, fn_syn)) {
    | Some(parent) => (
        new_hole(st, parent, Derived(parent, ArrowL)),
        new_hole(st, parent, Derived(parent, ArrowR)),
      )
    | None => MatchedTyp.arrow_tolerant(ctx, fn_ty)
    }
  | _ => MatchedTyp.arrow_tolerant(ctx, fn_ty)
  };

/* Branches whose types are joined (`if`, `case`, list literals) constrain
   each other, beyond what each one's expected type says. */
let record_join = (~site: Id.t, ~ctx: Ctx.t, tys: list(Typ.t)): unit =>
  switch (mode^, tys) {
  | (Infer(st), [t, ...ts]) =>
    Hashtbl.replace(st.extras, site, List.map(t' => (ctx, t, t'), ts))
  | _ => ()
  };

/* ===== Solver: union-find over types with implicit holes ===== */

module Solver = {
  type node = {
    uid: int,
    mutable parent: option(node),
    mutable shape: option(shape),
    mutable conflict: list(shape),
    mutable etc: bool,
    mutable holes: list(Id.t),
  }
  and shape =
    | Arrow(node, node)
    | Prod(list(node))
    | List(node)
    | Label(string, node)
    | Leaf(Typ.t, Ctx.t);

  type t = {
    st: infer_state,
    nodes: Hashtbl.t(Id.t, node),
    rigid: Hashtbl.t(Id.t, string),
    mutable count: int,
  };

  let create = (st, rigid) => {
    st,
    nodes: Hashtbl.create(64),
    rigid,
    count: 0,
  };

  let fresh = (s: t): node => {
    s.count = s.count + 1;
    {
      uid: s.count,
      parent: None,
      shape: None,
      conflict: [],
      etc: false,
      holes: [],
    };
  };

  let rec find = (n: node): node =>
    switch (n.parent) {
    | None => n
    | Some(p) =>
      let r = find(p);
      n.parent = Some(r);
      r;
    };

  let leaf_equal = ((t1, c1): (Typ.t, Ctx.t), (t2, c2): (Typ.t, Ctx.t)) =>
    switch (Typ.term_of(t1), Typ.term_of(t2)) {
    | (Atom(a), Atom(b)) => a == b
    | (Var(a), Var(b)) when a == b => true
    | _ =>
      Typ.fast_equal(Typ.normalize(c1, t1), Typ.normalize(c2, t2))
      || Typ.fast_equal(t1, t2)
    };

  let rec union = (a: node, b: node): unit => {
    let ra = find(a);
    let rb = find(b);
    if (ra !== rb) {
      rb.parent = Some(ra);
      ra.etc = ra.etc || rb.etc;
      ra.holes = ra.holes @ rb.holes;
      ra.conflict = ra.conflict @ rb.conflict;
      switch (ra.shape, rb.shape) {
      | (None, s) => ra.shape = s
      | (Some(_), None) => ()
      | (Some(s1), Some(s2)) => unify_shapes(ra, s1, s2)
      };
    };
  }
  and unify_shapes = (r: node, s1: shape, s2: shape): unit =>
    switch (s1, s2) {
    | (Arrow(a1, b1), Arrow(a2, b2)) =>
      union(a1, a2);
      union(b1, b2);
    | (Prod(ns1), Prod(ns2)) =>
      switch (align(ns1, ns2)) {
      | Some(pairs) => List.iter(((a, b)) => union(a, b), pairs)
      | None when labeled(ns1) || labeled(ns2) => ()
      | None => r.conflict = r.conflict @ [s2]
      }
    | (List(n1), List(n2)) => union(n1, n2)
    | (Label(l1, n1), Label(l2, n2)) when l1 == l2 => union(n1, n2)
    | (Label(_), Label(_)) => ()
    /* A labeled tuple and its labels' payload are interchangeable where
       Hazel lifts singletons; say nothing rather than something wrong. */
    | (Label(_), _)
    | (_, Label(_))
    | (Prod([_]), _)
    | (_, Prod([_])) => ()
    | (Leaf(t1, c1), Leaf(t2, c2)) when leaf_equal((t1, c1), (t2, c2)) =>
      ()
    | _ => r.conflict = r.conflict @ [s2]
    }
  /* Pair up tuple elements as labeled-tuple statics does: labeled ones by
     label, the rest by position; None if they don't line up. */
  and align =
      (ns1: list(node), ns2: list(node)): option(list((node, node))) => {
    let label_of = n =>
      switch (find(n).shape) {
      | Some(Label(l, inner)) => (Some(l), inner)
      | _ => (None, n)
      };
    let es1 = List.map(label_of, ns1);
    let es2 = List.map(label_of, ns2);
    if (List.length(es1) != List.length(es2)) {
      None;
    } else {
      let by_label =
        List.filter_map(
          ((l, n1)) =>
            switch (l) {
            | Some(l) =>
              List.find_map(
                ((l2, n2)) => l2 == Some(l) ? Some((l, n1, n2)) : None,
                es2,
              )
            | None => None
            },
          es1,
        );
      let matched = List.map(((l, _, _)) => Some(l), by_label);
      let rest = es =>
        List.filter_map(
          ((l, n)) => List.mem(l, matched) && l != None ? None : Some(n),
          es,
        );
      let (rest1, rest2) = (rest(es1), rest(es2));
      List.length(rest1) == List.length(rest2)
        ? Some(
            List.map(((_, a, b)) => (a, b), by_label)
            @ List.combine(rest1, rest2),
          )
        : None;
    };
  }
  and labeled = (ns: list(node)): bool =>
    List.exists(
      n =>
        switch (find(n).shape) {
        | Some(Label(_)) => true
        | _ => false
        },
      ns,
    );

  let is_alias = (ctx: Ctx.t, x: string): bool =>
    switch (Ctx.lookup_tvar(ctx, x)) {
    | Some(Singleton(_)) => true
    | _ => false
    };

  let structure = (s: t, sh: shape): node => {
    let n = fresh(s);
    n.shape = Some(sh);
    n;
  };

  let raw = (s: t, id: Id.t): node =>
    switch (Hashtbl.find_opt(s.nodes, id)) {
    | Some(n) => n
    | None =>
      let n = fresh(s);
      n.holes = [id];
      Hashtbl.add(s.nodes, id, n);
      switch (Hashtbl.find_opt(s.rigid, id)) {
      | Some(a) => n.shape = Some(Leaf(Var(a) |> Typ.temp, Ctx.empty))
      | None => ()
      };
      n;
    };

  /* A derived hole's parent is the arrow of it and its sibling. */
  let rec of_hole = (s: t, id: Id.t): node =>
    switch (Hashtbl.find_opt(s.nodes, id)) {
    | Some(n) => n
    | None =>
      let n = raw(s, id);
      switch (Hashtbl.find_opt(s.st.holes, id)) {
      | Some({kind: Derived(parent, _), _}) =>
        let l = raw(s, hole_id(parent, Derived(parent, ArrowL)));
        let r = raw(s, hole_id(parent, Derived(parent, ArrowR)));
        union(of_hole(s, parent), structure(s, Arrow(l, r)));
      | _ => ()
      };
      n;
    };

  let rec of_typ = (s: t, ctx: Ctx.t, ty: Typ.t): node => {
    let ty = strip(ty);
    switch (hole_of(s.st, ty)) {
    | Some(id) => of_hole(s, id)
    | None =>
      /* A type variable that isn't an alias here is rigid, whether or not
         it is in scope (generalized ones are bound only in pass 2). */
      let whnf =
        switch (Typ.term_of(ty)) {
        | Var(x) when !is_alias(ctx, x) => ty
        | _ => Typ.weak_head_normalize(ctx, ty)
        };
      switch (Typ.term_of(whnf)) {
      | Unknown(_) => fresh(s)
      | Arrow(t1, t2) =>
        structure(s, Arrow(of_typ(s, ctx, t1), of_typ(s, ctx, t2)))
      | Prod(ts) => structure(s, Prod(List.map(of_typ(s, ctx), ts)))
      | List(t) => structure(s, List(of_typ(s, ctx, t)))
      | TupLabel({term: Label(l), _}, t) =>
        structure(s, Label(l, of_typ(s, ctx, t)))
      | _ => structure(s, Leaf(ty, ctx))
      };
    };
  };

  let add = (s: t, (ctx, t1, t2): constraint_): unit =>
    union(of_typ(s, ctx, t1), of_typ(s, ctx, t2));

  /* Resolve a node to a type. Unsolved classes go to `unsolved`; classes
     with conflicting constraints, and cyclic ones, to `?`. */
  let rec resolve =
          (
            ~norm: bool,
            ~unsolved: node => Typ.t,
            ~visiting: list(int)=[],
            n: node,
          )
          : Typ.t => {
    let r = find(n);
    let go = resolve(~norm, ~unsolved, ~visiting=[r.uid, ...visiting]);
    if (r.conflict != [] || List.mem(r.uid, visiting)) {
      unknown();
    } else {
      switch (r.shape) {
      | None => unsolved(r)
      | Some(Leaf({term: Var(x), _} as t, ctx)) when !is_alias(ctx, x) => t
      | Some(Leaf(t, ctx)) => norm ? Typ.normalize(ctx, t) : t
      | Some(Arrow(a, b)) => Arrow(go(a), go(b)) |> Typ.temp
      | Some(Prod(ns)) => Prod(List.map(go, ns)) |> Typ.temp
      | Some(List(a)) => List(go(a)) |> Typ.temp
      | Some(Label(l, a)) =>
        TupLabel(Label(l) |> Typ.temp, go(a)) |> Typ.temp
      };
    };
  };

  /* Unsolved roots reachable from n, left to right, without repeats. */
  let unsolved_roots = (n: node): list(node) => {
    let acc = ref([]);
    let rec go = (visiting, n) => {
      let r = find(n);
      if (!List.mem(r.uid, visiting) && r.conflict == []) {
        switch (r.shape) {
        | None =>
          if (!List.exists(r' => r' === r, acc^)) {
            acc := [r, ...acc^];
          }
        | Some(Leaf(_)) => ()
        | Some(Arrow(a, b)) =>
          go([r.uid, ...visiting], a);
          go([r.uid, ...visiting], b);
        | Some(Prod(ns)) => List.iter(go([r.uid, ...visiting]), ns)
        | Some(List(a))
        | Some(Label(_, a)) => go([r.uid, ...visiting], a)
        };
      };
    };
    go([], n);
    List.rev(acc^);
  };

  let shape_typ = (sh: shape): Typ.t =>
    switch (sh) {
    | Leaf(t, _) => t
    | Arrow(_) => Arrow(unknown(), unknown()) |> Typ.temp
    | Prod(ns) => Prod(List.map(_ => unknown(), ns)) |> Typ.temp
    | List(_) => List(unknown()) |> Typ.temp
    | Label(l, _) => TupLabel(Label(l) |> Typ.temp, unknown()) |> Typ.temp
    };

  /* The incompatible heads a conflicted class was asked to be. */
  let conflict_typs = (r: node): list(Typ.t) =>
    switch (r.shape) {
    | Some(sh) => List.map(shape_typ, [sh, ...r.conflict])
    | None => List.map(shape_typ, r.conflict)
    };
};

/* ===== Collecting constraints from the info map ===== */

let rec holes_in_typ = (st: infer_state, ty: Typ.t): list(Id.t) =>
  switch (hole_of(st, ty)) {
  | Some(id) => [id]
  | None =>
    switch (Typ.term_of(ty)) {
    | Parens(t)
    | Projector(_, t)
    | List(t)
    | TupLabel(_, t)
    | Rec(_, t)
    | Poly(_, t) => holes_in_typ(st, t)
    | Arrow(t1, t2)
    | ProdProjection(t1, t2)
    | ProdExtension(t1, t2) => holes_in_typ(st, t1) @ holes_in_typ(st, t2)
    | Prod(ts) => List.concat_map(holes_in_typ(st), ts)
    | Sum(sm) =>
      List.concat_map(
        fun
        | ConstructorMap.Variant(_, _, Some(t)) => holes_in_typ(st, t)
        | _ => [],
        sm,
      )
    | _ => []
    }
  };

let rec has_hole = (st: infer_state, ty: Typ.t): bool =>
  hole_of(st, ty) != None
  || (
    switch (Typ.term_of(ty)) {
    | Parens(t)
    | Projector(_, t)
    | List(t)
    | TupLabel(_, t)
    | Rec(_, t)
    | Poly(_, t) => has_hole(st, t)
    | Arrow(t1, t2)
    | ProdProjection(t1, t2)
    | ProdExtension(t1, t2) => has_hole(st, t1) || has_hole(st, t2)
    | Prod(ts) => List.exists(has_hole(st), ts)
    | Sum(sm) =>
      List.exists(
        fun
        | ConstructorMap.Variant(_, _, Some(t)) => has_hole(st, t)
        | _ => false,
        sm,
      )
    | _ => false
    }
  );

/* The subsumption constraint an info records, if it can tell a hole
   anything: one side must mention a hole, and the check must not have
   failed (a failed check is an error, not information). */
let constraints_of_info = (st: infer_state, info: Info.t): list(constraint_) => {
  let mismatched =
    List.exists(
      fun
      | Mark.ExpectationMismatch(_) => true
      | _ => false,
    );
  switch (info) {
  | InfoExp({ctx, ana, elab_syn_ty, marks, _})
  | InfoPat({ctx, ana, elab_syn_ty, marks, _}) =>
    !Typ.is_syn(ana)
    && (has_hole(st, ana) || has_hole(st, elab_syn_ty))
    && !mismatched(marks)
      ? [(ctx, ana, elab_syn_ty)] : []
  | _ => []
  };
};

/* Labeled-tuple operations (`.`, tuple extension, the label builtins)
   accept `?` structurally but give no constraint a hole could be solved
   by; a type variable would not do, so such holes are never generalized. */
let gradual_use = (ty: Typ.t): unit =>
  switch (mode^) {
  | Infer(st) =>
    List.iter(h => Hashtbl.replace(st.gradual, h, ()), holes_in_typ(st, ty))
  | _ => ()
  };

/* The result of a gradual use (above) of a hole: unknown now, but maybe
   known once the hole is filled, so it must not be generalized either. */
let gradual_result = (~site: Id.t): Typ.t =>
  switch (mode^) {
  | Infer(st) =>
    let h = new_hole(st, site, Opaque);
    Hashtbl.replace(st.gradual, Typ.rep_id(h), ());
    h;
  | _ => unknown()
  };

let is_implicit_hole = (ty: Typ.t): bool =>
  switch (mode^) {
  | Infer(st) => hole_of(st, ty) != None
  | _ => false
  };

let is_exp_hole = (info: Info.t): bool =>
  switch (info) {
  | InfoExp({cls: Exp(EmptyHole | MultiHole | Invalid), _}) => true
  | _ => false
  };

/* The infos (deduplicated) and ids of a subterm. */
let subtree = (m: StaticsBase.Map.t, e: Exp.t): (list(Info.t), Id.Set.t) => {
  let infos = ref([]);
  let ids = ref(Id.Set.empty);
  let seen = Hashtbl.create(64);
  let _ =
    Grammar.map_exp_annotation(
      ({ids: tag_ids, _}: IdTagged.IdTag.t) =>
        List.iter(
          id => {
            ids := Id.Set.add(id, ids^);
            switch (StaticsBase.Map.lookup(id, m)) {
            | Some(info) =>
              let rid = Info.id_of(info);
              if (!Hashtbl.mem(seen, rid)) {
                Hashtbl.add(seen, rid, ());
                infos := [info, ...infos^];
              };
            | None => ()
            };
          },
          tag_ids,
        ),
      e,
    );
  (infos^, ids^);
};

let rigid_of_gens = (gens: Hashtbl.t(Id.t, gen)): Hashtbl.t(Id.t, string) => {
  let rigid = Hashtbl.create(16);
  Hashtbl.iter(
    (_, g: gen) =>
      List.iter(((h, a)) => Hashtbl.replace(rigid, h, a), g.holes),
    gens,
  );
  rigid;
};

let tvar_names = (avoid: list(string), n: int): list(string) => {
  let letters = "abcdefghijklmnopqrstuvwxyz";
  let name = i =>
    i < 26
      ? String.make(1, letters.[i])
      : String.make(1, letters.[i mod 26]) ++ string_of_int(i / 26);
  let rec go = (i, acc) =>
    List.length(acc) == n
      ? List.rev(acc)
      : {
        let x = name(i);
        List.mem(x, avoid) ? go(i + 1, acc) : go(i + 1, [x, ...acc]);
      };
  go(0, []);
};

let ctx_tvar_names = (ctx: Ctx.t): list(string) =>
  List.filter_map(
    fun
    | Ctx.TVarEntry({name, _}) => Some(name)
    | _ => None,
    ctx.entries,
  );

/* Entries bound inside a scope: those newer than the scope's own ctx. */
let entries_since = (outer: Ctx.t, inner: Ctx.t): list(Ctx.entry) => {
  let rec go = es =>
    switch (es) {
    | [] => []
    | _ when es === outer.entries => []
    | [e, ...es] => [e, ...go(es)]
    };
  go(inner.entries);
};

let poly_of = (gen: gen, ty: Typ.t): Typ.t =>
  List.fold_right(
    (a, body) => Typ.temp(Poly(TPat.temp(Var(a): TPat.term), body)),
    gen.tvars,
    ty,
  );

/* Pass 1, at `let p = def`: decide the holes `def` owns from `def` alone
   and, if `p` is a variable (`generalize`), which of them to generalize
   over; return the scheme `p` gets in the body. */
let generalize_let =
    (
      ~let_id: Id.t,
      ~generalize: bool,
      ~ctx: Ctx.t,
      ~def: Exp.t,
      ~def_ty: Typ.t,
      m: StaticsBase.Map.t,
    )
    : option(Typ.t) =>
  switch (mode^) {
  | Infer(st) when !has_hole(st, def_ty) =>
    Hashtbl.remove(st.gens, let_id);
    None;
  | Infer(st) =>
    let t0 = now();
    Hashtbl.remove(st.gens, let_id);
    let (infos, ids) = subtree(m, def);
    let s = Solver.create(st, rigid_of_gens(st.gens));
    List.iter(
      info => List.iter(Solver.add(s), constraints_of_info(st, info)),
      infos,
    );
    Id.Set.iter(
      id =>
        switch (Hashtbl.find_opt(st.extras, id)) {
        | Some(cs) => List.iter(Solver.add(s), cs)
        | None => ()
        },
      ids,
    );
    List.iter(
      (info: Info.t) =>
        if (is_exp_hole(info)) {
          List.iter(
            fun
            | Ctx.VarEntry({typ, _}) =>
              List.iter(
                h => Solver.(find(of_hole(s, h)).etc = true),
                holes_in_typ(st, typ),
              )
            | _ => (),
            entries_since(ctx, Info.ctx_of(info)),
          );
        },
      infos,
    );
    let def_node = Solver.of_typ(s, ctx, def_ty);
    let owned = h =>
      switch (root_site(st, h)) {
      | Some(site) => Id.Set.mem(site, ids)
      | None => false
      };
    let generalizable =
      (generalize ? Solver.unsolved_roots(def_node) : [])
      |> List.filter((r: Solver.node) =>
           !r.etc
           && r.holes != []
           && List.for_all(owned, r.holes)
           && !List.exists(Hashtbl.mem(st.gradual), r.holes)
         );
    let nested =
      Hashtbl.fold(
        (id, g: gen, acc) => Id.Set.mem(id, ids) ? g.tvars @ acc : acc,
        st.gens,
        [],
      );
    let tvars =
      tvar_names(ctx_tvar_names(ctx) @ nested, List.length(generalizable));
    let named = List.combine(generalizable, tvars);
    /* An unsolved class: its type variable if generalized; `?` if it is
       this definition's alone (nothing outside may decide it later); else
       still the outer hole it belongs to. */
    let unsolved = (r: Solver.node) =>
      switch (List.find_opt(((r', _)) => r' === r, named)) {
      | Some((_, a)) => Var(a) |> Typ.temp
      | None when List.for_all(owned, r.holes) => unknown()
      | None => hole_typ(List.find(h => !owned(h), r.holes))
      };
    Hashtbl.iter(
      (h, n) =>
        if (owned(h)) {
          let r = Solver.find(n);
          Hashtbl.replace(
            st.decided,
            h,
            r.conflict != []
              ? Conflicted(Solver.conflict_typs(r))
              : Decided(Solver.resolve(~norm=false, ~unsolved, n)),
          );
        },
      s.nodes,
    );
    let_time := let_time^ +. now() -. t0;
    if (generalizable == []) {
      None;
    } else {
      let gen = {
        tvars,
        holes:
          List.concat_map(
            ((r: Solver.node, a)) => List.map(h => (h, a), r.holes),
            named,
          ),
      };
      Hashtbl.replace(st.gens, let_id, gen);
      Some(poly_of(gen, Solver.resolve(~norm=false, ~unsolved, def_node)));
    };
  | _ => None
  };

/* End of pass 1: solve everything at once and fill every hole. */
let solve = (st: infer_state, m: StaticsBase.Map.t): apply_state => {
  let s = Solver.create(st, rigid_of_gens(st.gens));
  let seen = Hashtbl.create(256);
  Id.Map.iter(
    (_, info) => {
      let rid = Info.id_of(info);
      if (!Hashtbl.mem(seen, rid)) {
        Hashtbl.add(seen, rid, ());
        List.iter(Solver.add(s), constraints_of_info(st, info));
      };
    },
    m,
  );
  Hashtbl.iter((_, cs) => List.iter(Solver.add(s), cs), st.extras);
  let fills = Hashtbl.create(64);
  let conflicts = Hashtbl.create(8);
  let conflict = (id, tys) =>
    switch (root_site(st, id)) {
    | Some(site) =>
      if (!Hashtbl.mem(conflicts, site)) {
        Hashtbl.add(conflicts, site, tys);
      }
    | None => ()
    };
  Hashtbl.iter(
    (id, _) => {
      let n =
        switch (Hashtbl.find_opt(st.decided, id)) {
        | Some(Decided(ty)) => Solver.of_typ(s, Ctx.empty, ty)
        | Some(Conflicted(_))
        | None => Solver.of_hole(s, id)
        };
      let r = Solver.find(n);
      switch (Hashtbl.find_opt(st.decided, id)) {
      | Some(Conflicted(tys)) => conflict(id, tys)
      | _ when r.conflict != [] => conflict(id, Solver.conflict_typs(r))
      | _ when r.shape != None =>
        let unsolved = _ => unknown();
        Hashtbl.replace(
          fills,
          id,
          (
            Solver.resolve(~norm=false, ~unsolved, n),
            Solver.resolve(~norm=true, ~unsolved, n),
          ),
        );
      | _ => ()
      };
    },
    st.holes,
  );
  {
    fills,
    conflicts,
    gens: st.gens,
    instantiated: st.instantiated,
  };
};

/* Value restriction: only a definition that evaluates without running
   anything is generalized. Its hidden typfun would otherwise delay the
   definition to each use and repeat it there. */
let rec is_value = (e: Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | Fun(_)
  | TypFun(_)
  | FixF(_)
  | Var(_)
  | Atom(_)
  | Constructor(_)
  | Label(_)
  | BuiltinFun(_)
  | EmptyHole => true
  | ListLit(es)
  | Tuple(es) => List.for_all(is_value, es)
  | TupLabel(_, e)
  | Parens(e)
  | Asc(e, _)
  | Projector(_, e)
  | TyAlias(_, _, e) => is_value(e)
  | Cons(e1, e2)
  | Let(_, e1, e2) => is_value(e1) && is_value(e2)
  | Ap(_, {term: Constructor(_), _}, arg) => is_value(arg)
  /* `f(_, x)` makes a closure without running f */
  | DeferredAp(f, args) => is_value(f) && List.for_all(is_value, args)
  | _ => false
  };

/* Type variables written in a variable let's annotations that nothing in
   scope binds: the let binds them, rigid and scoped over its definition.
   Lowercase names only, and not a built-in type's name in the wrong case
   (`int`), so those typos stay "not bound". Nested
   variable lets bind their own. Not with inference off, as on dev. */
let written_tvars = (~ctx: Ctx.t, p: Pat.t, def: Exp.t): list(string) =>
  switch (mode^) {
  | Off => []
  | Infer(_)
  | Apply(_) =>
    let found = ref([]);
    let bound = ref([]);
    let under = (tp: TPat.t, f) => {
      let saved = bound^;
      switch (IdTagged.term_of(tp)) {
      | Var(a) => bound := [a, ...bound^]
      | _ => ()
      };
      let r = f();
      bound := saved;
      r;
    };
    let bindable = a =>
      Util.StringUtil.match(
        Util.StringUtil.regexp("^[a-z][A-Za-z0-9_']*$"),
        a,
      )
      && !
           List.exists(
             t => String.lowercase_ascii(t) == String.lowercase_ascii(a),
             Token.base_typs,
           );
    let f_typ = (continue, t: Typ.t) =>
      switch (Typ.term_of(t)) {
      | Var(a)
          when
            bindable(a)
            && !List.mem(a, bound^)
            && !List.mem(a, found^)
            && Ctx.lookup_tvar(ctx, a) == None =>
        found := [a, ...found^];
        t;
      | Poly(tp, _)
      | Rec(tp, _) => under(tp, () => continue(t))
      | _ => continue(t)
      };
    let rec f_exp = (continue, e: Exp.t) =>
      switch (Exp.term_of(e)) {
      | Let(q, _, body) when binds_variable(q) =>
        let _ = Exp.map_term(~f_exp, ~f_typ, body);
        e;
      | TypFun(tp, _, _)
      | TyAlias(tp, _, _) => under(tp, () => continue(e))
      | _ => continue(e)
      }
    and binds_variable = (q: Pat.t) =>
      switch (Pat.term_of(q)) {
      | Var(_)
      | Ap({term: Var(_), _}, _) => true
      | Asc(q, _) => binds_variable(q)
      | _ => false
      };
    let _ = Pat.map_term(~f_typ, p);
    let _ = Exp.map_term(~f_exp, ~f_typ, def);
    List.rev(found^);
  };

/* Pass 2, at a generalized let: the def's ctx gains its type variables. */
let let_gen = (~let_id: Id.t): option(gen) =>
  switch (mode^) {
  | Apply(ap) => Hashtbl.find_opt(ap.gens, let_id)
  | _ => None
  };

let extend_with_tvars = (~let_id: Id.t, ctx: Ctx.t, gen: gen): Ctx.t =>
  List.fold_left(
    (ctx, a) =>
      Ctx.extend_tvar(
        ctx,
        {
          name: a,
          id: derive("implicit-tvar-" ++ a, let_id),
          kind: Abstract,
        },
      ),
    ctx,
    gen.tvars,
  );

let wrap_typfuns = (~let_id: Id.t, gen: gen, elab: Exp.t): Exp.t =>
  List.fold_right(
    (a, body) =>
      IdTagged.mk_internal(
        [derive_implicit("implicit-typfun-" ++ a, let_id)],
        TypFun(
          IdTagged.mk_internal(
            [derive_implicit("implicit-tpat-" ++ a, let_id)],
            Var(a): TPat.term,
          ),
          body,
          None,
        ): Exp.term,
      ),
    gen.tvars,
    elab,
  );

let conflict_at = (site: Id.t): option(list(Typ.t)) =>
  switch (mode^) {
  | Apply(ap) => Hashtbl.find_opt(ap.conflicts, site)
  | _ => None
  };

/* Run statics with inference: once to infer, and, if there was anything to
   infer, once more with the solutions in place. */
let run =
    (~enabled: bool, pass: unit => ('a, StaticsBase.Map.t))
    : ('a, StaticsBase.Map.t) => {
  let prev = mode^;
  let st = {
    holes: Hashtbl.create(64),
    extras: Hashtbl.create(64),
    gens: Hashtbl.create(8),
    instantiated: Hashtbl.create(16),
    gradual: Hashtbl.create(8),
    decided: Hashtbl.create(64),
  };
  Fun.protect(
    ~finally=() => mode := prev,
    () => {
      mode := disabled || !enabled ? Off : Infer(st);
      let_time := 0.;
      let t0 = now();
      let (r1, m1) = pass();
      let t1 = now();
      if (Hashtbl.length(st.holes) == 0) {
        (r1, m1);
      } else {
        let ap = solve(st, m1);
        let t2 = now();
        mode := Apply(ap);
        let r2 = pass();
        if (timing) {
          Printf.printf(
            "inference: pass1 %.0fms (lets %.0fms) solve %.0fms pass2 %.0fms; %d holes, %d fills, %d gens, %d conflicts\n",
            1000. *. (t1 -. t0),
            1000. *. let_time^,
            1000. *. (t2 -. t1),
            1000. *. (now() -. t2),
            Hashtbl.length(st.holes),
            Hashtbl.length(ap.fills),
            Hashtbl.length(ap.gens),
            Hashtbl.length(ap.conflicts),
          );
        };
        r2;
      };
    },
  );
};
