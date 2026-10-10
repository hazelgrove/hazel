/* A Merkle hash of a type: computed from its constructor and its children's
   hashes, ignoring ids, so two copies of the same type hash alike. Each type
   object's hash is remembered in a JS WeakMap keyed by the object itself, so
   asking again about an object already seen is one lookup, and a type built
   from parts already seen costs only its new nodes.

   What it is for: answering a question about two types -- is_consistent,
   say -- once per pair of types, however many copies of them come by. The
   evaluator asks is_consistent of a constructor's own sum type against the
   type it is ascribed, for every node of a livelit's Html tree, every step;
   the two are copies of the same large Html type, and the answers were 88%
   of a Polygons click in Firefox.

   Two independent 30-bit hashes make the key, about 60 bits: two different
   types sharing one is negligible at the sizes involved, and the price of
   it would be one wrong cached answer to a question about those two types.

   A type holding an expression, a signature or projector data gets no hash
   (None), and callers then compute as if there were no cache. Binder names
   count, so alpha-equivalent types hash differently: a missed reuse, never a
   wrong one. An Unknown's provenance does not count; callers whose answer
   depends on it must not use this. */
open Js_of_ocaml;

type t = (int, int);

let mask = 0x3FFFFFFF;
let mix = ((a, b): t, (x, y): t): t => (
  (a * 31 + x) land mask,
  b lxor y * 1000003 land mask,
);
let of_int = (n: int): t => (n land mask, (n * 2654435 + 97) land mask);
let of_string = (s: string): t => (
  Hashtbl.hash(s) land mask,
  Hashtbl.seeded_hash(7919, s) land mask,
);
let tag = (n: int): t => of_int(n * 1009 + 17);
let combine = (n: int, parts: list(t)): t =>
  List.fold_left(mix, tag(n), parts);

/* The WeakMap, made on first use: native builds never evaluate. */
let table: Lazy.t(Js.Unsafe.any) =
  lazy(Js.Unsafe.new_obj(Js.Unsafe.get(Js.Unsafe.global, "WeakMap"), [||]));

let remembered = (ty: TermBase.Typ.t): option(option(t)) => {
  let v: Js.Optdef.t(option(t)) =
    Js.Unsafe.meth_call(
      Lazy.force(table),
      "get",
      [|Js.Unsafe.inject(ty)|],
    );
  Js.Optdef.to_option(v);
};
let remember = (ty: TermBase.Typ.t, h: option(t)): option(t) => {
  ignore(
    Js.Unsafe.meth_call(
      Lazy.force(table),
      "set",
      [|Js.Unsafe.inject(ty), Js.Unsafe.inject(h)|],
    ),
  );
  h;
};

let rec hash = (ty: TermBase.Typ.t): option(t) =>
  switch (remembered(ty)) {
  | Some(h) => h
  | None => remember(ty, compute(ty))
  }
and compute = (ty: TermBase.Typ.t): option(t) => {
  open Util.OptUtil.Syntax;
  let all = (n, tys) => {
    let+ hs = Util.OptUtil.sequence(List.map(hash, tys));
    combine(n, hs);
  };
  switch (ty.term) {
  | Unknown(_) => Some(tag(1))
  | Atom(c) => Some(combine(2, [of_int(Hashtbl.hash(c))]))
  | DrvQuoteTy(s) => Some(combine(3, [of_int(Hashtbl.hash(s))]))
  | Var(x) => Some(combine(4, [of_string(x)]))
  | List(t) => all(5, [t])
  | Arrow(a, b) => all(6, [a, b])
  | Sum(m) =>
    let+ entries =
      Util.OptUtil.sequence(
        List.map(
          (v: ConstructorMap.variant(TermBase.Typ.t)) =>
            switch (v) {
            | Variant(c, _, None) => Some(combine(31, [of_string(c)]))
            | Variant(c, _, Some(t)) =>
              let+ h = hash(t);
              combine(32, [of_string(c), h]);
            | BadEntry(t) =>
              let+ h = hash(t);
              combine(33, [h]);
            },
          m,
        ),
      );
    combine(7, entries);
  | Prod(ts) => all(8, ts)
  | ExplicitNonlabel => Some(tag(9))
  | Label(l) => Some(combine(10, [of_string(l)]))
  | TupLabel(a, b) => all(11, [a, b])
  | Parens(t) => all(12, [t])
  | Splice(t) => all(13, [t])
  | Rec(p, t) =>
    let* hp = tpat(p);
    let+ ht = hash(t);
    combine(14, [hp, ht]);
  | Poly(p, t) =>
    let* hp = tpat(p);
    let+ ht = hash(t);
    combine(15, [hp, ht]);
  | ProdProjection(a, b) => all(16, [a, b])
  | ProdExtension(a, b) => all(17, [a, b])
  | Projector(_)
  | ProofOf(_)
  | Sig(_) => None
  };
}
and tpat = (p: TermBase.TPat.t): option(t) =>
  switch (p.term) {
  | Var(x) => Some(combine(21, [of_string(x)]))
  | EmptyHole => Some(tag(22))
  | Invalid(s) => Some(combine(23, [of_string(s)]))
  | MultiHole(_) => None
  };
