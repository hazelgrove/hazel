/* MAKETERM

     This parses tile structure into term structure.
     The language syntax, as determined by Form.re, is an
     open, data-driven system, so adding a syntactic form
     there will not trigger a static error here; you must
     remember to add a case below for each new form added
     to the syntax.

     WARNING: This module is still structurally in flux.
   */

open Util;
open Language;

/* Hack: Temporary construct internal to maketerm
 * to handle probe parsing; see `tokens` below */
let probe_wrap = ["PROBE_WRAP", "PROBE_WRAP"];
let is_probe_wrap = (==)(probe_wrap);

// TODO make less hacky
let tokens =
  Piece.get(
    _ => [],
    _ => [" "],
    (t: Tile.t) => t.shards |> List.map(List.nth(t.label)),
    _ =>
      /* Hack: These act as temporary wrappers for projectors,
       * which are retained through maketerm so as to be used in
       * dynamics. These are inserted and removed entirely internal
       * to maketerm. */
      probe_wrap,
  );

[@deriving (show({with_path: false}), sexp, yojson)]
type tile = (Id.t, Aba.t(Token.t, Any.t));
[@deriving (show({with_path: false}), sexp, yojson)]
type tiles = Aba.t(tile, Any.t);
let single = (id, subst) => ([(id, subst)], []);

[@deriving (show({with_path: false}), sexp, yojson)]
type unsorted =
  | Op(tiles)
  | Pre(tiles, Any.t)
  | Post(Any.t, tiles)
  | Bin(Any.t, tiles, Any.t);

type t = {
  term: Exp.t,
  terms: TermMap.t,
  projectors: Id.Map.t(Piece.projector),
};

let is_nary =
    (is_sort: Any.t => option('sort), delim: Token.t, (delims, kids): tiles)
    : option(list('sort)) =>
  if (delims |> List.map(snd) |> List.for_all((==)(([delim], [])))) {
    kids |> List.map(is_sort) |> OptUtil.sequence;
  } else {
    None;
  };

let is_tuple_exp = is_nary(Any.is_exp, ",");
let is_tuple_pat = is_nary(Any.is_pat, ",");
let is_tuple_typ = is_nary(Any.is_typ, ",");
let is_tuple_drv_exp = is_nary(Any.is_drv_exp, ",");
let is_typ_bsum = is_nary(Any.is_typ, "+");

let is_grout = tiles =>
  Aba.get_as(tiles) |> List.map(snd) |> List.for_all((==)(([" "], [])));

let is_rules = ((ts, kids): tiles): option(Aba.t(Pat.t, Exp.t)) => {
  open OptUtil.Syntax;
  let+ ps =
    (ts: list(tile))
    |> List.map(
         fun
         | (_, (["|", "=>"], [Pat(p)])) => Some(p)
         | _ => None: tile => option(TermBase.pat_t),
       )
    |> OptUtil.sequence
  and+ clauses =
    kids
    |> List.map(
         fun
         | Exp(clause) => Some(clause)
         | _ => None: TermBase.any_t => option(TermBase.exp_t),
       )
    |> OptUtil.sequence;
  Aba.mk(ps, clauses);
};
let is_drv_rules = ((ts, kids): tiles): option(Aba.t(Drv.Pat.t, Drv.Exp.t)) => {
  open OptUtil.Syntax;
  let+ ps =
    ts
    |> List.map(
         fun
         | (_, (["|", "=>"], [Grammar.Drv(Pat(p))])) => Some(p)
         | _ => None,
       )
    |> OptUtil.sequence
  and+ clauses =
    kids
    |> List.map(
         fun
         | Grammar.Drv(Exp(clause)) => Some(clause)
         | _ => None,
       )
    |> OptUtil.sequence;
  Aba.mk(ps, clauses);
};

let ids_of_tiles = (tiles: tiles) => List.map(fst, Aba.get_as(tiles));
let ids =
  fun
  | Op(tiles)
  | Pre(tiles, _)
  | Post(_, tiles)
  | Bin(_, tiles, _) => ids_of_tiles(tiles);

let kids_of_tile = ((_id, (_tokens, kids)): tile) => kids;
let kids_of_tiles = (tiles: tiles) =>
  tiles
  |> Aba.map_a(kids_of_tile)
  |> Aba.join(Fun.id, kid => [kid])
  |> List.concat;
let kids_of_unsorted =
  fun
  | Op(tiles) => kids_of_tiles(tiles)
  | Pre(tiles, r) => kids_of_tiles(tiles) @ [r]
  | Post(l, tiles) => [l] @ kids_of_tiles(tiles)
  | Bin(l, tiles, r) => [l] @ kids_of_tiles(tiles) @ [r];

// Need this map to collect all structural terms,
// not just the ones recognized in Statics.
// TODO unhack
let map: ref(TermMap.t) = ref(Id.Map.empty);
let return = (wrap, ids, tm) => {
  map := TermMap.add_all(ids, wrap(tm), map^);
  tm;
};

/* Map to collect projector ids */
let projectors: ref(Id.Map.t(Piece.projector)) = ref(Id.Map.empty);

/* Strip a projector from a segment and log it in the map */
let log_projector = (pr: Base.projector): unit => {
  projectors := Id.Map.add(pr.id, pr, projectors^);
};

/* Check if a term should be instrumented with a probe.
 * Precondition: The relevant projector must have been
 * logged before this is called */
let should_instrument = (id: Id.t): bool =>
  switch (Id.Map.find_opt(id, projectors^)) {
  | Some(pr) =>
    let (module P) = ProjectorInit.to_module(pr.kind);
    P.dynamics;
  | None => failwith("MakeTerm.exp: projector not found")
  };

let parse_sum_term: Typ.t => ConstructorMap.variant(Typ.t) =
  fun
  | {term: Var(ctr), annotation: {ids, _}} => Variant(ctr, ids, None)
  | {
      term: Ap({term: Var(ctr), annotation: {ids: ids_ctr, _}}, u),
      annotation: {ids: ids_ap, _},
    } =>
    Variant(ctr, ids_ctr @ ids_ap, Some(u))
  | t => BadEntry(t);

let mk_bad = (ctr, ids, value) => {
  let t: Typ.t = {
    annotation: {
      ids: ids,
    },
    term: Var(ctr),
  };
  switch (value) {
  | None => t
  | Some(u) => Ap(t, u) |> Typ.fresh
  };
};

let rec go_s = (s: Sort.t, skel: Skel.t, seg: Segment.t): Term.Any.t =>
  switch (s) {
  | Drv(drv) =>
    Drv(
      switch (drv) {
      | Jdmt
      | Ctx
      | Prop
      | Exp => Exp(drv_exp(unsorted(skel, seg)))
      | Pat => Pat(drv_pat(unsorted(skel, seg)))
      | Typ => Typ(drv_typ(unsorted(skel, seg)))
      | TPat => TPat(drv_tpat(unsorted(skel, seg)))
      },
    )
  | Pat => Pat(pat(unsorted(skel, seg)))
  | TPat => TPat(tpat(unsorted(skel, seg)))
  | Typ => Typ(typ(unsorted(skel, seg)))
  | Exp => Exp(exp(unsorted(skel, seg)))
  | Rul => Rul(rul(unsorted(skel, seg)))
  | Any =>
    let tm = unsorted(skel, seg);
    let ids = ids(tm);
    switch (ListUtil.hd_opt(ids)) {
    | None => Exp(exp(unsorted(skel, seg)))
    | Some(id) =>
      switch (TileMap.find_opt(id, TileMap.mk(seg))) {
      | None => Exp(exp(unsorted(skel, seg)))
      | Some(t) =>
        if (t.mold.out == Any) {
          Exp(exp(unsorted(skel, seg)));
        } else {
          go_s(t.mold.out, skel, seg);
        }
      }
    };
  }
and drv_exp = unsorted => {
  let (term, inner_ids) = drv_exp_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(
    e => Drv(Exp(e)),
    ids,
    {
      annotation: {
        ids: ids,
      },
      term,
    },
  );
}
and drv_exp_term: unsorted => (Drv.Exp.term, list(Id.t)) = {
  let ret = (tm: Drv.Exp.term) => (tm, []);
  let hole: unsorted => DrvTermBase.exp_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, t)], [])) as tm =>
    switch (t) {
    | ([t], []) =>
      switch (t) {
      | "Truth" => ret(Truth)
      | "Falsity" => ret(Falsity)
      | "True" => ret(True)
      | "False" => ret(False)
      | _ when Form.is_wild(t) => ret(ExpHole)
      | _ when Form.is_empty_list(t) => ret(Ctx([]))
      | _ when Form.is_empty_tuple(t) => ret(Triv)
      | _ when Form.is_int(t) => ret(NumLit(int_of_string(t)))
      | _ when Form.is_typ_var(t) => ret(Var(t))
      | _ => ret(hole(tm))
      }
    | (["val", "end"], [Drv(Exp(e))]) => ret(Val(e))
    | (["valid", "end"], [Drv(Typ(t))]) => ret(Type(t))
    | (["[", "]"], [Drv(Exp(body))]) =>
      switch (body.term) {
      | Tuple(es) => (Ctx(es), IdTagged.ids(body))
      | Pair(e1, e2) => (Ctx([e1, e2]), IdTagged.ids(body))
      | _ => ret(Ctx([body]))
      }
    | (["(", ")"], [Drv(Exp(body))]) =>
      switch (body.term) {
      // Note(zhiyao): a standard pair includes a parens
      | Tuple([e1, e2]) => (Pair(e1, e2), IdTagged.ids(body))
      | _ => ret(Parens(body))
      }
    | (["case", "end"], [Drv(Exp(body))]) =>
      switch (body.term) {
      | Case(_) as term => (term, IdTagged.ids(body))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Drv(Exp(l)), ([(_id, ([t], []))], []), Drv(Exp(r))) as tm =>
    switch (t) {
    | "\\=/" => ret(Eval(l, r))
    | "|-" => ret(Entail(l, r))
    | "," => ret(Tuple([l, r]))
    | "::" => ret(Cons(l, r))
    | "@" => ret(Concat(l, r))
    | "/\\" => ret(And(l, r))
    | "\\/" => ret(Or(l, r))
    | "==>" => ret(Impl(l, r))
    | "+" => ret(BinOp(Plus, l, r))
    | "-" => ret(BinOp(Minus, l, r))
    | "*" => ret(BinOp(Times, l, r))
    | "==" => ret(BinOp(Eq, l, r))
    | "<" => ret(BinOp(Lt, l, r))
    | ">" => ret(BinOp(Gt, l, r))
    | "." =>
      switch (r.term) {
      | Var("fst") => (PrjL(l), IdTagged.ids(r))
      | Var("snd") => (PrjR(l), IdTagged.ids(r))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Drv(Exp(l)), tiles, Drv(Exp(r))) as tm =>
    switch (is_tuple_drv_exp(tiles)) {
    | Some([]) => ret(Pair(l, r))
    | Some(between_kids) => ret(Tuple([l] @ between_kids @ [r]))
    | None =>
      switch (is_drv_rules(tiles)) {
      | Some(([x, y], [e1])) => ret(Case(l, x, e1, y, r))
      | _ => ret(hole(tm))
      }
    }
  | Bin(Drv(Exp(l)), ([(_id, ([t], []))], []), Drv(Typ(r))) as tm =>
    switch (t) {
    | ":" => ret(HasType(l, r))
    | "=>" => ret(Syn(l, r))
    | "<=" => ret(Ana(l, r))
    | _ => ret(hole(tm))
    }
  | Pre(([(_id, (["$"], []))], []), Drv(Exp(r))) as tm =>
    switch (r.term) {
    | Var(v) => (Quote("$" ++ v), IdTagged.ids(r))
    | _ => ret(hole(tm))
    }
  | Pre(([(_id, t)], []), Drv(Exp(r))) as tm =>
    switch (t) {
    | (["-"], []) => ret(Neg(r))
    | (["!"], []) => ret(Impl(r, Falsity |> Drv.Exp.fresh))
    | (["|-"], []) => ret(Entail(Ctx([]) |> Drv.Exp.fresh, r))
    | (["if", "then", "else"], [Drv(Exp(cond)), Drv(Exp(conseq))]) =>
      ret(If(cond, conseq, r))
    | (["let", "=", "in"], [Drv(Pat(pat)), Drv(Exp(def))]) =>
      ret(Let(pat, def, r))
    | (["fix", "->"], [Drv(Pat(pat))]) => ret(Fix(pat, r))
    | (["fun", "->"], [Drv(Pat(pat))]) => ret(Fun(pat, r))
    | _ => ret(hole(tm))
    }
  | Pre(([(_id, (labels, [Drv(Typ(l))]))], []), Drv(Typ(r))) as tm =>
    switch (labels) {
    | ["consistent", "~"] => ret(Consistent(l, r))
    | ["matched_arrow", "with"] => ret(MatchedArrow(l, r))
    | ["matched_prod", "with"] => ret(MatchedProd(l, r))
    | ["matched_sum", "with"] => ret(MatchedSum(l, r))
    | _ => ret(hole(tm))
    }
  | Post(Drv(Exp(l)), ([(_id, t)], [])) as tm =>
    switch (t) {
    | (["(", ")"], [Drv(Exp(r))]) =>
      switch (l.term) {
      | Var("L") => (InjL(r), IdTagged.ids(l))
      | Var("R") => (InjR(r), IdTagged.ids(l))
      | Var("roll") => (Roll(r), IdTagged.ids(l))
      | Var("unroll") => (Unroll(r), IdTagged.ids(l))
      | _ => ret(Ap(l, r))
      }
    | _ => ret(hole(tm))
    }
  | _ as tm => ret(hole(tm));
}
and drv_pat = unsorted => {
  let (term, inner_ids) = drv_pat_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(
    p => Drv(Pat(p)),
    ids,
    {
      annotation: {
        ids: ids,
      },
      term,
    },
  );
}
and drv_pat_term: unsorted => (Drv.Pat.term, list(Id.t)) = {
  let ret = (tm: Drv.Pat.term) => (tm, []);
  let hole: unsorted => DrvTermBase.pat_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, ([t], []))], [])) as tm =>
    switch (t) {
    | _ when Form.is_typ_var(t) => ret(Var(t))
    | _ => ret(hole(tm))
    }
  | Op(([(_id, (["(", ")"], [Drv(Pat(body))]))], [])) =>
    ret(Parens(body))
  | Post(Drv(Pat(l)), ([(_id, (["(", ")"], [Drv(Pat(r))]))], [])) as tm =>
    switch (l.term) {
    | Var("L") => (InjL(r), IdTagged.ids(l))
    | Var("R") => (InjR(r), IdTagged.ids(l))
    | _ => ret(hole(tm))
    }
  | Bin(Drv(Pat(l)), ([(_id, ([":"], []))], []), Drv(Typ(r))) =>
    ret(Cast(l, r))
  | Bin(Drv(Pat(l)), ([(_id, ([","], []))], []), Drv(Pat(r))) =>
    ret(Pair(l, r))
  | Pre(([(_id, (["$"], []))], []), Drv(Pat(r))) as tm =>
    switch (r.term) {
    | Var(v) => (Quote("$" ++ v), IdTagged.ids(r))
    | _ => ret(hole(tm))
    }
  | _ as tm => ret(hole(tm));
}

and drv_typ = unsorted => {
  let (term, inner_ids) = drv_typ_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(
    ty => Drv(Typ(ty)),
    ids,
    {
      annotation: {
        ids: ids,
      },
      term,
    },
  );
}
and drv_typ_term: unsorted => (Drv.Typ.term, list(Id.t)) = {
  let ret = (tm: Drv.Typ.term) => (tm, []);
  let hole: unsorted => DrvTermBase.typ_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, ([t], []))], [])) as tm =>
    switch (t) {
    | "Num" => ret(Num)
    | "Bool" => ret(Bool)
    | "1"
    | "Unit" => ret(Unit)
    | _ when Form.is_explicit_hole(t) => ret(TypHole)
    | _ when Form.is_typ_var(t) => ret(Var(t))
    | _ => ret(hole(tm))
    }
  | Op(([(_id, (["(", ")"], [Drv(Typ(body))]))], [])) =>
    ret(Parens(body))
  | Pre(([(_id, (["rec", "->"], [Drv(TPat(p))]))], []), Drv(Typ(t))) =>
    ret(Rec(p, t))
  | Pre(([(_id, (["$"], []))], []), Drv(Typ(r))) as tm =>
    switch (r.term) {
    | Var(v) => (Quote("$" ++ v), IdTagged.ids(r))
    | _ => ret(hole(tm))
    }
  | Bin(Drv(Typ(l)), ([(_id, ([t], []))], []), Drv(Typ(r))) as tm =>
    switch (t) {
    | "->" => ret(Arrow(l, r))
    | "*" => ret(Prod(l, r))
    | "+" => ret(Sum(l, r))
    | _ => ret(hole(tm))
    }
  | _ as tm => ret(hole(tm));
}

and drv_tpat = unsorted => {
  let (term, inner_ids) = drv_tpat_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(
    tpat => Drv(TPat(tpat)),
    ids,
    {
      annotation: {
        ids: ids,
      },
      term,
    },
  );
}
and drv_tpat_term: unsorted => (Drv.TPat.term, list(Id.t)) = {
  let ret = (tm: Drv.TPat.term) => (tm, []);
  let hole: unsorted => DrvTermBase.tpat_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, ([t], []))], [])) when Form.is_typ_var(t) => ret(Var(t))
  | Pre(([(_id, (["$"], []))], []), Drv(TPat(r))) as tm =>
    switch (r.term) {
    | Var(v) => (Quote("$" ++ v), IdTagged.ids(r))
    | _ => ret(hole(tm))
    }
  | _ as tm => ret(hole(tm));
}

and exp = unsorted => {
  let (term, inner_ids) = exp_term(unsorted);
  // Note(zhiyao): the root of Editor.t can change, however the term is
  // still Exp, so the easiest way to get drv terms is to catch a
  // multi-hole error here.
  switch (term) {
  | MultiHole([Drv(_), ..._]) =>
    let (term, inner_ids) = drv_exp_term(unsorted);
    let ids = ids(unsorted) @ inner_ids;
    let exp =
      return(
        e => Drv(Exp(e)),
        ids,
        {
          annotation: {
            ids: ids,
          },
          term,
        },
      );
    Grammar.DrvExp(Exp(exp), Jdmt) |> IdTagged.fresh;
  | _ =>
    let ids = ids(unsorted) @ inner_ids;
    let e: TermBase.exp_t =
      return(
        e => Exp(e),
        ids,
        {
          annotation: {
            ids: ids,
          },
          term,
        },
      );
    switch (term) {
    | TupLabel(_) =>
      // The tile id is the id of the tuple not the tuplabel
      let (e_term, rewrap) = IdTagged.unwrap(e);
      rewrap(Tuple([e_term |> Exp.fresh]): Exp.term);
    | _ => e
    };
  };
}
and exp_term: unsorted => (Exp.term, list(Id.t)) = {
  let ret = (tm: Exp.term) => (tm, []);
  let hole = unsorted => Exp.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    // single-tile case
    | ([(id, t)], []) =>
      switch (t) {
      | ([t], []) when Form.is_empty_tuple(t) => ret(Tuple([]))
      | ([t], []) when Form.is_wild(t) => ret(Deferral(OutsideAp))
      | ([t], []) when Form.is_empty_list(t) => ret(ListLit([]))
      | ([t], []) when Form.is_bool(t) =>
        ret(Atom(Bool(bool_of_string(t))))
      | ([t], []) when Form.is_undefined(t) => ret(Undefined)
      | ([t], []) when Form.is_int(t) =>
        ret(Atom(Int(Bigint.of_string(t))))
      | ([t], []) when Form.is_string(t) =>
        ret(Atom(String(Form.strip_quotes(t))))
      | ([t], []) when Form.is_float(t) =>
        ret(Atom(Float(float_of_string(t))))
      | ([t], []) when Form.is_livelit(t) =>
        ret(LivelitName(Form.parse_livelit(t)))
      | ([t], []) when Form.is_var(t) => ret(Var(t))
      | ([t], []) when Form.is_ctr(t) => ret(Constructor(t, None))
      | (["(", ")"], [Exp(body)]) => ret(Parens(body))
      | (label, [Exp(body)]) when is_probe_wrap(label) =>
        // Temporary wrapping form to persist projector probes
        ret(should_instrument(id) ? Probe(body, Probe.empty) : body.term)
      | (["[", "]"], [Exp(body)]) =>
        switch (body) {
        | {annotation: {ids}, term: Tuple(es)} => (ListLit(es), ids)
        | term => ret(ListLit([term]))
        }
      | (["test", "end"], [Exp(test)]) => ret(Test(test))
      | (
          ["case", "end"],
          [Rul({term: Rules(scrut, rules), annotation: {ids, _}})],
        ) => (
          Match(scrut, rules),
          ids,
        )
      // Note(zhiyao): convert Drv to Exp
      | (["of_jdmt", "end"], [Drv(Exp(j))]) => ret(DrvExp(Exp(j), Jdmt))
      | (["of_ctx", "end"], [Drv(Exp(c))]) => ret(DrvExp(Exp(c), Ctx))
      | (["of_prop", "end"], [Drv(Exp(p))]) => ret(DrvExp(Exp(p), Prop))
      | (["of_alfa_exp", "end"], [Drv(Exp(e))]) =>
        ret(DrvExp(Exp(e), Exp))
      | (["of_alfa_typ", "end"], [Drv(Typ(t))]) =>
        ret(DrvExp(Typ(t), Typ))
      | (["of_alfa_pat", "end"], [Drv(Pat(p))]) =>
        ret(DrvExp(Pat(p), Pat))
      | (["of_alfa_tpat", "end"], [Drv(TPat(tp))]) =>
        ret(DrvExp(TPat(tp), TPat))
      | ([t], []) when t != " " && !Form.is_explicit_hole(t) =>
        ret(Invalid(t))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Pre(tiles, Exp(r)) as tm =>
    switch (tiles) {
    | ([(_id, t)], []) =>
      ret(
        switch (t) {
        | (["$"], []) => UnOp(Meta(Unquote), r)
        | (["-"], []) => UnOp(Int(Minus), r)
        | (["!"], []) => UnOp(Bool(Not), r)
        | (["fun", "->"], [Pat(pat)]) => Fun(pat, r, None, None)
        | (["fix", "->"], [Pat(pat)]) => FixF(pat, r, None)
        | (["typfun", "->"], [TPat(tpat)]) => TypFun(tpat, r, None)
        | (["let", "=", "in"], [Pat(pat), Exp(def)]) => Let(pat, def, r)
        | (["hide", "in"], [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Eval, One),
              pat: filter,
            }),
            r,
          )
        | (["eval", "in"], [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Eval, All),
              pat: filter,
            }),
            r,
          )
        | (["pause", "in"], [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Step, One),
              pat: filter,
            }),
            r,
          )
        | (["debug", "in"], [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Step, All),
              pat: filter,
            }),
            r,
          )
        | (["use", "in"], [Typ(ty)]) => Use(ty, r)
        | (["type", "=", "in"], [TPat(tpat), Typ(def)]) =>
          TyAlias(tpat, def, r)
        | (["if", "then", "else"], [Exp(cond), Exp(conseq)]) =>
          If(cond, conseq, r)
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Post(Exp(l), tiles) as tm =>
    switch (tiles) {
    | ([(_id, t)], []) =>
      switch (t) {
      | (["()"], []) =>
        ret(
          Ap(
            Forward,
            l,
            {
              annotation: {
                ids: [Id.nullary_ap_flag],
              },
              term: Tuple([]),
            },
          ),
        )
      | (["(", ")"], [Exp(arg)]) =>
        let use_deferral = (arg: Exp.t): Exp.t => {
          annotation: {
            ids: IdTagged.ids(arg),
          },
          term: Deferral(InAp),
        };
        switch (arg.term) {
        | Var(l) when Form.is_livelit(l) =>
          ret(LivelitName(Form.parse_livelit(l)))
        | _ when Exp.is_deferral(arg) =>
          ret(DeferredAp(l, [use_deferral(arg)]))
        | Tuple(es) when List.exists(Exp.is_deferral, es) => (
            DeferredAp(
              l,
              List.map(
                arg => Exp.is_deferral(arg) ? use_deferral(arg) : arg,
                es,
              ),
            ),
            IdTagged.ids(arg),
          )
        | _ => ret(Ap(Forward, l, arg))
        };
      | (["@<", ">"], [Typ(ty)]) => ret(TypAp(l, ty))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Exp(l), tiles, Typ(r)) as tm =>
    switch (tiles) {
    | ([(_id, ([":"], []))], []) =>
      ret(Cast(l, Unknown(Internal) |> Typ.fresh, r))
    | _ => ret(hole(tm))
    }
  | Bin(Exp(l), tiles, Exp(r)) as tm =>
    switch (is_tuple_exp(tiles)) {
    | Some(between_kids) =>
      let tuple_children: list(Exp.t) =
        [l]
        @ between_kids
        @ [r]
        |> List.map((child: Exp.t) => {
             switch (child) {
             | {term: Tuple([{term: TupLabel(_) as tl, _}]), _} as tup =>
               // We use the Id for the tuple as the ids for the tuplabels
               let (_, rewrap) = IdTagged.unwrap(tup);
               rewrap(tl);
             | _ => child
             }
           });

      ret(Tuple(tuple_children));
    | None =>
      switch (tiles) {
      | ([(_id, t)], []) =>
        ret(
          switch (t) {
          | (["+"], []) => BinOp(Int(Plus), l, r)
          | (["-"], []) => BinOp(Int(Minus), l, r)
          | (["*"], []) => BinOp(Int(Times), l, r)
          | (["**"], []) => BinOp(Int(Power), l, r)
          | (["/"], []) => BinOp(Int(Divide), l, r)
          | (["<"], []) => BinOp(Int(LessThan), l, r)
          | ([">"], []) => BinOp(Int(GreaterThan), l, r)
          | (["<="], []) => BinOp(Int(LessThanOrEqual), l, r)
          | ([">="], []) => BinOp(Int(GreaterThanOrEqual), l, r)
          | (["=="], []) => BinOp(Int(Equals), l, r)
          | (["!="], []) => BinOp(Int(NotEquals), l, r)
          | (["+."], []) => BinOp(Float(Plus), l, r)
          | (["-."], []) => BinOp(Float(Minus), l, r)
          | (["*."], []) => BinOp(Float(Times), l, r)
          | (["/."], []) => BinOp(Float(Divide), l, r)
          | (["**."], []) => BinOp(Float(Power), l, r)
          | (["<."], []) => BinOp(Float(LessThan), l, r)
          | ([">."], []) => BinOp(Float(GreaterThan), l, r)
          | (["<=."], []) => BinOp(Float(LessThanOrEqual), l, r)
          | ([">=."], []) => BinOp(Float(GreaterThanOrEqual), l, r)
          | (["==."], []) => BinOp(Float(Equals), l, r)
          | (["!=."], []) => BinOp(Float(NotEquals), l, r)
          | (["&&"], []) => BinOp(Bool(And), l, r)
          | (["||"], []) => BinOp(Bool(Or), l, r)
          | (["::"], []) => Cons(l, r)
          | ([";"], []) => Seq(l, r)
          | (["++"], []) => BinOp(String(Concat), l, r)
          | (["$=="], []) => BinOp(String(Equals), l, r)
          | (["="], []) =>
            switch (l.term) {
            | Var(name) =>
              TupLabel(
                {
                  annotation: l.annotation,
                  term: Label(name),
                },
                r,
              )
            | EmptyHole => TupLabel(l, r)
            | _ =>
              let (e_term, rewrap) = IdTagged.unwrap(l);

              TupLabel(
                rewrap(MultiHole([Exp(e_term |> Exp.fresh)]): Exp.term),
                r,
              );
            }
          | (["."], []) =>
            switch (r.term) {
            | Var(name) =>
              Dot(
                l,
                {
                  annotation: r.annotation,
                  term: Label(name),
                },
              )
            | EmptyHole => Dot(l, r)
            | _ =>
              let (e_term, rewrap) = IdTagged.unwrap(r);

              Dot(
                l,
                rewrap(MultiHole([Exp(e_term |> Exp.fresh)]): Exp.term),
              );
            }
          | (["|>"], []) => Ap(Reverse, r, l)
          | (["@"], []) => ListConcat(l, r)
          | _ => hole(tm)
          },
        )
      | _ => ret(hole(tm))
      }
    }
  | tm => ret(hole(tm));
}
and pat = unsorted => {
  let (term, inner_ids) = pat_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  let p =
    return(
      p => Pat(p),
      ids,
      {
        annotation: {
          ids: ids,
        },
        term,
      },
    );
  switch (term) {
  | TupLabel(_) => Tuple([p]) |> Pat.fresh
  | _ => p
  };
}
and pat_term: unsorted => (Pat.term, list(Id.t)) = {
  let ret = (term: Pat.term) => (term, []);
  let hole = unsorted => Pat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(id, tile)], []) =>
      ret(
        switch (tile) {
        | ([t], []) when Form.is_empty_tuple(t) => Tuple([])
        | ([t], []) when Form.is_empty_list(t) => ListLit([])
        | ([t], []) when Form.is_bool(t) => Atom(Bool(bool_of_string(t)))
        | ([t], []) when Form.is_float(t) =>
          Atom(Float(float_of_string(t)))
        | ([t], []) when Form.is_int(t) => Atom(Int(Bigint.of_string(t)))
        | ([t], []) when Form.is_string(t) =>
          Atom(String(Form.strip_quotes(t)))
        | ([t], []) when Form.is_var(t) => Var(t)
        | ([t], []) when Form.is_wild(t) => Wild
        | ([t], []) when Form.is_ctr(t) => Constructor(t, None)
        | ([t], []) when t != " " && !Form.is_explicit_hole(t) =>
          Invalid(t)
        | (["(", ")"], [Pat(body)]) => Parens(body)
        | (label, [Pat(body)]) when is_probe_wrap(label) =>
          should_instrument(id) ? Probe(body, Probe.empty) : body.term
        | (["[", "]"], [Pat(body)]) =>
          switch (body) {
          | {term: Tuple(ps), _} => ListLit(ps)
          | term => ListLit([term])
          }
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Post(Pat(l), tiles) as tm =>
    switch (tiles) {
    | ([(_id, t)], []) =>
      ret(
        switch (t) {
        | (["(", ")"], [Pat(arg)]) => Ap(l, arg)
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Pre(([(_id, (["$"], []))], []), Pat(r)) as tm =>
    switch (r.term) {
    | Var(v) => (Var("$" ++ v), IdTagged.ids(r))
    | _ => ret(hole(tm))
    }
  | Bin(Pat(p), tiles, Typ(ty)) as tm =>
    switch (tiles) {
    | ([(_id, ([":"], []))], []) =>
      ret(Cast(p, ty, Unknown(Internal) |> Typ.fresh))
    | _ => ret(hole(tm))
    }
  | Bin(Pat(l), tiles, Pat(r)) as tm =>
    switch (is_tuple_pat(tiles)) {
    | Some(between_kids) =>
      let tuple_children =
        [l]
        @ between_kids
        @ [r]
        |> List.map((child: Pat.t) => {
             switch (child) {
             | {term: Tuple([{term: TupLabel(_), _} as tl]), _} => tl
             | _ => child
             }
           });
      ret(Tuple(tuple_children));
    | None =>
      switch (tiles) {
      | ([(_id, (["="], []))], []) =>
        switch (l.term) {
        | Var(name) =>
          ret(
            TupLabel(
              {
                annotation: l.annotation,
                term: Label(name),
              },
              r,
            ),
          )
        | EmptyHole => ret(TupLabel(l, r))
        | _ =>
          let (e_term, rewrap) = IdTagged.unwrap(l);
          ret(
            TupLabel(
              rewrap(MultiHole([Pat(e_term |> Pat.fresh)]): Pat.term),
              r,
            ),
          );
        }
      | ([(_id, (["::"], []))], []) => ret(Cons(l, r))
      | _ => ret(hole(tm))
      }
    }
  | tm => ret(hole(tm));
}
and typ = unsorted => {
  let (term, inner_ids) = typ_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  let t =
    return(
      ty => Typ(ty),
      ids,
      {
        term,
        annotation: {
          ids: ids,
        },
      },
    );
  switch (term) {
  | TupLabel(_) => Prod([t]) |> Typ.fresh
  | _ => t
  };
}
and typ_term: unsorted => (Typ.term, list(Id.t)) = {
  let ret = (term: Typ.term) => (term, []);
  let hole = unsorted => Typ.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | ([t], []) when Form.is_empty_tuple(t) => Prod([])
        | (["Bool"], []) => Atom(Bool)
        | (["Int"], []) => Atom(Int)
        | (["SInt"], []) => Atom(SInt)
        | (["Float"], []) => Atom(Float)
        | (["String"], []) => Atom(String)
        | (["Nat"], []) => Atom(Nat)
        | ([t], []) when Form.is_typ_var(t) => Var(t)
        | (["(", ")"], [Typ(body)]) => Parens(body)
        | (label, [Typ(body)]) when is_probe_wrap(label) => body.term
        | (["[", "]"], [Typ(body)]) => List(body)
        | ([t], []) when t != " " && !Form.is_explicit_hole(t) =>
          Unknown(Hole(Invalid(t)))
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Post(Typ(t), tiles) as tm =>
    switch (tiles) {
    | ([(_, (["(", ")"], [Typ(typ)]))], []) => ret(Ap(t, typ))
    | _ => ret(hole(tm))
    }
  /* forall and rec have to be before sum so that they bind tighter.
   * Thus `rec A -> Left(A) + Right(B)` get parsed as `rec A -> (Left(A) + Right(B))`
   * If this is below the case for sum, then it gets parsed as an invalid form. */
  | Pre(([(_id, (["forall", "->"], [TPat(tpat)]))], []), Typ(t)) =>
    ret(Forall(tpat, t))
  | Pre(([(_id, (["rec", "->"], [TPat(tpat)]))], []), Typ(t)) =>
    ret(Rec(tpat, t))
  | Pre(tiles, Typ({term: Sum(t0), annotation: {ids, _}})) as tm =>
    /* Case for leading prefix + preceeding a sum */
    switch (tiles) {
    | ([(_, (["+"], []))], []) => (Sum(t0), ids)
    | _ => ret(hole(tm))
    }
  | Pre(tiles, Typ(t)) as tm =>
    switch (tiles) {
    | ([(_, (["+"], []))], []) =>
      ret(Sum([parse_sum_term(t)] |> ConstructorMap.mk(~mk_bad)))
    | _ => ret(hole(tm))
    }
  | Bin(Typ(t1), tiles, Typ(t2)) as tm when is_typ_bsum(tiles) != None =>
    switch (is_typ_bsum(tiles)) {
    | Some(between_kids) =>
      ret(
        Sum(
          List.map(parse_sum_term, [t1] @ between_kids @ [t2])
          |> ConstructorMap.mk(~mk_bad),
        ),
      )
    | None => ret(hole(tm))
    }
  | Bin(Typ(l), tiles, Typ(r)) as tm =>
    switch (is_tuple_typ(tiles)) {
    | Some(between_kids) =>
      let tuple_children: list(Typ.t) =
        [l]
        @ between_kids
        @ [r]
        |> List.map((child: Typ.t) => {
             switch (child) {
             | {term: Prod([{term: TupLabel(_), _} as tl]), _} => tl
             | _ => child
             }
           });

      ret(Prod(tuple_children));
    | None =>
      switch (tiles) {
      | ([(_id, (["->"], []))], []) => ret(Arrow(l, r))
      | ([(_id, (["="], []))], []) =>
        switch (l.term) {
        | Var(name) =>
          ret(
            TupLabel(
              {
                annotation: l.annotation,
                term: Label(name),
              },
              r,
            ),
          )
        | _ => ret(TupLabel(l, r))
        }
      | _ => ret(hole(tm))
      }
    }
  | tm => ret(hole(tm));
}
and tpat = unsorted => {
  let term = tpat_term(unsorted);
  let ids = ids(unsorted);
  return(
    ty => TPat(ty),
    ids,
    {
      term,
      annotation: {
        ids: ids,
      },
    },
  );
}
and tpat_term: unsorted => TPat.term = {
  let ret = (term: TPat.term) => term;
  let hole = unsorted => TPat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | ([t], []) when Form.is_typ_var(t) => Var(t)
        | ([t], []) when t != " " && !Form.is_explicit_hole(t) =>
          Invalid(t)
        | (label, [TPat(body)]) when is_probe_wrap(label) => body.term
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | (Pre(_) | Post(_)) as tm => ret(hole(tm))
  | tm => ret(hole(tm));
}

// and rul = unsorted => {
//   let term = rul_term(unsorted);
//   let ids = ids(unsorted);
//   return(r => Rul(r), ids, {ids, term});
// }
and rul = (unsorted): Rul.t => {
  let hole: Rul.term = Hole(kids_of_unsorted(unsorted));
  switch (exp(unsorted)) {
  | {term: MultiHole(_), _} =>
    switch (unsorted) {
    | Bin(Exp(scrut), tiles, Exp(last_clause)) =>
      switch (is_rules(tiles)) {
      | Some((ps, leading_clauses)) => {
          annotation: {
            ids: ids(unsorted),
          },
          term:
            Rules(scrut, List.combine(ps, leading_clauses @ [last_clause])),
        }
      | None => {
          term: hole,
          annotation: {
            ids: ids(unsorted),
          },
        }
      }
    | _ => {
        term: hole,
        annotation: {
          ids: ids(unsorted),
        },
      }
    }
  | e => {
      term: Rules(e, []),
      annotation: {
        ids: [],
      },
    }
  };
}

and unsorted = (skel: Skel.t, seg: Segment.t): unsorted => {
  /* Remove projectors. We do this here as opposed to removing
   * them in an external call to save a whole-syntax pass. */
  let tile_kids = (p: Piece.t): list(Term.Any.t) =>
    switch (p) {
    | Secondary(_)
    | Grout(_) => []
    | Projector({syntax, _} as pr) =>
      let _ = log_projector(pr);
      let sort = Piece.sort(syntax) |> fst;
      let seg = Piece.unparenthesize(syntax);
      [go_s(sort, Segment.skel(seg), seg)];
    | Tile({mold, shards, children, _}) =>
      Aba.aba_triples(Aba.mk(shards, children))
      |> List.map(((l, kid, r)) => {
           let s = l + 1 == r ? List.nth(mold.in_, l) : Sort.Any;
           go_s(s, Segment.skel(kid), kid);
         })
    };

  let root: Aba.t(Piece.t, Skel.t) =
    Skel.root(skel) |> Aba.map_a(List.nth(seg));

  // maintaining this alternating ordered structure
  // for handling incomplete forms later
  let tiles =
    root
    |> Aba.map_abas(((p_l, kid, p_r)) => {
         let (_, s_l) = Piece.nib_sorts(p_l);
         let (s_r, _) = Piece.nib_sorts(p_r);
         let s = s_l == s_r ? s_l : Sort.Any;
         go_s(s, kid, seg);
       })
    |> Aba.map_a(p
         // TODO throw proper exception
         => (Piece.id(p), Aba.mk(tokens(p), tile_kids(p))));

  let (l_sort, r_sort) = {
    let p_l = Aba.first_a(root);
    let p_r = Aba.last_a(root);
    // TODO throw proper exceptions
    let (l, _) = Option.get(Piece.nibs(p_l));
    let (_, r) = Option.get(Piece.nibs(p_r));
    (l.sort, r.sort);
  };

  switch (skel) {
  | Op(_) => Op(tiles)
  | Pre(_, r) => Pre(tiles, go_s(r_sort, r, seg))
  | Post(l, _) => Post(go_s(l_sort, l, seg), tiles)
  | Bin(l, _, r) => Bin(go_s(l_sort, l, seg), tiles, go_s(r_sort, r, seg))
  };
};

let go =
  Core.Memo.general(
    ~cache_size_bound=1000,
    seg => {
      map := TermMap.empty;
      projectors := Id.Map.empty;
      let term = exp(unsorted(Segment.skel(seg), seg));
      {
        term,
        terms: map^,
        projectors: projectors^,
      };
    },
  );

let for_projection =
  /* Returns Nul() unless segment represents a well-structured term in isolation.
   * This means that the term is complete, modulo non-empty holes and sort errors.
   * Specifically, it ensures there are no incomplete tiles in the segment, and
   * that no contained sub-segment is non-convex. However, there can still be convex
   * holes, singleton multiholes representing sort errors, non-singleton multiholes
   * representing missing infix operators, and invalid tokens. */
  Core.Memo.general(~cache_size_bound=1000, (seg: Segment.t) =>
    if (!Segment.deep_tile_complete(seg)) {
      None; /* Returns None if any subsegment contains incomplete tiles */
    } else if (Segment.is_padded(seg)) {
      None; /* Returns None if the segment has secondary around it */
    } else {
      switch (Segment.skel(seg)) {
      | exception _ => None /* Returns None if any subsegment is non-convex */
      | skel =>
        let (unsorted, sort) = (
          unsorted(skel, seg),
          Segment.sort_of(skel, seg),
        );
        switch (sort) {
        | Drv(_) => None // TODO(zhiyao): handle this case
        | Exp =>
          switch (exp(unsorted)) {
          | {term: Tuple(_), _} => None
          | _ => Some(Grammar.Exp(exp(unsorted)))
          }
        | Pat =>
          switch (pat(unsorted)) {
          | {term: Tuple(_), _} => None
          | _ => Some(Pat(pat(unsorted)))
          }
        | Typ =>
          switch (typ(unsorted)) {
          | {term: Prod(_), _} => None
          | _ => Some(Typ(typ(unsorted)))
          }
        | TPat =>
          switch (tpat(unsorted)) {
          | _ => Some(TPat(tpat(unsorted)))
          }
        /* Rul case below prevents returning pseudo-terms
         * consisting of case scrutinee + rule(s) */
        | Rul => None
        | Any => Some(Any()) /* grout */
        };
      };
    }
  );

let from_zip_for_sem =
    (~dump_backpack: bool, ~erase_buffer: bool, z: Zipper.t) => {
  let seg = Zipper.smart_seg(~dump_backpack, ~erase_buffer, z);
  go(seg);
};

let from_zip_for_sem =
  Core.Memo.general(
    ~cache_size_bound=1000,
    from_zip_for_sem(~dump_backpack=true, ~erase_buffer=true),
  );
