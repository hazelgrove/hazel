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
open Any;

// TODO make less hacky
let tokens =
  Piece.get(
    _ => [],
    _ => [" "],
    (t: Tile.t) => t.shards |> List.map(List.nth(t.label)),
    _ => [],
  );

[@deriving (show({with_path: false}), sexp, yojson)]
type tile = (Id.t, Aba.t(Token.t, t));
[@deriving (show({with_path: false}), sexp, yojson)]
type tiles = Aba.t(tile, t);
let single = (id, subst) => ([(id, subst)], []);

[@deriving (show({with_path: false}), sexp, yojson)]
type unsorted =
  | Op(tiles)
  | Pre(tiles, t)
  | Post(t, tiles)
  | Bin(t, tiles, t);

type t = {
  term: UExp.t,
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
let is_tuple_prop = is_nary(Any.is_alfa_exp, ",");
let is_typ_bsum = is_nary(Any.is_typ, "+");

let is_grout = tiles =>
  Aba.get_as(tiles) |> List.map(snd) |> List.for_all((==)(([" "], [])));

let is_rules = ((ts, kids): tiles): option(Aba.t(UPat.t, UExp.t)) => {
  open OptUtil.Syntax;
  let+ ps =
    ts
    |> List.map(
         fun
         | (_, (["|", "=>"], [Any.Pat(p)])) => Some(p)
         | _ => None,
       )
    |> OptUtil.sequence
  and+ clauses =
    kids
    |> List.map(
         fun
         | Exp(clause) => Some(clause)
         | _ => None,
       )
    |> OptUtil.sequence;
  Aba.mk(ps, clauses);
};
let is_alfa_rules =
    ((ts, kids): tiles): option(Aba.t(Drv.Pat.t, Drv.Exp.t)) => {
  open OptUtil.Syntax;
  let+ ps =
    ts
    |> List.map(
         fun
         | (_, (["|", "=>"], [Any.Drv(Pat(p))])) => Some(p)
         | _ => None,
       )
    |> OptUtil.sequence
  and+ clauses =
    kids
    |> List.map(
         fun
         | Drv(Exp(clause)) => Some(clause)
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
let rm_and_log_projectors = (seg: Segment.t): Segment.t =>
  List.map(
    fun
    | Piece.Projector(pr) => {
        projectors := Id.Map.add(pr.id, pr, projectors^);
        pr.syntax;
      }
    | x => x,
    seg,
  );

let parse_sum_term: UTyp.t => ConstructorMap.variant(UTyp.t) =
  fun
  | {term: Var(ctr), ids, _} => Variant(ctr, ids, None)
  | {term: Ap({term: Var(ctr), ids: ids_ctr, _}, u), ids: ids_ap, _} =>
    Variant(ctr, ids_ctr @ ids_ap, Some(u))
  | t => BadEntry(t);

let mk_bad = (ctr, ids, value) => {
  let t: Typ.t = {ids, copied: false, term: Var(ctr)};
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
      | Prop => failwith("unexpected drv sort")
      | Exp => Exp(alfa_exp(unsorted(skel, seg)))
      | Rul => Rul(alfa_rul(unsorted(skel, seg)))
      | Pat => Pat(alfa_pat(unsorted(skel, seg)))
      | Typ => Typ(alfa_typ(unsorted(skel, seg)))
      | TPat => TPat(alfa_tpat(unsorted(skel, seg)))
      },
    )
  | Pat => Pat(pat(unsorted(skel, seg)))
  | TPat => TPat(tpat(unsorted(skel, seg)))
  | Typ => Typ(typ(unsorted(skel, seg)))
  | Exp => Exp(exp(unsorted(skel, seg)))
  | Rul => Rul(rul(unsorted(skel, seg)))
  | Nul => Nul() //TODO
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
and alfa_exp = unsorted => {
  let (term, inner_ids) = alfa_exp_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(e => Drv(Exp(e)), ids, {ids, copied: false, term});
}
and alfa_exp_term: unsorted => (Drv.Exp.term, list(Id.t)) = {
  let ret = (tm: Drv.Exp.term) => (tm, []);
  let hole = unsorted => Drv.Exp.hole(kids_of_unsorted(unsorted));
  fun
  | Op(([(_id, t)], [])) as tm =>
    switch (t) {
    | ([t], []) =>
      switch (t) {
      | "Truth" => ret(Truth)
      | "Falsity" => ret(Falsity)
      | "True" => ret(True)
      | "False" => ret(False)
      | "L" => ret(InjL)
      | "R" => ret(InjR)
      | "roll" => ret(Roll)
      | "unroll" => ret(Unroll)
      | _ when Form.is_empty_list(t) => ret(Ctx([]))
      | _ when Form.is_empty_tuple(t) => ret(Triv)
      | _ when Form.is_int(t) => ret(NumLit(int_of_string(t)))
      | _ when Form.is_typ_var(t) => ret(Var(t))
      | _ => ret(hole(tm))
      }
    | (["val", "end"], [Drv(Exp(e))]) => ret(Val(e))
    | (["valid", "end"], [Drv(Typ(t))]) => ret(Type(t))
    | (["[", "]"], [Drv(Exp(body))]) =>
      switch (body) {
      | {ids, copied: false, term: Tuple(es)} => (Ctx(es), ids)
      | term => ret(Ctx([term]))
      }
    | (["(", ")"], [Drv(Exp(body))]) => ret(Parens(body))
    | (["{", "}"], [Pat(var)]) => ret(Abbr(var))
    | (["case", "end"], [Drv(Rul({ids, term: Rules(scrut, rules), _}))]) => (
        Case(scrut, rules),
        ids,
      )
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
    | "+" => ret(Plus(l, r))
    | "-" => ret(Minus(l, r))
    | "*" => ret(Times(l, r))
    | "==" => ret(Eq(l, r))
    | "<" => ret(Lt(l, r))
    | ">" => ret(Gt(l, r))
    | _ => ret(hole(tm))
    }
  | Bin(Drv(Exp(l)), tiles, Drv(Exp(r))) as tm =>
    switch (is_tuple_prop(tiles)) {
    | Some(between_kids) => ret(Tuple([l] @ between_kids @ [r]))
    | None => ret(hole(tm))
    }
  | Bin(Drv(Exp(l)), ([(_id, ([t], []))], []), Drv(Typ(r))) as tm =>
    switch (t) {
    | ":" => ret(HasType(l, r))
    | "=>" => ret(Syn(l, r))
    | "<=" => ret(Ana(l, r))
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
  | Post(Drv(Exp(l)), ([(_id, t)], [])) as tm =>
    switch (t) {
    | ([".fst"], []) => ret(PrjL(l))
    | ([".snd"], []) => ret(PrjR(l))
    | (["(", ")"], [Drv(Exp(r))]) => ret(Ap(l, r))
    | _ => ret(hole(tm))
    }
  | _ as tm => ret(hole(tm));
}
and alfa_rul = (unsorted: unsorted) => {
  let hole = Drv.Rul.hole(kids_of_unsorted(unsorted));
  switch (alfa_exp(unsorted)) {
  | {term: Hole(MultiHole(_)), _} =>
    switch (unsorted) {
    | Bin(Drv(Exp(scrut)), tiles, Drv(Exp(last_clause))) =>
      switch (is_alfa_rules(tiles)) {
      | Some((ps, leading_clauses)) => {
          ids: ids(unsorted),
          term:
            Rules(scrut, List.combine(ps, leading_clauses @ [last_clause])),
          copied: false,
        }
      | None => {ids: ids(unsorted), term: hole, copied: false}
      }
    | _ => {ids: ids(unsorted), term: hole, copied: false}
    }
  | _ => {ids: ids(unsorted), term: hole, copied: false}
  };
}
and alfa_pat = unsorted => {
  let (term, inner_ids) = alfa_pat_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(p => Drv(Pat(p)), ids, {ids, term, copied: false});
}
and alfa_pat_term: unsorted => (Drv.Pat.term, list(Id.t)) = {
  let ret = (tm: Drv.Pat.term) => (tm, []);
  let hole = unsorted => Drv.Pat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(([(_id, ([t], []))], [])) as tm =>
    switch (t) {
    | "L" => ret(InjL)
    | "R" => ret(InjR)
    | _ when Form.is_typ_var(t) => ret(Var(t))
    | _ => ret(hole(tm))
    }
  | Op(([(_id, (["(", ")"], [Drv(Pat(body))]))], [])) =>
    ret(Parens(body))
  | Post(Drv(Pat(l)), ([(_id, (["(", ")"], [Drv(Pat(r))]))], [])) =>
    ret(Ap(l, r))
  | Bin(Drv(Pat(l)), ([(_id, ([":"], []))], []), Drv(Typ(r))) =>
    ret(Cast(l, r))
  | Bin(Drv(Pat(l)), ([(_id, ([","], []))], []), Drv(Pat(r))) =>
    ret(Pair(l, r))
  | _ as tm => ret(hole(tm));
}

and alfa_typ = unsorted => {
  let (term, inner_ids) = alfa_typ_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(ty => Drv(Typ(ty)), ids, {ids, term, copied: false});
}
and alfa_typ_term: unsorted => (Drv.Typ.term, list(Id.t)) = {
  let ret = (tm: Drv.Typ.term) => (tm, []);
  let hole = unsorted => Drv.Typ.hole(kids_of_unsorted(unsorted));
  fun
  | Op(([(_id, ([t], []))], [])) as tm =>
    switch (t) {
    | "Num" => ret(Num)
    | "Bool" => ret(Bool)
    | "1"
    | "Unit" => ret(Unit)
    | _ when Form.is_typ_var(t) => ret(Var(t))
    | _ => ret(hole(tm))
    }
  | Op(([(_id, (["(", ")"], [Drv(Typ(body))]))], [])) =>
    ret(Parens(body))
  | Op(([(_id, (["{", "}"], [Pat(var)]))], [])) => ret(Abbr(var))
  | Pre(([(_id, (["rec", "->"], [Drv(TPat(p))]))], []), Drv(Typ(t))) =>
    ret(Rec(p, t))
  | Bin(Drv(Typ(l)), ([(_id, ([t], []))], []), Drv(Typ(r))) as tm =>
    switch (t) {
    | "->" => ret(Arrow(l, r))
    | "*" => ret(Prod(l, r))
    | "+" => ret(Sum(l, r))
    | _ => ret(hole(tm))
    }
  | _ as tm => ret(hole(tm));
}

and alfa_tpat = unsorted => {
  let (term, inner_ids) = alfa_tpat_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(tpat => Drv(TPat(tpat)), ids, {ids, term, copied: false});
}
and alfa_tpat_term: unsorted => (Drv.TPat.term, list(Id.t)) = {
  let ret = (tm: Drv.TPat.term) => (tm, []);
  let hole = unsorted => Drv.TPat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(([(_id, ([t], []))], [])) when Form.is_typ_var(t) => ret(Var(t))
  | _ as tm => ret(hole(tm));
}

and exp = unsorted => {
  let (term, inner_ids) = exp_term(unsorted);
  switch (term) {
  | MultiHole([Drv(_), ..._]) =>
    let (term, inner_ids) = alfa_exp_term(unsorted);
    let ids = ids(unsorted) @ inner_ids;
    let exp = return(e => Drv(Exp(e)), ids, {ids, copied: false, term});
    TermBase.Exp.Term(Drv(Exp(exp)), Drv(Jdmt)) |> IdTagged.fresh;
  | _ =>
    let ids = ids(unsorted) @ inner_ids;
    return(e => Exp(e), ids, {ids, copied: false, term});
  };
}
and exp_term: unsorted => (UExp.term, list(Id.t)) = {
  let ret = (tm: UExp.term) => (tm, []);
  let hole = unsorted => UExp.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    // single-tile case
    | ([(_id, t)], []) =>
      switch (t) {
      | ([t], []) when Form.is_empty_tuple(t) => ret(Tuple([]))
      | ([t], []) when Form.is_wild(t) => ret(Deferral(OutsideAp))
      | ([t], []) when Form.is_empty_list(t) => ret(ListLit([]))
      | ([t], []) when Form.is_bool(t) => ret(Bool(bool_of_string(t)))
      | ([t], []) when Form.is_undefined(t) => ret(Undefined)
      | ([t], []) when Form.is_int(t) => ret(Int(int_of_string(t)))
      | ([t], []) when Form.is_string(t) =>
        ret(String(Form.strip_quotes(t)))
      | ([t], []) when Form.is_float(t) => ret(Float(float_of_string(t)))
      | ([t], []) when Form.is_var(t) => ret(Var(t))
      | ([t], []) when Form.is_ctr(t) =>
        ret(Constructor(t, Unknown(Internal) |> Typ.temp))
      | (["(", ")"], [Exp(body)]) => ret(Parens(body))
      | (["[", "]"], [Exp(body)]) =>
        switch (body) {
        | {ids, copied: false, term: Tuple(es)} => (ListLit(es), ids)
        | term => ret(ListLit([term]))
        }
      | (["test", "end"], [Exp(test)]) => ret(Test(test))
      | (["case", "end"], [Rul({ids, term: Rules(scrut, rules), _})]) => (
          Match(scrut, rules),
          ids,
        )
      // | (["of_typ", "end"], [Drv(Typ(ty))]) => ret(Term(Drv(Typ(ty))))
      // | (["of_pat", "end"], [Drv(Pat(p))]) => ret(Term(Drv(Pat(p))))
      // | (["of_tpat", "end"], [Drv(TPat(tp))]) =>
      //   ret(Term(Drv(TPat(tp))))
      // | (["of_ctxt", "end"], [Drv(Exp(ctx))]) =>
      //   ret(Term(Drv(Exp(ctx))))
      | (["of_jdmt", "end"], [Drv(Exp(j))]) =>
        ret(Term(Drv(Exp(j)), Drv(Jdmt)))
      | (["of_ctx", "end"], [Drv(Exp(c))]) =>
        ret(Term(Drv(Exp(c)), Drv(Ctx)))
      | (["of_prop", "end"], [Drv(Exp(p))]) =>
        ret(Term(Drv(Exp(p)), Drv(Prop)))
      | (["of_alfa_exp", "end"], [Drv(Exp(e))]) =>
        ret(Term(Drv(Exp(e)), Drv(Exp)))
      | (["of_alfa_typ", "end"], [Drv(Typ(t))]) =>
        ret(Term(Drv(Typ(t)), Drv(Typ)))
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
          Filter(Filter({act: (Eval, One), pat: filter}), r)
        | (["eval", "in"], [Exp(filter)]) =>
          Filter(Filter({act: (Eval, All), pat: filter}), r)
        | (["pause", "in"], [Exp(filter)]) =>
          Filter(Filter({act: (Step, One), pat: filter}), r)
        | (["debug", "in"], [Exp(filter)]) =>
          Filter(Filter({act: (Step, All), pat: filter}), r)
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
            {ids: [Id.nullary_ap_flag], copied: false, term: Tuple([])},
          ),
        )
      | (["(", ")"], [Exp(arg)]) =>
        let use_deferral = (arg: UExp.t): UExp.t => {
          ids: arg.ids,
          copied: false,
          term: Deferral(InAp),
        };
        switch (arg.term) {
        | _ when UExp.is_deferral(arg) =>
          ret(DeferredAp(l, [use_deferral(arg)]))
        | Tuple(es) when List.exists(UExp.is_deferral, es) => (
            DeferredAp(
              l,
              List.map(
                arg => UExp.is_deferral(arg) ? use_deferral(arg) : arg,
                es,
              ),
            ),
            arg.ids,
          )
        | _ => ret(Ap(Forward, l, arg))
        };
      | (["@<", ">"], [Typ(ty)]) => ret(TypAp(l, ty))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Exp(l), tiles, Exp(r)) as tm =>
    switch (is_tuple_exp(tiles)) {
    | Some(between_kids) => ret(Tuple([l] @ between_kids @ [r]))
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
  return(p => Pat(p), ids, {ids, term, copied: false});
}
and pat_term: unsorted => (UPat.term, list(Id.t)) = {
  let ret = (term: UPat.term) => (term, []);
  let hole = unsorted => UPat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | ([t], []) when Form.is_empty_tuple(t) => Tuple([])
        | ([t], []) when Form.is_empty_list(t) => ListLit([])
        | ([t], []) when Form.is_bool(t) => Bool(bool_of_string(t))
        | ([t], []) when Form.is_float(t) => Float(float_of_string(t))
        | ([t], []) when Form.is_int(t) => Int(int_of_string(t))
        | ([t], []) when Form.is_string(t) => String(Form.strip_quotes(t))
        | ([t], []) when Form.is_var(t) => Var(t)
        | ([t], []) when Form.is_wild(t) => Wild
        | ([t], []) when Form.is_ctr(t) =>
          Constructor(t, Unknown(Internal) |> Typ.fresh)
        | ([t], []) when t != " " && !Form.is_explicit_hole(t) =>
          Invalid(t)
        | (["(", ")"], [Pat(body)]) => Parens(body)
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
  | Pre(_) as tm => ret(hole(tm))
  | Bin(Pat(p), tiles, Typ(ty)) as tm =>
    switch (tiles) {
    | ([(_id, ([":"], []))], []) =>
      ret(Cast(p, ty, Unknown(Internal) |> Typ.fresh))
    | _ => ret(hole(tm))
    }
  | Bin(Pat(l), tiles, Pat(r)) as tm =>
    switch (is_tuple_pat(tiles)) {
    | Some(between_kids) => ret(Tuple([l] @ between_kids @ [r]))
    | None =>
      switch (tiles) {
      | ([(_id, (["::"], []))], []) => ret(Cons(l, r))
      | _ => ret(hole(tm))
      }
    }
  | tm => ret(hole(tm));
}
and typ = unsorted => {
  let (term, inner_ids) = typ_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(ty => Typ(ty), ids, {ids, term, copied: false});
}
and typ_term: unsorted => (UTyp.term, list(Id.t)) = {
  let ret = (term: UTyp.term) => (term, []);
  let hole = unsorted => UTyp.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | ([t], []) when Form.is_empty_tuple(t) => Prod([])
        | (["Bool"], []) => Bool
        | (["Int"], []) => Int
        | (["Float"], []) => Float
        | (["String"], []) => String
        | ([t], []) when Form.is_typ_var(t) => Var(t)
        | (["(", ")"], [Typ(body)]) => Parens(body)
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
  | Pre(tiles, Typ({term: Sum(t0), ids, _})) as tm =>
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
    | Some(between_kids) => ret(Prod([l] @ between_kids @ [r]))
    | None =>
      switch (tiles) {
      | ([(_id, (["->"], []))], []) => ret(Arrow(l, r))
      | _ => ret(hole(tm))
      }
    }
  | tm => ret(hole(tm));
}
and tpat = unsorted => {
  let term = tpat_term(unsorted);
  let ids = ids(unsorted);
  return(ty => TPat(ty), ids, {ids, term, copied: false});
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
and rul = (unsorted: unsorted): Rul.t => {
  let hole = Rul.Hole(kids_of_unsorted(unsorted));
  switch (exp(unsorted)) {
  | {term: MultiHole(_), _} =>
    switch (unsorted) {
    | Bin(Exp(scrut), tiles, Exp(last_clause)) =>
      switch (is_rules(tiles)) {
      | Some((ps, leading_clauses)) => {
          ids: ids(unsorted),
          term:
            Rules(scrut, List.combine(ps, leading_clauses @ [last_clause])),
          copied: false,
        }
      | None => {ids: ids(unsorted), term: hole, copied: false}
      }
    | _ => {ids: ids(unsorted), term: hole, copied: false}
    }
  | e => {ids: [], term: Rules(e, []), copied: false}
  };
}

and unsorted = (skel: Skel.t, seg: Segment.t): unsorted => {
  /* Remove projectors. We do this here as opposed to removing
   * them in an external call to save a whole-syntax pass. */
  let seg = rm_and_log_projectors(seg);
  let tile_kids = (p: Piece.t): list(Term.Any.t) =>
    switch (p) {
    | Secondary(_)
    | Grout(_) => []
    | Projector(_) => []
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
      {term, terms: map^, projectors: projectors^};
    },
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
