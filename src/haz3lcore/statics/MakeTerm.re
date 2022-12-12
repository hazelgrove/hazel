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
open Term;

// TODO make less hacky
let tokens =
  Piece.get(
    _ => [],
    _ => [" "],
    (t: Tile.t) => t.shards |> List.map(List.nth(t.label)),
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

type dark_id = int;
let dark_gen = ref(-1);
let dark_id = () => {
  let id = dark_gen^;
  dark_gen := id - 1;
  id;
};
let dark_hole = (~ids=[], s: Sort.t): t => {
  let id = dark_id();
  switch (s) {
  // put dark id last to avoid messing with rep id
  | Exp => Exp({ids: ids @ [id], term: EmptyHole})
  | _ => failwith("dark_hole todo")
  };
};

// TODO flesh out incomplete cases
// TODO review dark hole
let _complete_root =
  fun
  | Op(_) as root => root
  | Pre(tiles, r) as root =>
    switch (tiles) {
    | ([(id, tile)], []) =>
      switch (tile) {
      | (["("], []) => Op(single(id, (["(", ")"], [r])))
      | (["let"], []) =>
        Pre(
          single(id, (Labels.let_, [r, dark_hole(Exp)])),
          dark_hole(Exp),
        )
      | (["let", "="], [pat]) =>
        Pre(single(id, (Labels.let_, [pat, r])), dark_hole(Exp))
      | _ => root
      }
    | _ => root
    }
  | root => root;

let is_nary =
    (is_sort: any => option('sort), delim: Token.t, (delims, kids): tiles)
    : option(list('sort)) =>
  if (delims |> List.map(snd) |> List.for_all((==)(([delim], [])))) {
    kids |> List.map(is_sort) |> OptUtil.sequence;
  } else {
    None;
  };

let is_tuple_exp = is_nary(TermBase.Any.is_exp, ",");
let is_tuple_pat = is_nary(TermBase.Any.is_pat, ",");
let is_tuple_typ = is_nary(TermBase.Any.is_typ, ",");
let is_typ_sum = is_nary(TermBase.Any.is_tsum, "+");

let is_grout = tiles =>
  Aba.get_as(tiles) |> List.map(snd) |> List.for_all((==)(([" "], [])));

let is_rules = ((ts, kids): tiles): option(Aba.t(UPat.t, UExp.t)) => {
  open OptUtil.Syntax;
  let+ ps =
    ts
    |> List.map(
         fun
         | (_, (["|", "=>"], [Pat(p)])) => Some(p)
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
let return_dark_hole = (~ids=[], s) => {
  let hole = dark_hole(~ids, s);
  map := TermMap.add_all(Term.ids(hole), hole, map^);
  hole;
};

let rec go_s = (s: Sort.t, skel: Skel.t, seg: Segment.t): any =>
  switch (s) {
  | Pat => Pat(pat(unsorted(skel, seg)))
  | TPat => TPat(tpat(unsorted(skel, seg)))
  | TSum => TSum(tsum(unsorted(skel, seg)))
  | Typ => Typ(typ(unsorted(skel, seg)))
  | Exp => Exp(exp(unsorted(skel, seg)))
  | Rul => Rul(rul(unsorted(skel, seg)))
  | Nul => Nul() //TODO
  | Any =>
    let tm = unsorted(skel, seg);
    let ids = ids(tm);
    switch (ListUtil.hd_opt(ids)) {
    | None => return_dark_hole(Exp)
    | Some(id) =>
      switch (TileMap.find_opt(id, TileMap.mk(seg))) {
      | None => return_dark_hole(~ids, Exp)
      | Some(t) =>
        if (t.mold.out == Any) {
          return_dark_hole(~ids, Exp);
        } else {
          go_s(t.mold.out, skel, seg);
        }
      }
    };
  }

and exp = unsorted => {
  let (term, inner_ids) = exp_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(e => Exp(e), ids, {ids, term});
}
and exp_term: unsorted => (UExp.term, list(Id.t)) = {
  let ret = (tm: UExp.term) => (tm, []);
  let _unrecog = UExp.Invalid(UnrecognizedTerm);
  let hole = unsorted => Term.UExp.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    // single-tile case
    | ([(_id, t)], []) =>
      switch (t) {
      | (["triv"], []) => ret(Triv)
      | (["true"], []) => ret(Bool(true))
      | (["false"], []) => ret(Bool(false))
      | ([t], []) when Form.is_float(t) => ret(Float(float_of_string(t)))
      | ([t], []) when Form.is_int(t) => ret(Int(int_of_string(t)))
      | ([t], []) when Form.is_var(t) => ret(Var(t))
      | ([t], []) when Form.is_string(t) => ret(String(t))
      | ([t], []) when Form.is_tag(t) => ret(Tag(t))
      | (["test", "end"], [Exp(test)]) => ret(Test(test))
      | (["(", ")"], [Exp(body)]) => ret(Parens(body))
      | (["nil"], []) => ret(ListLit([]))
      | (["[", "]"], [Exp(body)]) =>
        switch (body) {
        | {ids, term: Tuple(es)} => (ListLit(es), ids)
        | term => ret(ListLit([term]))
        }
      | (["case", "end"], [Rul({ids, term: Rules(scrut, rules)})]) => (
          Match(scrut, rules),
          ids,
        )
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Pre(tiles, Exp(r)) as tm =>
    switch (tiles) {
    | ([(_id, t)], []) =>
      ret(
        switch (t) {
        | (["-"], []) => UnOp(Int(Minus), r)
        | (["fun", "->"], [Pat(pat)]) => Fun(pat, r)
        | (["let", "=", "in"], [Pat(pat), Exp(def)]) => Let(pat, def, r)
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
      ret(
        switch (t) {
        | (["(", ")"], [Exp(arg)]) => Ap(l, arg)
        | _ => hole(tm)
        },
      )
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
          | (["/"], []) => BinOp(Int(Divide), l, r)
          | (["<"], []) => BinOp(Int(LessThan), l, r)
          | ([">"], []) => BinOp(Int(GreaterThan), l, r)
          | (["<="], []) => BinOp(Int(LessThanOrEqual), l, r)
          | ([">="], []) => BinOp(Int(GreaterThanOrEqual), l, r)
          | (["=="], []) => BinOp(Int(Equals), l, r)
          | (["+."], []) => BinOp(Float(Plus), l, r)
          | (["-."], []) => BinOp(Float(Minus), l, r)
          | (["*."], []) => BinOp(Float(Times), l, r)
          | (["/."], []) => BinOp(Float(Divide), l, r)
          | (["<."], []) => BinOp(Float(LessThan), l, r)
          | ([">."], []) => BinOp(Float(GreaterThan), l, r)
          | (["<=."], []) => BinOp(Float(LessThanOrEqual), l, r)
          | ([">=."], []) => BinOp(Float(GreaterThanOrEqual), l, r)
          | (["==."], []) => BinOp(Float(Equals), l, r)
          | (["&&"], []) => BinOp(Bool(And), l, r)
          | (["||"], []) => BinOp(Bool(Or), l, r)
          | (["::"], []) => Cons(l, r)
          | ([";"], []) => Seq(l, r)
          | (["$=="], []) => BinOp(String(Equals), l, r)
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
  return(p => Pat(p), ids, {ids, term});
}
and pat_term: unsorted => (UPat.term, list(Id.t)) = {
  let ret = (term: UPat.term) => (term, []);
  let _unrecog = UPat.Invalid(UnrecognizedTerm);
  let hole = unsorted => Term.UPat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (["triv"], []) => ret(Triv)
      | (["true"], []) => ret(Bool(true))
      | (["false"], []) => ret(Bool(false))
      | (["(", ")"], [Pat(body)]) => ret(Parens(body))
      | (["[", "]"], [Pat(body)]) =>
        switch (body) {
        | {ids, term: Tuple(ps)} => (ListLit(ps), ids)
        | term => ret(ListLit([term]))
        }
      | ([t], []) when Form.is_float(t) => ret(Float(float_of_string(t)))
      | ([t], []) when Form.is_int(t) => ret(Int(int_of_string(t)))
      | ([t], []) when Form.is_tag(t) => ret(Tag(t))
      | ([t], []) when Form.is_var(t) => ret(Var(t))
      | ([t], []) when Form.is_wild(t) => ret(Wild)
      | ([t], []) when Form.is_listnil(t) => ret(ListLit([]))
      | ([t], []) when Form.is_string(t) => ret(String(t))
      | _ => ret(hole(tm))
      }
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
    | ([(_id, ([":"], []))], []) => ret(TypeAnn(p, ty))
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

and tsum = unsorted => {
  let (term, inner_ids) = tsum_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(p => TSum(p), ids, {ids, term});
}
and tsum_term: unsorted => (UTSum.term, list(Id.t)) = {
  let ret = (term: UTSum.term) => (term, []);
  let hole = unsorted => Term.UTSum.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | ([tag], []) when Form.is_typ_var(tag) =>
          //TODO(andrew): as david about _id here
          Ap(tag, {ids: [_id], term: Tuple([])})
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Post(TSum({term: Ap(tag, _), _}), tiles) as tm =>
    switch (tiles) {
    | ([(_id, t)], []) =>
      ret(
        switch (t) {
        | (["(", ")"], [Typ({term: typ, _})]) =>
          Ap(tag, {ids: [_id], term: typ})
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Pre(_) as tm => ret(hole(tm))
  | Bin(TSum(l), tiles, TSum(r)) as tm =>
    switch (is_typ_sum(tiles)) {
    | Some(between_kids) => ret(Sum([l] @ between_kids @ [r]))
    | None =>
      switch (tiles) {
      | _ => ret(hole(tm))
      }
    }
  | tm => ret(hole(tm));
}

and typ = unsorted => {
  let (term, inner_ids) = typ_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(ty => Typ(ty), ids, {ids, term});
}
and typ_term: unsorted => (UTyp.term, list(Id.t)) = {
  let ret = (term: UTyp.term) => (term, []);
  let _unrecog = UTyp.Invalid(UnrecognizedTerm);
  let hole = unsorted => Term.UTyp.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (["Unit"], []) => ret(Tuple([]))
      | (["Bool"], []) => ret(Bool)
      | (["Int"], []) => ret(Int)
      | (["Float"], []) => ret(Float)
      | (["String"], []) => ret(String)
      | ([t], []) when Form.is_typ_var(t) => ret(Var(t))
      | (["(", ")"], [Typ(body)]) => ret(Parens(body))
      | (["[", "]"], [Typ(body)]) => ret(List(body))
      | (["sum", "end"], [TSum(x)]) =>
        //TODO(andrew): modelling after listlit
        (Sum(x), x.ids)
      //(Sum(x), ids)
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | (Pre(_) | Post(_)) as tm => ret(hole(tm))
  | Bin(Typ(l), tiles, Typ(r)) as tm =>
    switch (is_tuple_typ(tiles)) {
    | Some(between_kids) => ret(Tuple([l] @ between_kids @ [r]))
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
  return(ty => TPat(ty), ids, {ids, term});
}
and tpat_term: unsorted => UTPat.term = {
  let ret = (term: UTPat.term) => term;
  let hole = unsorted => Term.UTPat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | ([t], []) when Form.is_typ_var(t) => Var(t)
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
and rul = (unsorted: unsorted): URul.t => {
  let hole = Term.URul.Hole(kids_of_unsorted(unsorted));
  switch (exp(unsorted)) {
  | {term: MultiHole(_), _} =>
    switch (unsorted) {
    | Bin(Exp(scrut), tiles, Exp(last_clause)) =>
      switch (is_rules(tiles)) {
      | Some((ps, leading_clauses)) => {
          ids: ids(unsorted),
          term:
            Rules(scrut, List.combine(ps, leading_clauses @ [last_clause])),
        }
      | None => {ids: ids(unsorted), term: hole}
      }
    | _ => {ids: ids(unsorted), term: hole}
    }
  | e => {ids: [], term: Rules(e, [])}
  };
}

and unsorted = (skel: Skel.t, seg: Segment.t): unsorted => {
  let tile_kids = (p: Piece.t): list(any) =>
    switch (p) {
    | Whitespace(_)
    | Grout(_) => []
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
      let e = exp(unsorted(Segment.skel(seg), seg));
      (e, map^);
    },
  );
