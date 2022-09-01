open Util;
open Term;

// TODO make less hacky
let tokens =
  Piece.get(
    _ => [],
    _ => [" "],
    (t: Tile.t) => t.shards |> List.map(List.nth(t.label)),
  );

type tile = (Id.t, Aba.t(Token.t, t));
type tiles = Aba.t(tile, t);
let single = (id, subst) => ([(id, subst)], []);

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
let dark_hole = (s: Sort.t): t => {
  let id = dark_id();
  switch (s) {
  | Exp => Exp({ids: [id], term: EmptyHole})
  | _ => failwith("dark_hole todo")
  };
};

// TODO flesh out incomplete cases
let complete_root =
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

let is_tuple_exp = ((commas, kids): tiles): option(list(UExp.t)) =>
  if (commas |> List.map(snd) |> List.for_all((==)(([","], [])))) {
    kids
    |> List.map(
         fun
         | Exp(e) => Some(e)
         | _ => None,
       )
    |> OptUtil.sequence;
  } else {
    None;
  };
let is_tuple_pat = ((commas, kids): tiles): option(list(UPat.t)) =>
  if (commas |> List.map(snd) |> List.for_all((==)(([","], [])))) {
    kids
    |> List.map(
         fun
         | Pat(p) => Some(p)
         | _ => None,
       )
    |> OptUtil.sequence;
  } else {
    None;
  };
let is_tuple_typ = ((commas, kids): tiles): option(list(UTyp.t)) =>
  if (commas |> List.map(snd) |> List.for_all((==)(([","], [])))) {
    kids
    |> List.map(
         fun
         | Typ(ty) => Some(ty)
         | _ => None,
       )
    |> OptUtil.sequence;
  } else {
    None;
  };

let is_grout = tiles =>
  Aba.get_as(tiles) |> List.map(snd) |> List.for_all((==)(([" "], [])));

let is_rules = ((ts, kids): tiles): option(Aba.t((Id.t, UPat.t), UExp.t)) => {
  open OptUtil.Syntax;
  let+ ps =
    ts
    |> List.map(
         fun
         | (id, (["|", "=>"], [Pat(p)])) => Some((id, p))
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

// let have_sort = (tms: list(any)) =>
//   tms
//   |> List.map(

//   )

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

let rec go_s = (s: Sort.t, skel: Skel.t, seg: Segment.t): any =>
  switch (s) {
  | Pat => Pat(pat(unsorted(skel, seg)))
  | Typ => Typ(typ(unsorted(skel, seg)))
  | Exp => Exp(exp(unsorted(skel, seg)))
  | Rul => Rul(rul(unsorted(skel, seg)))
  | Nul => Nul() //TODO
  | Any => Any() //TODO
  }

and exp = unsorted => {
  let (term, inner_ids) = exp_term(unsorted);
  {ids: ids(unsorted) @ inner_ids, term};
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
      | (["test", "end"], [Exp(test)]) => ret(Test(test))
      | (["(", ")"], [Exp(body)]) => ret(Parens(body))
      | (["nil"], []) => ret(ListLit([]))
      | (["[", "]"], [Exp(body)]) =>
        switch (body) {
        | {ids, term: Tuple(es)} => (ListLit(es), ids)
        | term => ret(ListLit([term]))
        }
      | (["case", "end"], [Rul(Rules(ids, scrut, rules))]) => (
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
          | (["=="], []) => BinOp(Int(Equals), l, r)
          | (["+."], []) => BinOp(Float(Plus), l, r)
          | (["-."], []) => BinOp(Float(Minus), l, r)
          | (["*."], []) => BinOp(Float(Times), l, r)
          | (["/."], []) => BinOp(Float(Divide), l, r)
          | (["<."], []) => BinOp(Float(LessThan), l, r)
          | ([">."], []) => BinOp(Float(GreaterThan), l, r)
          | (["==."], []) => BinOp(Float(Equals), l, r)
          | (["&&"], []) => BinOp(Bool(And), l, r)
          | (["||"], []) => BinOp(Bool(Or), l, r)
          | (["::"], []) => Cons(l, r)
          | ([";"], []) => Seq(l, r)
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
  {ids: ids(unsorted) @ inner_ids, term};
}
and pat_term: unsorted => (UPat.term, list(Id.t)) = {
  let ret = (term: UPat.term) => (term, []);
  let _unrecog = UPat.Invalid(UnrecognizedTerm);
  let hole = unsorted => Term.UPat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | (["triv"], []) => Triv
        | (["true"], []) => Bool(true)
        | (["false"], []) => Bool(false)
        | (["(", ")"], [Pat(body)]) => Parens(body)
        | ([t], []) when Form.is_float(t) => Float(float_of_string(t))
        | ([t], []) when Form.is_int(t) => Int(int_of_string(t))
        | ([t], []) when Form.is_var(t) => Var(t)
        | ([t], []) when Form.is_wild(t) => Wild
        | ([t], []) when Form.is_listnil(t) => ListNil
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | (Pre(_) | Post(_)) as tm => ret(hole(tm))
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

and typ = unsorted => {
  let term = typ_term(unsorted);
  {ids: ids(unsorted), term};
}
and typ_term: unsorted => UTyp.term = {
  let _unrecog = UTyp.Invalid(UnrecognizedTerm);
  let hole = unsorted => Term.UTyp.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (["Unit"], []) => Tuple([])
      | (["Bool"], []) => Bool
      | (["Int"], []) => Int
      | (["Float"], []) => Float
      | (["(", ")"], [Typ(body)]) => Parens(body)
      | (["[", "]"], [Typ(body)]) => List(body)
      | _ => hole(tm)
      }
    | _ => hole(tm)
    }
  | (Pre(_) | Post(_)) as tm => hole(tm)
  | Bin(Typ(l), tiles, Typ(r)) as tm =>
    switch (is_tuple_typ(tiles)) {
    | Some(between_kids) => Tuple([l] @ between_kids @ [r])
    | None =>
      switch (tiles) {
      | ([(_id, (["->"], []))], []) => Arrow(l, r)
      | _ => hole(tm)
      }
    }
  | tm => hole(tm);
}

and rul = (unsorted: unsorted): URul.t =>
  switch (unsorted) {
  | Bin(Exp(scrut), tiles, Exp(last_clause)) =>
    switch (is_rules(tiles)) {
    | Some((ps, leading_clauses)) =>
      let (ids, ps) = List.split(ps);
      Rules(ids, scrut, List.combine(ps, leading_clauses @ [last_clause]));
    | None => Invalid(UnrecognizedTerm)
    }
  | _ => Invalid(UnrecognizedTerm)
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

/* MAKETERM

   This parses tile structure into term structure.
   The language syntax, as determined by Form.re, is an
   open, data-driven system, so adding a syntactic form
   there will not trigger a static error here; you must
   remember to add a case below for each new form added
   to the syntax.

   WARNING: This module is still structurally in flux.

   Note that this parser is very much a work in progress.
   There is some structural complexity here, resulting from
   differing treatment of children external and internal to
   tiles which has yet to be fully resolved. In particular,
   the case where the external children have differing sorts
   from the out sort of their parent such as type annotations.
   Likely the sort_dispatch and piece_and_outside_kids functions
   can be improved to produce a single list of all children
   of a term, annotated with their sorts, simplifying of_piece.

   Furthermore, there are conceptually unresolved issues
   relating to the treatement of incomplete tiles. Right
   now we're kind of punting on this; incomplete pieces
   are filtered from segments, which has some nice properties
   but is not ideal. If this simplification is removed, there
   will need to be a more general heuristic for assigning
   shapes and sorts to incomplete tiles; see piece_and_outside_kids
   and Piece.get_outside_sorts which it calls. */

// let rec sort_dispatch = (ps: Segment.t, kid: Skel.t, s: Sort.t): Term.any =>
//   switch (s) {
//   | Pat => Pat(of_seg_and_skel_pat(ps, kid))
//   | Typ => Typ(of_seg_and_skel_typ(ps, kid))
//   | Exp => Exp(of_seg_and_skel_exp(ps, kid))
//   | Rul => Rul(of_seg_and_skel_rul(ps, kid))
//   | Nul => Nul() //TODO
//   | Any => Any() //TODO
//   }
// and piece_and_outside_kids =
//     (~default_sort, ps: Segment.t, skel: Skel.t): (Piece.t, list(Term.any)) => {
//   let piece_at = List.nth(ps);
//   let (p, outside_kids) =
//     switch (skel) {
//     | Op(idx) => (piece_at(idx), [])
//     | Pre(idx, skel') => (piece_at(idx), [skel'])
//     | Post(skel', idx) => (piece_at(idx), [skel'])
//     | Bin(skel_l, idx, skel_r) => (piece_at(idx), [skel_l, skel_r])
//     };
//   let outside_sorts = Piece.get_outside_sorts(~default_sort, p);
//   let outside_kids =
//     if (List.length(outside_sorts) != List.length(outside_kids)) {
//       //TODO(HACK): remove after fixing Piece.get_outside_sorts
//       print_endline("WARNING: of_seg_and_skel: list mismatch");
//       [];
//     } else {
//       assert(List.length(outside_kids) == List.length(outside_sorts));
//       List.map2(sort_dispatch(ps), outside_kids, outside_sorts);
//     };
//   (p, outside_kids);
// }
// and uexp_of_seg = (ps: Segment.t): UExp.t => {
//   /* NOTE(andrew): filtering out incomplete tiles for now.
//      TODO: better approach which e.g. still provides feedback
//      inside incomplete tile children */
//   ps
//   |> List.filter(Piece.is_complete)
//   |> Segment.skel
//   |> of_seg_and_skel_exp(ps);
// }
// and upat_of_seg = (ps: Segment.t): UPat.t => {
//   /* NOTE(andrew): filtering out incomplete tiles for now.
//      TODO: better approach which e.g. still provides feedback
//      inside incomplete tile children */
//   ps
//   |> List.filter(Piece.is_complete)
//   |> Segment.skel
//   |> of_seg_and_skel_pat(ps);
// }
// and utyp_of_seg = (ps: Segment.t): UTyp.t => {
//   /* NOTE(andrew): filtering out incomplete tiles for now.
//      TODO: better approach which e.g. still provides feedback
//      inside incomplete tile children */
//   ps
//   |> List.filter(Piece.is_complete)
//   |> Segment.skel
//   |> of_seg_and_skel_typ(ps);
// }
// and urul_of_seg = (ps: Segment.t): URul.s => {
//   /* NOTE(andrew): filtering out incomplete tiles for now.
//      TODO: better approach which e.g. still provides feedback
//      inside incomplete tile children */
//   ps
//   |> List.filter(Piece.is_complete)
//   |> Segment.skel
//   |> of_seg_and_skel_rul(ps);
// }
// and of_seg_and_skel_exp = (ps: Segment.t, skel: Skel.t): UExp.t => {
//   let (p, kids) = piece_and_outside_kids(~default_sort=Exp, ps, skel);
//   of_piece_exp(p, kids);
// }
// and of_seg_and_skel_pat = (ps: Segment.t, skel: Skel.t): UPat.t => {
//   let (p, kids) = piece_and_outside_kids(~default_sort=Pat, ps, skel);
//   of_piece_pat(p, kids);
// }
// and of_seg_and_skel_typ = (ps: Segment.t, skel: Skel.t): UTyp.t => {
//   let (p, kids) = piece_and_outside_kids(~default_sort=Typ, ps, skel);
//   of_piece_typ(p, kids);
// }
// and of_seg_and_skel_rul = (ps: Segment.t, skel: Skel.t): URul.s => {
//   let (p, kids) = piece_and_outside_kids(~default_sort=Rul, ps, skel);
//   of_piece_rul(p, kids);
// }
// //TODO: improve/consolidate of_nary fns below
// and of_multi_exp = (id: Id.t, l: UExp.t, r: UExp.t): UExp.t => {
//   let wrap_multi = (ids, es): UExp.t => {
//     id,
//     term: MultiHole([id] @ ids, es),
//   };
//   switch (l, r) {
//   | ({term: MultiHole(l_ids, ls), _}, {term: MultiHole(r_ids, rs), _}) =>
//     wrap_multi(l_ids @ r_ids, ls @ rs)
//   | (l, {term: MultiHole(r_ids, rs), _}) => wrap_multi(r_ids, [l] @ rs)
//   | ({term: MultiHole(l_ids, ls), _}, r) => wrap_multi(l_ids, ls @ [r])
//   | (l, r) => wrap_multi([], [l, r])
//   };
// }
// and of_tuple_exp = (id: Id.t, l: UExp.t, r: UExp.t): UExp.t => {
//   let wrap_tuple = (ids, es): UExp.t => {id, term: Tuple([id] @ ids, es)};
//   switch (l, r) {
//   | ({term: Tuple(l_ids, ls), _}, {term: Tuple(r_ids, rs), _}) =>
//     wrap_tuple(l_ids @ r_ids, ls @ rs)
//   | (l, {term: Tuple(r_ids, rs), _}) => wrap_tuple(r_ids, [l] @ rs)
//   | ({term: Tuple(l_ids, ls), _}, r) => wrap_tuple(l_ids, ls @ [r])
//   | (l, r) => wrap_tuple([], [l, r])
//   };
// }
// and of_tuple_pat = (id: Id.t, l: UPat.t, r: UPat.t): UPat.t => {
//   let wrap_tuple = (ids, es): UPat.t => {id, term: Tuple([id] @ ids, es)};
//   switch (l, r) {
//   | ({term: Tuple(l_ids, ls), _}, {term: Tuple(r_ids, rs), _}) =>
//     wrap_tuple(l_ids @ r_ids, ls @ rs)
//   | (l, {term: Tuple(r_ids, rs), _}) => wrap_tuple(r_ids, [l] @ rs)
//   | ({term: Tuple(l_ids, ls), _}, r) => wrap_tuple(l_ids, ls @ [r])
//   | (l, r) => wrap_tuple([], [l, r])
//   };
// }
// and of_tuple_typ = (id: Id.t, l: UTyp.t, r: UTyp.t): UTyp.t => {
//   let wrap_tuple = (ids, es): UTyp.t => {id, term: Tuple([id] @ ids, es)};
//   switch (l, r) {
//   | ({term: Tuple(l_ids, ls), _}, {term: Tuple(r_ids, rs), _}) =>
//     wrap_tuple(l_ids @ r_ids, ls @ rs)
//   | (l, {term: Tuple(r_ids, rs), _}) => wrap_tuple(r_ids, [l] @ rs)
//   | ({term: Tuple(l_ids, ls), _}, r) => wrap_tuple(l_ids, ls @ [r])
//   | (l, r) => wrap_tuple([], [l, r])
//   };
// }
// and of_multi_pat = (id: Id.t, l: UPat.t, r: UPat.t): UPat.t => {
//   let wrap_multi = (ids, es): UPat.t => {
//     id,
//     term: MultiHole([id] @ ids, es),
//   };
//   switch (l, r) {
//   | ({term: MultiHole(l_ids, ls), _}, {term: MultiHole(r_ids, rs), _}) =>
//     wrap_multi(l_ids @ r_ids, ls @ rs)
//   | (l, {term: MultiHole(r_ids, rs), _}) => wrap_multi(r_ids, [l] @ rs)
//   | ({term: MultiHole(l_ids, ls), _}, r) => wrap_multi(l_ids, ls @ [r])
//   | (l, r) => wrap_multi([], [l, r])
//   };
// }
// and of_multi_typ = (id: Id.t, l: UTyp.t, r: UTyp.t): UTyp.t => {
//   let wrap_multi = (ids, es): UTyp.t => {
//     id,
//     term: MultiHole([id] @ ids, es),
//   };
//   switch (l, r) {
//   | ({term: MultiHole(l_ids, ls), _}, {term: MultiHole(r_ids, rs), _}) =>
//     wrap_multi(l_ids @ r_ids, ls @ rs)
//   | (l, {term: MultiHole(r_ids, rs), _}) => wrap_multi(r_ids, [l] @ rs)
//   | ({term: MultiHole(l_ids, ls), _}, r) => wrap_multi(l_ids, ls @ [r])
//   | (l, r) => wrap_multi([], [l, r])
//   };
// }
// and of_piece_exp = (p: Piece.t, outside_kids: list(Term.any)): UExp.t => {
//   switch (p) {
//   | Whitespace({id, _}) => {id, term: Invalid(Whitespace)}
//   | Grout({id, shape}) =>
//     switch (shape, outside_kids) {
//     | (Convex, []) => {id, term: EmptyHole}
//     | (Concave, [Exp(l), Exp(r)]) => of_multi_exp(id, l, r)
//     | _ => {id, term: Invalid(MalformedGrout)}
//     }
//   | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
//     let term: UExp.term =
//       switch (label, outside_kids, inside_kids) {
//       | _ when !Tile.is_complete(t) =>
//         // TODO(andrew): more principled handling of incomplete tiles
//         EmptyHole
//       // TODO(andrew): should Form.re handle atomic conversion?
//       | (["triv"], [], []) => Triv
//       | (["true"], [], []) => Bool(true)
//       | (["false"], [], []) => Bool(false)
//       | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
//       | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
//       | ([t], [], []) when Form.is_var(t) => Var(t)
//       | ([","], [Exp(l), Exp(r)], []) => of_tuple_exp(id, l, r).term
//       | (["+"], [Exp(l), Exp(r)], []) => BinOp(Int(Plus), l, r)
//       | (["-"], [Exp(l), Exp(r)], []) => BinOp(Int(Minus), l, r)
//       | (["*"], [Exp(l), Exp(r)], []) => BinOp(Int(Times), l, r)
//       | (["/"], [Exp(l), Exp(r)], []) => BinOp(Int(Divide), l, r)
//       | (["<"], [Exp(l), Exp(r)], []) => BinOp(Int(LessThan), l, r)
//       | ([">"], [Exp(l), Exp(r)], []) => BinOp(Int(GreaterThan), l, r)
//       | (["=="], [Exp(l), Exp(r)], []) => BinOp(Int(Equals), l, r)
//       | (["+."], [Exp(l), Exp(r)], []) => BinOp(Float(Plus), l, r)
//       | (["-."], [Exp(l), Exp(r)], []) => BinOp(Float(Minus), l, r)
//       | (["*."], [Exp(l), Exp(r)], []) => BinOp(Float(Times), l, r)
//       | (["/."], [Exp(l), Exp(r)], []) => BinOp(Float(Divide), l, r)
//       | (["<."], [Exp(l), Exp(r)], []) => BinOp(Float(LessThan), l, r)
//       | ([">."], [Exp(l), Exp(r)], []) => BinOp(Float(GreaterThan), l, r)
//       | (["==."], [Exp(l), Exp(r)], []) => BinOp(Float(Equals), l, r)
//       | (["&&"], [Exp(l), Exp(r)], []) => BinOp(Bool(And), l, r)
//       | (["||"], [Exp(l), Exp(r)], []) => BinOp(Bool(Or), l, r)
//       | (["::"], [Exp(l), Exp(r)], []) => Cons(l, r)
//       | ([";"], [Exp(l), Exp(r)], []) => Seq(l, r)
//       | (["-"], [Exp(e)], []) => UnOp(Int(Minus), e)
//       | (["test", "end"], [], [test]) => Test(uexp_of_seg(test))
//       | (["fun", "->"], [Exp(body)], [pat]) =>
//         Fun(upat_of_seg(pat), body)
//       | (["let", "=", "in"], [Exp(body)], [pat, def]) =>
//         Let(upat_of_seg(pat), uexp_of_seg(def), body)
//       | (["if", "then", "else"], [Exp(alt)], [cond, conseq]) =>
//         If(uexp_of_seg(cond), uexp_of_seg(conseq), alt)
//       | (["(", ")"], [Exp(fn)], [arg]) => Ap(fn, uexp_of_seg(arg))
//       | (["(", ")"], [], [body]) => Parens(uexp_of_seg(body))
//       | (["nil"], [], []) => ListLit([id], [])
//       | (["[", "]"], [], [body]) =>
//         switch (uexp_of_seg(body)) {
//         | {term: Tuple(ids, es), _} => ListLit([id] @ ids, es)
//         | term => ListLit([id], [term])
//         }
//       | (["case", "of"], [Rul({ids, rules})], [scrut]) =>
//         Match(ids, uexp_of_seg(scrut), rules)
//       | _ => Invalid(UnrecognizedTerm)
//       };
//     {id, term};
//   };
// }
// and of_piece_pat = (p: Piece.t, outside_kids: list(Term.any)): UPat.t => {
//   switch (p) {
//   | Whitespace({id, _}) => {id, term: Invalid(Whitespace)}
//   | Grout({id, shape}) =>
//     switch (shape, outside_kids) {
//     | (Convex, []) => {id, term: EmptyHole}
//     | (Concave, [Pat(l), Pat(r)]) => of_multi_pat(id, l, r)
//     | _ => {id, term: Invalid(MalformedGrout)}
//     }
//   | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
//     let term: UPat.term =
//       switch (label, outside_kids, inside_kids) {
//       | _ when !Tile.is_complete(t) =>
//         //MultiHole([id], List.map(upat_of_seg, inside_kids))
//         EmptyHole
//       //Invalid(IncompleteTile)
//       // TODO(andrew): should Form.re handle atomic conversion?
//       | (["triv"], [], []) => Triv
//       | (["true"], [], []) => Bool(true)
//       | (["false"], [], []) => Bool(false)
//       | (["(", ")"], [], [body]) => Parens(upat_of_seg(body))
//       | ([","], [Pat(l), Pat(r)], []) => of_tuple_pat(id, l, r).term
//       | (["::"], [Pat(l), Pat(r)], []) => Cons(l, r)
//       | ([":"], [Pat(p), Typ(ty)], []) => TypeAnn(p, ty)
//       /* WARNING: is_float must come first because is_int's regexp is strictly more general */
//       | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
//       | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
//       | ([t], [], []) when Form.is_var(t) => Var(t)
//       | ([t], [], []) when Form.is_wild(t) => Wild
//       | ([t], [], []) when Form.is_listnil(t) => ListNil
//       | _ => Invalid(UnrecognizedTerm)
//       };
//     {id, term};
//   };
// }
// and of_piece_typ = (p: Piece.t, outside_kids: list(Term.any)): UTyp.t => {
//   switch (p) {
//   | Whitespace({id, _}) => {id, term: Invalid(Whitespace)}
//   | Grout({id, shape}) =>
//     switch (shape, outside_kids) {
//     | (Convex, []) => {id, term: EmptyHole}
//     | (Concave, [Typ(l), Typ(r)]) => of_multi_typ(id, l, r)
//     | _ => {id, term: Invalid(MalformedGrout)}
//     }
//   | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
//     let term: UTyp.term =
//       switch (label, outside_kids, inside_kids) {
//       | _ when !Tile.is_complete(t) => Invalid(IncompleteTile)
//       // TODO(andrew): should Form.re handle atomic conversion?
//       | (["Unit"], [], []) => Tuple([id], [])
//       | (["Bool"], [], []) => Bool
//       | (["Int"], [], []) => Int
//       | (["Float"], [], []) => Float
//       | (["->"], [Typ(l), Typ(r)], []) => Arrow(l, r)
//       | ([","], [Typ(l), Typ(r)], []) => of_tuple_typ(id, l, r).term
//       | (["(", ")"], [], [body]) => Parens(utyp_of_seg(body))
//       | (["[", "]"], [], [body]) => List(utyp_of_seg(body))
//       | _ => Invalid(UnrecognizedTerm)
//       };
//     {id, term};
//   };
// }
// and of_piece_rul = (p: Piece.t, outside_kids: list(Term.any)): URul.s => {
//   switch (p) {
//   | Whitespace({id: _, _}) => URul.mks([], [])
//   | Grout({id, shape}) =>
//     switch (shape, outside_kids) {
//     | (Convex, []) => URul.mks([id], [])
//     | (Concave, [Rul(l), Rul(r)]) =>
//       URul.mks(l.ids @ r.ids, l.rules @ r.rules)
//     | _ => URul.mks([], [])
//     }
//   | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
//     switch (label, outside_kids, inside_kids) {
//     | _ when !Tile.is_complete(t) => URul.mks([], [])
//     | (["=>"], [Pat(p), Exp(e)], []) => URul.mks([id], [(p, e)])
//     | (["|"], [Rul(l), Rul(r)], []) =>
//       URul.mks([id] @ l.ids @ r.ids, l.rules @ r.rules)
//     | (["|"], [Rul({ids, rules})], []) => URul.mks([id] @ ids, rules)
//     | _ => URul.mks([], [])
//     }
//   };
// };

let go =
  Core_kernel.Memo.general(~cache_size_bound=1000, seg =>
    exp(unsorted(Segment.skel(seg), seg))
  );
