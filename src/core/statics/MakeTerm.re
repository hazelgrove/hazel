open Term;

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

let rec sort_dispatch = (ps: Segment.t, kid: Skel.t, s: Sort.t): Term.any =>
  switch (s) {
  | Pat => Pat(of_seg_and_skel_pat(ps, kid))
  | Typ => Typ(of_seg_and_skel_typ(ps, kid))
  | Exp => Exp(of_seg_and_skel_exp(ps, kid))
  | Rul => Rul() //TODO
  | Nul => Nul()
  | Any => Any()
  }
and piece_and_outside_kids =
    (~default_sort, ps: Segment.t, skel: Skel.t): (Piece.t, list(Term.any)) => {
  let piece_at = List.nth(ps);
  let (p, outside_kids) =
    switch (skel) {
    | Op(idx) => (piece_at(idx), [])
    | Pre(idx, skel') => (piece_at(idx), [skel'])
    | Post(skel', idx) => (piece_at(idx), [skel'])
    | Bin(skel_l, idx, skel_r) => (piece_at(idx), [skel_l, skel_r])
    };
  let outside_sorts = Piece.get_outside_sorts(~default_sort, p);
  let outside_kids =
    if (List.length(outside_sorts) != List.length(outside_kids)) {
      //TODO(HACK): remove after fixing Piece.get_outside_sorts
      print_endline("WARNING: of_seg_and_skel: list mismatch");
      [];
    } else {
      List.map2(sort_dispatch(ps), outside_kids, outside_sorts);
    };
  (p, outside_kids);
}
and uexp_of_seg = (ps: Segment.t): UExp.t => {
  /* NOTE(andrew): filtering out incomplete tiles for now.
     TODO: better approach which e.g. still provides feedback
     inside incomplete tile children */
  ps
  |> List.filter(Piece.is_complete)
  |> Segment.skel
  |> of_seg_and_skel_exp(ps);
}
and upat_of_seg = (ps: Segment.t): UPat.t => {
  /* NOTE(andrew): filtering out incomplete tiles for now.
     TODO: better approach which e.g. still provides feedback
     inside incomplete tile children */
  ps
  |> List.filter(Piece.is_complete)
  |> Segment.skel
  |> of_seg_and_skel_pat(ps);
}
and utyp_of_seg = (ps: Segment.t): UTyp.t => {
  /* NOTE(andrew): filtering out incomplete tiles for now.
     TODO: better approach which e.g. still provides feedback
     inside incomplete tile children */
  ps
  |> List.filter(Piece.is_complete)
  |> Segment.skel
  |> of_seg_and_skel_typ(ps);
}
and of_seg_and_skel_exp = (ps: Segment.t, skel: Skel.t): UExp.t => {
  let (p, kids) = piece_and_outside_kids(~default_sort=Exp, ps, skel);
  of_piece_exp(p, kids);
}
and of_seg_and_skel_pat = (ps: Segment.t, skel: Skel.t): UPat.t => {
  let (p, kids) = piece_and_outside_kids(~default_sort=Pat, ps, skel);
  of_piece_pat(p, kids);
}
and of_seg_and_skel_typ = (ps: Segment.t, skel: Skel.t): UTyp.t => {
  let (p, kids) = piece_and_outside_kids(~default_sort=Typ, ps, skel);
  of_piece_typ(p, kids);
}
//TODO: improve/consolidate of_nary fns below
and of_nary_exp = (id: Id.t, l: UExp.t, r: UExp.t): UExp.t => {
  let wrap_multi = (ids, es): UExp.t => {
    id,
    term: MultiHole([id] @ ids, es),
  };
  switch (l, r) {
  | ({term: MultiHole(l_ids, ls), _}, {term: MultiHole(r_ids, rs), _}) =>
    wrap_multi(l_ids @ r_ids, ls @ rs)
  | (l, {term: MultiHole(r_ids, rs), _}) => wrap_multi(r_ids, [l] @ rs)
  | ({term: MultiHole(l_ids, ls), _}, r) => wrap_multi(l_ids, ls @ [r])
  | (l, r) => wrap_multi([], [l, r])
  };
}
and of_ntuple_exp = (id: Id.t, l: UExp.t, r: UExp.t): UExp.t => {
  let wrap_ntuple = (ids, es): UExp.t => {
    id,
    term: NTuple([id] @ ids, es),
  };
  switch (l, r) {
  | ({term: NTuple(l_ids, ls), _}, {term: NTuple(r_ids, rs), _}) =>
    wrap_ntuple(l_ids @ r_ids, ls @ rs)
  | (l, {term: NTuple(r_ids, rs), _}) => wrap_ntuple(r_ids, [l] @ rs)
  | ({term: NTuple(l_ids, ls), _}, r) => wrap_ntuple(l_ids, ls @ [r])
  | (l, r) => wrap_ntuple([], [l, r])
  };
}
and of_nary_pat = (id: Id.t, l: UPat.t, r: UPat.t): UPat.t => {
  let wrap_multi = (ids, es): UPat.t => {
    id,
    term: MultiHole([id] @ ids, es),
  };
  switch (l, r) {
  | ({term: MultiHole(l_ids, ls), _}, {term: MultiHole(r_ids, rs), _}) =>
    wrap_multi(l_ids @ r_ids, ls @ rs)
  | (l, {term: MultiHole(r_ids, rs), _}) => wrap_multi(r_ids, [l] @ rs)
  | ({term: MultiHole(l_ids, ls), _}, r) => wrap_multi(l_ids, ls @ [r])
  | (l, r) => wrap_multi([], [l, r])
  };
}
and of_nary_typ = (id: Id.t, l: UTyp.t, r: UTyp.t): UTyp.t => {
  let wrap_multi = (ids, es): UTyp.t => {
    id,
    term: MultiHole([id] @ ids, es),
  };
  switch (l, r) {
  | ({term: MultiHole(l_ids, ls), _}, {term: MultiHole(r_ids, rs), _}) =>
    wrap_multi(l_ids @ r_ids, ls @ rs)
  | (l, {term: MultiHole(r_ids, rs), _}) => wrap_multi(r_ids, [l] @ rs)
  | ({term: MultiHole(l_ids, ls), _}, r) => wrap_multi(l_ids, ls @ [r])
  | (l, r) => wrap_multi([], [l, r])
  };
}
and of_piece_exp = (p: Piece.t, outside_kids: list(Term.any)): UExp.t => {
  let invalid = (p: Piece.t): UExp.t => {id: (-1), term: Invalid(p)};
  switch (p) {
  | Whitespace(_) => invalid(p)
  | Grout({id, shape}) =>
    switch (shape, outside_kids) {
    | (Convex, []) => {id, term: EmptyHole}
    | (Concave, [Exp(l), Exp(r)]) => of_nary_exp(id, l, r)
    | _ => {id, term: Invalid(p)}
    }
  | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
    let term: UExp.term =
      switch (label, outside_kids, inside_kids) {
      | _ when !Tile.is_complete(t) =>
        // TODO: more principled handling of incomplete tiles
        EmptyHole
      // TODO(andrew): Form.re should handle monotile recognition
      | (["true"], [], []) => Bool(true)
      | (["false"], [], []) => Bool(false)
      | (["nil"], [], []) => ListNil
      | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
      | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => Var(t)
      | ([","], [Exp(l), Exp(r)], []) => of_ntuple_exp(id, l, r).term
      //Pair(l, r)
      | (["+"], [Exp(l), Exp(r)], []) => BinOp(Int(Plus), l, r)
      | (["-"], [Exp(l), Exp(r)], []) => BinOp(Int(Minus), l, r)
      | (["*"], [Exp(l), Exp(r)], []) => BinOp(Int(Times), l, r)
      | (["/"], [Exp(l), Exp(r)], []) => BinOp(Int(Divide), l, r)
      | (["<"], [Exp(l), Exp(r)], []) => BinOp(Int(LessThan), l, r)
      | ([">"], [Exp(l), Exp(r)], []) => BinOp(Int(GreaterThan), l, r)
      | (["=="], [Exp(l), Exp(r)], []) => BinOp(Int(Equals), l, r)
      | (["+."], [Exp(l), Exp(r)], []) => BinOp(Float(Plus), l, r)
      | (["-."], [Exp(l), Exp(r)], []) => BinOp(Float(Minus), l, r)
      | (["*."], [Exp(l), Exp(r)], []) => BinOp(Float(Times), l, r)
      | (["/."], [Exp(l), Exp(r)], []) => BinOp(Float(Divide), l, r)
      | (["<."], [Exp(l), Exp(r)], []) => BinOp(Float(LessThan), l, r)
      | ([">."], [Exp(l), Exp(r)], []) => BinOp(Float(GreaterThan), l, r)
      | (["==."], [Exp(l), Exp(r)], []) => BinOp(Float(Equals), l, r)
      | (["&&"], [Exp(l), Exp(r)], []) => BinOp(Bool(And), l, r)
      | (["||"], [Exp(l), Exp(r)], []) => BinOp(Bool(Or), l, r)
      | (["::"], [Exp(l), Exp(r)], []) => Cons(l, r)
      | ([";"], [Exp(l), Exp(r)], []) => Seq(l, r)
      | (["test", "end"], [], [test]) => Test(uexp_of_seg(test))
      | (["fun", "->"], [Exp(body)], [pat]) =>
        Fun(upat_of_seg(pat), body)
      | (["funann", ":", "->"], [Exp(body)], [pat, typ]) =>
        FunAnn(upat_of_seg(pat), utyp_of_seg(typ), body)
      | (["let", "=", "in"], [Exp(body)], [pat, def]) =>
        Let(upat_of_seg(pat), uexp_of_seg(def), body)
      | (["letann", ":", "=", "in"], [Exp(body)], [pat, typ, def]) =>
        LetAnn(upat_of_seg(pat), utyp_of_seg(typ), uexp_of_seg(def), body)
      | (["if", "then", "else"], [Exp(alt)], [cond, conseq]) =>
        If(uexp_of_seg(cond), uexp_of_seg(conseq), alt)
      | (["(", ")"], [Exp(fn)], [arg]) => Ap(fn, uexp_of_seg(arg))
      | (["(", ")"], [], [body]) => Parens(uexp_of_seg(body))
      | (["[", "]"], [], [body]) =>
        switch (uexp_of_seg(body)) {
        | {term: NTuple(ids, es), _} => ListLit([id] @ ids, es)
        | term => ListLit([id], [term])
        }
      | _ => Invalid(p)
      };
    {id, term};
  };
}
and of_piece_pat = (p: Piece.t, outside_kids: list(Term.any)): UPat.t => {
  let invalid: UPat.t = {id: (-1), term: Invalid(p)};
  switch (p) {
  | Whitespace(_) => invalid
  | Grout({id, shape}) =>
    switch (shape, outside_kids) {
    | (Convex, []) => {id, term: EmptyHole}
    | (Concave, [Pat(l), Pat(r)]) => of_nary_pat(id, l, r)
    | _ => {id, term: Invalid(p)}
    }
  | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
    let term: UPat.term =
      switch (label, outside_kids, inside_kids) {
      | _ when !Tile.is_complete(t) => Invalid(p)
      // TODO(andrew): Form.re should handle monotile recognition
      | (["true"], [], []) => Bool(true)
      | (["false"], [], []) => Bool(false)
      | (["(", ")"], [], [body]) => Parens(upat_of_seg(body))
      | ([","], [Pat(l), Pat(r)], []) => Pair(l, r)
      | ([":"], [Pat(p), Typ(ty)], []) => TypeAnn(p, ty)
      /* WARNING: is_float must come first because is_int's regexp is strictly more general */
      | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
      | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => Var(t)
      | ([t], [], []) when Form.is_wild(t) => Wild
      | _ => Invalid(p)
      };
    {id, term};
  };
}
and of_piece_typ = (p: Piece.t, outside_kids: list(Term.any)): UTyp.t => {
  let invalid: UTyp.t = {id: (-1), term: Invalid(p)};
  switch (p) {
  | Whitespace(_) => invalid
  | Grout({id, shape}) =>
    switch (shape, outside_kids) {
    | (Convex, []) => {id, term: EmptyHole}
    | (Concave, [Typ(l), Typ(r)]) => of_nary_typ(id, l, r)
    | _ => {id, term: Invalid(p)}
    }
  | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
    let term: UTyp.term =
      switch (label, outside_kids, inside_kids) {
      | _ when !Tile.is_complete(t) => Invalid(p)
      // TODO(andrew): Form.re should handle monotile recognition
      | (["Unit"], [], []) => Unit
      | (["Bool"], [], []) => Bool
      | (["Int"], [], []) => Int
      | (["Float"], [], []) => Float
      | (["->"], [Typ(l), Typ(r)], []) => Arrow(l, r)
      | ([","], [Typ(l), Typ(r)], []) => Prod(l, r)
      | (["(", ")"], [], [body]) => Parens(utyp_of_seg(body))
      | _ => Invalid(p)
      };
    {id, term};
  };
};

let go = Core_kernel.Memo.general(~cache_size_bound=1000, uexp_of_seg);
