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
  | Rul => Rul(of_seg_and_skel_rul(ps, kid))
  | Nul => Nul() //TODO
  | Any => Any() //TODO
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
      assert(List.length(outside_kids) == List.length(outside_sorts));
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
and urul_of_seg = (ps: Segment.t): URul.t => {
  /* NOTE(andrew): filtering out incomplete tiles for now.
     TODO: better approach which e.g. still provides feedback
     inside incomplete tile children */
  ps
  |> List.filter(Piece.is_complete)
  |> Segment.skel
  |> of_seg_and_skel_rul(ps);
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
and of_seg_and_skel_rul = (ps: Segment.t, skel: Skel.t): URul.t => {
  let (p, kids) = piece_and_outside_kids(~default_sort=Rul, ps, skel);
  of_piece_rul(p, kids);
}
//TODO: improve/consolidate of_nary fns below
and of_multi_exp = (id: Id.t, l: UExp.t, r: UExp.t): UExp.t => {
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
and of_tuple_exp = (id: Id.t, l: UExp.t, r: UExp.t): UExp.t => {
  let wrap_tuple = (ids, es): UExp.t => {id, term: Tuple([id] @ ids, es)};
  switch (l, r) {
  | ({term: Tuple(l_ids, ls), _}, {term: Tuple(r_ids, rs), _}) =>
    wrap_tuple(l_ids @ r_ids, ls @ rs)
  | (l, {term: Tuple(r_ids, rs), _}) => wrap_tuple(r_ids, [l] @ rs)
  | ({term: Tuple(l_ids, ls), _}, r) => wrap_tuple(l_ids, ls @ [r])
  | (l, r) => wrap_tuple([], [l, r])
  };
}
and of_tuple_pat = (id: Id.t, l: UPat.t, r: UPat.t): UPat.t => {
  let wrap_tuple = (ids, es): UPat.t => {id, term: Tuple([id] @ ids, es)};
  switch (l, r) {
  | ({term: Tuple(l_ids, ls), _}, {term: Tuple(r_ids, rs), _}) =>
    wrap_tuple(l_ids @ r_ids, ls @ rs)
  | (l, {term: Tuple(r_ids, rs), _}) => wrap_tuple(r_ids, [l] @ rs)
  | ({term: Tuple(l_ids, ls), _}, r) => wrap_tuple(l_ids, ls @ [r])
  | (l, r) => wrap_tuple([], [l, r])
  };
}
and of_tuple_typ = (id: Id.t, l: UTyp.t, r: UTyp.t): UTyp.t => {
  let wrap_tuple = (ids, es): UTyp.t => {id, term: Tuple([id] @ ids, es)};
  switch (l, r) {
  | ({term: Tuple(l_ids, ls), _}, {term: Tuple(r_ids, rs), _}) =>
    wrap_tuple(l_ids @ r_ids, ls @ rs)
  | (l, {term: Tuple(r_ids, rs), _}) => wrap_tuple(r_ids, [l] @ rs)
  | ({term: Tuple(l_ids, ls), _}, r) => wrap_tuple(l_ids, ls @ [r])
  | (l, r) => wrap_tuple([], [l, r])
  };
}
and of_multi_pat = (id: Id.t, l: UPat.t, r: UPat.t): UPat.t => {
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
and of_multi_typ = (id: Id.t, l: UTyp.t, r: UTyp.t): UTyp.t => {
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
and of_multi_rul = (id: Id.t, l: URul.t, r: URul.t): URul.t => {
  let wrap_multi = (ids, es): URul.t => {
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
and of_rules_rul = (id: Id.t, l: URul.t, r: URul.t): URul.t => {
  //TODO(andrew): this doesnt make sense
  let wrap_rules = (ids, es: list((UPat.t, UExp.t))): URul.t => {
    id,
    term: Rules([id] @ ids, es),
  };
  switch (l, r) {
  | ({term: Rules(l_ids, ls), _}, {term: Rules(r_ids, rs), _}) =>
    wrap_rules(l_ids @ r_ids, ls @ rs)
  | ({term: Rules(_), _}, _) => l //TODO
  | (_, {term: Rules(_), _}) => r //TODO
  | (l, r) => {id, term: MultiHole([id], [l, r])}
  };
}
and of_piece_exp = (p: Piece.t, outside_kids: list(Term.any)): UExp.t => {
  switch (p) {
  | Whitespace({id, _}) => {id, term: Invalid(Whitespace, p)}
  | Grout({id, shape}) =>
    switch (shape, outside_kids) {
    | (Convex, []) => {id, term: EmptyHole}
    | (Concave, [Exp(l), Exp(r)]) => of_multi_exp(id, l, r)
    | _ => {id, term: Invalid(MalformedGrout, p)}
    }
  | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
    let term: UExp.term =
      switch (label, outside_kids, inside_kids) {
      | _ when !Tile.is_complete(t) =>
        // TODO: more principled handling of incomplete tiles
        EmptyHole
      //Invalid(IncompleteTile,p)
      // TODO(andrew): should Form.re handle atomic conversion?
      | (["triv"], [], []) => Triv
      | (["true"], [], []) => Bool(true)
      | (["false"], [], []) => Bool(false)
      | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
      | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => Var(t)
      | ([","], [Exp(l), Exp(r)], []) => of_tuple_exp(id, l, r).term
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
      | (["-"], [Exp(e)], []) => UnOp(Int(Minus), e)
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
      | (["nil"], [], []) => ListLit([], [])
      | (["[", "]"], [], [body]) =>
        switch (uexp_of_seg(body)) {
        | {term: Tuple(ids, es), _} => ListLit([id] @ ids, es)
        | term => ListLit([id], [term])
        }
      | (["case", "of"], [Rul(rules)], [scrut]) =>
        Match(uexp_of_seg(scrut), rules)
      | _ => Invalid(UnrecognizedTerm, p)
      };
    {id, term};
  };
}
and of_piece_pat = (p: Piece.t, outside_kids: list(Term.any)): UPat.t => {
  switch (p) {
  | Whitespace({id, _}) => {id, term: Invalid(Whitespace, p)}
  | Grout({id, shape}) =>
    switch (shape, outside_kids) {
    | (Convex, []) => {id, term: EmptyHole}
    | (Concave, [Pat(l), Pat(r)]) => of_multi_pat(id, l, r)
    | _ => {id, term: Invalid(MalformedGrout, p)}
    }
  | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
    let term: UPat.term =
      switch (label, outside_kids, inside_kids) {
      | _ when !Tile.is_complete(t) =>
        //MultiHole([id], List.map(upat_of_seg, inside_kids))
        EmptyHole
      //Invalid(IncompleteTile,p)
      // TODO(andrew): should Form.re handle atomic conversion?
      | (["triv"], [], []) => Triv
      | (["true"], [], []) => Bool(true)
      | (["false"], [], []) => Bool(false)
      | (["(", ")"], [], [body]) => Parens(upat_of_seg(body))
      | ([","], [Pat(l), Pat(r)], []) => of_tuple_pat(id, l, r).term
      | ([":"], [Pat(p), Typ(ty)], []) => TypeAnn(p, ty)
      /* WARNING: is_float must come first because is_int's regexp is strictly more general */
      | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
      | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => Var(t)
      | ([t], [], []) when Form.is_wild(t) => Wild
      | _ => Invalid(UnrecognizedTerm, p)
      };
    {id, term};
  };
}
and of_piece_typ = (p: Piece.t, outside_kids: list(Term.any)): UTyp.t => {
  switch (p) {
  | Whitespace({id, _}) => {id, term: Invalid(Whitespace, p)}
  | Grout({id, shape}) =>
    switch (shape, outside_kids) {
    | (Convex, []) => {id, term: EmptyHole}
    | (Concave, [Typ(l), Typ(r)]) => of_multi_typ(id, l, r)
    | _ => {id, term: Invalid(MalformedGrout, p)}
    }
  | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
    let term: UTyp.term =
      switch (label, outside_kids, inside_kids) {
      | _ when !Tile.is_complete(t) => Invalid(IncompleteTile, p)
      // TODO(andrew): should Form.re handle atomic conversion?
      | (["Unit"], [], []) => Tuple([id], [])
      | (["Bool"], [], []) => Bool
      | (["Int"], [], []) => Int
      | (["Float"], [], []) => Float
      | (["->"], [Typ(l), Typ(r)], []) => Arrow(l, r)
      | ([","], [Typ(l), Typ(r)], []) => of_tuple_typ(id, l, r).term
      | (["(", ")"], [], [body]) => Parens(utyp_of_seg(body))
      | (["[", "]"], [], [body]) => List(utyp_of_seg(body))
      | _ => Invalid(UnrecognizedTerm, p)
      };
    {id, term};
  };
}
and of_piece_rul = (p: Piece.t, outside_kids: list(Term.any)): URul.t => {
  switch (p) {
  | Whitespace({id, _}) => {id, term: Invalid(Whitespace, p)}
  | Grout({id, shape}) =>
    switch (shape, outside_kids) {
    | (Convex, []) => {id, term: EmptyHole}
    | (Concave, [Rul(l), Rul(r)]) => of_multi_rul(id, l, r)
    | _ => {id, term: Invalid(MalformedGrout, p)}
    }
  | Tile({id, label, children: inside_kids, mold: _, shards: _} as t) =>
    let term: URul.term =
      switch (label, outside_kids, inside_kids) {
      | _ when !Tile.is_complete(t) => Invalid(IncompleteTile, p)
      | (["=>"], [Pat(l), Exp(r)], []) => Rules([id], [(l, r)]) //TODO:unfuck
      | (["|"], [Rul(l), Rul(r)], []) => of_rules_rul(id, l, r).term
      | (["|"], [Rul({term: Rules(ids, rs), id: _})], []) =>
        Rules(ids @ [id], rs)
      | (["|"], [Rul(x)], []) => x.term //TODO:unfuck
      | _ => Invalid(UnrecognizedTerm, p)
      };
    {id, term};
  };
};

let go = Core_kernel.Memo.general(~cache_size_bound=1000, uexp_of_seg);
