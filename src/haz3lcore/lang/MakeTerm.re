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

/* The dispatch head of a root piece: its effective token list (the
 * tokens of its PRESENT shards) classified against the grammar's
 * label menu. TRUST BOUNDARY: the head is a pure function of the
 * effective label, never of the tile's stored form — the stored
 * atomic class/sort may be stale relative to the context this parse
 * sees (e.g. a TyVar-classified var in Exp context), and an
 * incomplete tile's present shards may spell a different complete
 * label (a partial `let _ = _ in` missing its `in` spells the ModLet
 * label ["let","="]). So atomic cases classify by token predicate on
 * Mono's token; incomplete tiles fall through to the hole path
 * (Partial) unless their present shards spell a complete label; and
 * match arms cover the full label-equivalence class (e.g. `+` = Plus
 * | TypPlus | TypSumSingle | Drv(Plus | Sum)), so a stale-molded
 * tile still parses by its label, exactly as under string matching. */
[@deriving (show({with_path: false}), sexp, yojson)]
type head =
  | Multi(Form.compound_form) /* complete multi-token label (class rep) */
  | Mono(option(Form.compound_form), Token.t) /* single present token */
  | ProjWrap /* projector: in-effect acts as a convex wrapping form */
  | Sec /* secondary: no tokens */
  | Partial; /* several tokens spelling no complete label */

/* Representative of a label's equivalence class: the first form
 * in declaration order carrying exactly this label. */
let class_rep = (lbl: Label.t): option(Form.compound_form) =>
  switch (Form.compound_defs(lbl)) {
  | [(cf, _), ..._] => Some(cf)
  | [] => None
  };

let mono = (t: Token.t): head => Mono(class_rep([t]), t);

let head: Piece.t => head =
  Piece.get(
    _ => Sec,
    _ => mono(" "),
    (t: Tile.t) =>
      switch (t.shards |> List.map(Tile.token(t))) {
      | [tok] => mono(tok)
      | toks =>
        switch (class_rep(toks)) {
        | Some(cf) => Multi(cf)
        | None => Partial
        }
      },
    _ => ProjWrap,
  );

let is_paren_lbl: Form.compound_form => bool =
  fun
  | ParensExp
  | ParensPat
  | ParensTyp
  | ParensTPat
  | ApExp
  | ApPat
  | ApTyp
  | Drv(ApExp | ApPat | ParenProp | ParenExp | ParenPat | ParenTyp) => true
  | _ => false;

[@deriving (show({with_path: false}), sexp, yojson)]
type tile = (Id.t, (head, list(Any.t)));
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
  term_data: TermData.t,
  projectors: Id.Map.t(Piece.projector),
  projector_list: list(Id.t),
};

let is_nary =
    (is_sort: Any.t => option('sort), delim: Token.t, (delims, kids): tiles)
    : option(list('sort)) =>
  if (delims |> List.map(snd) |> List.for_all((==)((mono(delim), [])))) {
    kids |> List.map(is_sort) |> OptUtil.sequence;
  } else {
    None;
  };

let is_tuple_exp = is_nary(Any.is_exp, ",");
let is_tuple_pat = is_nary(Any.is_pat, ",");
let is_tuple_typ = is_nary(Any.is_typ, ",");
let is_tuple_drv_exp = is_nary(Any.is_drv_exp, ",");
let is_typ_bsum = is_nary(Any.is_typ, "+");
let is_mod_seq = is_nary(Any.is_mod, ";");
let is_sig_seq = is_nary(Any.is_sig, ";");

/* Flatten a module term into a list of module items.
   Module sequences (from semicolons) are stored as MultiHole([Mod(m1), Mod(m2)])
   during parsing and need to be flattened into a proper list for Module(items).
   Non-Mod children (from broken parse states during editing) are wrapped as
   module items to isolate breakage — valid items keep their statics. */
let rec flatten_mod = (m: TermBase.Mod.t): list(TermBase.Mod.t) =>
  switch (m.term) {
  | MultiHole(kids) =>
    kids
    |> List.map(
         fun
         | Grammar.Mod(m) => flatten_mod(m)
         | Grammar.Exp(e) => [Mod.fresh(ModExp(e))]
         | other => [Mod.fresh(ModExp(Exp.fresh(MultiHole([other]))))],
       )
    |> List.flatten
  | ModLet(_, _)
  | ModType(_, _)
  | ModExp(_)
  | ModuleMod(_, _)
  | EmptyHole
  | Invalid(_) => [m]
  };

/* Flatten a signature term into a list of signature items.
   Signature sequences (from semicolons) are stored as MultiHole([Sig(s1), Sig(s2)])
   during parsing and need to be flattened into a proper list for Sig(items).
   Non-Sig children are wrapped to isolate breakage, matching flatten_mod. */
let rec flatten_sig = (s: TermBase.Sig.t): list(TermBase.Sig.t) =>
  switch (s.term) {
  | MultiHole(kids) =>
    kids
    |> List.map(
         fun
         | (Grammar.Sig(s): TermBase.Any.t) => flatten_sig(s)
         | other => [Sig.fresh(MultiHole([other]))],
       )
    |> List.flatten
  | SigLet(_)
  | SigType(_, _)
  | EmptyHole
  | Invalid(_) => [s]
  };

let is_grout = tiles =>
  Aba.get_as(tiles)
  |> List.map(snd)
  |> List.for_all((==)((mono(" "), [])));

let is_rules = ((ts, kids): tiles): option(Aba.t(Pat.t, Exp.t)) => {
  open OptUtil.Syntax;
  let+ ps =
    (ts: list(tile))
    |> List.map(
         fun
         | (_, (Multi(Rule | Drv(Rule)), [Pat(p)])) => Some(p)
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
         | (_, (Multi(Rule | Drv(Rule)), [Grammar.Drv(Pat(p))])) =>
           Some(p)
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

let kids_of_tile = ((_id, (_head, kids)): tile) => kids;
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

let term_data: ref(TermData.t) = ref(Id.Map.empty);
let record_term_data = (sort: Sort.t, seg: Segment.t, skel: Skel.t): unit =>
  term_data :=
    Aba.get_as(Aba.map_a(List.nth(seg), Skel.root(skel)))
    |> List.fold_left(
         (map, p) =>
           Id.Map.add(Piece.id(p), TermData.mk(p, sort, skel, seg), map),
         term_data^,
       );

/* Map to collect projector ids */
let projectors: ref(Id.Map.t(Piece.projector)) = ref(Id.Map.empty);
let projector_list: ref(list(Id.t)) = ref([]);

/* Map from tile IDs to their outer secondary (before, after) */
let secondary_map: ref(Segment.SecondaryCollection.secondary_map) =
  ref(Id.Map.empty);

/* Look up outer secondary for a term by its representative ID */
let get_secondary = (ids: list(Id.t)): IdTagged.IdTag.secondary_runs =>
  switch (ids) {
  | [id, ..._] =>
    switch (Id.Map.find_opt(id, secondary_map^)) {
    | Some(sec) => sec
    | None => IdTagged.IdTag.empty_secondary
    }
  | [] => IdTagged.IdTag.empty_secondary
  };

/* Track IDs that are "adopted" from inner terms into outer multi-tile forms.
 *
 * PROBLEM: List literals and case expressions are multi-tile forms where the
 * delimiters (`[`/`]`, `case`/`end`) combine with inner delimiters (commas,
 * `|`/`=>` rules) to form a single term. During bottom-up parsing, the inner
 * content is first parsed as a separate term (Tuple or Rules), creating
 * term_data entries. When the outer delimiters are matched, the inner term
 * is absorbed and its IDs become part of the outer term. However, the
 * term_data entries for these adopted IDs retain stale skeleton/segment info
 * from the inner term, breaking the invariant that term_data[id] reflects
 * the actual term that id belongs to.
 *
 * SOLUTION: Track adopted IDs at absorption points (ListLit, Match, ListLit
 * patterns), then consolidate their term_data entries to match the outer
 * term's data after parsing completes. This is O(k) where k is the number
 * of adopted IDs, rather than O(n) over all term_data entries.
 *
 * EXAMPLE: This inconsistency can cause issues for any code that relies on
 * term_data for spatial reasoning. For instance, with `[1, 2]` formatted as:
 *   [
 *     1, 2
 *   ]
 * The comma is first parsed as part of a Tuple on row 1, creating term_data
 * with a skeleton spanning only row 1. When `[`/`]` absorbs this into a
 * ListLit, the comma's term_data still has the Tuple's skeleton. MultiProbe
 * uses term_data to find terms ending on each row, so it sees a "phantom
 * tuple" on row 1 and selects it for probing, but Arms can't find the
 * correct term to draw decorations for. Consolidation fixes this by giving
 * the comma the ListLit's skeleton, so lookups return the correct spatial
 * extent.
 *
 * NOTE: This is somewhat ad-hoc - these absorption forms don't have first-class
 * syntactic support, so we handle them imperatively at each absorption site.
 * A cleaner solution would require richer syntax representation for multi-tile
 * forms, but this targeted fix avoids that complexity. */
let adopted_ids: ref(list(Id.t)) = ref([]);

/* Strip a projector from a segment and log it in the map */
let log_projector = (pr: Base.projector): unit => {
  projectors := Id.Map.add(pr.id, pr, projectors^);
  projector_list := [pr.id, ...projector_list^];
};

/* Convert IdTagged secondary to ConstructorMap secondary.
   Both use the same Secondary.t type, so this is just a type cast. */
let to_variant_secondary =
    (sec: IdTagged.IdTag.secondary_runs): ConstructorMap.secondary_runs => sec;

let parse_sum_term: Typ.t => ConstructorMap.variant(Typ.t) =
  fun
  | {term: Var(ctr), annotation: {ids, secondary}} =>
    Variant(
      ctr,
      {
        ids,
        secondary: to_variant_secondary(secondary),
      },
      None,
    )
  /* Constructor applications in sum type definitions are implemented as having type sort;
     until they are reimplemented as their own sort, we must prevent these from being parsed
     into types, where they can go on to mess with statics. Thus we let them fall through to
     be parsed as multiholes, and then recognize them when we parse sum type definitions */
  | {
      term:
        Unknown(
          Hole(
            MultiHole([
              Typ({
                term: Var(ctr),
                annotation: {ids: ids_ctr, secondary: (inner_before, _)},
              }),
              Typ(u),
            ]),
          ),
        ),
      annotation: {ids: ids_ap, secondary: (_, outer_after)},
    } =>
    /* For constructor applications, use the inner before (constructor's leading space)
       and outer after (trailing space on the whole application) for round-tripping */
    Variant(
      ctr,
      {
        ids: ids_ctr @ ids_ap,
        secondary: (inner_before, outer_after),
      },
      Some(u),
    )
  | t => BadEntry(t);

let mk_bad = (ctr, ids, value) => {
  let t: Typ.t = {
    annotation: IdTagged.IdTag.mk(ids, get_secondary(ids)),
    term: Var(ctr),
  };
  switch (value) {
  | None => t
  | Some(u) => Unknown(Hole(MultiHole([Typ(t), Typ(u)]))) |> Typ.fresh
  };
};

let is_hole_label = (t: string) =>
  t == " "
  || Token.is_explicit_hole(t)
  || Token.is_implicit_hole_marker(t)
  || Token.is_llm_hole(t);

let rec go_s = (s: Sort.t, skel: Skel.t, seg: Segment.t): Any.t =>
  switch (s) {
  | Drv(drv) =>
    Drv(
      switch (drv) {
      | Jdmt
      | Ctx
      | Prop
      | Exp => Exp(drv_exp(unsorted(Drv(Exp), skel, seg)))
      | Pat => Pat(drv_pat(unsorted(Drv(Pat), skel, seg)))
      | Typ => Typ(drv_typ(unsorted(Drv(Typ), skel, seg)))
      | TPat => TPat(drv_tpat(unsorted(Drv(TPat), skel, seg)))
      },
    )
  | Pat => Pat(pat(unsorted(Pat, skel, seg)))
  | TPat => TPat(tpat(unsorted(TPat, skel, seg)))
  | Typ => Typ(typ(unsorted(Typ, skel, seg)))
  | Exp => Exp(exp(unsorted(Exp, skel, seg)))
  | Rul => Rul(rul(unsorted(Rul, skel, seg)))
  | Mod => Mod(mod_(unsorted(Mod, skel, seg))) /* Phase 1.2: proper module parsing */
  | Sig => Sig(sig_(unsorted(Sig, skel, seg)))
  | MPat => MPat(mpat(unsorted(MPat, skel, seg)))
  | Any =>
    let sort = Segment.sort_of(skel, seg);
    if (sort == Any) {
      Exp(exp(unsorted(Exp, skel, seg)));
    } else {
      go_s(sort, skel, seg);
    };
  }
and drv_exp = unsorted => {
  let (term, inner_ids) = drv_exp_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(
    e => Drv(Exp(e)),
    ids,
    {
      annotation: IdTagged.IdTag.mk(ids, IdTagged.IdTag.empty_secondary),
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
    | (Mono(_, t), []) =>
      switch (t) {
      | "Truth" => ret(Truth)
      | "Falsity" => ret(Falsity)
      | "True" => ret(True)
      | "False" => ret(False)
      | _ when Token.is_wild(t) => ret(ExpHole)
      | _ when Token.is_empty_list(t) => ret(Ctx([]))
      | _ when Token.is_empty_tuple(t) => ret(Triv)
      | _ when Token.is_int(t) => ret(NumLit(int_of_string(t)))
      | _
          when
            Token.is_var(t)
            && String.length(t) > 1
            && String.sub(t, 0, 1) == "$" =>
        ret(Quote(t))
      | _ when Token.is_typ_var(t) => ret(Var(t))
      | _ => ret(hole(tm))
      }
    | (Multi(Drv(Val)), [Drv(Exp(e))]) => ret(Val(e))
    | (Multi(Drv(Valid)), [Drv(Typ(t))]) => ret(Type(t))
    | (
        Multi(ListLitExp | ListLitPat | ListTyp | Drv(List)),
        [Drv(Exp(body))],
      ) =>
      switch (body.term) {
      | Tuple(es) => (Ctx(es), IdTagged.ids(body))
      | Pair(e1, e2) => (Ctx([e1, e2]), IdTagged.ids(body))
      | _ => ret(Ctx([body]))
      }
    | (Multi(cf), [Drv(Exp(body))]) when is_paren_lbl(cf) =>
      switch (body.term) {
      /* A standard Drv pair is parenthesised, so here we collapse
         [Parens(Tuple(e1, e2))] into [Pair(e1, e2)]. */
      | Tuple([e1, e2]) => (Pair(e1, e2), IdTagged.ids(body))
      | _ => ret(Parens(body))
      }
    | (Multi(Case | Drv(Case)), [Drv(Exp(body))]) =>
      switch (body.term) {
      | Case(_) as term => (term, IdTagged.ids(body))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Drv(Exp(l)), ([(_id, (Mono(op, _), []))], []), Drv(Exp(r))) as tm =>
    switch (op) {
    | Some(Drv(Eval)) => ret(Eval(l, r))
    | Some(Drv(Entail | UnaryEntail)) => ret(Entail(l, r))
    | Some(CommaExp | CommaPat | CommaTyp | Drv(CommaExp | CommaPat)) =>
      ret(Tuple([l, r]))
    | Some(ConsExp | ConsPat | Drv(Cons)) => ret(Cons(l, r))
    | Some(ListConcat | Drv(Concat)) => ret(Concat(l, r))
    | Some(Drv(And)) => ret(And(l, r))
    | Some(LogicalOrLegacy | Drv(Or)) => ret(Or(l, r))
    | Some(Drv(Impl)) => ret(Impl(l, r))
    | Some(Plus | TypPlus | TypSumSingle | Drv(Plus | Sum)) =>
      ret(BinOp(Plus, l, r))
    | Some(Minus | UnaryMinus | Drv(Neg | Minus)) =>
      ret(BinOp(Minus, l, r))
    | Some(Times | Drv(Times | Prod)) => ret(BinOp(Times, l, r))
    | Some(Equals | Drv(Eq)) => ret(BinOp(Eq, l, r))
    | Some(Lt | Drv(Lt)) => ret(BinOp(Lt, l, r))
    | Some(Gt | Drv(Gt)) => ret(BinOp(Gt, l, r))
    | Some(DotExp | DotTyp | ProdProjection | Drv(Dot)) =>
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
  | Bin(Drv(Exp(l)), ([(_id, (Mono(op, _), []))], []), Drv(Typ(r))) as tm =>
    switch (op) {
    | Some(Typeann | TypeAsc | Drv(HasType | Cast) | MPatTypeann) =>
      ret(HasType(l, r))
    | Some(Drv(Syn)) => ret(Syn(l, r))
    | Some(Lte | Drv(Ana)) => ret(Ana(l, r))
    | _ => ret(hole(tm))
    }
  | Pre(([(_id, t)], []), Drv(Exp(r))) as tm =>
    switch (t) {
    | (Mono(Some(Minus | UnaryMinus | Drv(Neg | Minus)), _), []) =>
      ret(Neg(r))
    | (Mono(Some(Not | Drv(Not)), _), []) =>
      ret(Impl(r, Falsity |> Drv.Exp.fresh))
    | (Mono(Some(Drv(Entail | UnaryEntail)), _), []) =>
      ret(Entail(Ctx([]) |> Drv.Exp.fresh, r))
    | (Multi(If | Drv(If)), [Drv(Exp(cond)), Drv(Exp(conseq))]) =>
      ret(If(cond, conseq, r))
    | (Multi(Let | Drv(Let)), [Drv(Pat(pat)), Drv(Exp(def))]) =>
      ret(Let(pat, def, r))
    | (Multi(Fix | Drv(Fix)), [Drv(Pat(pat))]) => ret(Fix(pat, r))
    | (Multi(Fun | Drv(Fun)), [Drv(Pat(pat))]) => ret(Fun(pat, r))
    | _ => ret(hole(tm))
    }
  | Pre(([(_id, (hd, [Drv(Typ(l))]))], []), Drv(Typ(r))) as tm =>
    switch (hd) {
    | Multi(Drv(Consistent)) => ret(Consistent(l, r))
    | Multi(Drv(MatchedArrow)) => ret(MatchedArrow(l, r))
    | Multi(Drv(MatchedProd)) => ret(MatchedProd(l, r))
    | Multi(Drv(MatchedSum)) => ret(MatchedSum(l, r))
    | _ => ret(hole(tm))
    }
  | Post(Drv(Exp(l)), ([(_id, t)], [])) as tm =>
    switch (t) {
    | (Multi(cf), [Drv(Exp(r))]) when is_paren_lbl(cf) =>
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
      annotation: IdTagged.IdTag.mk(ids, IdTagged.IdTag.empty_secondary),
      term,
    },
  );
}
and drv_pat_term: unsorted => (Drv.Pat.term, list(Id.t)) = {
  let ret = (tm: Drv.Pat.term) => (tm, []);
  let hole: unsorted => DrvTermBase.pat_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, (Mono(_, t), []))], [])) as tm =>
    switch (t) {
    | _
        when
          Token.is_var(t)
          && String.length(t) > 1
          && String.sub(t, 0, 1) == "$" =>
      ret(Quote(t))
    | _ when Token.is_typ_var(t) => ret(Var(t))
    | _ => ret(hole(tm))
    }
  | Op(([(_id, (Multi(cf), [Drv(Pat(body))]))], []))
      when is_paren_lbl(cf) =>
    ret(Parens(body))
  | Post(Drv(Pat(l)), ([(_id, (Multi(cf), [Drv(Pat(r))]))], [])) as tm
      when is_paren_lbl(cf) =>
    switch (l.term) {
    | Var("L") => (InjL(r), IdTagged.ids(l))
    | Var("R") => (InjR(r), IdTagged.ids(l))
    | _ => ret(hole(tm))
    }
  | Bin(
      Drv(Pat(l)),
      (
        [
          (
            _id,
            (
              Mono(
                Some(Typeann | TypeAsc | Drv(HasType | Cast) | MPatTypeann),
                _,
              ),
              [],
            ),
          ),
        ],
        [],
      ),
      Drv(Typ(r)),
    ) =>
    ret(Cast(l, r))
  | Bin(
      Drv(Pat(l)),
      (
        [
          (
            _id,
            (
              Mono(
                Some(
                  CommaExp | CommaPat | CommaTyp | Drv(CommaExp | CommaPat),
                ),
                _,
              ),
              [],
            ),
          ),
        ],
        [],
      ),
      Drv(Pat(r)),
    ) =>
    ret(Pair(l, r))
  | _ as tm => ret(hole(tm));
}

and drv_typ = unsorted => {
  let (term, inner_ids) = drv_typ_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(
    ty => Drv(Typ(ty)),
    ids,
    {
      annotation: IdTagged.IdTag.mk(ids, IdTagged.IdTag.empty_secondary),
      term,
    },
  );
}
and drv_typ_term: unsorted => (Drv.Typ.term, list(Id.t)) = {
  let ret = (tm: Drv.Typ.term) => (tm, []);
  let hole: unsorted => DrvTermBase.typ_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, (Mono(_, t), []))], [])) as tm =>
    switch (t) {
    | "Num" => ret(Num)
    | "Bool" => ret(Bool)
    | "1"
    | "Unit" => ret(Unit)
    | _ when Token.is_explicit_hole(t) || Token.is_implicit_hole_marker(t) =>
      ret(TypHole)
    | _
        when
          Token.is_var(t)
          && String.length(t) > 1
          && String.sub(t, 0, 1) == "$" =>
      ret(Quote(t))
    | _ when Token.is_typ_var(t) => ret(Var(t))
    | _ => ret(hole(tm))
    }
  | Op(([(_id, (Multi(cf), [Drv(Typ(body))]))], []))
      when is_paren_lbl(cf) =>
    ret(Parens(body))
  | Pre(
      ([(_id, (Multi(Rec | Drv(Rec)), [Drv(TPat(p))]))], []),
      Drv(Typ(t)),
    ) =>
    ret(Rec(p, t))
  | Bin(Drv(Typ(l)), ([(_id, (Mono(op, _), []))], []), Drv(Typ(r))) as tm =>
    switch (op) {
    | Some(TypeArrow | Drv(Arrow)) => ret(Arrow(l, r))
    | Some(Times | Drv(Times | Prod)) => ret(Prod(l, r))
    | Some(Plus | TypPlus | TypSumSingle | Drv(Plus | Sum)) =>
      ret(Sum(l, r))
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
      annotation: IdTagged.IdTag.mk(ids, IdTagged.IdTag.empty_secondary),
      term,
    },
  );
}
and drv_tpat_term: unsorted => (Drv.TPat.term, list(Id.t)) = {
  let ret = (tm: Drv.TPat.term) => (tm, []);
  let hole: unsorted => DrvTermBase.tpat_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, (Mono(_, t), []))], []))
      when
        Token.is_var(t)
        && String.length(t) > 1
        && String.sub(t, 0, 1) == "$" =>
    ret(Quote(t))
  | Op(([(_id, (Mono(_, t), []))], [])) when Token.is_typ_var(t) =>
    ret(Var(t))
  | _ as tm => ret(hole(tm));
}

and exp = unsorted => {
  let (term, inner_ids) = exp_term(unsorted);
  /* The editor root can change while the expression itself is still sort
     [Exp]; when an [Exp]-sort traversal trips over a Drv child we get a
     [MultiHole([Drv(_), ...])], which we intercept and re-parse at the
     [Drv(Exp)] level. */
  switch (term) {
  | MultiHole([Drv(_), ..._]) =>
    let (term, inner_ids) = drv_exp_term(unsorted);
    let ids = ids(unsorted) @ inner_ids;
    let exp =
      return(
        e => Drv(Exp(e)),
        ids,
        IdTagged.mk(ids, get_secondary(ids), term),
      );
    Grammar.DrvQuote(Exp(exp), Jdmt) |> IdTagged.fresh;
  | _ =>
    let ids = ids(unsorted) @ inner_ids;
    let e: TermBase.exp_t =
      return(e => Exp(e), ids, IdTagged.mk(ids, get_secondary(ids), term));
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
    | ([(_id, t)], []) =>
      switch (t) {
      | (Mono(_, t), []) when Token.is_empty_tuple(t) => ret(Tuple([]))
      | (Mono(_, t), []) when Token.is_wild(t) => ret(Deferral(OutsideAp))
      | (Mono(_, t), []) when Token.is_empty_list(t) => ret(ListLit([]))
      | (Mono(_, t), []) when Token.is_empty_module(t) => ret(Module([]))
      | (Mono(_, t), []) when Token.is_bool(t) =>
        ret(Atom(Bool(bool_of_string(t))))
      | (Mono(_, t), []) when Token.is_undefined(t) => ret(Undefined)
      | (Mono(_, t), []) when Token.is_int(t) =>
        ret(Atom(Int(Bigint.of_string(t))))
      | (Mono(_, t), []) when Token.is_string(t) =>
        ret(Atom(String(Token.strip_quotes(t))))
      | (Mono(_, t), []) when Token.is_quoted_label(t) =>
        ret(Label(Token.strip_quotes(~quote=Token.label_delim, t)))
      | (Mono(_, t), []) when Token.is_float(t) =>
        ret(Atom(Float(float_of_string(t))))
      | (Mono(_, t), []) when Token.is_livelit(t) =>
        ret(LivelitName(Token.parse_livelit(t)))
      | (Mono(_, t), []) when Token.is_var(t) => ret(Var(t))
      | (Mono(_, t), []) when Token.is_ctr(t) => ret(Constructor(t, None))
      | (Multi(ModBody | SigBody), [Mod(body)]) =>
        /* ModBody absorption: inner Mod's semicolon IDs become part of Module.
           With flat Skel (ModSeq is chainable), body.annotation.ids contains
           ALL semicolon IDs when body is MultiHole. These are absorbed so the
           Module expression gets IDs = [curly_brace_id] @ [all_semicolon_ids].
           IMPORTANT: Only absorb for MultiHole (multiple items with semicolons).
           Single items have ModLet/ModType IDs that would conflict with expansion. */
        switch (body) {
        | {term: EmptyHole, _} => ret(Module([body]))
        | {annotation: {ids, _}, term: MultiHole(_)} =>
          adopted_ids := ids @ adopted_ids^;
          (Module(flatten_mod(body)), ids);
        | _ => ret(Module(flatten_mod(body)))
        }
      | (Multi(ModBody | SigBody), [Exp(body)]) => ret(Parens(body))
      | (Multi(cf), [Exp(body)]) when is_paren_lbl(cf) =>
        ret(Parens(body))
      | (ProjWrap, [Exp(body)]) => ret(body.term)
      | (
          Multi(ListLitExp | ListLitPat | ListTyp | Drv(List)),
          [Exp(body)],
        ) =>
        /* ListLit absorption: inner Tuple's comma IDs become part of ListLit.
           ID order: [bracket_id] @ comma_ids (outer first, then adopted).
           IMPORTANT: Must align with ExpToSegment.exp_to_pretty ListLit case,
           which expects List.hd = bracket, List.tl = commas. */
        switch (body) {
        | {annotation: {ids, _}, term: Tuple(es)} =>
          adopted_ids := ids @ adopted_ids^;
          // Addresses tup_labels in lists like: [l=32, 1]
          (
            ListLit(
              List.map(
                (list_item: Grammar.exp_t(IdTagged.IdTag.t)) => {
                  let (e, rewrap) = IdTagged.unwrap(list_item);
                  switch (e) {
                  | TupLabel(_) =>
                    rewrap(Tuple([e |> Exp.fresh]): TermBase.exp_term)
                  | _ => list_item
                  };
                },
                es,
              ),
            ),
            ids,
          );
        | term => ret(ListLit([term]))
        }
      | (Multi(Test), [Exp(test)]) => ret(Test(test))
      | (Multi(ProofObject), [Exp(proof)]) => ret(ProofObject(proof))
      | (Multi(HintedTest), [Exp(hint), Exp(test)]) =>
        ret(HintedTest(test, hint))
      | (Multi(Case | Drv(Case)), [Rul({term, annotation: {ids, _}})]) =>
        /* Match absorption: inner Rules' |/=> IDs become part of Match.
           ID order: [case_end_id] @ rule_ids (outer first, then adopted).
           IMPORTANT: Must align with ExpToSegment.exp_to_pretty Match case,
           which expects List.hd = case/end, List.tl = rules. */
        switch (term) {
        | Rules(scrut, rules) =>
          adopted_ids := ids @ adopted_ids^;
          (Match(scrut, rules), ids);
        // If the rule parser is correct, below should be impossible
        | MultiHole(anys) => (MultiHole(anys), ids)
        | Invalid(string) => (Invalid(string), ids)
        }
      /* The [of_*] / [end] delimiters lift a Drv term back up to sort Exp
         by wrapping it in a [DrvQuote] node tagged with its derivation sort. */
      | (Multi(Drv(OfJdmt)), [Drv(Exp(j))]) =>
        ret(DrvQuote(Exp(j), Jdmt))
      | (Multi(Drv(OfCtx)), [Drv(Exp(c))]) =>
        ret(DrvQuote(Exp(c), Ctx))
      | (Multi(Drv(OfProp)), [Drv(Exp(p))]) =>
        ret(DrvQuote(Exp(p), Prop))
      | (Multi(Drv(OfAlfaExp)), [Drv(Exp(e))]) =>
        ret(DrvQuote(Exp(e), Exp))
      | (Multi(Drv(OfAlfaTyp)), [Drv(Typ(t))]) =>
        ret(DrvQuote(Typ(t), Typ))
      | (Multi(Drv(OfAlfaPat)), [Drv(Pat(p))]) =>
        ret(DrvQuote(Pat(p), Pat))
      | (Multi(Drv(OfAlfaTPat)), [Drv(TPat(tp))]) =>
        ret(DrvQuote(TPat(tp), TPat))
      | (Mono(_, t), []) when is_hole_label(t) => ret(hole(tm))
      | (Mono(_, t), []) when t != " " && !Token.is_explicit_hole(t) =>
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
        | (Mono(Some(Minus | UnaryMinus | Drv(Neg | Minus)), _), []) =>
          UnOp(Int(Minus), r)
        | (Mono(Some(Not | Drv(Not)), _), []) => UnOp(Bool(Not), r)
        | (Multi(Fun | Drv(Fun)), [Pat(pat)]) => Fun(pat, r, None, None)
        | (Multi(Forall), [Pat(pat)]) => Forall(pat, r)
        | (Multi(Fix | Drv(Fix)), [Pat(pat)]) => FixF(pat, r, None)
        | (Multi(TypFun), [TPat(tpat)]) => TypFun(tpat, r, None)
        | (Multi(Let | Drv(Let)), [Pat(pat), Exp(def)]) => Let(pat, def, r)
        | (Multi(ModuleExp), [MPat(mp), Exp(def)]) => ModuleExp(mp, def, r)
        | (Multi(Theorem), [Pat(pat), Exp(thm)]) => Theorem(pat, thm, r)
        | (Multi(FilterHide), [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Eval, One),
              pat: filter,
            }),
            r,
          )
        | (Multi(FilterEval), [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Eval, All),
              pat: filter,
            }),
            r,
          )
        | (Multi(FilterPause), [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Step, One),
              pat: filter,
            }),
            r,
          )
        | (Multi(FilterDebug), [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Step, All),
              pat: filter,
            }),
            r,
          )
        | (Multi(Use), [Typ(ty)]) => Use(ty, r)
        | (Multi(TypeAlias), [TPat(tpat), Typ(def)]) =>
          TyAlias(tpat, def, r)
        | (Multi(If | Drv(If)), [Exp(cond), Exp(conseq)]) =>
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
      | (Mono(Some(ApExpEmpty | ApPatEmpty | Drv(ApExpEmpty)), _), []) =>
        ret(
          Ap(
            Forward,
            l,
            {
              annotation:
                IdTagged.IdTag.mk(
                  [Id.nullary_ap_flag],
                  get_secondary([Id.nullary_ap_flag]),
                ),
              term: Tuple([]),
            },
          ),
        )
      | (Multi(cf), [Exp(arg)]) when is_paren_lbl(cf) =>
        let use_deferral = (arg: Exp.t): Exp.t => {
          let deferral_ids = IdTagged.ids(arg);
          {
            annotation:
              IdTagged.IdTag.mk(deferral_ids, get_secondary(deferral_ids)),
            term: Deferral(InAp),
          };
        };
        switch (arg.term) {
        | Var(l) when Token.is_livelit(l) =>
          ret(LivelitName(Token.parse_livelit(l)))
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
      | (Multi(ApExpTyp), [Typ(ty)]) => ret(TypAp(l, ty))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Exp(l), tiles, Typ(r)) as tm =>
    switch (tiles) {
    | (
        [
          (
            _id,
            (
              Mono(
                Some(Typeann | TypeAsc | Drv(HasType | Cast) | MPatTypeann),
                _,
              ),
              [],
            ),
          ),
        ],
        [],
      ) =>
      ret(Asc(l, r))
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
             | {term: Tuple([{term: _ as tl, _}]), _} as tup =>
               // We use the Id for the tuple as the ids for the tuplabels
               let (_, rewrap) = IdTagged.unwrap(tup);
               rewrap(tl);
             | _ => child
             }
           });
      ret(Tuple(tuple_children));
    | None =>
      switch (tiles) {
      | ([(_id, (op, _))], []) =>
        ret(
          switch (op) {
          | Mono(Some(Plus | TypPlus | TypSumSingle | Drv(Plus | Sum)), _) =>
            BinOp(Int(Plus), l, r)
          | Mono(Some(Minus | UnaryMinus | Drv(Neg | Minus)), _) =>
            BinOp(Int(Minus), l, r)
          | Mono(Some(Times | Drv(Times | Prod)), _) =>
            BinOp(Int(Times), l, r)
          | Mono(Some(Power), _) => BinOp(Int(Power), l, r)
          | Mono(Some(Divide), _) => BinOp(Int(Divide), l, r)
          | Mono(Some(Lt | Drv(Lt)), _) => BinOp(Int(LessThan), l, r)
          | Mono(Some(Gt | Drv(Gt)), _) => BinOp(Int(GreaterThan), l, r)
          | Mono(Some(Lte | Drv(Ana)), _) =>
            BinOp(Int(LessThanOrEqual), l, r)
          | Mono(Some(Gte), _) => BinOp(Int(GreaterThanOrEqual), l, r)
          | Mono(Some(Equals | Drv(Eq)), _) => BinOp(Poly(Equals), l, r)
          | Mono(Some(NotEquals), _) => BinOp(Poly(NotEquals), l, r)
          | Mono(Some(FPlus), _) => BinOp(Float(Plus), l, r)
          | Mono(Some(FMinus), _) => BinOp(Float(Minus), l, r)
          | Mono(Some(FTimes), _) => BinOp(Float(Times), l, r)
          | Mono(Some(FDivide), _) => BinOp(Float(Divide), l, r)
          | Mono(Some(FPower), _) => BinOp(Float(Power), l, r)
          | Mono(Some(FLt), _) => BinOp(Float(LessThan), l, r)
          | Mono(Some(FGt), _) => BinOp(Float(GreaterThan), l, r)
          | Mono(Some(FLte), _) => BinOp(Float(LessThanOrEqual), l, r)
          | Mono(Some(FGte), _) => BinOp(Float(GreaterThanOrEqual), l, r)
          | Mono(Some(FEquals), _) => BinOp(Float(Equals), l, r)
          | Mono(Some(FNotEquals), _) => BinOp(Float(NotEquals), l, r)
          | Mono(Some(LogicalAnd), _) => BinOp(Bool(And), l, r)
          | Mono(Some(LogicalOr), _) => BinOp(Bool(Or), l, r)
          | Mono(Some(ConsExp | ConsPat | Drv(Cons)), _) => Cons(l, r)
          | Mono(Some(CellJoin | ModSeq | SigSeq), _) => Seq(l, r)
          | Mono(Some(StringConcat), _) => BinOp(String(Concat), l, r)
          | Mono(Some(TupleExtension | ProdExtension), _) =>
            TupleExtension(l, r)
          | Mono(
              Some(TupleLabeledExp | TupleLabeledPat | TupleLabeledTyp),
              _,
            ) =>
            switch (l.term) {
            | Deferral(_) =>
              TupLabel(
                {
                  annotation: l.annotation,
                  term: ExplicitNonlabel,
                },
                r,
              ) // Unlabeled tuple using deferred ap in tuplabel
            | Var(name)
            | Constructor(name, None) =>
              TupLabel(
                {
                  annotation: l.annotation,
                  term: Label(name),
                },
                r,
              )
            | Label(_) => TupLabel(l, r)
            | EmptyHole => TupLabel(l, r)
            | _ =>
              let (e_term, rewrap) = IdTagged.unwrap(l);

              TupLabel(
                rewrap(MultiHole([Exp(e_term |> Exp.fresh)]): Exp.term),
                r,
              );
            }
          | Mono(Some(DotExp | DotTyp | ProdProjection | Drv(Dot)), _) =>
            switch (r.term) {
            | Var(name)
            | Constructor(name, _) =>
              Dot(
                l,
                {
                  annotation: r.annotation,
                  term: Label(name),
                },
              )
            | Label(_) => Dot(l, r)
            | EmptyHole => Dot(l, r)
            | _ =>
              let (e_term, rewrap) = IdTagged.unwrap(r);

              Dot(
                l,
                rewrap(MultiHole([Exp(e_term |> Exp.fresh)]): Exp.term),
              );
            }
          | Mono(Some(Pipeline), _) => Ap(Reverse, r, l)
          | Mono(Some(ListConcat | Drv(Concat)), _) => ListConcat(l, r)
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
    return(p => Pat(p), ids, IdTagged.mk(ids, get_secondary(ids), term));
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
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (Mono(_, t), []) when Token.is_empty_tuple(t) => ret(Tuple([]))
      | (Mono(_, t), []) when Token.is_empty_list(t) => ret(ListLit([]))
      | (Mono(_, t), []) when Token.is_bool(t) =>
        ret(Atom(Bool(bool_of_string(t))))
      | (Mono(_, t), []) when Token.is_float(t) =>
        ret(Atom(Float(float_of_string(t))))
      | (Mono(_, t), []) when Token.is_int(t) =>
        ret(Atom(Int(Bigint.of_string(t))))
      | (Mono(_, t), []) when Token.is_string(t) =>
        ret(Atom(String(Token.strip_quotes(t))))
      | (Mono(_, t), []) when Token.is_quoted_label(t) =>
        ret(Label(Token.strip_quotes(~quote=Token.label_delim, t)))
      | (Mono(_, t), []) when Token.is_var(t) => ret(Var(t))
      | (Mono(_, t), []) when Token.is_wild(t) => ret(Wild)
      | (Mono(_, t), []) when Token.is_ctr(t) => ret(Constructor(t, None))
      | (Multi(cf), [Pat(body)]) when is_paren_lbl(cf) =>
        ret(Parens(body))
      | (ProjWrap, [Pat(body)]) => ret(body.term)
      | (
          Multi(ListLitExp | ListLitPat | ListTyp | Drv(List)),
          [Pat(body)],
        ) =>
        /* ListLit pattern absorption: inner Tuple's comma IDs become part of ListLit.
           ID order: [bracket_id] @ comma_ids (outer first, then adopted).
           IMPORTANT: Must align with ExpToSegment.pat_to_pretty ListLit case,
           which expects List.hd = bracket, List.tl = commas. */
        switch (body) {
        | {term: Tuple(ps), annotation: {ids, _}} =>
          adopted_ids := ids @ adopted_ids^;
          (ListLit(ps), ids);
        | term => ret(ListLit([term]))
        }
      | (Mono(_, t), []) when is_hole_label(t) => ret(hole(tm))
      | (Mono(_, t), []) => ret(Invalid(t))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Post(Pat(l), tiles) as tm =>
    switch (tiles) {
    | ([(_id, t)], []) =>
      ret(
        switch (t) {
        | (Mono(Some(ApExpEmpty | ApPatEmpty | Drv(ApExpEmpty)), _), []) =>
          Ap(
            l,
            {
              annotation: {
                ids: [Id.nullary_ap_flag],
                secondary: ([], []),
              },
              term: Tuple([]),
            },
          )
        | (Multi(cf), [Pat(arg)]) when is_paren_lbl(cf) => Ap(l, arg)
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Bin(Pat(p), tiles, Typ(ty)) as tm =>
    switch (tiles) {
    | (
        [
          (
            _id,
            (
              Mono(
                Some(Typeann | TypeAsc | Drv(HasType | Cast) | MPatTypeann),
                _,
              ),
              [],
            ),
          ),
        ],
        [],
      ) =>
      ret(Asc(p, ty))
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
      | (
          [
            (
              _id,
              (
                Mono(
                  Some(TupleLabeledExp | TupleLabeledPat | TupleLabeledTyp),
                  _,
                ),
                [],
              ),
            ),
          ],
          [],
        ) =>
        switch (l.term) {
        | Wild =>
          ret(
            TupLabel(
              {
                annotation: l.annotation,
                term: ExplicitNonlabel,
              },
              r,
            ),
          ) // Unlabeled tuple using deferred ap in tuplabel
        | Var(name)
        | Constructor(name, None) =>
          ret(
            TupLabel(
              {
                annotation: l.annotation,
                term: Label(name),
              },
              r,
            ),
          )
        | Label(_) => ret(TupLabel(l, r))
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
      | (
          [(_id, (Mono(Some(ConsExp | ConsPat | Drv(Cons)), _), []))],
          [],
        ) =>
        ret(Cons(l, r))
      | _ => ret(hole(tm))
      }
    }
  | tm => ret(hole(tm));
}
and typ = unsorted => {
  let (term, inner_ids) = typ_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  let t =
    return(ty => Typ(ty), ids, IdTagged.mk(ids, get_secondary(ids), term));
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
    | ([(_id, (Multi(ModBody | SigBody), [Sig(body)]))], []) =>
      /* SigBody: parse signature body, similar to ModBody in exp_term */
      switch (body) {
      | {term: EmptyHole, _} => ret(Sig([]))
      | {annotation: {ids, _}, term: MultiHole(_)} =>
        adopted_ids := ids @ adopted_ids^;
        (Sig(flatten_sig(body)), ids);
      | _ => ret(Sig(flatten_sig(body)))
      }
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | (Mono(_, t), []) when Token.is_empty_tuple(t) => Prod([])
        | (Mono(_, t), []) when Token.is_empty_module(t) => Sig([])
        | (Mono(_, "Bool"), []) => Atom(Bool)
        | (Mono(_, "Int"), []) => Atom(Int)
        | (Mono(_, "SInt"), []) => Atom(SInt)
        | (Mono(_, "Float"), []) => Atom(Float)
        | (Mono(_, "String"), []) => Atom(String)
        | (Mono(_, "Nat"), []) => Atom(Nat)
        | (Mono(_, "Void"), []) => Sum([])
        | (Mono(_, "DrvJdmt"), []) => DrvQuoteTy(Jdmt)
        | (Mono(_, "DrvCtx"), []) => DrvQuoteTy(Ctx)
        | (Mono(_, "DrvProp"), []) => DrvQuoteTy(Prop)
        | (Mono(_, "ALFAExp"), []) => DrvQuoteTy(Exp)
        | (Mono(_, "DrvPat"), []) => DrvQuoteTy(Pat)
        | (Mono(_, "ALFATyp"), []) => DrvQuoteTy(Typ)
        | (Mono(_, "DrvTPat"), []) => DrvQuoteTy(TPat)
        | (Mono(_, "_"), []) => ExplicitNonlabel
        | (Multi(ProofOf), [Exp(exp)]) => ProofOf(exp)
        | (Mono(_, t), []) when Token.is_typ_var(t) => Var(t)
        | (Mono(_, t), []) when Token.is_quoted_label(t) =>
          Label(Token.sub(t, 1, Token.length(t) - 2))
        | (Multi(cf), [Typ(body)]) when is_paren_lbl(cf) => Parens(body)
        | (ProjWrap, [Typ(body)]) => body.term
        | (
            Multi(ListLitExp | ListLitPat | ListTyp | Drv(List)),
            [Typ(body)],
          ) =>
          List(body)
        | (Mono(_, t), []) when is_hole_label(t) => hole(tm)
        | (Mono(_, t), []) => Unknown(Hole(Invalid(t)))
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Post(Typ(_t), tiles) as tm =>
    switch (tiles) {
    /* Type aps which would otherwise be parsed here are recognized in sum type parsing above */
    | _ => ret(hole(tm))
    }
  /* poly and rec have to be before sum so that they bind tighter.
   * Thus `rec A -> Left(A) + Right(B)` get parsed as `rec A -> (Left(A) + Right(B))`
   * If this is below the case for sum, then it gets parsed as an invalid form. */
  | Pre(([(_id, (Multi(Poly), [TPat(tpat)]))], []), Typ(t)) =>
    ret(Poly(tpat, t))
  | Pre(([(_id, (Multi(Rec | Drv(Rec)), [TPat(tpat)]))], []), Typ(t)) =>
    ret(Rec(tpat, t))
  | Pre(tiles, Typ({term: Sum(t0), annotation: {ids, _}})) as tm =>
    /* Case for leading prefix + preceeding a sum */
    switch (tiles) {
    | (
        [
          (
            _,
            (
              Mono(Some(Plus | TypPlus | TypSumSingle | Drv(Plus | Sum)), _),
              [],
            ),
          ),
        ],
        [],
      ) =>
      adopted_ids := ids @ adopted_ids^;
      (Sum(t0), ids);
    | _ => ret(hole(tm))
    }
  | Pre(tiles, Typ(t)) as tm =>
    switch (tiles) {
    | (
        [
          (
            _,
            (
              Mono(Some(Plus | TypPlus | TypSumSingle | Drv(Plus | Sum)), _),
              [],
            ),
          ),
        ],
        [],
      ) =>
      ret(Sum([parse_sum_term(t)]))
    | _ => ret(hole(tm))
    }
  | Bin(Typ(t1), tiles, Typ(t2)) as tm when is_typ_bsum(tiles) != None =>
    switch (is_typ_bsum(tiles)) {
    | Some(between_kids) =>
      ret(Sum(List.map(parse_sum_term, [t1] @ between_kids @ [t2])))
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
      | ([(_id, (Mono(Some(TypeArrow | Drv(Arrow)), _), []))], []) =>
        ret(Arrow(l, r))
      | (
          [
            (
              _id,
              (
                Mono(
                  Some(TupleLabeledExp | TupleLabeledPat | TupleLabeledTyp),
                  _,
                ),
                [],
              ),
            ),
          ],
          [],
        ) =>
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
      | (
          [
            (
              _id,
              (
                Mono(Some(DotExp | DotTyp | ProdProjection | Drv(Dot)), _),
                [],
              ),
            ),
          ],
          [],
        ) =>
        switch (r.term) {
        | Var(name) =>
          ret(
            ProdProjection(
              l,
              {
                annotation: r.annotation,
                term: Label(name),
              },
            ),
          )
        | _ => ret(ProdProjection(l, r))
        }
      | (
          [(_id, (Mono(Some(TupleExtension | ProdExtension), _), []))],
          [],
        ) =>
        ret(ProdExtension(l, r))
      | _ => ret(hole(tm))
      }
    }
  | tm => ret(hole(tm));
}
and tpat = unsorted => {
  let term = tpat_term(unsorted);
  let ids = ids(unsorted);
  return(ty => TPat(ty), ids, IdTagged.mk(ids, get_secondary(ids), term));
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
        | (Mono(_, t), []) when Token.is_typ_var(t) => Var(t)
        | (Mono(_, t), []) when is_hole_label(t) => hole(tm)
        | (Mono(_, t), []) => Invalid(t)
        | (ProjWrap, [TPat(body)]) => body.term
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | (Pre(_) | Post(_)) as tm => ret(hole(tm))
  | tm => ret(hole(tm));
}
/* Phase 1.2: Module parsing - placeholder implementation */
and mod_ = unsorted => {
  let term = mod_term(unsorted);
  let ids = ids(unsorted);
  return(m => Mod(m), ids, IdTagged.mk(ids, get_secondary(ids), term));
}
and mod_term: unsorted => TermBase.Mod.term = {
  let ret = (term: TermBase.Mod.term) => term;
  let hole = unsorted => Mod.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (Mono(_, t), []) when is_hole_label(t) => ret(hole(tm))
      | _ =>
        /* Try parsing as expression and wrap as ModExp */
        let e = exp(Op(tiles));
        switch (e.term) {
        | EmptyHole
        | MultiHole(_)
        | Invalid(_) => ret(hole(tm))
        | _ => ret(ModExp(e))
        };
      }
    | _ => ret(hole(tm))
    }
  /* ModSeq: semicolon-separated module items (like tuples with commas) */
  | Bin(Mod(m1), tiles, Mod(m2)) =>
    switch (is_mod_seq(tiles)) {
    | Some(between_kids) =>
      /* Flatten all mod items into MultiHole, like tuples flatten into Tuple */
      let all_items =
        [Grammar.Mod(m1)]
        @ List.map(m => Grammar.Mod(m), between_kids)
        @ [Grammar.Mod(m2)];
      ret(MultiHole(all_items));
    | None => ret(hole(Bin(Mod(m1), tiles, Mod(m2))))
    }
  /* ModLet: let p = e - the pattern is inside the tile, expression is the body */
  | Pre(([(_id, (Multi(ModLet), [Pat(p)]))], []), Exp(e)) =>
    ret(ModLet(p, e))
  /* ModuleMod: module M = e - MPat inside tile, expression is the body */
  | Pre(([(_id, (Multi(ModuleMod), [MPat(mp)]))], []), Exp(e)) =>
    ret(ModuleMod(mp, e))
  /* ModType: type t = T - the tpat is inside the tile, type is the body */
  | Pre(([(_id, (Multi(ModType | SigType), [TPat(tp)]))], []), Typ(ty)) =>
    ret(ModType(tp, ty))
  /* Expression-level structures (binary ops, prefix, postfix) - wrap as ModExp */
  | Bin(Exp(_), _, Exp(_)) as tm => ret(ModExp(exp(tm)))
  | Pre(_, Exp(_)) as tm => ret(ModExp(exp(tm)))
  | Post(Exp(_), _) as tm => ret(ModExp(exp(tm)))
  | (Pre(_) | Post(_) | Bin(_)) as tm => ret(hole(tm));
}
and sig_ = unsorted => {
  let term = sig_term(unsorted);
  let ids = ids(unsorted);
  return(s => Sig(s), ids, IdTagged.mk(ids, get_secondary(ids), term));
}
and sig_term: unsorted => TermBase.Sig.term = {
  let ret = (term: TermBase.Sig.term) => term;
  let hole = unsorted => Sig.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (Mono(_, t), []) when is_hole_label(t) => ret(hole(tm))
      | (Mono(_, t), []) => ret(Invalid(t))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  /* SigSeq: semicolon-separated signature items */
  | Bin(Sig(s1), tiles, Sig(s2)) =>
    switch (is_sig_seq(tiles)) {
    | Some(between_kids) =>
      let sig_to_any = (s): TermBase.Any.t => Grammar.Sig(s);
      let all_items =
        [sig_to_any(s1)]
        @ List.map(sig_to_any, between_kids)
        @ [sig_to_any(s2)];
      ret(MultiHole(all_items));
    | None => ret(hole(Bin(Sig(s1), tiles, Sig(s2))))
    }
  /* SigLet: let p - the pattern is the body */
  | Pre(([(_id, (Mono(Some(SigLet), _), []))], []), Pat(p)) =>
    ret(SigLet(p))
  /* SigType: type t = T - the tpat is inside the tile, type is the body */
  | Pre(([(_id, (Multi(ModType | SigType), [TPat(tp)]))], []), Typ(ty)) =>
    ret(SigType(tp, ty))
  | (Pre(_) | Post(_) | Bin(_)) as tm => ret(hole(tm));
}
and mpat = unsorted => {
  let term = mpat_term(unsorted);
  let ids = ids(unsorted);
  return(mp => MPat(mp), ids, IdTagged.mk(ids, get_secondary(ids), term));
}
and mpat_term: unsorted => TermBase.MPat.term = {
  let ret = (term: TermBase.MPat.term) => term;
  let hole = unsorted => MPat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, (Mono(_, t), []))], [])
        when Token.is_var(t) || Token.is_ctr(t) =>
      ret(Var(t))
    | ([(_id, (Mono(_, t), []))], []) when is_hole_label(t) =>
      ret(hole(tm))
    | ([(_id, (Mono(_, t), []))], []) => ret(Invalid(t))
    | _ => ret(hole(tm))
    }
  | Bin(MPat(mp), tiles, Typ(ty)) as tm =>
    switch (tiles) {
    | (
        [
          (
            _id,
            (
              Mono(
                Some(Typeann | TypeAsc | Drv(HasType | Cast) | MPatTypeann),
                _,
              ),
              [],
            ),
          ),
        ],
        [],
      ) =>
      ret(Asc(mp, ty))
    | _ => ret(hole(tm))
    }
  | (Pre(_) | Post(_) | Bin(_)) as tm => ret(hole(tm));
}

and rul = (unsorted): Rul.t => {
  let e = exp(unsorted);
  let mk_rules = (scrut: Exp.t, rules, ids): Rul.t => {
    term: Rules(scrut, rules),
    annotation: IdTagged.IdTag.mk(ids, get_secondary(ids)),
  };
  switch (e) {
  | {term: MultiHole(_), _} =>
    switch (unsorted) {
    | Bin(Exp(scrut), tiles, Exp(last_clause)) =>
      switch (is_rules(tiles)) {
      | Some((ps, leading_clauses)) =>
        mk_rules(
          scrut,
          List.combine(ps, leading_clauses @ [last_clause]),
          ids(unsorted),
        )
      | None => mk_rules(e, [], [Id.invalid])
      }
    | _ => mk_rules(e, [], [Id.invalid])
    }
  | _ => mk_rules(e, [], [Id.invalid])
  };
}

and unsorted = (sort: Sort.t, skel: Skel.t, seg: Segment.t): unsorted => {
  /* Remove projectors. We do this here as opposed to removing
   * them in an external call to save a whole-syntax pass. */
  let tile_kids = (p: Piece.t): list(Any.t) =>
    switch (p) {
    | Secondary(_)
    | Grout(_) => []
    | Projector({id, kind, model, syntax, _} as pr) =>
      let _ = log_projector(pr);
      let sort = Piece.sort(syntax) |> fst;
      let seg = Piece.unparenthesize(syntax);
      let inner = go_s(sort, Segment.skel(seg), seg);
      /* Construct Projector term with proper annotation, preserving
       * projector metadata (kind, model) in the term for round-tripping */
      let projector_data: Grammar.projector_data = {
        kind,
        model,
      };
      let wrapped =
        switch (inner) {
        | Grammar.Exp(e) =>
          Grammar.Exp({
            term: Projector(projector_data, e),
            annotation: IdTagged.IdTag.mk([id], get_secondary([id])),
          })
        | Grammar.Pat(p) =>
          Grammar.Pat({
            term: Projector(projector_data, p),
            annotation: IdTagged.IdTag.mk([id], get_secondary([id])),
          })
        | Grammar.Typ(t) =>
          Grammar.Typ({
            term: Projector(projector_data, t),
            annotation: IdTagged.IdTag.mk([id], get_secondary([id])),
          })
        | _ => inner
        };
      [wrapped];
    | Tile({shards, children, _} as t) =>
      Aba.aba_triples(Aba.mk(shards, children))
      |> List.map(((l, kid, r)) => {
           let s = l + 1 == r ? List.nth(Tile.mold(t).in_, l) : Sort.Any;
           go_s(s, Segment.skel(~sort=s, kid), kid);
         })
    };

  /* Capture term ranges */
  record_term_data(sort, seg, skel);

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
         => (Piece.id(p), (head(p), tile_kids(p))));

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

/* Consolidate term_data for adopted IDs (see adopted_ids comment above).
 * Updates each adopted ID's term_data entry to match the rep_id's entry,
 * giving adopted IDs the correct skeleton/segment of the outer term.
 *
 * IMPORTANT: We preserve the original root_piece and sort for each adopted ID.
 * root_piece: identifies the actual tile, needed by Arms.tiles_data for decoration.
 * sort: the tile's own sort context (e.g. Mod for semicolons inside Module body),
 * needed by Code.re's sort-consistency check.
 * Only skel and base_seg should be updated to match the outer term. */
let consolidate_adopted = (): unit => {
  adopted_ids^
  |> List.iter(id => {
       switch (Id.Map.find_opt(id, map^)) {
       | None => ()
       | Some(term) =>
         let rep = Language.Any.rep_id(term);
         switch (
           Id.Map.find_opt(rep, term_data^),
           Id.Map.find_opt(id, term_data^),
         ) {
         | (Some(rep_data), Some(old_data)) =>
           /* Preserve the original root_piece and sort while updating skel/base_seg.
              root_piece: identifies the actual tile for shard decoration.
              sort: the adopted tile's own sort context (e.g. Mod for semicolons
              inside a Module body), not the outer expression's sort. */
           term_data :=
             Id.Map.add(
               id,
               {
                 ...rep_data,
                 root_piece: old_data.root_piece,
                 sort: old_data.sort,
               },
               term_data^,
             )
         | (Some(rep_data), None) =>
           /* Fallback: if no old entry, use rep_data as-is (shouldn't happen normally) */
           term_data := Id.Map.add(id, rep_data, term_data^)
         | (None, _) => ()
         };
       }
     });
};

let go =
  Core.Memo.general(
    ~cache_size_bound=1000,
    seg => {
      map := TermMap.empty;
      term_data := Id.Map.empty;
      projectors := Id.Map.empty;
      projector_list := [];
      adopted_ids := [];
      secondary_map := Segment.SecondaryCollection.collect(seg);
      let term = exp(unsorted(Exp, Segment.skel(seg), seg));
      consolidate_adopted();
      {
        term,
        term_data: term_data^,
        terms: map^,
        projectors: projectors^,
        projector_list: projector_list^,
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
        let sort = Segment.sort_of(skel, seg);
        let unsorted = unsorted(sort, skel, seg);
        switch (sort) {
        | Drv(_) =>
          switch (drv_exp(unsorted)) {
          | {term: Tuple(_), _} => None
          | _ => Some(Grammar.Drv(Exp(drv_exp(unsorted))))
          }
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
        | Mod => Some(Mod(mod_(unsorted)))
        | Sig => Some(Sig(sig_(unsorted)))
        | MPat => Some(MPat(mpat(unsorted)))
        /* Default unresolved sort to Exp, matching go_s above.
         * Reject bare Tuple(_) for the same reason as the Exp
         * branch: at top level it isn't well-structured in
         * isolation. */
        | Any =>
          switch (exp(unsorted)) {
          | {term: Tuple(_), _} => None
          | e => Some(Grammar.Exp(e))
          }
        };
      };
    }
  );

let from_zip_for_sem = (z: Zipper.t, ~root) =>
  go(Dump.to_segment(z, ~root));

let from_zip_for_sem =
  Core.Memo.general(~cache_size_bound=1000, from_zip_for_sem);
