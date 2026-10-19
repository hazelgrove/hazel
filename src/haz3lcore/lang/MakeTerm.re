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

/* The dispatch payload of a root piece is its stored form id, with
 * TokInfix collapsed to Tok: the backup-infix bit is mold-only, the
 * token means the same thing. Compound identity is trusted as
 * spelled at edit time; a Tok's meaning is re-derived from its token
 * by the Token.is_* guards below, since a tile's stored sort can be
 * stale. Incomplete tiles match no compound arm (too few kids for
 * the arm's kid pattern) and parse as holes. */
[@deriving (show({with_path: false}), sexp, yojson)]
type head =
  | F(Form.t) /* tile */
  | ProjWrap /* projector: in-effect acts as a convex wrapping form */
  | Sec; /* secondary: no tokens */

let head: Piece.t => head =
  Piece.get(
    _ => Sec,
    /* grout: no tile can carry token " ", so Tok(" ") is
     * collision-free and parses as hole via is_hole_label below */
    _ => F(Tok(" ")),
    (t: Tile.t) =>
      F(
        switch (t.form) {
        | TokInfix(tok) => Tok(tok)
        | f => f
        },
      ),
    _ => ProjWrap,
  );

/* "()" is spelled both by the empty-tuple token (convex position)
 * and by the nullary-ap families (postfix); the two storages must
 * parse identically in each shape position, so every "()" arm
 * accepts both. */
let is_empty_tuple_form: Form.t => bool =
  fun
  | Tok(t)
  | TokInfix(t) => Token.is_empty_tuple(t)
  | Compound(ApEmpty) => true
  | Compound(_) => false;

[@deriving (show({with_path: false}), sexp, yojson)]
type tile = (Id.t, (head, list(Any.t)));
[@deriving (show({with_path: false}), sexp, yojson)]
type tiles = Aba.t(tile, Any.t);
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
    (
      is_sort: Any.t => option('sort),
      is_delim: Form.t => bool,
      (delims, kids): tiles,
    )
    : option(list('sort)) =>
  if (delims
      |> List.map(snd)
      |> List.for_all(
           fun
           | (F(f), []) => is_delim(f)
           | _ => false,
         )) {
    kids |> List.map(is_sort) |> OptUtil.sequence;
  } else {
    None;
  };

/* the n-ary delimiter families (all arity 1) */
let is_comma_form: Form.t => bool =
  fun
  | Compound(Comma) => true
  | _ => false;
let is_plus_form: Form.t => bool =
  fun
  | Compound(Plus) => true
  | _ => false;
let is_semi_form: Form.t => bool =
  fun
  | Compound(CellJoin) => true
  | _ => false;

let is_tuple_exp = is_nary(Any.is_exp, is_comma_form);
let is_tuple_pat = is_nary(Any.is_pat, is_comma_form);
let is_tuple_typ = is_nary(Any.is_typ, is_comma_form);
let is_tuple_drv_exp = is_nary(Any.is_drv_exp, is_comma_form);
let is_typ_bsum = is_nary(Any.is_typ, is_plus_form);
let is_mod_seq = is_nary(Any.is_mod, is_semi_form);
let is_sig_seq = is_nary(Any.is_sig, is_semi_form);

/* Flatten a module term into a list of module items.
   Module sequences (from semicolons) are stored as MultiHole([Mod(m1), Mod(m2)])
   during parsing and need to be flattened into a proper list for Module(items).
   Non-Mod children (from broken parse states during editing) are wrapped as
   module items to isolate breakage — valid items keep their statics. A
   wrapper's id derives from its child's, so the member keeps its outline
   row across parses */
let rec flatten_mod = (m: TermBase.Mod.t): list(TermBase.Mod.t) =>
  switch (m.term) {
  | MultiHole(kids) =>
    kids
    |> List.map(
         fun
         | Grammar.Mod(m) => flatten_mod(m)
         | Grammar.Exp(e) => [
             IdTagged.mk_internal(
               [Id.derive(~salt="mod-exp", Exp.rep_id(e))],
               ModExp(e): TermBase.Mod.term,
             ),
           ]
         | other => {
             let id = Id.derive(~salt="mod-other", Any.rep_id(other));
             [
               IdTagged.mk_internal(
                 [id],
                 ModExp(
                   IdTagged.mk_internal([id], MultiHole([other]): Exp.term),
                 ): TermBase.Mod.term,
               ),
             ];
           },
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

let is_rules = ((ts, kids): tiles): option(Aba.t(Pat.t, Exp.t)) => {
  open OptUtil.Syntax;
  let+ ps =
    (ts: list(tile))
    |> List.map(
         fun
         | (_, (F(Compound(Rule)), [Pat(p)])) => Some(p)
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
         | (_, (F(Compound(Rule)), [Grammar.Drv(Pat(p))])) => Some(p)
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

let term_data: ref(TermData.t) = ref(Id.Map.empty);

/* the levels this parse reads, by first piece: secondary collection
   records each one's pieces and skel, so the parse indexes an array and
   doesn't build the skel again */
let levels: ref(Segment.SecondaryCollection.levels) =
  ref(Hashtbl.create(1));

let level_of = (seg: Segment.t): option(Segment.SecondaryCollection.level) =>
  switch (seg) {
  | [p, ..._] =>
    switch (Hashtbl.find_opt(levels^, Piece.id(p))) {
    | Some(l) when l.l_seg === seg => Some(l)
    | _ => None
    }
  | [] => None
  };

/* [seg] as an array, for indexing by skel position */
let pieces_of = (seg: Segment.t): array(Piece.t) =>
  switch (level_of(seg)) {
  | Some(l) => l.l_arr
  | None =>
    let arr = Array.of_list(seg);
    Segment.SecondaryCollection.record(levels^, seg, arr, None);
    arr;
  };

let has_concave_grout =
  Array.exists((p: Piece.t) =>
    switch (p) {
    | Grout({shape: Concave, _}) => true
    | _ => false
    }
  );

/* Segment.skel(~sort, seg), from collection when it read the same: its
   skels are Exp-sorted, which only Mod and Sig read differently, at
   concave grout */
let skel_of = (~sort: Sort.t, seg: Segment.t): Skel.t =>
  switch (level_of(seg)) {
  | Some({l_skel: Some(skel), l_arr, _})
      when sort != Mod && sort != Sig || !has_concave_grout(l_arr) => skel
  | _ => Segment.skel(~sort, seg)
  };

let sort_of = (skel: Skel.t, seg: Segment.t): Sort.t =>
  Piece.sort(pieces_of(seg)[Aba.first_a(Skel.root(skel))]) |> fst;

let record_term_data = (sort: Sort.t, seg: Segment.t, skel: Skel.t): unit => {
  let arr = pieces_of(seg);
  term_data :=
    Aba.get_as(Skel.root(skel))
    |> List.fold_left(
         (map, i) => {
           let p = arr[i];
           Id.Map.add(Piece.id(p), TermData.mk(p, sort, skel, seg), map);
         },
         term_data^,
       );
};

/* Map to collect projector ids */
let projectors: ref(Id.Map.t(Piece.projector)) = ref(Id.Map.empty);
let projector_list: ref(list(Id.t)) = ref([]);

/* Map from tile IDs to their outer secondary (before, after) */
let secondary_map: ref(Segment.SecondaryCollection.secondary_map) =
  ref(Id.Map.empty);

/* module bodies parsed member by member (Incr): brace id -> the member
   list's term and its projector ids, in go's order, which the brace's
   child reads instead of parsing it. Set per parse by [fresh] */
let body_hooks: ref(Id.Map.t((Any.t, list(Id.t)))) = ref(Id.Map.empty);

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

/* Shard provenance from canonical completion: tile id -> shard mask
 * (physically present indices + partially-typed prefixes). Empty
 * unless parsing a canonically completed segment. Written only by
 * [fresh]; read via get_incomplete. */
let shard_masks: ref(Id.Map.t(IdTagged.IdTag.incomplete_mask)) =
  ref(Id.Map.empty);

/* The subset of this term's tiles that were completed, with their
 * originally-present shards, for the annotation. */
let get_incomplete = (ids: list(Id.t)): IdTagged.IdTag.incomplete_tiles =>
  Id.Map.is_empty(shard_masks^)
    ? []
    : ids
      |> List.filter_map(id =>
           Id.Map.find_opt(id, shard_masks^)
           |> Option.map(mask => (id, mask))
         );

/* Surface spelling of the token a *_term branch just parsed, when its
 * canonical print differs (int/float spellings, hole tokens). Set only
 * by non-recursive single-token branches, taken (and cleared) by the
 * sort wrapper immediately after — so the value can never leak across
 * terms. */
let pending_lexeme: ref(option(string)) = ref(None);
let set_lexeme = (t: Token.t): unit => pending_lexeme := Some(t);
let take_lexeme = (): option(string) => {
  let l = pending_lexeme^;
  pending_lexeme := None;
  l;
};

/* Register the wrapped term under every id and build its annotation
 * from those same ids (present-shard masks, outer secondary, optional
 * surface lexeme). Callers take_lexeme() BEFORE calling, never inside:
 * exp's Drv-reparse branch relies on the top-of-exp take to discard a
 * lexeme set by a discarded unknown-op parse. */
let return = (wrap, ~lexeme=None, ids, term) => {
  let tm =
    IdTagged.mk(
      ~incomplete=get_incomplete(ids),
      ~lexeme,
      ids,
      get_secondary(ids),
      term,
    );
  map := TermMap.add_all(ids, wrap(tm), map^);
  tm;
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
  | {term: Var(ctr), annotation: {ids, secondary, _}} =>
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
                annotation: {ids: ids_ctr, secondary: (inner_before, _), _},
              }),
              Typ(u),
            ]),
          ),
        ),
      annotation: {ids: ids_ap, secondary: (_, outer_after), _},
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

/* The token of a single-token form (grout's pseudo-token " " excluded):
   the lexeme an unknown-operator hole records. Label-derived, so a
   defined operator stranded at a foreign sort (a Compound) counts too. */
let op_lexeme = (f: Form.t): option(Token.t) =>
  switch (Form.label_of(f)) {
  | [t] when t != " " => Some(t)
  | _ => None
  };

let is_hole_label = (t: string) =>
  t == " "
  || Token.is_explicit_hole(t)
  || Token.is_implicit_hole_marker(t)
  || Token.is_concave_hole_marker(t)
  || Token.is_llm_hole(t);

/* A construct whose root tiles are all exp-only forms (if/then/else,
   case/end, ...) stranded in a non-exp context: no typ/pat pattern can
   match it, so without rerouting it falls to hole() and the tiles are
   dropped on print. Labels with a mold at the host sort (parens,
   lists, ->) are NOT rerouted — the sorted cases handle those. */
let is_exp_only_head = (~host: Sort.t, h: head): bool =>
  switch (h) {
  | F(f) =>
    let label = Form.label_of(f);
    List.length(label) >= 2
    && (
      switch (Form.base_candidates(label)) {
      | [] => false
      | cands =>
        List.exists(((_, m): (Form.t, Mold.t)) => m.out == Sort.Exp, cands)
        && List.for_all(((_, m): (Form.t, Mold.t)) => m.out != host, cands)
      }
    );
  | ProjWrap
  | Sec => false
  };

let root_heads: unsorted => list(head) =
  tm => {
    let of_tiles = (tiles: tiles) =>
      Aba.get_as(tiles) |> List.map(((_, (h, _))) => h);
    switch (tm) {
    | Op(tiles)
    | Pre(tiles, _)
    | Post(_, tiles)
    | Bin(_, tiles, _) => of_tiles(tiles)
    };
  };

let all_exp_only = (~host: Sort.t, tm: unsorted): bool =>
  switch (root_heads(tm)) {
  | [] => false
  | heads => List.for_all(is_exp_only_head(~host), heads)
  };

/* Is this kid a constructor-named type variable? (the head of a
   potential sum-type constructor application) */
let is_ctr_headed: Any.t => bool =
  fun
  | Typ({term: Var(name), _}) => Token.is_ctr(name)
  | _ => false;

let is_exp_kid: Any.t => bool =
  fun
  | Exp(_) => true
  | _ => false;
let is_pat_kid: Any.t => bool =
  fun
  | Pat(_) => true
  | _ => false;

/* a term the parse makes itself (a singleton labeled tuple, a cross-sort
   hole) gets an id derived from the piece it wraps, not a fresh one, so
   two parses of the same pieces agree */
let derived = (~salt: string, ids: list(Id.t), term: 'a): IdTagged.t('a) =>
  IdTagged.mk_internal(
    [
      switch (ids) {
      | [id, ..._] => Id.derive(~salt, id)
      | [] => Id.mk()
      },
    ],
    term,
  );
let derived_hole = (hole: list(Any.t) => 'a, kid: Any.t): IdTagged.t('a) =>
  derived(~salt="cross-sort", [Any.rep_id(kid)], hole([kid]));

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
    let sort = sort_of(skel, seg);
    if (sort == Any) {
      Exp(exp(unsorted(Exp, skel, seg)));
    } else {
      go_s(sort, skel, seg);
    };
  }
and drv_exp = unsorted => {
  let (term, inner_ids) = drv_exp_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(e => Drv(Exp(e)), ids, term);
}
and drv_exp_term: unsorted => (Drv.Exp.term, list(Id.t)) = {
  let ret = (tm: Drv.Exp.term) => (tm, []);
  let hole: unsorted => DrvTermBase.exp_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, t)], [])) as tm =>
    switch (t) {
    /* "()" must mean Triv under both its storages; this is the
     * nullary-ap one, Tok("()") hits is_empty_tuple below */
    | (F(Compound(ApEmpty)), []) => ret(Triv)
    | (F(Tok(t)), []) =>
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
    | (F(Compound(Val)), [Drv(Exp(e))]) => ret(Val(e))
    | (F(Compound(Valid)), [Drv(Typ(t))]) => ret(Type(t))
    | (F(Compound(ListLit)), [Drv(Exp(body))]) =>
      switch (body.term) {
      | Tuple(es) => (Ctx(es), IdTagged.ids(body))
      | Pair(e1, e2) => (Ctx([e1, e2]), IdTagged.ids(body))
      | _ => ret(Ctx([body]))
      }
    | (F(Compound(Parens)), [Drv(Exp(body))]) =>
      switch (body.term) {
      /* A standard Drv pair is parenthesised, so here we collapse
         [Parens(Tuple(e1, e2))] into [Pair(e1, e2)]. */
      | Tuple([e1, e2]) => (Pair(e1, e2), IdTagged.ids(body))
      | _ => ret(Parens(body))
      }
    | (F(Compound(Case)), [Drv(Exp(body))]) =>
      switch (body.term) {
      | Case(_) as term => (term, IdTagged.ids(body))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Drv(Exp(l)), ([(_id, (F(op), []))], []), Drv(Exp(r))) as tm =>
    switch (op) {
    | Compound(Eval) => ret(Eval(l, r))
    | Compound(Entail) => ret(Entail(l, r))
    | Compound(Comma) => ret(Tuple([l, r]))
    | Compound(Cons) => ret(Cons(l, r))
    | Compound(ListConcat) => ret(Concat(l, r))
    | Compound(And) => ret(And(l, r))
    | Compound(LogicalOrLegacy) => ret(Or(l, r))
    | Compound(Impl) => ret(Impl(l, r))
    | Compound(Plus) => ret(BinOp(Plus, l, r))
    | Compound(Minus) => ret(BinOp(Minus, l, r))
    | Compound(Times) => ret(BinOp(Times, l, r))
    | Compound(Equals) => ret(BinOp(Eq, l, r))
    | Compound(Lt) => ret(BinOp(Lt, l, r))
    | Compound(Gt) => ret(BinOp(Gt, l, r))
    | Compound(Dot) =>
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
  | Bin(Drv(Exp(l)), ([(_id, (F(op), []))], []), Drv(Typ(r))) as tm =>
    switch (op) {
    | Compound(TypeAsc) => ret(HasType(l, r))
    | Compound(Syn) => ret(Syn(l, r))
    | Compound(Lte) => ret(Ana(l, r))
    | _ => ret(hole(tm))
    }
  | Pre(([(_id, t)], []), Drv(Exp(r))) as tm =>
    switch (t) {
    | (F(Compound(UnaryMinus)), []) => ret(Neg(r))
    | (F(Compound(Not)), []) => ret(Impl(r, Falsity |> Drv.Exp.fresh))
    | (F(Compound(UnaryEntail)), []) =>
      ret(Entail(Ctx([]) |> Drv.Exp.fresh, r))
    | (F(Compound(If)), [Drv(Exp(cond)), Drv(Exp(conseq))]) =>
      ret(If(cond, conseq, r))
    | (F(Compound(Let)), [Drv(Pat(pat)), Drv(Exp(def))]) =>
      ret(Let(pat, def, r))
    | (F(Compound(Fix)), [Drv(Pat(pat))]) => ret(Fix(pat, r))
    | (F(Compound(Fun)), [Drv(Pat(pat))]) => ret(Fun(pat, r))
    | _ => ret(hole(tm))
    }
  | Pre(([(_id, (hd, [Drv(Typ(l))]))], []), Drv(Typ(r))) as tm =>
    switch (hd) {
    | F(Compound(Consistent)) => ret(Consistent(l, r))
    | F(Compound(MatchedArrow)) => ret(MatchedArrow(l, r))
    | F(Compound(MatchedProd)) => ret(MatchedProd(l, r))
    | F(Compound(MatchedSum)) => ret(MatchedSum(l, r))
    | _ => ret(hole(tm))
    }
  | Post(Drv(Exp(l)), ([(_id, t)], [])) as tm =>
    switch (t) {
    | (F(Compound(Ap)), [Drv(Exp(r))]) =>
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
  return(p => Drv(Pat(p)), ids, term);
}
and drv_pat_term: unsorted => (Drv.Pat.term, list(Id.t)) = {
  let ret = (tm: Drv.Pat.term) => (tm, []);
  let hole: unsorted => DrvTermBase.pat_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, (F(Tok(t)), []))], [])) as tm =>
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
  | Op(([(_id, (F(Compound(Parens)), [Drv(Pat(body))]))], [])) =>
    ret(Parens(body))
  | Post(Drv(Pat(l)), ([(_id, (F(Compound(Ap)), [Drv(Pat(r))]))], [])) as tm =>
    switch (l.term) {
    | Var("L") => (InjL(r), IdTagged.ids(l))
    | Var("R") => (InjR(r), IdTagged.ids(l))
    | _ => ret(hole(tm))
    }
  | Bin(
      Drv(Pat(l)),
      ([(_id, (F(Compound(TypeAsc)), []))], []),
      Drv(Typ(r)),
    ) =>
    ret(Cast(l, r))
  | Bin(
      Drv(Pat(l)),
      ([(_id, (F(Compound(Comma)), []))], []),
      Drv(Pat(r)),
    ) =>
    ret(Pair(l, r))
  | _ as tm => ret(hole(tm));
}

and drv_typ = unsorted => {
  let (term, inner_ids) = drv_typ_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(ty => Drv(Typ(ty)), ids, term);
}
and drv_typ_term: unsorted => (Drv.Typ.term, list(Id.t)) = {
  let ret = (tm: Drv.Typ.term) => (tm, []);
  let hole: unsorted => DrvTermBase.typ_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, (F(Tok(t)), []))], [])) as tm =>
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
  | Op(([(_id, (F(Compound(Parens)), [Drv(Typ(body))]))], [])) =>
    ret(Parens(body))
  | Pre(
      ([(_id, (F(Compound(Rec)), [Drv(TPat(p))]))], []),
      Drv(Typ(t)),
    ) =>
    ret(Rec(p, t))
  | Bin(Drv(Typ(l)), ([(_id, (F(op), []))], []), Drv(Typ(r))) as tm =>
    switch (op) {
    | Compound(TypeArrow) => ret(Arrow(l, r))
    | Compound(Times) => ret(Prod(l, r))
    | Compound(Plus) => ret(Sum(l, r))
    | _ => ret(hole(tm))
    }
  | _ as tm => ret(hole(tm));
}

and drv_tpat = unsorted => {
  let (term, inner_ids) = drv_tpat_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(tpat => Drv(TPat(tpat)), ids, term);
}
and drv_tpat_term: unsorted => (Drv.TPat.term, list(Id.t)) = {
  let ret = (tm: Drv.TPat.term) => (tm, []);
  let hole: unsorted => DrvTermBase.tpat_term =
    unsorted => Hole(Any.drv_hole(kids_of_unsorted(unsorted)));
  fun
  | Op(([(_id, (F(Tok(t)), []))], []))
      when
        Token.is_var(t)
        && String.length(t) > 1
        && String.sub(t, 0, 1) == "$" =>
    ret(Quote(t))
  | Op(([(_id, (F(Tok(t)), []))], [])) when Token.is_typ_var(t) =>
    ret(Var(t))
  | _ as tm => ret(hole(tm));
}

and exp = unsorted => {
  let (term, inner_ids) = exp_term(unsorted);
  let lexeme = take_lexeme();
  /* The editor root can change while the expression itself is still sort
     [Exp]; when an [Exp]-sort traversal trips over a Drv child we get a
     [MultiHole([Drv(_), ...])], which we intercept and re-parse at the
     [Drv(Exp)] level. */
  switch (term) {
  | MultiHole([Drv(_), ..._]) =>
    let (term, inner_ids) = drv_exp_term(unsorted);
    let ids = ids(unsorted) @ inner_ids;
    let exp = return(e => Drv(Exp(e)), ids, term);
    Grammar.DrvQuote(Exp(exp), Jdmt) |> IdTagged.fresh;
  | _ =>
    let ids = ids(unsorted) @ inner_ids;
    let e: TermBase.exp_t = return(e => Exp(e), ~lexeme, ids, term);
    switch (term) {
    | TupLabel(_) =>
      // The tile id is the id of the tuple not the tuplabel
      let (e_term, rewrap) = IdTagged.unwrap(e);
      rewrap(Tuple([derived(~salt="tuplabel", ids, e_term)]): Exp.term);
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
      | (F(f), []) when is_empty_tuple_form(f) => ret(Tuple([]))
      | (F(Tok(t)), []) when Token.is_wild(t) => ret(Deferral(OutsideAp))
      | (F(Tok(t)), []) when Token.is_empty_list(t) => ret(ListLit([]))
      | (F(Tok(t)), []) when Token.is_empty_module(t) => ret(Module([]))
      | (F(Tok(t)), []) when Token.is_bool(t) =>
        ret(Atom(Bool(bool_of_string(t))))
      | (F(Tok(t)), []) when Token.is_undefined(t) => ret(Undefined)
      | (F(Tok(t)), []) when Token.is_int(t) =>
        set_lexeme(t);
        ret(Atom(Int(Bigint.of_string(t))));
      | (F(Tok(t)), []) when Token.is_string(t) =>
        ret(Atom(String(Token.strip_quotes(t))))
      | (F(Tok(t)), []) when Token.is_quoted_label(t) =>
        set_lexeme(t);
        ret(Label(Token.strip_quotes(~quote=Token.label_delim, t)));
      | (F(Tok(t)), []) when Token.is_float(t) =>
        set_lexeme(t);
        ret(Atom(Float(float_of_string(t))));
      | (F(Tok(t)), []) when Token.is_livelit(t) =>
        ret(LivelitName(Token.parse_livelit(t)))
      | (F(Tok(t)), []) when Token.is_var(t) => ret(Var(t))
      | (F(Tok(t)), []) when Token.is_ctr(t) => ret(Constructor(t, None))
      | (F(Compound(ModBody)), [Mod(body)]) =>
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
      | (F(Compound(ModBody)), [Exp(body)]) => ret(Parens(body))
      | (F(Compound(Parens)), [Exp(body)]) => ret(Parens(body))
      | (F(Compound(Parens)), [kid]) =>
        /* Cross-sort parens (orphan-closer completion): the kid parses
           at the tile's sort, not Exp; hole() would drop the tile and
           strand its shard mask. */
        ret(Parens(derived_hole(Exp.hole, kid)))
      | (ProjWrap, [Exp(body)]) => ret(body.term)
      | (F(Compound(ListLit)), [kid]) when !is_exp_kid(kid) =>
        /* Cross-sort list brackets (see the cross-sort parens case) */
        ret(ListLit([derived_hole(Exp.hole, kid)]))
      | (F(Compound(ListLit)), [Exp(body)]) =>
        /* ListLit absorption: inner Tuple's comma IDs become part of ListLit.
           ID order: [bracket_id] @ comma_ids (outer first, then adopted).
           IMPORTANT: Must align with ExpToSegment.exp_to_pretty ListLit case,
           which expects List.hd = bracket, List.tl = commas. */
        switch (body) {
        | {term: Tuple([{term: TupLabel(_), _}]), _} =>
          /* Single labeled element: body IS the synthesized singleton
             wrapper carrying the = tile's id — keep it whole so the
             printer's singleton-unwrap restores the id (adopting its
             ids as comma ids would strand them: a one-element list
             has no commas) */
          ret(ListLit([body]))
        | {term: Tuple([]), _} =>
          /* Unit literal body: [()] is a one-element list of unit, not
             an empty element list (#1792) */
          ret(ListLit([body]))
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
                    rewrap(
                      Tuple([
                        derived(
                          ~salt="tuplabel-item",
                          list_item.annotation.ids,
                          e,
                        ),
                      ]): TermBase.exp_term,
                    )
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
      | (F(Compound(Test)), [Exp(test)]) => ret(Test(test))
      | (F(Compound(ProofObject)), [Exp(proof)]) =>
        ret(ProofObject(proof))
      | (F(Compound(HintedTest)), [Exp(hint), Exp(test)]) =>
        ret(HintedTest(test, hint))
      | (F(Compound(Case)), [Rul({term, annotation: {ids, _}})]) =>
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
      | (F(Compound(OfJdmt)), [Drv(Exp(j))]) =>
        ret(DrvQuote(Exp(j), Jdmt))
      | (F(Compound(OfCtx)), [Drv(Exp(c))]) =>
        ret(DrvQuote(Exp(c), Ctx))
      | (F(Compound(OfProp)), [Drv(Exp(p))]) =>
        ret(DrvQuote(Exp(p), Prop))
      | (F(Compound(OfAlfaExp)), [Drv(Exp(e))]) =>
        ret(DrvQuote(Exp(e), Exp))
      | (F(Compound(OfAlfaTyp)), [Drv(Typ(t))]) =>
        ret(DrvQuote(Typ(t), Typ))
      | (F(Compound(OfAlfaPat)), [Drv(Pat(p))]) =>
        ret(DrvQuote(Pat(p), Pat))
      | (F(Compound(OfAlfaTPat)), [Drv(TPat(tp))]) =>
        ret(DrvQuote(TPat(tp), TPat))
      | (F(Tok(t)), []) when is_hole_label(t) =>
        set_lexeme(t);
        ret(hole(tm));
      | (F(Tok(t)), []) when t != " " && !Token.is_explicit_hole(t) =>
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
        | (F(Compound(UnaryMinus)), []) => UnOp(Int(Minus), r)
        | (F(Compound(Not)), []) => UnOp(Bool(Not), r)
        | (F(Compound(Fun)), [Pat(pat)]) => Fun(pat, r, None, None)
        | (F(Compound(Forall)), [Pat(pat)]) => Forall(pat, r)
        | (F(Compound(Fix)), [Pat(pat)]) => FixF(pat, r, None)
        | (F(Compound(TypFun)), [TPat(tpat)]) => TypFun(tpat, r, None)
        | (F(Compound(Let)), [Pat(pat), Exp(def)]) => Let(pat, def, r)
        | (F(Compound(ModuleExp)), [MPat(mp), Exp(def)]) =>
          ModuleExp(mp, def, r)
        | (F(Compound(Theorem)), [Pat(pat), Exp(thm)]) =>
          Theorem(pat, thm, r)
        | (F(Compound(FilterHide)), [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Eval, One),
              pat: filter,
            }),
            r,
          )
        | (F(Compound(FilterEval)), [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Eval, All),
              pat: filter,
            }),
            r,
          )
        | (F(Compound(FilterPause)), [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Step, One),
              pat: filter,
            }),
            r,
          )
        | (F(Compound(FilterDebug)), [Exp(filter)]) =>
          Filter(
            Filter({
              act: (Step, All),
              pat: filter,
            }),
            r,
          )
        | (F(Compound(Use)), [Typ(ty)]) => Use(ty, r)
        | (F(Compound(TypeAlias)), [TPat(tpat), Typ(def)]) =>
          TyAlias(tpat, def, r)
        | (F(Compound(If)), [Exp(cond), Exp(conseq)]) =>
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
      | (F(f), []) when is_empty_tuple_form(f) =>
        ret(
          Ap(
            Forward,
            l,
            {
              /* Flag id is a shared sentinel, not a tile id — looking it
                 up in the secondary map would fetch unrelated runs */
              annotation: IdTagged.IdTag.mk_internal([Id.nullary_ap_flag]),
              term: Tuple([]),
            },
          ),
        )
      | (F(Compound(Ap)), [Exp(arg)]) =>
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
      | (F(Compound(ApExpTyp)), [Typ(ty)]) => ret(TypAp(l, ty))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Exp(l), tiles, Typ(r)) as tm =>
    switch (tiles) {
    | ([(_id, (F(Compound(TypeAsc)), []))], []) => ret(Asc(l, r))
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
      | ([(_id, (op, []))], []) =>
        ret(
          switch (op) {
          | F(Compound(Plus)) => BinOp(Int(Plus), l, r)
          | F(Compound(Minus)) => BinOp(Int(Minus), l, r)
          | F(Compound(Times)) => BinOp(Int(Times), l, r)
          | F(Compound(Power)) => BinOp(Int(Power), l, r)
          | F(Compound(Divide)) => BinOp(Int(Divide), l, r)
          | F(Compound(Lt)) => BinOp(Int(LessThan), l, r)
          | F(Compound(Gt)) => BinOp(Int(GreaterThan), l, r)
          | F(Compound(Lte)) => BinOp(Int(LessThanOrEqual), l, r)
          | F(Compound(Gte)) => BinOp(Int(GreaterThanOrEqual), l, r)
          | F(Compound(Equals)) => BinOp(Poly(Equals), l, r)
          | F(Compound(NotEquals)) => BinOp(Poly(NotEquals), l, r)
          | F(Compound(FPlus)) => BinOp(Float(Plus), l, r)
          | F(Compound(FMinus)) => BinOp(Float(Minus), l, r)
          | F(Compound(FTimes)) => BinOp(Float(Times), l, r)
          | F(Compound(FDivide)) => BinOp(Float(Divide), l, r)
          | F(Compound(FPower)) => BinOp(Float(Power), l, r)
          | F(Compound(FLt)) => BinOp(Float(LessThan), l, r)
          | F(Compound(FGt)) => BinOp(Float(GreaterThan), l, r)
          | F(Compound(FLte)) => BinOp(Float(LessThanOrEqual), l, r)
          | F(Compound(FGte)) => BinOp(Float(GreaterThanOrEqual), l, r)
          | F(Compound(FEquals)) => BinOp(Float(Equals), l, r)
          | F(Compound(FNotEquals)) => BinOp(Float(NotEquals), l, r)
          | F(Compound(LogicalAnd)) => BinOp(Bool(And), l, r)
          | F(Compound(LogicalOr)) => BinOp(Bool(Or), l, r)
          | F(Compound(Cons)) => Cons(l, r)
          | F(Compound(CellJoin)) => Seq(l, r)
          | F(Compound(StringConcat)) => BinOp(String(Concat), l, r)
          | F(Compound(TupleExtension)) => TupleExtension(l, r)
          | F(Compound(TupleLabeled)) =>
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
              /* Wrapper shares the inner term's ids but no secondary
                 (the inner term's own wrap emits it); a fresh inner
                 would churn ids and drop secondary on every roundtrip */
              TupLabel(
                {
                  annotation: IdTagged.IdTag.mk_internal(IdTagged.ids(l)),
                  term: MultiHole([Exp(l)]),
                },
                r,
              )
            }
          | F(Compound(Dot)) =>
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
              /* See the TupLabel case above */
              Dot(
                l,
                {
                  annotation: IdTagged.IdTag.mk_internal(IdTagged.ids(r)),
                  term: MultiHole([Exp(r)]),
                },
              )
            }
          | F(Compound(Pipeline)) => Ap(Reverse, r, l)
          | F(Compound(ListConcat)) => ListConcat(l, r)
          | F(f) when op_lexeme(f) != None =>
            /* Unknown infix operator: kids survive as a MultiHole and
               the operator token as its lexeme (printed back by
               ExpToSegment; elaborated as a stuck application). The
               guard matters: grout arrives as the pseudo-token " ",
               and juxtaposition must NOT carry a lexeme — the
               elaborator uses its presence to distinguish stuck
               applications from transient juxtaposition. */
            Option.iter(set_lexeme, op_lexeme(f));
            hole(tm);
          | _ => hole(tm)
          },
        )
      | _ => ret(hole(tm))
      }
    }
  | Bin(_, ([(_id, (F(f), []))], []), _) as tm when op_lexeme(f) != None => {
      /* Unknown infix operator between mixed-sort kids (e.g. the : in
         `? : ? t ?` parses as Bin(Exp, [:], Typ) in a typ context, so
         the same-sort Bin patterns above cannot match) */

      Option.iter(set_lexeme, op_lexeme(f));
      ret(hole(tm));
    }
  | Pre(([(_id, (F(f), []))], []), _) as tm when op_lexeme(f) != None => {
      /* Stranded prefix op; printed back by the 1-kid MultiHole op
         branches */
      Option.iter(set_lexeme, op_lexeme(f));
      ret(hole(tm));
    }
  | tm => ret(hole(tm));
}
and pat = unsorted => {
  let (term, inner_ids) = pat_term(unsorted);
  let lexeme = take_lexeme();
  let ids = ids(unsorted) @ inner_ids;

  let p = return(p => Pat(p), ~lexeme, ids, term);
  switch (term) {
  | TupLabel(_) => derived(~salt="tuplabel-pat", ids, Tuple([p]): Pat.term)
  | _ => p
  };
}
and pat_term: unsorted => (Pat.term, list(Id.t)) = {
  let ret = (term: Pat.term) => (term, []);
  let hole = unsorted => Pat.hole(kids_of_unsorted(unsorted));
  fun
  | tm when all_exp_only(~host=Sort.Pat, tm) =>
    /* see typ_term: exp-only forms stranded in pattern context */
    ret(MultiHole([Exp(exp(tm))]))
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (F(f), []) when is_empty_tuple_form(f) => ret(Tuple([]))
      | (F(Tok(t)), []) when Token.is_empty_list(t) => ret(ListLit([]))
      | (F(Tok(t)), []) when Token.is_bool(t) =>
        ret(Atom(Bool(bool_of_string(t))))
      | (F(Tok(t)), []) when Token.is_float(t) =>
        set_lexeme(t);
        ret(Atom(Float(float_of_string(t))));
      | (F(Tok(t)), []) when Token.is_int(t) =>
        set_lexeme(t);
        ret(Atom(Int(Bigint.of_string(t))));
      | (F(Tok(t)), []) when Token.is_string(t) =>
        ret(Atom(String(Token.strip_quotes(t))))
      | (F(Tok(t)), []) when Token.is_quoted_label(t) =>
        set_lexeme(t);
        ret(Label(Token.strip_quotes(~quote=Token.label_delim, t)));
      | (F(Tok(t)), []) when Token.is_var(t) => ret(Var(t))
      | (F(Tok(t)), []) when Token.is_wild(t) => ret(Wild)
      | (F(Tok(t)), []) when Token.is_ctr(t) => ret(Constructor(t, None))
      | (F(Compound(Parens)), [Pat(body)]) => ret(Parens(body))
      | (F(Compound(Parens)), [kid]) =>
        /* Cross-sort parens (see the exp case) */
        ret(Parens(derived_hole(Pat.hole, kid)))
      | (ProjWrap, [Pat(body)]) => ret(body.term)
      | (F(Compound(ListLit)), [kid]) when !is_pat_kid(kid) =>
        /* Cross-sort list brackets (see the cross-sort parens case) */
        ret(ListLit([derived_hole(Pat.hole, kid)]))
      | (F(Compound(ListLit)), [Pat(body)]) =>
        /* ListLit pattern absorption: inner Tuple's comma IDs become part of ListLit.
           ID order: [bracket_id] @ comma_ids (outer first, then adopted).
           IMPORTANT: Must align with ExpToSegment.pat_to_pretty ListLit case,
           which expects List.hd = bracket, List.tl = commas. */
        switch (body) {
        | {term: Tuple([]), _} =>
          /* Unit literal body: [()] is a one-element list of unit, not
             an empty element list (#1792) */
          ret(ListLit([body]))
        | {term: Tuple(ps), annotation: {ids, _}} =>
          adopted_ids := ids @ adopted_ids^;
          (ListLit(ps), ids);
        | term => ret(ListLit([term]))
        }
      | (F(Tok(t)), []) when is_hole_label(t) =>
        set_lexeme(t);
        ret(hole(tm));
      | (F(Tok(t)), []) => ret(Invalid(t))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Post(Pat(l), tiles) as tm =>
    switch (tiles) {
    | ([(_id, t)], []) =>
      ret(
        switch (t) {
        | (F(f), []) when is_empty_tuple_form(f) =>
          Ap(
            l,
            {
              annotation: {
                ids: [Id.nullary_ap_flag],
                secondary: ([], []),
                incomplete: [],
                lexeme: None,
              },
              term: Tuple([]),
            },
          )
        | (F(Compound(Ap)), [Pat(arg)]) => Ap(l, arg)
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | Bin(Pat(p), tiles, Typ(ty)) as tm =>
    switch (tiles) {
    | ([(_id, (F(Compound(TypeAsc)), []))], []) => ret(Asc(p, ty))
    | ([(_id, (F(f), []))], []) when op_lexeme(f) != None =>
      /* Unknown infix operator (see the exp Bin fallthrough) */
      Option.iter(set_lexeme, op_lexeme(f));
      ret(hole(tm));
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
      | ([(_id, (F(Compound(TupleLabeled)), []))], []) =>
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
              rewrap(
                MultiHole([
                  Pat(derived(~salt="label-hole", l.annotation.ids, e_term)),
                ]): Pat.term,
              ),
              r,
            ),
          );
        }
      | ([(_id, (F(Compound(Cons)), []))], []) => ret(Cons(l, r))
      | ([(_id, (F(f), []))], []) when op_lexeme(f) != None =>
        /* Unknown infix operator (see the exp Bin fallthrough) */
        Option.iter(set_lexeme, op_lexeme(f));
        ret(hole(tm));
      | _ => ret(hole(tm))
      }
    }
  | Pre(tiles, Pat(r)) as tm =>
    switch (tiles) {
    | ([(_id, (F(Compound(UnaryMinus)), []))], []) =>
      /* Negative literal patterns: the pattern grammar has no unary ops,
         and a matched value is a plain negative atom, so fold the minus
         into the literal. The float fold is exact: IEEE negation only
         flips the sign bit and correctly-rounded decimal conversion
         commutes with sign, so -. parse(s) == parse("-" ++ s). The
         literal's ids are adopted, as in ListLit absorption. Non-literal
         operands (`-x`) stay holes carrying the minus as their lexeme. */
      switch (r) {
      | {term: Atom(Int(n)), annotation: {ids, _}} =>
        adopted_ids := ids @ adopted_ids^;
        (Atom(Int(Bigint.neg(n))), ids);
      | {term: Atom(Float(f)), annotation: {ids, _}} =>
        adopted_ids := ids @ adopted_ids^;
        (Atom(Float(-. f)), ids);
      | _ =>
        set_lexeme("-");
        ret(hole(tm));
      }
    | ([(_id, (F(f), []))], []) when op_lexeme(f) != None =>
      /* Stranded prefix op; printed back by the 1-kid MultiHole op
         branches */
      Option.iter(set_lexeme, op_lexeme(f));
      ret(hole(tm));
    | _ => ret(hole(tm))
    }
  | Bin(_, ([(_id, (F(f), []))], []), _) as tm when op_lexeme(f) != None => {
      /* Unknown infix operator between mixed-sort kids (e.g. the : in
         `? : ? t ?` parses as Bin(Exp, [:], Typ) in a typ context, so
         the same-sort Bin patterns above cannot match) */

      Option.iter(set_lexeme, op_lexeme(f));
      ret(hole(tm));
    }
  | Pre(([(_id, (F(f), []))], []), _) as tm when op_lexeme(f) != None => {
      /* Stranded prefix op; printed back by the 1-kid MultiHole op
         branches */
      Option.iter(set_lexeme, op_lexeme(f));
      ret(hole(tm));
    }
  | tm => ret(hole(tm));
}
and typ = unsorted => {
  let (term, inner_ids) = typ_term(unsorted);
  let lexeme = take_lexeme();
  let ids = ids(unsorted) @ inner_ids;
  let t = return(ty => Typ(ty), ~lexeme, ids, term);
  switch (term) {
  | TupLabel(_) => derived(~salt="tuplabel-typ", ids, Prod([t]): Typ.term)
  | _ => t
  };
}
and typ_term: unsorted => (Typ.term, list(Id.t)) = {
  let ret = (term: Typ.term) => (term, []);
  let hole = unsorted => Typ.hole(kids_of_unsorted(unsorted));
  fun
  | tm when all_exp_only(~host=Sort.Typ, tm) =>
    /* exp-only forms stranded in type context (`? : if ? then ? else
       ?` mid-edit) parse as exp inside a hole wrapper, keeping their
       tiles, ids, and shard masks printable */
    ret(Unknown(Hole(MultiHole([Exp(exp(tm))]))))
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, (F(Compound(ModBody)), [Sig(body)]))], []) =>
      /* SigBody: parse signature body, similar to ModBody in exp_term */
      switch (body) {
      | {term: EmptyHole, _} => ret(Sig([]))
      | {annotation: {ids, _}, term: MultiHole(_)} =>
        adopted_ids := ids @ adopted_ids^;
        (Sig(flatten_sig(body)), ids);
      | _ => ret(Sig(flatten_sig(body)))
      }
    | ([(_id, tile)], []) =>
      /* Atomic type spellings (base atoms, Void, Drv quote types):
       * table-driven via BaseAtom (see Language.BaseAtom.table) */
      let base_atom =
        switch (tile) {
        | (F(Tok(t)), []) => Language.BaseAtom.typ_term_of(t)
        | _ => None
        };
      ret(
        switch (base_atom, tile) {
        | (_, (F(f), [])) when is_empty_tuple_form(f) => Prod([])
        | (_, (F(Tok(t)), [])) when Token.is_empty_module(t) => Sig([])
        | (Some(term), _) => term
        | (_, (F(Tok("_")), [])) => ExplicitNonlabel
        | (_, (F(Compound(ProofOf)), [Exp(exp)])) => ProofOf(exp)
        | (_, (F(Tok(t)), [])) when Token.is_typ_var(t) => Var(t)
        | (_, (F(Tok(t)), [])) when Token.is_quoted_label(t) =>
          set_lexeme(t);
          Label(Token.strip_quotes(~quote=Token.label_delim, t));
        | (_, (F(Compound(Parens)), [Typ(body)])) => Parens(body)
        | (_, (F(Compound(Parens)), [kid])) =>
          /* Cross-sort parens (see the exp case) */
          Parens(derived_hole(Typ.hole, kid))
        | (_, (ProjWrap, [Typ(body)])) => body.term
        | (_, (F(Compound(ListLit)), [Typ(body)])) => List(body)
        | (_, (F(Compound(ListLit)), [kid])) =>
          /* Cross-sort list brackets (see the cross-sort parens case) */
          List(derived_hole(Typ.hole, kid))
        | (_, (F(Tok(t)), [])) when is_hole_label(t) =>
          set_lexeme(t);
          hole(tm);
        | (_, (F(Tok(t)), [])) => Unknown(Hole(Invalid(t)))
        | _ => hole(tm)
        },
      );
    | _ => ret(hole(tm))
    }
  | Post(l, tiles) as tm =>
    switch (tiles) {
    /* Type aps which would otherwise be parsed here are recognized in sum type parsing above */
    | ([(id, (F(Compound(Parens | Ap)), [kid]))], [])
        when get_incomplete([id]) != [] || !is_ctr_headed(l) =>
      /* Keep the parens tile as a juxtaposed kid so it reprints
         (hole() would drop it) — except for constructor-headed
         post-parens (`A(Int)`), which must fall through bare: sum-type
         parsing reads the MultiHole shape for ctor aps, and a Parens
         node there prints as A((Int)). */
      let t =
        switch (l) {
        | Typ(t) => t
        | _ => derived_hole(Typ.hole, l)
        };

      let arg =
        switch (kid) {
        | Typ(arg) => arg
        | _ => derived_hole(Typ.hole, kid)
        };
      ret(
        Unknown(
          Hole(
            MultiHole([
              Typ(t),
              Typ({
                annotation: {
                  ...IdTagged.IdTag.mk_internal([id]),
                  incomplete: get_incomplete([id]),
                },
                term: Parens(arg),
              }),
            ]),
          ),
        ),
      );
    | _ => ret(hole(tm))
    }
  /* poly and rec have to be before sum so that they bind tighter.
   * Thus `rec A -> Left(A) + Right(B)` get parsed as `rec A -> (Left(A) + Right(B))`
   * If this is below the case for sum, then it gets parsed as an invalid form. */
  | Pre(([(_id, (F(Compound(Poly)), [TPat(tpat)]))], []), Typ(t)) =>
    ret(Poly(tpat, t))
  | Pre(([(_id, (F(Compound(Rec)), [TPat(tpat)]))], []), Typ(t)) =>
    ret(Rec(tpat, t))
  | Pre(tiles, Typ({term: Sum(t0), annotation: {ids, _}})) as tm =>
    /* Case for leading prefix + preceeding a sum */
    switch (tiles) {
    | ([(_, (F(Compound(SumSingle)), []))], []) =>
      adopted_ids := ids @ adopted_ids^;
      (Sum(t0), ids);
    | _ => ret(hole(tm))
    }
  | Pre(tiles, Typ(t)) as tm =>
    switch (tiles) {
    | ([(_, (F(Compound(SumSingle)), []))], []) =>
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
      | ([(_id, (F(Compound(TypeArrow)), []))], []) => ret(Arrow(l, r))
      | ([(_id, (F(Compound(TupleLabeled)), []))], []) =>
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
      | ([(_id, (F(Compound(Dot)), []))], []) =>
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
      | ([(_id, (F(Compound(TupleExtension)), []))], []) =>
        ret(ProdExtension(l, r))
      | ([(_id, (F(f), []))], []) when op_lexeme(f) != None =>
        /* Unknown infix operator (see the exp Bin fallthrough) */
        Option.iter(set_lexeme, op_lexeme(f));
        ret(hole(tm));
      | _ => ret(hole(tm))
      }
    }
  | Bin(_, ([(_id, (F(f), []))], []), _) as tm when op_lexeme(f) != None => {
      /* Unknown infix operator between mixed-sort kids (e.g. the : in
         `? : ? t ?` parses as Bin(Exp, [:], Typ) in a typ context, so
         the same-sort Bin patterns above cannot match) */

      Option.iter(set_lexeme, op_lexeme(f));
      ret(hole(tm));
    }
  | Pre(([(_id, (F(f), []))], []), _) as tm when op_lexeme(f) != None => {
      /* Stranded prefix op; printed back by the 1-kid MultiHole op
         branches */
      Option.iter(set_lexeme, op_lexeme(f));
      ret(hole(tm));
    }
  | tm => ret(hole(tm));
}
and tpat = unsorted => {
  let term = tpat_term(unsorted);
  let lexeme = take_lexeme();
  let ids = ids(unsorted);
  return(ty => TPat(ty), ~lexeme, ids, term);
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
        | (F(Tok(t)), []) when Token.is_typ_var(t) => Var(t)
        | (F(Tok(t)), []) when is_hole_label(t) =>
          set_lexeme(t);
          hole(tm);
        | (F(Tok(t)), []) => Invalid(t)
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
  let lexeme = take_lexeme();
  let ids = ids(unsorted);
  return(m => Mod(m), ~lexeme, ids, term);
}
and mod_term: unsorted => TermBase.Mod.term = {
  let ret = (term: TermBase.Mod.term) => term;
  let hole = unsorted => Mod.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (F(Tok(t)), []) when is_hole_label(t) =>
        set_lexeme(t);
        ret(hole(tm));
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
  | Pre(([(_id, (F(Compound(ModLet)), [Pat(p)]))], []), Exp(e)) =>
    ret(ModLet(p, e))
  /* ModuleMod: module M = e - MPat inside tile, expression is the body */
  | Pre(([(_id, (F(Compound(ModuleMod)), [MPat(mp)]))], []), Exp(e)) =>
    ret(ModuleMod(mp, e))
  /* ModType: type t = T - the tpat is inside the tile, type is the body */
  | Pre(([(_id, (F(Compound(ModType)), [TPat(tp)]))], []), Typ(ty)) =>
    ret(ModType(tp, ty))
  /* Expression-level structures (binary ops, prefix, postfix) - wrap as ModExp */
  | Bin(Exp(_), _, Exp(_)) as tm => ret(ModExp(exp(tm)))
  | Pre(_, Exp(_)) as tm => ret(ModExp(exp(tm)))
  | Post(Exp(_), _) as tm => ret(ModExp(exp(tm)))
  | (Pre(_) | Post(_) | Bin(_)) as tm => ret(hole(tm));
}
and sig_ = unsorted => {
  let term = sig_term(unsorted);
  let lexeme = take_lexeme();
  let ids = ids(unsorted);
  return(s => Sig(s), ~lexeme, ids, term);
}
and sig_term: unsorted => TermBase.Sig.term = {
  let ret = (term: TermBase.Sig.term) => term;
  let hole = unsorted => Sig.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, tile)], []) =>
      switch (tile) {
      | (F(Tok(t)), []) when is_hole_label(t) =>
        set_lexeme(t);
        ret(hole(tm));
      | (F(Tok(t)), []) => ret(Invalid(t))
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
  | Pre(([(_id, (F(Compound(SigLet)), []))], []), Pat(p)) =>
    ret(SigLet(p))
  /* SigType: type t = T - the tpat is inside the tile, type is the body */
  | Pre(([(_id, (F(Compound(ModType)), [TPat(tp)]))], []), Typ(ty)) =>
    ret(SigType(tp, ty))
  | (Pre(_) | Post(_) | Bin(_)) as tm => ret(hole(tm));
}
and mpat = unsorted => {
  let term = mpat_term(unsorted);
  let lexeme = take_lexeme();
  let ids = ids(unsorted);
  return(mp => MPat(mp), ~lexeme, ids, term);
}
and mpat_term: unsorted => TermBase.MPat.term = {
  let ret = (term: TermBase.MPat.term) => term;
  let hole = unsorted => MPat.hole(kids_of_unsorted(unsorted));
  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_id, (F(Tok(t)), []))], [])
        when Token.is_var(t) || Token.is_ctr(t) =>
      ret(Var(t))
    | ([(_id, (F(Tok(t)), []))], []) when is_hole_label(t) =>
      set_lexeme(t);
      ret(hole(tm));
    | ([(_id, (F(Tok(t)), []))], []) => ret(Invalid(t))
    | _ => ret(hole(tm))
    }
  | Bin(MPat(mp), tiles, Typ(ty)) as tm =>
    switch (tiles) {
    | ([(_id, (F(Compound(TypeAsc)), []))], []) => ret(Asc(mp, ty))
    | _ => ret(hole(tm))
    }
  | (Pre(_) | Post(_) | Bin(_)) as tm => ret(hole(tm));
}

and rul = (unsorted): Rul.t => {
  let e = exp(unsorted);
  let mk_rules = (scrut: Exp.t, rules, ids): Rul.t => {
    term: Rules(scrut, rules),
    annotation:
      IdTagged.IdTag.mk(
        ~incomplete=get_incomplete(ids),
        ids,
        get_secondary(ids),
      ),
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
      let inner = go_s(sort, skel_of(~sort=Exp, seg), seg);
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
            annotation:
              IdTagged.IdTag.mk(
                ~incomplete=get_incomplete([id]),
                [id],
                get_secondary([id]),
              ),
          })
        | Grammar.Pat(p) =>
          Grammar.Pat({
            term: Projector(projector_data, p),
            annotation:
              IdTagged.IdTag.mk(
                ~incomplete=get_incomplete([id]),
                [id],
                get_secondary([id]),
              ),
          })
        | Grammar.Typ(t) =>
          Grammar.Typ({
            term: Projector(projector_data, t),
            annotation:
              IdTagged.IdTag.mk(
                ~incomplete=get_incomplete([id]),
                [id],
                get_secondary([id]),
              ),
          })
        | _ => inner
        };
      [wrapped];
    | Tile({id, _}) when Id.Map.mem(id, body_hooks^) =>
      let (members, plist) = Id.Map.find(id, body_hooks^);
      projector_list := plist @ projector_list^;
      [members];
    | Tile({shards, children, _} as t) =>
      Aba.aba_triples(Aba.mk(shards, children))
      |> List.map(((l, kid, r)) => {
           let s = l + 1 == r ? List.nth(Tile.mold(t).in_, l) : Sort.Any;
           go_s(s, skel_of(~sort=s, kid), kid);
         })
    };

  /* Capture term ranges */
  record_term_data(sort, seg, skel);

  let arr = pieces_of(seg);
  let root: Aba.t(Piece.t, Skel.t) =
    Skel.root(skel) |> Aba.map_a(i => arr[i]);

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
let reconsolidate =
    (adopted: list(Id.t), m: TermMap.t, td0: TermData.t): TermData.t =>
  List.fold_left(
    (td, id) =>
      switch (Id.Map.find_opt(id, m)) {
      | None => td
      | Some(term) =>
        let rep = Language.Any.rep_id(term);
        switch (Id.Map.find_opt(rep, td), Id.Map.find_opt(id, td)) {
        | (Some(rep_data), Some(old_data)) =>
          Id.Map.add(
            id,
            TermData.{
              ...rep_data,
              root_piece: old_data.root_piece,
              sort: old_data.sort,
            },
            td,
          )
        | (Some(rep_data), None) => Id.Map.add(id, rep_data, td)
        | (None, _) => td
        };
      },
    td0,
    adopted,
  );

let consolidate_adopted = (): unit =>
  term_data := reconsolidate(adopted_ids^, map^, term_data^);

/* every parse runs inside [fresh]: accumulators start empty; masks and
 * any stray lexeme are cleared on exit, even on a raise, while the
 * accumulators stay readable */
let fresh =
    (
      ~masks: Id.Map.t(IdTagged.IdTag.incomplete_mask)=Id.Map.empty,
      ~hooks: Id.Map.t((Any.t, list(Id.t)))=Id.Map.empty,
      ~skel=?,
      seg: Segment.t,
      parse: unit => 'a,
    )
    : 'a => {
  map := TermMap.empty;
  term_data := Id.Map.empty;
  projectors := Id.Map.empty;
  projector_list := [];
  adopted_ids := [];
  levels := Hashtbl.create(16);
  body_hooks := hooks;
  secondary_map :=
    Segment.SecondaryCollection.collect(
      ~levels=levels^,
      ~skip=id => Id.Map.mem(id, hooks),
      ~skel?,
      seg,
    );
  shard_masks := masks;
  pending_lexeme := None;
  Fun.protect(
    ~finally=
      () => {
        shard_masks := Id.Map.empty;
        pending_lexeme := None;
        body_hooks := Id.Map.empty;
      },
    parse,
  );
};

/* Unmemoized parse. ~masks carries shard provenance from canonical
 * completion (tile id -> originally-present shard indices); it cannot be
 * folded into a segment-keyed memo because the same completed segment can
 * arise from different visible segments with different masks. */
let go_impl =
    (~masks: Id.Map.t(IdTagged.IdTag.incomplete_mask)=Id.Map.empty, seg) =>
  fresh(
    ~masks,
    seg,
    () => {
      let term = exp(unsorted(Exp, skel_of(~sort=Exp, seg), seg));
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

let go =
  /* small bound: each key pins a whole segment and its term/term_data,
     and every edit mints a new segment; a frame needs only a few */
  Core.Memo.general(~cache_size_bound=8, go_impl(~masks=Id.Map.empty));

/* a Mod-rooted editor's Module wrapper: statics/elab reuse and DefStatics'
   per-document slot key on its id, so it is derived from the first item's
   (stable per document, distinct across documents) */
let empty_mod_wrap_id: Id.t = Id.mk();
let wrap_module = (items: list(Mod.t)): Exp.t => {
  let id =
    switch (items) {
    | [first, ..._] => Id.derive(~salt="module-root", Mod.rep_id(first))
    | [] => empty_mod_wrap_id
    };
  IdTagged.fast_copy(id, Exp.fresh(Module(items)));
};

/* monolithic parse of a Mod-rooted segment, which [go] would misparse at
   Exp: the parity reference and go_incr's fallback */
let go_mod_root_impl =
    (~masks: Id.Map.t(IdTagged.IdTag.incomplete_mask)=Id.Map.empty, seg) =>
  fresh(
    ~masks,
    seg,
    () => {
      let items =
        switch (go_s(Sort.Mod, skel_of(~sort=Mod, seg), seg)) {
        | Mod(m) => flatten_mod(m)
        | _ => []
        };
      consolidate_adopted();
      let term = wrap_module(items);
      {
        term,
        term_data: term_data^,
        terms: TermMap.add_all(term.annotation.ids, Exp(term), map^),
        projectors: projectors^,
        projector_list: projector_list^,
      };
    },
  );

let go_mod_root =
  Core.Memo.general(
    ~cache_size_bound=8,
    go_mod_root_impl(~masks=Id.Map.empty),
  );

let for_projection =
  /* Returns Nul() unless segment represents a well-structured term in isolation.
   * This means that the term is complete, modulo non-empty holes and sort errors.
   * Specifically, it ensures there are no incomplete tiles in the segment, and
   * that no contained sub-segment is non-convex. However, there can still be convex
   * holes, singleton multiholes representing sort errors, non-singleton multiholes
   * representing missing infix operators, and invalid tokens. */
  Core.Memo.general(~cache_size_bound=8, (seg: Segment.t) =>
    if (!Segment.deep_tile_complete(seg)) {
      None; /* Returns None if any subsegment contains incomplete tiles */
    } else if (Segment.is_padded(seg)) {
      None; /* Returns None if the segment has secondary around it */
    } else {
      switch (Segment.skel(seg)) {
      | exception _ => None /* Returns None if any subsegment is non-convex */
      | skel =>
        let sort = sort_of(skel, seg);
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

/* per-item parsing: each top-level item parses alone (memoized on piece
   identity) and grafts into a chain, as in DefStatics. a nonconvex item
   gets a convex grout body that the graft replaces */
module Incr = {
  /* the top level's items (ItemSpans), each slice keyed by its anchor */
  let keyed = (~root: Sort.t, seg: Segment.t): list((Id.t, Segment.t)) =>
    ItemSpans.keyed(ItemSpans.top(~root, seg));
  let slices = (~root: Sort.t, seg: Segment.t): list(Segment.t) =>
    List.map(snd, keyed(~root, seg));

  let seg_eq = Segment.ptr_eq;
  /* an item's pieces as an entry's: physically, but whitespace an open
     tile reaches is rewrapped each key */
  let same_item = Segment.same_pieces;

  /* last whole-segment term, and the root and division it was read at:
     shared by term_of, term_of_mod and go_incr, so a hit needs all to
     match */
  let last: ref(option((Sort.t, ItemSpans.ends, Segment.t, Exp.t))) =
    ref(None);
  let analyzed: ref(int) = ref(0); /* observability for tests */
  /* per-item parses that gave up for a whole parse; parity tests assert 0 */
  let fell_back = ref(0);

  /* [attempt] on an item's pieces; if they're nonconvex (the item's body
     operand was the next item), on them plus a convex grout standing in,
     whose id is returned for the graft to replace. Derived from the last
     piece, so two parses of the same pieces agree */
  let with_body_grout =
      (attempt: Segment.t => option('a), pieces: Segment.t)
      : option(('a, option(Id.t))) =>
    switch (attempt(pieces)) {
    | Some(r) => Some((r, None))
    | None =>
      let hole_id =
        switch (ListUtil.last_opt(pieces)) {
        | Some(p) => Id.derive(~salt="body-grout", Piece.id(p))
        | None => Id.mk()
        };
      attempt(
        pieces
        @ [
          Piece.Grout({
            id: hole_id,
            shape: Convex,
          }),
        ],
      )
      |> Option.map(r => (r, Some(hole_id)));
    };

  /* collecting secondaries per slice matches the whole-segment
     collection (test-gated): a Pre node owns only its before-run, so no
     secondary run crosses a cut after an `in` tile */
  let parse_item =
      (~masks=Id.Map.empty, ~hooks=Id.Map.empty, pieces: Segment.t)
      : (Exp.t, option(Id.t)) => {
    let attempt = (ps: Segment.t) =>
      switch (Segment.skel(ps)) {
      | skel =>
        Some(
          fresh(~masks, ~hooks, ~skel, ps, () =>
            exp(unsorted(Exp, skel, ps))
          ),
        )
      | exception _ => None
      };
    switch (with_body_grout(attempt, pieces)) {
    | Some(r) => r
    | None => (Exp.fresh(EmptyHole), None) /* degenerate input */
    };
  };

  /* a mod-item slice ending in `;` is nonconvex: the appended grout
     parses to a trailing EmptyHole item, dropped here (the caller scrubs
     its id). mod items have no body, so composition is a plain concat,
     which a `;` parsed inside an item (a stray line above a member) breaks.
     Read Mod-sorted, as go reads a member list: a concave grout (a missing
     `;`) separates members, where the collection's Exp-sorted skel would
     bind it inside the first member's definition. [cut]: the slice is an
     unfinished member cut at its member line, whose open form takes the
     grout */
  let parse_item_mod =
      (
        ~masks=Id.Map.empty,
        ~hooks=Id.Map.empty,
        ~cut=false,
        pieces: Segment.t,
      )
      : (list(Mod.t), option(Id.t)) => {
    let attempt = (ps: Segment.t): option(list(Mod.t)) =>
      switch (Segment.skel(ps)) {
      | skel =>
        fresh(~masks, ~hooks, ~skel, ps, () =>
          switch (go_s(Sort.Mod, skel_of(~sort=Mod, ps), ps)) {
          | Mod(m) => Some(flatten_mod(m))
          | _ => None
          }
        )
      | exception _ => None
      };
    switch (with_body_grout(attempt, pieces)) {
    | Some((items, None)) => (items, None)
    | Some((items, Some(hole_id))) =>
      let is_hole = (m: Mod.t) => List.mem(hole_id, m.annotation.ids);
      switch (ListUtil.split_last_opt(items)) {
      | Some((items, hole)) when is_hole(hole) => (items, Some(hole_id))
      | Some(_) when cut => (items, Some(hole_id))
      | _ => failwith("MakeTerm.Incr: mod item absorbed its `;`")
      };
    | None => ([], None)
    };
  };

  /* [e] with [acc] in its body hole, if the hole is down the
     binding-form spine; [rebuilt] sees each rebuilt node, innermost first */
  let graft_spine =
      (~rebuilt=(e: Exp.t) => e, hole_id: Id.t, acc: Exp.t, e: Exp.t)
      : option(Exp.t) => {
    let rec spine = (e: Exp.t): option(Exp.t) =>
      if (List.mem(hole_id, e.annotation.ids)) {
        Some(acc);
      } else {
        let re = (term: Exp.term) =>
          rebuilt({
            ...e,
            term,
          });
        switch (e.term) {
        | Let(p, d, b) => spine(b) |> Option.map(b => re(Let(p, d, b)))
        | Theorem(p, d, b) =>
          spine(b) |> Option.map(b => re(Theorem(p, d, b)))
        | Use(t, b) => spine(b) |> Option.map(b => re(Use(t, b)))
        | Seq(a, b) => spine(b) |> Option.map(b => re(Seq(a, b)))
        | TyAlias(tp, ty, b) =>
          spine(b) |> Option.map(b => re(TyAlias(tp, ty, b)))
        | ModuleExp(mp, d, b) =>
          spine(b) |> Option.map(b => re(ModuleExp(mp, d, b)))
        | Filter(f, b) => spine(b) |> Option.map(b => re(Filter(f, b)))
        | Parens(b) => spine(b) |> Option.map(b => re(Parens(b)))
        | _ => None
        };
      };
    spine(e);
  };

  /* [e] with [acc] in its synthetic body hole, plus the hole's rebuilt
     ancestors, outermost first (the term map must hold them grafted).
     the binding-form spine first, cheaply; then any Exp position (an
     operand typed above a let, a let under a fun, `if` or operator). a
     lost hole raises: callers parse whole rather than drop what follows */
  let graft_at = (hole_id: Id.t, acc: Exp.t, e: Exp.t): (Exp.t, list(Exp.t)) => {
    let is_hole = (e: Exp.t) => List.mem(hole_id, e.annotation.ids);
    let path = ref([]);
    let rebuilt = (e: Exp.t): Exp.t => {
      path := [e, ...path^];
      e;
    };
    let anywhere = (e: Exp.t): Exp.t => {
      let found = ref(false);
      let keep = (_, x) => x;
      /* off the hole's path, the original node */
      let f_exp = (descend, e: Exp.t) =>
        if (found^) {
          e;
        } else if (is_hole(e)) {
          found := true;
          acc;
        } else {
          let e' = descend(e);
          found^ ? rebuilt(e') : e;
        };
      let e =
        Exp.map_term(
          ~f_exp,
          ~f_pat=keep,
          ~f_typ=keep,
          ~f_tpat=keep,
          ~f_mod=keep,
          ~f_sig=keep,
          ~f_mpat=keep,
          e,
        );
      found^ ? e : failwith("MakeTerm.Incr: body hole not found");
    };
    let e =
      switch (graft_spine(~rebuilt, hole_id, acc, e)) {
      | Some(e) => e
      | None => anywhere(e)
      };
    (e, path^);
  };

  /* grafts each item into its predecessor's body hole; [on_path] sees
     each graft's rebuilt ancestors. a non-last item always has a hole */
  let rec graft_items =
          (~on_path=_ => (), items: list((Exp.t, option(Id.t)))): Exp.t =>
    switch (items) {
    | [] => Exp.fresh(EmptyHole)
    | [(term, _)] => term
    | [(term, hole), ...rest] =>
      let below = graft_items(~on_path, rest);
      switch (hole) {
      | Some(h) =>
        let (term, path) = graft_at(h, below, term);
        on_path(path);
        term;
      | None => failwith("MakeTerm.Incr: inner item without a body hole")
      };
    };

  /* one item's parse. Its maps hold its own bindings: a module literal's
     leave out its members' (their entries hold them), and a member's
     leave out what its level supplies, its `;`'s term (the module's, once
     composed) and its top pieces' TermData (the level's frame) */
  type entry_full = {
    f_pieces: Segment.t,
    f_term: Exp.t, /* Exp mode; EmptyHole placeholder in Mod mode */
    f_hole: option(Id.t),
    f_mods: list(Mod.t), /* Mod mode; [] in Exp mode */
    f_map: TermMap.t,
    f_td: TermData.t,
    f_proj: Id.Map.t(Base.projector),
    /* its projector ids, its members' included, in go's order */
    f_plist: list(Id.t),
    /* ids adopted by a term whose rep is one of the item's top pieces,
       redone against its level's frame: (id, rep, its own TermData) */
    f_adopted: list((Id.t, Id.t, option(TermData.data))),
    /* a member's separators, whose terms its level supplies */
    f_seps: list(Id.t),
    /* a module literal: its brace, and its member level per member */
    f_kids: option((Id.t, list((Id.t, entry_full)))),
  };

  /* what a level's union loses and gains: entries, each by its own
     bindings */
  type delta = {
    gone: list(entry_full),
    come: list(entry_full),
  };

  let rec subtree = (e: entry_full): list(entry_full) => [
    e,
    ...kids_subtree(e),
  ]
  and kids_subtree = (e: entry_full): list(entry_full) =>
    switch (e.f_kids) {
    | Some((_, kids)) => List.concat_map(((_, k)) => subtree(k), kids)
    | None => []
    };

  /* one per editor: the root, division and last build's entries, and its
     PRE-FIXUP unions (every entry's own bindings, members' included), so
     the next build applies a delta instead of re-merging */
  type cache = {
    mutable c_prev:
      option(
        (
          Sort.t,
          ItemSpans.ends,
          list((Id.t, entry_full)),
          TermMap.t,
          TermData.t,
          Id.Map.t(Base.projector),
        ),
      ),
  };
  let mk_cache = (): cache => {c_prev: None};

  /* go_incr: go's full record composed per item. per-item parses differ
     from go only in the frames of the levels cut into items, which
     record_top_frame, fix_spine and the adopted ids' redo replay; parity
     with go is test-gated (Test_MakeTermIncr, Test_ModRoot) */

  let full_analyzed = ref(0); /* observability for tests */

  /* strip the synthetic body hole from captured maps: go never sees
     that grout, so nothing keyed by it may survive into the union */
  let scrub_hole = (hole: option(Id.t), (m, td)) =>
    switch (hole) {
    | None => (m, td)
    | Some(h) => (Id.Map.remove(h, m), Id.Map.remove(h, td))
    };

  /* replays [unsorted]'s sort propagation over a level's skel, recording
     TermData with the WHOLE level as base_seg (tile children are separate
     frames, so only this one differs from go) */
  let record_top_frame =
      (~root: Sort.t=Exp, ~skel=?, td0: TermData.t, seg: Segment.t)
      : TermData.t => {
    let td = ref(td0);
    let arr = Array.of_list(seg);
    let resolve = (s: Sort.t, skel: Skel.t): Sort.t =>
      switch (s) {
      | Any =>
        let so = Piece.sort(arr[Aba.first_a(Skel.root(skel))]) |> fst;
        so == Any ? root : so;
      | Drv(Jdmt | Ctx | Prop | Exp) => Drv(Exp)
      | s => s
      };
    let rec sim = (s: Sort.t, skel: Skel.t): unit => {
      let s = resolve(s, skel);
      let root = Skel.root(skel) |> Aba.map_a(i => arr[i]);
      Aba.get_as(root)
      |> List.iter(p =>
           td := Id.Map.add(Piece.id(p), TermData.mk(p, s, skel, seg), td^)
         );
      Aba.aba_triples(root)
      |> List.iter(((p_l, kid, p_r)) => {
           let (_, s_l) = Piece.nib_sorts(p_l);
           let (s_r, _) = Piece.nib_sorts(p_r);
           sim(s_l == s_r ? s_l : Sort.Any, kid);
         });
      let (l_sort, r_sort) = {
        let p_l = Aba.first_a(root);
        let p_r = Aba.last_a(root);
        let (l, _) = Option.get(Piece.nibs(p_l));
        let (_, r) = Option.get(Piece.nibs(p_r));
        (l.sort, r.sort);
      };
      switch (skel) {
      | Op(_) => ()
      | Pre(_, r) => sim(r_sort, r)
      | Post(l, _) => sim(l_sort, l)
      | Bin(l, _, r) =>
        sim(l_sort, l);
        sim(r_sort, r);
      };
    };
    sim(
      root,
      switch (skel) {
      | Some(skel) => skel
      | None => Segment.skel(~sort=root == Sort.Mod ? Mod : Exp, seg)
      },
    );
    td^;
  };

  /* adopted ids redone against their rep's TermData in [td], keeping
     their own root piece and sort (as reconsolidate does) */
  let refix =
      (adopted: list((Id.t, Id.t, option(TermData.data))), td: TermData.t)
      : TermData.t =>
    List.fold_left(
      (td, (id, rep, own: option(TermData.data))) =>
        switch (Id.Map.find_opt(rep, td)) {
        | Some(rd) =>
          Id.Map.add(
            id,
            switch (own) {
            | Some(o) =>
              TermData.{
                ...rd,
                root_piece: o.root_piece,
                sort: o.sort,
              }
            | None => rd
            },
            td,
          )
        | None => td
        },
      td,
      adopted,
    );

  /* re-add the grafts' rebuilt nodes so the term map holds grafted terms,
     not holed bodies; inner first, as go adds them */
  let fix_spine = (m: TermMap.t, outer_first: list(Exp.t)): TermMap.t =>
    List.fold_left(
      (m, e: Exp.t) => TermMap.add_all(e.annotation.ids, Exp(e), m),
      m,
      List.rev(outer_first),
    );

  /* a member list composed as go_s(Mod, …) composes it: go's tree over
     the members' separators, each node read by mod_ (a `;` chain over Mod
     members; members around a missing `;`'s grout each at its own sort),
     the nodes below the root returned for the level's map. A Read level
     cut at a member line, where no skel splits: a flat MultiHole of the
     members carrying the root's separators */
  exception Recompose;

  type seps =
    | Leaf(Skel.t)
    | Node(list(Piece.t), list(seps));

  let compose_members =
      (~masks, ml: ItemSpans.level, kids: list((Id.t, entry_full)))
      : (Any.t, Skel.t, TermMap.t) => {
    let skel = Segment.skel(~sort=Mod, ml.seg);
    let items = List.concat_map(((_, e)) => e.f_mods, kids);
    let ends = List.concat_map(((_, e)) => e.f_seps, kids);
    let is_sep = (p: Piece.t) =>
      switch (p) {
      | Tile(t) => Tile.is_semi(t) && t.sort == Sort.Mod
      | Grout({shape: Concave, _}) => true
      | _ => false
      };
    let rec tree = (sk: Skel.t): seps =>
      switch (sk) {
      | Bin(l, root, r)
          when List.for_all(i => is_sep(ml.pieces[i]), Aba.get_as(root)) =>
        Node(
          List.map(i => ml.pieces[i], Aba.get_as(root)),
          [tree(l), ...List.map(tree, Aba.get_bs(root))] @ [tree(r)],
        )
      | _ => Leaf(sk)
      };
    /* its leaves, and its separators in order */
    let rec walk = (t: seps): (int, list(Id.t)) =>
      switch (t) {
      | Leaf(_) => (1, [])
      | Node(ps, ks) =>
        let ws = List.map(walk, ks);
        let rec ids = (ws, ps) =>
          switch (ws, ps) {
          | ([(_, i)], []) => i
          | ([(_, i), ...ws], [p, ...ps]) =>
            i @ [Piece.id(p), ...ids(ws, ps)]
          | _ => []
          };
        (List.fold_left((n, (m, _)) => n + m, 0, ws), ids(ws, ps));
      };
    let t = tree(skel);
    let (leaves, tree_ends) = walk(t);
    let mod_items = () => List.map(m => Grammar.Mod(m), items);
    if (leaves == List.length(items) && tree_ends == ends) {
      let rest = ref(items);
      let below = ref(TermMap.empty);
      /* a leaf as go reads it at [sort]: at Any, at its first piece's */
      let rec kid = (sort: Sort.t, t: seps): Any.t =>
        switch (t, rest^) {
        | (Leaf(sk), [m, ...ms]) =>
          rest := ms;
          let first = ml.pieces[Aba.first_a(Skel.root(sk))];
          switch (sort, fst(Piece.sort(first)), m.term) {
          | (Mod, _, _)
          | (_, Mod, _) => Mod(m)
          | (_, Exp | Any, ModExp(e)) => Exp(e)
          | _ => raise(Recompose)
          };
        | (Leaf(_), []) => raise(Recompose)
        | (Node(ps, ks), _) => node(~is_below=true, ps, ks)
        }
      and node = (~is_below: bool, ps, ks): Any.t => {
        let ks =
          List.map(kid(List.for_all(Piece.is_semi, ps) ? Mod : Any), ks);
        let tiles =
          Aba.mk(
            List.map(p => (Piece.id(p), (head(p), [])), ps),
            ListUtil.sublist((1, List.length(ks) - 1), ks),
          );
        let (m, mm) =
          fresh(
            ~masks,
            [],
            () => {
              let m =
                mod_(Bin(List.hd(ks), tiles, ListUtil.last(ks)): unsorted);
              (m, map^);
            },
          );
        if (is_below) {
          below := Id.Map.union((_, a, _) => Some(a), mm, below^);
        };
        Mod(m);
      };
      switch (t) {
      | Leaf(_) => (kid(Mod, t), skel, below^)
      | Node(ps, ks) =>
        let top = node(~is_below=false, ps, ks);
        (top, skel, below^);
      };
    } else if (ml.cut) {
      /* the module adopts what it adopts read whole, so extents stay as
         written; each other separator's term is the member list */
      let root_ids =
        switch (t) {
        | Node(ps, _) => List.map(Piece.id, ps)
        | Leaf(_) => []
        };
      let flat = (ids: list(Id.t)): Any.t =>
        Mod(
          IdTagged.mk(
            ~incomplete=[],
            ids,
            IdTagged.IdTag.empty_secondary,
            MultiHole(mod_items()): TermBase.Mod.term,
          ),
        );
      switch (items) {
      | [] => raise(Recompose)
      | [m] when ends == [] => (Mod(m), skel, TermMap.empty)
      | _ => (
          flat(root_ids),
          skel,
          List.fold_left(
            (map, id) =>
              List.mem(id, root_ids)
                ? map : Id.Map.add(id, flat([id]), map),
            TermMap.empty,
            ends,
          ),
        )
      };
    } else {
      raise(Recompose);
    };
  };

  /* an item's adopted ids whose rep is one of its top pieces, with their
     own TermData; at a member level, its maps without what the level
     supplies (its separators' terms and its top pieces' TermData), and
     those separators */
  let localize =
      (~member: bool, ps: Segment.t, m: TermMap.t, td: TermData.t, adopted) => {
    let tops =
      List.fold_left(
        (s, p) => Piece.is_secondary(p) ? s : Id.Set.add(Piece.id(p), s),
        Id.Set.empty,
        ps,
      );
    let top_adopted =
      List.filter_map(
        id =>
          switch (Id.Map.find_opt(id, m)) {
          | Some(term) =>
            let rep = Language.Any.rep_id(term);
            Id.Set.mem(rep, tops)
              ? Some((id, rep, Id.Map.find_opt(id, td))) : None;
          | None => None
          },
        adopted,
      );
    if (member) {
      let seps = ItemSpans.separators(ps);
      let m = List.fold_left(Fun.flip(Id.Map.remove), m, seps);
      let td = Id.Set.fold(Id.Map.remove, tops, td);
      let td =
        List.fold_left(
          (td, (id, _, _)) => Id.Map.remove(id, td),
          td,
          top_adopted,
        );
      (m, td, top_adopted, seps);
    } else {
      (m, td, top_adopted, []);
    };
  };

  /* a level's entries, reusing [prev]'s by anchor while their pieces are
     physically the same, then [shared]'s (another cache's parses of the
     same pieces); a changed module literal or let chain reuses its items
     likewise. [member]: a level whose frame its parent supplies (module
     members, and a let chain among them); [graft]: one whose spine its
     parent grafts (a let chain); [keyed]: its slices, if not ItemSpans';
     [count] counts parses */
  let rec parse_level =
          (
            ~root: Sort.t,
            ~member: bool,
            ~graft=false,
            ~masks,
            ~count: ref(int),
            ~prev: list((Id.t, entry_full)),
            ~shared: list((Id.t, entry_full)),
            ~keyed=?,
            lv: ItemSpans.level,
          )
          : (list((Id.t, entry_full)), delta) => {
    let table = (es: list((Id.t, entry_full))) => {
      let t = Hashtbl.create(List.length(es) + 1);
      List.iter(((k, e)) => Hashtbl.replace(t, k, e), es);
      t;
    };
    let prev_tbl = table(prev);
    let shared_tbl = table(shared);
    let gone = ref([]);
    let come = ref([]);
    let entries =
      List.mapi(
        (k, (key, ps)) => {
          let p = Hashtbl.find_opt(prev_tbl, key);
          let s = Hashtbl.find_opt(shared_tbl, key);
          switch (p, s) {
          | (Some(e), _) when same_item(e.f_pieces, ps) => (key, e)
          | (_, Some(e)) when same_item(e.f_pieces, ps) =>
            Option.iter(p => gone := subtree(p) @ gone^, p);
            come := subtree(e) @ come^;
            (key, e);
          | _ =>
            let (e, d) =
              parse_item_full(
                ~root,
                ~member,
                ~graft,
                ~masks,
                ~count,
                ~prev=p,
                ~shared=s,
                lv,
                k,
                ps,
              );
            gone := d.gone @ gone^;
            come := d.come @ come^;
            (key, e);
          };
        },
        Option.value(keyed, ~default=ItemSpans.keyed(lv)),
      );
    let cur = table(entries);
    let vanished =
      List.concat_map(
        ((key, e)) => Hashtbl.mem(cur, key) ? [] : subtree(e),
        prev,
      );
    (
      entries,
      {
        gone: vanished @ gone^,
        come: come^,
      },
    );
  }

  /* an item alone; a module literal as its shell around its members, and
     a member that is a let chain as its items, each parsed alone (if they
     won't compose, the item parses whole: a fallback) */
  and parse_item_full =
      (
        ~root: Sort.t,
        ~member: bool,
        ~graft: bool,
        ~masks,
        ~count: ref(int),
        ~prev: option(entry_full),
        ~shared: option(entry_full),
        lv: ItemSpans.level,
        k: int,
        ps: Segment.t,
      )
      : (entry_full, delta) =>
    switch (
      root == Sort.Mod
        ? Option.bind(ItemSpans.chain(lv, k), cl =>
            parse_chain(
              ~member,
              ~masks,
              ~count,
              ~prev,
              ~shared,
              lv,
              k,
              ps,
              cl,
            )
          )
        : None
    ) {
    | Some(r) => r
    | None =>
      parse_whole(
        ~root,
        ~member,
        ~graft,
        ~masks,
        ~count,
        ~prev,
        ~shared,
        lv,
        k,
        ps,
      )
    }

  and parse_whole =
      (
        ~root: Sort.t,
        ~member: bool,
        ~graft: bool,
        ~masks,
        ~count: ref(int),
        ~prev: option(entry_full),
        ~shared: option(entry_full),
        lv: ItemSpans.level,
        k: int,
        ps: Segment.t,
      )
      : (entry_full, delta) => {
    let body =
      k < Array.length(lv.spans)
        ? Option.bind(lv.spans[k].body, brace =>
            Option.map(ml => (brace, ml), ItemSpans.members(lv, k))
          )
        : None;
    /* the shell's hook and frame, and the member level */
    let members =
      switch (body) {
      | None => None
      | Some((brace, ml)) =>
        let pk = kids_of(prev, brace);
        switch (
          {
            let (kids, kd) =
              parse_level(
                ~root=Mod,
                ~member=true,
                ~masks,
                ~count,
                ~prev=Option.value(pk, ~default=[]),
                ~shared=Option.value(kids_of(shared, brace), ~default=[]),
                ml,
              );
            let (term, skel, level_map) = compose_members(~masks, ml, kids);
            let plist =
              List.concat_map(((_, e)) => e.f_plist, List.rev(kids));
            let frame =
              record_top_frame(~root=Mod, ~skel, Id.Map.empty, ml.seg)
              |> refix(List.concat_map(((_, e)) => e.f_adopted, kids));
            (brace, kids, kd, term, plist, (frame, level_map), pk != None);
          }
        ) {
        | r => Some(r)
        | exception _ =>
          incr(fell_back);
          None;
        };
      };
    incr(count);
    let hooks =
      switch (members) {
      | Some((brace, _, _, term, plist, _, _)) =>
        Id.Map.singleton(brace, (term, plist))
      | None => Id.Map.empty
      };
    /* an unfinished member cut at its member line: no separator ends it */
    let cut =
      lv.cut && k < Array.length(lv.spans) - 1 && lv.spans[k].sep == None;
    let (f_term, f_hole, f_mods, hole) =
      if (root == Sort.Mod) {
        let (items, hole) = parse_item_mod(~masks, ~hooks, ~cut, ps);
        (Exp.fresh(EmptyHole), None, items, hole);
      } else {
        let (term, hole) = parse_item(~masks, ~hooks, ps);
        (term, hole, [], hole);
      };
    Option.iter(
      ((_, _, _, _, _, (frame, level_map), _)) => {
        term_data := Id.Map.union((_, f, _) => Some(f), frame, term_data^);
        map := Id.Map.union((_, l, _) => Some(l), level_map, map^);
      },
      members,
    );
    consolidate_adopted();
    let (m, td) = scrub_hole(hole, (map^, term_data^));
    let (f_map, f_td, f_adopted, f_seps) =
      localize(~member, ps, m, td, adopted_ids^);
    /* the spine its parent grafts supplies the nodes down to its hole */
    let f_map =
      switch (graft, f_hole) {
      | (true, Some(h)) =>
        List.fold_left(
          (m, node: Exp.t) =>
            List.fold_left(
              (m, id) => Id.Map.remove(id, m),
              m,
              node.annotation.ids,
            ),
          f_map,
          snd(graft_at(h, Exp.fresh(EmptyHole), f_term)),
        )
      | _ => f_map
      };
    let e = {
      f_pieces: ps,
      f_term,
      f_hole,
      f_mods,
      f_map,
      f_td,
      f_proj: projectors^,
      f_plist: projector_list^,
      f_adopted,
      f_seps,
      f_kids:
        Option.map(
          ((brace, kids, _, _, _, _, _)) => (brace, kids),
          members,
        ),
    };
    let delta =
      switch (members, prev) {
      | (Some((_, _, kd, _, _, _, true)), Some(p)) => {
          gone: [p, ...kd.gone],
          come: [e, ...kd.come],
        }
      | (Some((_, _, kd, _, _, _, _)), p) => {
          gone: Option.fold(~none=[], ~some=subtree, p),
          come: [e, ...kd.come],
        }
      | (None, p) => {
          gone: Option.fold(~none=[], ~some=subtree, p),
          come: [e],
        }
      };
    (e, delta);
  }

  /* a member that is a let chain: its items parsed at Exp (the first with
     the member's leading trivia, the last with its trailing whitespace, as
     their nodes own them), grafted, under a shell that parses the rest of
     the member around a placeholder. None: it parses whole */
  and parse_chain =
      (
        ~member: bool,
        ~masks,
        ~count: ref(int),
        ~prev: option(entry_full),
        ~shared: option(entry_full),
        lv: ItemSpans.level,
        k: int,
        ps: Segment.t,
        cl: ItemSpans.level,
      )
      : option((entry_full, delta)) =>
    switch (
      {
        let sp = lv.spans[k];
        let (c0, c1) = sp.core;
        let arr = lv.pieces;
        let rec semi_at = i =>
          i >= sp.hi ? sp.hi : Piece.is_semi(arr[i]) ? i : semi_at(i + 1);
        let stop = semi_at(c1);
        let sub = (a, b) => Array.to_list(Array.sub(arr, a, b - a));
        let keyed = ItemSpans.keyed(cl);
        let last = List.length(keyed) - 1;
        let keyed =
          List.mapi(
            (i, (key, s)) =>
              (
                key,
                (i == 0 ? sub(sp.lo, c0) : [])
                @ s
                @ (i == last ? sub(c1, stop) : []),
              ),
            keyed,
          );
        let pk = kids_of(prev, sp.anchor);
        let (kids, kd) =
          parse_level(
            ~root=Exp,
            ~member,
            ~graft=true,
            ~masks,
            ~count,
            ~prev=Option.value(pk, ~default=[]),
            ~shared=Option.value(kids_of(shared, sp.anchor), ~default=[]),
            ~keyed,
            cl,
          );
        let rebuilt = ref([]);
        let e =
          graft_items(
            ~on_path=path => rebuilt := path @ rebuilt^,
            List.map(((_, e)) => (e.f_term, e.f_hole), kids),
          );
        let ph = Id.derive(~salt="chain-shell", sp.anchor);
        incr(count);
        let (items, hole) =
          parse_item_mod(
            ~masks,
            [
              Piece.Grout({
                id: ph,
                shape: Convex,
              }),
              ...sub(stop, sp.hi),
            ],
          );
        /* as mod_ reads the chain: ModExp under its head's ids */
        let modexp: Mod.t =
          IdTagged.mk(
            ~incomplete=e.annotation.incomplete,
            e.annotation.ids,
            e.annotation.secondary,
            ModExp(e): TermBase.Mod.term,
          );
        let f_mods =
          switch (items) {
          | [{annotation: {ids: [id], _}, _}, ...rest] when id == ph => [
              modexp,
              ...rest,
            ]
          | _ => raise(Recompose)
          };
        consolidate_adopted();
        let (m, td) = scrub_hole(hole, (map^, term_data^));
        let m =
          fix_spine(Id.Map.remove(ph, m), rebuilt^)
          |> TermMap.add_all(e.annotation.ids, Mod(modexp));
        let (f_map, f_td, f_adopted, f_seps) =
          localize(~member, ps, m, Id.Map.remove(ph, td), adopted_ids^);
        let entry = {
          f_pieces: ps,
          f_term: Exp.fresh(EmptyHole),
          f_hole: None,
          f_mods,
          f_map,
          f_td,
          f_proj: projectors^,
          f_plist:
            projector_list^
            @ List.concat_map(((_, e)) => e.f_plist, List.rev(kids)),
          f_adopted:
            f_adopted @ List.concat_map(((_, e)) => e.f_adopted, kids),
          f_seps,
          f_kids: Some((sp.anchor, kids)),
        };
        (
          entry,
          switch (pk, prev) {
          | (Some(_), Some(p)) => {
              gone: [p, ...kd.gone],
              come: [entry, ...kd.come],
            }
          | (_, p) => {
              gone: Option.fold(~none=[], ~some=subtree, p),
              come: [entry, ...kd.come],
            }
          },
        );
      }
    ) {
    | r => Some(r)
    | exception _ =>
      incr(fell_back);
      None;
    }

  and kids_of = (e: option(entry_full), id: Id.t) =>
    switch (e) {
    | Some({f_kids: Some((b, kids)), _}) when b == id => Some(kids)
    | _ => None
    };

  /* a level's term: a Mod root's items concatenate under the stable
     wrapper; an expression level's graft */
  let compose = (~root: Sort.t, ~on_path=?, entries): Exp.t =>
    root == Sort.Mod
      ? wrap_module(List.concat_map(((_, e)) => e.f_mods, entries))
      : graft_items(
          ~on_path?,
          List.map(((_, e)) => (e.f_term, e.f_hole), entries),
        );

  /* the statics path's last entries, per root. ~masks only concern items
     whose completion changed, and those pieces are new objects, so these
     entries never carry a stale mask; nor do [shared]'s, an editor's
     go_incr cache, whose parses of the same pieces stand in for parsing
     them again (members included) */
  let term_cache:
    ref(option((Sort.t, ItemSpans.ends, list((Id.t, entry_full))))) =
    ref(None);

  /* [ends]: the division read (ItemSpans); complete programs read alike
     either way, so semantics, which reads completed ones, takes Read to
     share the editor's parses */
  let term_of_root =
      (
        ~masks: Id.Map.t(IdTagged.IdTag.incomplete_mask)=Id.Map.empty,
        ~ends=ItemSpans.Written,
        ~root: Sort.t,
        ~shared: option(cache)=?,
        seg: Segment.t,
      )
      : Exp.t => {
    let root = root == Sort.Mod ? Sort.Mod : Sort.Exp;
    analyzed := 0;
    switch (last^) {
    | Some((r, e, prev_seg, prev_term))
        when r == root && e == ends && seg_eq(prev_seg, seg) => prev_term
    | _ =>
      let prev =
        switch (term_cache^) {
        | Some((r, e, entries)) when r == root && e == ends => entries
        | _ => []
        };
      let shared =
        switch (shared) {
        | Some({c_prev: Some((r, e, entries, _, _, _))})
            when r == root && e == ends => entries
        | _ => []
        };
      let term =
        try({
          let (entries, _) =
            parse_level(
              ~root,
              ~member=false,
              ~masks,
              ~count=analyzed,
              ~prev,
              ~shared,
              ItemSpans.top(~ends, ~root, seg),
            );
          term_cache := Some((root, ends, entries));
          compose(~root, entries);
        }) {
        | _ =>
          incr(fell_back);
          (
            switch (root == Sort.Mod, Id.Map.is_empty(masks)) {
            | (true, true) => go_mod_root(seg)
            | (true, false) => go_mod_root_impl(~masks, seg)
            | (false, true) => go(seg)
            | (false, false) => go_impl(~masks, seg)
            }
          ).
            term;
        };
      last := Some((root, ends, seg, term));
      term;
    };
  };
  let term_of = (~masks=?, seg) => term_of_root(~masks?, ~root=Exp, seg);
  let term_of_mod = (~masks=?, seg) => term_of_root(~masks?, ~root=Mod, seg);

  let go_incr' =
      (
        ~ends=ItemSpans.Written,
        ~root: Sort.t=Exp,
        ~cache: cache,
        seg: Segment.t,
      )
      : t => {
    let (prev, m_map0, m_td0, m_proj0) =
      switch (cache.c_prev) {
      | Some((r, e, entries, m_map, m_td, m_proj)) when r == root && e == ends => (
          entries,
          m_map,
          m_td,
          m_proj,
        )
      | _ => ([], Id.Map.empty, Id.Map.empty, Id.Map.empty)
      };
    let (entries, d) =
      parse_level(
        ~root,
        ~member=false,
        ~masks=Id.Map.empty,
        ~count=full_analyzed,
        ~prev,
        ~shared=[],
        ItemSpans.top(~ends, ~root, seg),
      );
    /* the pre-fixup unions: drop the bindings of entries gone, add those
       of entries come */
    let drop = (m, keys) =>
      Id.Map.fold((id, _, m) => Id.Map.remove(id, m), keys, m);
    let add = (m, kvs) => Id.Map.fold(Id.Map.add, kvs, m);
    let (m_map, m_td, m_proj) =
      List.fold_left(
        ((m_map, m_td, m_proj), e) =>
          (
            drop(m_map, e.f_map),
            drop(m_td, e.f_td),
            drop(m_proj, e.f_proj),
          ),
        (m_map0, m_td0, m_proj0),
        d.gone,
      );
    let (m_map, m_td, m_proj) =
      List.fold_left(
        ((m_map, m_td, m_proj), e) =>
          (add(m_map, e.f_map), add(m_td, e.f_td), add(m_proj, e.f_proj)),
        (m_map, m_td, m_proj),
        d.come,
      );
    cache.c_prev = Some((root, ends, entries, m_map, m_td, m_proj));
    /* every graft's rebuilt nodes, outermost first */
    let rebuilt = ref([]);
    let term =
      compose(~root, ~on_path=path => rebuilt := path @ rebuilt^, entries);
    last := Some((root, ends, seg, term)); /* share with term_of (statics path) */
    let terms =
      fix_spine(
        TermMap.add_all(term.annotation.ids, Exp(term), m_map),
        rebuilt^,
      );
    let term_data =
      record_top_frame(~root, m_td, seg)
      |> refix(List.concat_map(((_, e)) => e.f_adopted, entries));
    /* go conses projector ids left to right, so later items come first */
    let projector_list =
      List.concat_map(((_, e)) => e.f_plist, List.rev(entries));
    {
      term,
      terms,
      term_data,
      projectors: m_proj,
      projector_list,
    };
  };

  /* [ends]: the division read (ItemSpans): the editor's syntax reads
     Read, so an unfinished member's open form stops at its member line
     and the members below keep their parses */
  let go_incr =
      (
        ~ends=ItemSpans.Written,
        ~root: Sort.t=Exp,
        ~cache: cache,
        seg: Segment.t,
      )
      : t =>
    switch (go_incr'(~ends, ~root, ~cache, seg)) {
    | r => r
    | exception _ =>
      incr(fell_back);
      root == Sort.Mod ? go_mod_root(seg) : go(seg);
    };
};

/* Retain the exact completion that supplied semantic hole IDs. A separate
   completion run may mint different grout IDs, even for the same source. */
[@deriving (show({with_path: false}), sexp, yojson)]
type completion_snapshot = {
  source: Segment.t,
  completed: Segment.t,
};

let semantic_source = (z: Zipper.t): Segment.t =>
  z
  |> Zipper.clear_unparsed_buffer
  |> Zipper.unselect_and_zip(~erase_buffer=true);

/* the segment semantics sees, with its provenance masks: completed per
   item, so complete items keep the piece identity Incr keys on */
let semantic_segment =
    (~root: Sort.t, seg: Segment.t)
    : (Segment.t, Id.Map.t(IdTagged.IdTag.incomplete_mask)) => {
  let result = CanonicalCompletion.complete_items(~sort=root, seg);
  (
    result.completed_seg,
    CanonicalCompletion.masks_of_records(result.shard_records),
  );
};

/* [source] as semantics reads it: its completion, and that parsed per
   item. Raw, an unfinished member's open form takes the members after it
   into one item, one recursion through the rest of its module. [shared]:
   an editor's parse cache */
let semantic_parse =
    (~root: Sort.t, ~shared=?, source: Segment.t): (Segment.t, Exp.t) => {
  let (seg, masks) = semantic_segment(~root, source);
  (seg, Incr.term_of_root(~masks, ~ends=Read, ~root, ~shared?, seg));
};
let semantic_term = (~root: Sort.t, ~shared=?, source: Segment.t): Exp.t =>
  snd(semantic_parse(~root, ~shared?, source));

let from_zip_for_sem_with_completion = (z: Zipper.t, ~root: Sort.t) => {
  /* Semantic terms come from the canonical completion of the visible
   * segment (caret-independent, provenance-recorded), replacing the
   * old caret-sensitive missing-shard dump. A Mod root completes and
   * reads as a member list; every other root at Exp. */
  let seg = semantic_source(z);
  let sort = root == Sort.Mod ? Sort.Mod : Sort.Exp;
  let result = CanonicalCompletion.complete_segment_deep(~sort, seg);
  let masks = CanonicalCompletion.masks_of_records(result.shard_records);
  (
    sort == Sort.Mod
      ? go_mod_root_impl(~masks, result.completed_seg)
      : go_impl(~masks, result.completed_seg),
    {
      source: seg,
      completed: result.completed_seg,
    },
  );
};

let from_zip_for_sem_with_completion =
  /* small for the same reason as [go]: keys pin zippers */
  Core.Memo.general(~cache_size_bound=8, from_zip_for_sem_with_completion);

/* the semantic term of a cell rooted at [root]; [default] if it has no
   skeleton */
let from_zip_at =
    (~root: Sort.t, ~default: unit => 'a, term: unsorted => 'a, z: Zipper.t)
    : 'a => {
  let (seg, _) = semantic_segment(~root, semantic_source(z));
  fresh(seg, () =>
    switch (skel_of(~sort=Exp, seg)) {
    | exception _ => default()
    | skel => term(unsorted(root, skel, seg))
    }
  );
};

/* `name : T` headers, type-alias bodies and type-alias headers */
let from_zip_for_pat =
  from_zip_at(~root=Sort.Pat, ~default=() => Pat.fresh(EmptyHole), pat);
let from_zip_for_typ =
  from_zip_at(
    ~root=Sort.Typ,
    ~default=() => Typ.fresh(Unknown(Hole(EmptyHole))),
    typ,
  );
let from_zip_for_tpat =
  from_zip_at(~root=Sort.TPat, ~default=() => TPat.fresh(EmptyHole), tpat);

/* syntax data for a non-Exp-rooted cell, parsed at its own sort (the
   Exp-rooted [go] would flag every token sort-inconsistent); go_s also
   records the root term's ids in [map] */
let sorted_syntax_data_memo =
  Core.Memo.general(~cache_size_bound=8, ((root: Sort.t, seg: Segment.t)) =>
    fresh(seg, () =>
      switch (skel_of(~sort=Exp, seg)) {
      | exception _ => (TermMap.empty, Id.Map.empty, Id.Map.empty, [])
      | skel =>
        ignore(go_s(root, skel, seg));
        consolidate_adopted();
        (map^, term_data^, projectors^, projector_list^);
      }
    )
  );

let sorted_syntax_data = (~root: Sort.t, seg: Segment.t) =>
  sorted_syntax_data_memo((root, seg));

let from_zip_for_sem = (z, ~root) =>
  fst(from_zip_for_sem_with_completion(z, ~root));
