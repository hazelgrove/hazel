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

// TODO make less hacky
let tokens =
  Piece.get(
    _ => [],
    _ => [" "],
    (t: Tile.t) => t.shards |> List.map(List.nth(t.label)),
    _ =>
      /* Hack: These act as temporary wrappers for projectors,
       * given that they in-effect act as a convex wrapping form */
      ["PROJ_WRAP", "PROJ_WRAP"],
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
  term_data: TermData.t,
  projectors: Id.Map.t(Piece.projector),
  projector_list: list(Id.t),
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
let is_typ_bsum = is_nary(Any.is_typ, "+");
let is_mod_seq = is_nary(Any.is_mod, ";");

/* Flatten a module term into a list of module items.
   Module sequences (from semicolons) are stored as MultiHole([Mod(m1), Mod(m2)])
   during parsing and need to be flattened into a proper list for Module(items). */
let rec flatten_mod = (m: TermBase.Mod.t): list(TermBase.Mod.t) =>
  switch (m.term) {
  | MultiHole(kids) =>
    kids
    |> List.filter_map(
         fun
         | Grammar.Mod(m) => Some(flatten_mod(m))
         | _ => None,
       )
    |> List.flatten
  | ModLet(_, _)
  | ModType(_, _)
  | ModExp(_)
  | EmptyHole
  | Invalid(_) => [m]
  };

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
 * ListLit, the comma's term_data still has the Tuple's skeleton. AutoProbe
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
  t == " " || Token.is_explicit_hole(t) || Token.is_llm_hole(t);

let rec go_s = (s: Sort.t, skel: Skel.t, seg: Segment.t): Any.t =>
  switch (s) {
  | Pat => Pat(pat(unsorted(Pat, skel, seg)))
  | TPat => TPat(tpat(unsorted(TPat, skel, seg)))
  | Typ => Typ(typ(unsorted(Typ, skel, seg)))
  | Exp => Exp(exp(unsorted(Exp, skel, seg)))
  | Rul => Rul(rul(unsorted(Rul, skel, seg)))
  | Mod => Mod(mod_(unsorted(Mod, skel, seg))) /* Phase 1.2: proper module parsing */
  | Any =>
    let sort = Segment.sort_of(skel, seg);
    if (sort == Any) {
      Exp(exp(unsorted(Exp, skel, seg)));
    } else {
      go_s(sort, skel, seg);
    };
  }

and exp = unsorted => {
  let (term, inner_ids) = exp_term(unsorted);
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
      | ([t], []) when Token.is_empty_tuple(t) => ret(Tuple([]))
      | ([t], []) when Token.is_wild(t) => ret(Deferral(OutsideAp))
      | ([t], []) when Token.is_empty_list(t) => ret(ListLit([]))
      | ([t], []) when Token.is_empty_module(t) => ret(Module([]))
      | ([t], []) when Token.is_bool(t) =>
        ret(Atom(Bool(bool_of_string(t))))
      | ([t], []) when Token.is_undefined(t) => ret(Undefined)
      | ([t], []) when Token.is_int(t) =>
        ret(Atom(Int(Bigint.of_string(t))))
      | ([t], []) when Token.is_string(t) =>
        ret(Atom(String(Token.strip_quotes(t))))
      | ([t], []) when Token.is_quoted_label(t) =>
        ret(Label(Token.strip_quotes(~quote=Token.label_delim, t)))
      | ([t], []) when Token.is_float(t) =>
        ret(Atom(Float(float_of_string(t))))
      | ([t], []) when Token.is_livelit(t) =>
        ret(LivelitName(Token.parse_livelit(t)))
      | ([t], []) when Token.is_var(t) => ret(Var(t))
      | ([t], []) when Token.is_ctr(t) => ret(Constructor(t, None))
      | (["{", "}"], [Mod(body)]) =>
        /* ModBody absorption: inner Mod's semicolon IDs become part of Module.
           ID order: [curly_brace_id] @ semicolon_ids (outer first, then adopted).
           This ensures cursor inspector works for both curly braces AND semicolons.
           IMPORTANT: Only absorb when body is MultiHole (from semicolons).
           For single items, body.annotation.ids would be the ModLet/ModType ID,
           which is also used by the expanded Let/TyAlias - absorbing would duplicate. */
        switch (body) {
        | {term: EmptyHole, _} => ret(Module([]))
        | {annotation: {ids, _}, term: MultiHole(_)} =>
          /* Multiple items: absorb semicolon IDs */
          adopted_ids := ids @ adopted_ids^;
          (Module(flatten_mod(body)), ids)
        | _ =>
          /* Single item: don't absorb (would duplicate ModLet/ModType ID) */
          ret(Module(flatten_mod(body)))
        }
      | (["{", "}"], [Exp(body)])
      | (["(", ")"], [Exp(body)]) => ret(Parens(body))
      | (["PROJ_WRAP", "PROJ_WRAP"], [Exp(body)]) => ret(body.term)
      | (["[", "]"], [Exp(body)]) =>
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
      | (["test", "end"], [Exp(test)]) => ret(Test(test))
      | (["proof_object", "end"], [Exp(proof)]) =>
        ret(ProofObject(proof))
      | (["hint", "test", "end"], [Exp(hint), Exp(test)]) =>
        ret(HintedTest(test, hint))
      | (["case", "end"], [Rul({term, annotation: {ids, _}})]) =>
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
      | ([t], []) when is_hole_label(t) => ret(hole(tm))
      | ([t], []) when t != " " && !Token.is_explicit_hole(t) =>
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
        | (["forall", "->"], [Pat(pat)]) => Forall(pat, r)
        | (["fix", "->"], [Pat(pat)]) => FixF(pat, r, None)
        | (["typfun", "->"], [TPat(tpat)]) => TypFun(tpat, r, None)
        | (["let", "=", "in"], [Pat(pat), Exp(def)]) => Let(pat, def, r)
        | (["theorem", "=", "in"], [Pat(pat), Exp(thm)]) =>
          Theorem(pat, thm, r)
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
              annotation:
                IdTagged.IdTag.mk(
                  [Id.nullary_ap_flag],
                  get_secondary([Id.nullary_ap_flag]),
                ),
              term: Tuple([]),
            },
          ),
        )
      | (["(", ")"], [Exp(arg)]) =>
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
      | (["@<", ">"], [Typ(ty)]) => ret(TypAp(l, ty))
      | _ => ret(hole(tm))
      }
    | _ => ret(hole(tm))
    }
  | Bin(Exp(l), tiles, Typ(r)) as tm =>
    switch (tiles) {
    | ([(_id, ([":"], []))], []) => ret(Asc(l, r))
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
          | (["=="], []) => BinOp(Poly(Equals), l, r)
          | (["!="], []) => BinOp(Poly(NotEquals), l, r)
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
          | (["..."], []) => TupleExtension(l, r)
          | (["="], []) =>
            switch (l.term) {
            | Deferral(_) =>
              TupLabel(
                {
                  annotation: l.annotation,
                  term: ExplicitNonlabel,
                },
                r,
              ) // Unlabeled tuple using deferred ap in tuplabel
            | Var(name) =>
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
            | Label(_) => Dot(l, r)
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
      | ([t], []) when Token.is_empty_tuple(t) => ret(Tuple([]))
      | ([t], []) when Token.is_empty_list(t) => ret(ListLit([]))
      | ([t], []) when Token.is_bool(t) =>
        ret(Atom(Bool(bool_of_string(t))))
      | ([t], []) when Token.is_float(t) =>
        ret(Atom(Float(float_of_string(t))))
      | ([t], []) when Token.is_int(t) =>
        ret(Atom(Int(Bigint.of_string(t))))
      | ([t], []) when Token.is_string(t) =>
        ret(Atom(String(Token.strip_quotes(t))))
      | ([t], []) when Token.is_quoted_label(t) =>
        ret(Label(Token.strip_quotes(~quote=Token.label_delim, t)))
      | ([t], []) when Token.is_var(t) => ret(Var(t))
      | ([t], []) when Token.is_wild(t) => ret(Wild)
      | ([t], []) when Token.is_ctr(t) => ret(Constructor(t, None))
      | (["(", ")"], [Pat(body)]) => ret(Parens(body))
      | (["PROJ_WRAP", "PROJ_WRAP"], [Pat(body)]) => ret(body.term)
      | (["[", "]"], [Pat(body)]) =>
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
      | ([t], []) when is_hole_label(t) => ret(hole(tm))
      | ([t], []) => ret(Invalid(t))
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
    | ([(_id, ([":"], []))], []) => ret(Asc(p, ty))
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
    | ([(_id, tile)], []) =>
      ret(
        switch (tile) {
        | ([t], []) when Token.is_empty_tuple(t) => Prod([])
        | (["Bool"], []) => Atom(Bool)
        | (["Int"], []) => Atom(Int)
        | (["SInt"], []) => Atom(SInt)
        | (["Float"], []) => Atom(Float)
        | (["String"], []) => Atom(String)
        | (["Nat"], []) => Atom(Nat)
        | (["_"], []) => ExplicitNonlabel
        | (["proof_of", "end"], [Exp(exp)]) => ProofOf(exp)
        | ([t], []) when Token.is_typ_var(t) => Var(t)
        | ([t], []) when Token.is_quoted_label(t) =>
          Label(Token.sub(t, 1, Token.length(t) - 2))
        | (["(", ")"], [Typ(body)]) => Parens(body)
        | (["PROJ_WRAP", "PROJ_WRAP"], [Typ(body)]) => body.term
        | (["[", "]"], [Typ(body)]) => List(body)
        | ([t], []) when is_hole_label(t) => hole(tm)
        | ([t], []) => Unknown(Hole(Invalid(t)))
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
  | Pre(([(_id, (["poly", "->"], [TPat(tpat)]))], []), Typ(t)) =>
    ret(Poly(tpat, t))
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
    | ([(_, (["+"], []))], []) => ret(Sum([parse_sum_term(t)]))
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
      | ([(_id, (["."], []))], []) =>
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
      | ([(_id, (["..."], []))], []) => ret(ProdExtension(l, r))
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
        | ([t], []) when Token.is_typ_var(t) => Var(t)
        | ([t], []) when is_hole_label(t) => hole(tm)
        | ([t], []) => Invalid(t)
        | (["PROJ_WRAP", "PROJ_WRAP"], [TPat(body)]) => body.term
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
      | ([t], []) when is_hole_label(t) => ret(hole(tm))
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
      let all_items = [Grammar.Mod(m1)] @ List.map(m => Grammar.Mod(m), between_kids) @ [Grammar.Mod(m2)];
      ret(MultiHole(all_items))
    | None => ret(hole(Bin(Mod(m1), tiles, Mod(m2))))
    }
  /* ModLet: let p = e - the pattern is inside the tile, expression is the body */
  | Pre(([(_id, (["let", "="], [Pat(p)]))], []), Exp(e)) =>
    ret(ModLet(p, e))
  /* ModType: type t = T - the tpat is inside the tile, type is the body */
  | Pre(([(_id, (["type", "="], [TPat(tp)]))], []), Typ(ty)) =>
    ret(ModType(tp, ty))
  /* Expression-level structures (binary ops, prefix, postfix) - wrap as ModExp */
  | Bin(Exp(_), _, Exp(_)) as tm => ret(ModExp(exp(tm)))
  | Pre(_, Exp(_)) as tm => ret(ModExp(exp(tm)))
  | Post(Exp(_), _) as tm => ret(ModExp(exp(tm)))
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

/* Consolidate term_data for adopted IDs (see adopted_ids comment above).
 * Updates each adopted ID's term_data entry to match the rep_id's entry,
 * giving adopted IDs the correct skeleton/segment of the outer term.
 *
 * IMPORTANT: We preserve the original root_piece for each adopted ID.
 * The root_piece identifies the actual tile that the ID refers to, which
 * is needed by Arms.tiles_data to find the correct shards for decoration.
 * Only skel, sort, and base_seg should be updated to match the outer term. */
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
           /* Preserve the original root_piece while updating skel/sort/base_seg */
           term_data :=
             Id.Map.add(
               id,
               {
                 ...rep_data,
                 root_piece: old_data.root_piece,
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
        | Any => Some(Any()) /* grout */
        };
      };
    }
  );

let from_zip_for_sem = (z: Zipper.t) => go(Dump.to_segment(z));

let from_zip_for_sem =
  Core.Memo.general(~cache_size_bound=1000, from_zip_for_sem);
