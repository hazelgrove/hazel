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
  term_data: TermData.t,
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
  /* Constructor applications in sum type definitions are implemented as having type sort;
     until they are reimplemented as their own sort, we must prevent these from being parsed
     into types, where they can go on to mess with statics. Thus we let them fall through to
     be parsed as multiholes, and then recognize them when we parse sum type definitions */
  | {
      term:
        Unknown(
          Hole(
            MultiHole([
              Typ({term: Var(ctr), annotation: {ids: ids_ctr, _}}),
              Typ(u),
            ]),
          ),
        ),
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
  | Some(u) => Unknown(Hole(MultiHole([Typ(t), Typ(u)]))) |> Typ.fresh
  };
};

let is_hole_label = (t: string) =>
  t == " " || Token.is_explicit_hole(t) || Token.is_llm_hole(t);

let rec go_s = (s: Sort.t, skel: Skel.t, seg: Segment.t): Term.Any.t =>
  switch (s) {
  | Pat => Pat(pat(unsorted(Pat, skel, seg)))
  | TPat => TPat(tpat(unsorted(TPat, skel, seg)))
  | Typ => Typ(typ(unsorted(Typ, skel, seg)))
  | Exp => Exp(exp(unsorted(Exp, skel, seg)))
  | Rul => Rul(rul(unsorted(Rul, skel, seg)))
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
  // | TupLabel(_) =>
  //   // The tile id is the id of the tuple not the tuplabel
  //   let (e_term, rewrap) = IdTagged.unwrap(e);
  //   rewrap(Tuple([e_term |> Exp.fresh]): Exp.term);
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
    | ([(id, t)], []) =>
      switch (t) {
      | ([t], []) when Token.is_empty_tuple(t) => ret(Tuple([]))
      | ([t], []) when Token.is_wild(t) => ret(Deferral(OutsideAp))
      | ([t], []) when Token.is_empty_list(t) => ret(ListLit([]))
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
      | (["(", ")"], [Exp(body)]) => ret(Parens(body))
      | (label, [Exp(body)]) when is_probe_wrap(label) =>
        // Temporary wrapping form to persist projector probes
        ret(should_instrument(id) ? Probe(body, Probe.empty) : body.term)
      | (["[", "]"], [Exp(body)]) =>
        switch (body) {
        | {annotation: {ids}, term: Tuple(es)} =>
          // Addresses tup_labels in lists like: [l=32, 1]
          (
            ListLit(
              List.map(
                (list_item: Exp.tuple_entry) => {
                  switch (list_item) {
                  | Labeled(a, l, e) => (
                      {
                        annotation: a,
                        term: Tuple([Labeled(IdTagged.IdTag.fresh(), l, e)]),
                      }: Exp.t
                    )
                  | Unlabeled(e) => e
                  }
                },
                es,
              ),
            ),
            ids,
          )
        | term => ret(ListLit([term]))
        }
      | (["test", "end"], [Exp(test)]) => ret(Test(test))
      | (["hint", "test", "end"], [Exp(hint), Exp(test)]) =>
        ret(HintedTest(test, hint))
      | (["case", "end"], [Rul({term, annotation: {ids, _}})]) =>
        switch (term) {
        | Rules(scrut, rules) => (Match(scrut, rules), ids)
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
        | Var(l) when Token.is_livelit(l) =>
          ret(LivelitName(Token.parse_livelit(l)))
        | _ when Exp.is_deferral(arg) =>
          ret(DeferredAp(l, [use_deferral(arg)]))
        | Tuple(es)
            when
              List.exists(
                e => Exp.is_deferral(Grammar.get_tuple_entry_value(e)),
                es,
              ) => (
            DeferredAp(
              l,
              List.map(
                (arg: Exp.tuple_entry) => {
                  switch (arg) {
                  | Unlabeled(arg) =>
                    Exp.is_deferral(arg) ? use_deferral(arg) : arg
                  | _ =>
                    raise(
                      Failure(
                        "todo:  labeled argument in deferred application",
                      ),
                    )
                  }
                },
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
      let tuple_children: list(Exp.tuple_entry) =
        [l]
        @ between_kids
        @ [r]
        |> List.map((child: Exp.t) => {
             switch (child) {
             | {term: Tuple([Labeled(_, l, e)]), annotation: a} =>
               // We use the Id for the tuple as the ids for the tuplabels
               (Labeled(a, l, e): Exp.tuple_entry)
             | {term: Tuple([Unlabeled(_) as e]), _} => e
             | _ => Unlabeled(child)
             }
           });

      ret(Tuple(tuple_children));
    | None =>
      switch (tiles) {
      | ([(id, t)], []) =>
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
            | Var(name) =>
              Tuple([
                Labeled(
                  {ids: [id]},
                  {
                    term: Label(name),
                    annotation: l.annotation,
                  },
                  r,
                ),
              ])
            | Label(name) =>
              Tuple([
                Labeled(
                  {ids: [id]},
                  {
                    term: Label(name),
                    annotation: l.annotation,
                  },
                  r,
                ),
              ])
            | EmptyHole =>
              Tuple([
                Labeled(
                  {ids: [id]},
                  {
                    term: EmptyLabel,
                    annotation: l.annotation,
                  },
                  r,
                ),
              ])
            | _ =>
              Tuple([
                Labeled(
                  {ids: [id]},
                  {
                    term: MultiHole([Exp(l.term |> Exp.fresh)]),
                    annotation: l.annotation,
                  },
                  r,
                ),
              ])
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
        | ([t], []) when Token.is_empty_tuple(t) => Tuple([])
        | ([t], []) when Token.is_empty_list(t) => ListLit([])
        | ([t], []) when Token.is_bool(t) =>
          Atom(Bool(bool_of_string(t)))
        | ([t], []) when Token.is_float(t) =>
          Atom(Float(float_of_string(t)))
        | ([t], []) when Token.is_int(t) =>
          Atom(Int(Bigint.of_string(t)))
        | ([t], []) when Token.is_string(t) =>
          Atom(String(Token.strip_quotes(t)))
        | ([t], []) when Token.is_quoted_label(t) =>
          Label(Token.strip_quotes(~quote=Token.label_delim, t))
        | ([t], []) when Token.is_var(t) => Var(t)
        | ([t], []) when Token.is_wild(t) => Wild
        | ([t], []) when Token.is_ctr(t) => Constructor(t, None)
        | (["(", ")"], [Pat(body)]) => Parens(body)
        | (label, [Pat(body)]) when is_probe_wrap(label) =>
          should_instrument(id) ? Probe(body, Probe.empty) : body.term
        | (["[", "]"], [Pat(body)]) =>
          switch (body) {
          | {term: Tuple(ps), _} => ListLit(ps)
          | term => ListLit([term])
          }
        | ([t], []) when is_hole_label(t) => hole(tm)
        | ([t], []) => Invalid(t)
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
  // | TupLabel(_) => Prod([t]) |> Typ.fresh
  | _ => t // TODO
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
        | ([t], []) when Token.is_typ_var(t) => Var(t)
        | ([t], []) when Token.is_quoted_label(t) =>
          Label(Token.sub(t, 1, Token.length(t) - 2))
        | (["(", ")"], [Typ(body)]) => Parens(body)
        | (label, [Typ(body)]) when is_probe_wrap(label) => body.term
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
      let tuple_children: list(Typ.tuple_entry) =
        [l]
        @ between_kids
        @ [r]
        |> List.map((child: Typ.t) => {
             switch (child) {
             | {term: Prod([Labeled(_, l, e)]), annotation: a} =>
               // We use the Id for the tuple as the ids for the tuplabels
               (Labeled(a, l, e): Typ.tuple_entry)
             | {term: Prod([Unlabeled(_) as e]), _} => e

             | _ => Unlabeled(child)
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
        | ([t], []) when Token.is_typ_var(t) => Var(t)
        | ([t], []) when is_hole_label(t) => hole(tm)
        | ([t], []) => Invalid(t)
        | (label, [TPat(body)]) when is_probe_wrap(label) => body.term
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
  | (Pre(_) | Post(_)) as tm => ret(hole(tm))
  | tm => ret(hole(tm));
}

and rul = (unsorted): Rul.t => {
  let e = exp(unsorted);
  let mk_rules = (scrut: Exp.t, rules, ids): Rul.t => {
    term: Rules(scrut, rules),
    annotation: {
      ids: ids,
    },
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

let go =
  Core.Memo.general(
    ~cache_size_bound=1000,
    seg => {
      map := TermMap.empty;
      term_data := Id.Map.empty;
      projectors := Id.Map.empty;
      let term = exp(unsorted(Exp, Segment.skel(seg), seg));
      {
        term,
        term_data: term_data^,
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
        | Any => Some(Any()) /* grout */
        };
      };
    }
  );

let from_zip_for_sem = (z: Zipper.t) => go(Dump.to_segment(z));

let from_zip_for_sem =
  Core.Memo.general(~cache_size_bound=1000, from_zip_for_sem);
