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
  | Bin(Any.t, tiles, Any.t)
  | Hole(Id.t); /* Structural hole — produces EmptyHole directly */

type t = {
  term: Exp.t,
  terms: TermMap.t,
  term_data: TermData.t,
  projectors: Id.Map.t(Piece.projector),
  projector_list: list(Id.t),
  /* Reverse map: secondary piece ID → owning term ID.
   * Built by inverting secondary_map after all terms (including holes)
   * have been processed. Enables cursor inspector to look up term info
   * when the caret is on whitespace. */
  ws_to_term: Id.Map.t(Id.t),
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
  | Bin(_, tiles, _) => ids_of_tiles(tiles)
  | Hole(id) => [id];

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
  | Bin(l, tiles, r) => [l] @ kids_of_tiles(tiles) @ [r]
  | Hole(_) => [];

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
    Aba.get_as(Skel.root(skel))
    |> List.filter_map(
         fun
         | Skel.Piece(idx) => Some(List.nth(seg, idx))
         | Skel.Hole(_) => None,
       )
    |> List.fold_left(
         (map, p) =>
           Id.Map.add(Piece.id(p), TermData.mk(p, sort, skel, seg), map),
         term_data^,
       );

/* Map to collect projector ids */
let projectors: ref(Id.Map.t(Piece.projector)) = ref(Id.Map.empty);
let projector_list: ref(list(Id.t)) = ref([]);

/* Map from tile IDs to their outer secondary (before, after) */
type secondary_map = Id.Map.t(Language.IdTagged.IdTag.secondary_runs);
let secondary_map: ref(secondary_map) = ref(Id.Map.empty);

/* Conflict-boundary secondary keyed by the segment index of the right-adjacent
 * piece (List.length(seg) for trailing edge, -1 for leading edge).
 * Populated by assign_secondary, consumed by unsorted when processing Hole refs. */
let hole_secondary: ref(list((int, list(Secondary.t)))) = ref([]);

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

/* Assign secondary (whitespace/comments) to tiles based on adjacent nib shapes.
 * This is a flat O(n) left-to-right scan. At each boundary between tiles,
 * the nib shapes determine ownership — space goes to the convex side:
 *   Convex-Concave  → LEFT tile (trailing/after)
 *   Concave-Convex  → RIGHT tile (leading/before)
 *   Convex-Convex   → LEFT tile (conflict: missing operator — both convex,
 *                      pick left as trailing; hole is infix so can't own)
 *   Concave-Concave → HOLE (conflict: missing operand — neither convex,
 *                      stash for operand-position hole)
 * Ccv-Ccv runs are stashed in hole_secondary keyed by the segment index of
 * the right-adjacent piece. unsorted picks them up when processing Hole refs.
 * Recurses into tile children and projector syntax. */
let rec assign_secondary = (seg: Segment.t): unit => {
  let boundary = Nib.Shape.concave();
  let prev_right_shape = ref(boundary);
  let pending_secondary: ref(list(Secondary.t)) = ref([]);
  let prev_tile_id: ref(option(Id.t)) = ref(None);
  let cur_idx = ref(0);

  let add_after = (id: Id.t, after: list(Secondary.t)) => {
    let before =
      switch (Id.Map.find_opt(id, secondary_map^)) {
      | Some((b, _)) => b
      | None => []
      };
    secondary_map := Id.Map.add(id, (before, after), secondary_map^);
  };

  let process_tile =
      (idx: int, id: Id.t, l_shape: Nib.Shape.t, r_shape: Nib.Shape.t) => {
    let run = pending_secondary^;
    pending_secondary := [];
    switch (prev_right_shape^, l_shape) {
    | (Convex, Concave(_)) =>
      /* Run goes to previous tile (after) */
      switch (prev_tile_id^) {
      | Some(pid) => add_after(pid, run)
      | None => ()
      }
    | (Concave(_), Convex) =>
      /* Run goes to this tile (before) */
      secondary_map := Id.Map.add(id, (run, []), secondary_map^)
    | (Convex, Convex) =>
      /* Missing operator (both sides convex). Assign to left tile
       * (convex side), consistent with how real operators work:
       * operands own adjacent spaces, operators have empty secondary. */
      switch (prev_tile_id^) {
      | Some(pid) => add_after(pid, run)
      | None => () /* Can't happen: prev starts as Ccv */
      }
    | (Concave(_), Concave(_)) =>
      /* Missing operand (neither side convex). Stash for hole —
       * this hole is in operand position (Op), so wrap_with_secondary
       * correctly wraps the hole term. */
      hole_secondary := [(idx, run), ...hole_secondary^]
    };
    prev_right_shape := r_shape;
    prev_tile_id := Some(id);
  };

  List.iter(
    fun
    | (Piece.Secondary(s): Piece.t) => {
        pending_secondary := pending_secondary^ @ [s];
        cur_idx := cur_idx^ + 1;
      }
    | Tile(t) => {
        let idx = cur_idx^;
        cur_idx := cur_idx^ + 1;
        let (l_shape, r_shape) = Tile.shapes(t);
        process_tile(idx, t.id, l_shape, r_shape);
        List.iter(assign_secondary, t.children);
      }
    | Projector(p) => {
        let idx = cur_idx^;
        cur_idx := cur_idx^ + 1;
        let (l_shape, r_shape) = ProjectorCore.shapes(p);
        process_tile(idx, p.id, l_shape, r_shape);
        assign_secondary(Piece.unparenthesize(p.syntax));
      },
    seg,
  );

  /* Handle trailing secondary */
  let run = pending_secondary^;
  if (run != []) {
    switch (prev_right_shape^, boundary) {
    | (Convex, Concave(_)) =>
      /* Normal trailing: assign to last tile (after) */
      switch (prev_tile_id^) {
      | Some(pid) => add_after(pid, run)
      | None => ()
      }
    | (Concave(_), Concave(_)) =>
      /* Conflict: missing operand at end (e.g., "1+ "). Stash for hole. */
      hole_secondary := [(List.length(seg), run), ...hole_secondary^]
    | _ => ()
    };
  };
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
  Token.is_explicit_hole(t) || Token.is_llm_hole(t);

let rec go_s =
        (
          ~left_bound=0,
          ~right_bound=?,
          s: Sort.t,
          skel: Skel.t,
          seg: Segment.t,
        )
        : Any.t => {
  let right_bound = Option.value(right_bound, ~default=List.length(seg));
  let u = sort => unsorted(~left_bound, ~right_bound, sort, skel, seg);
  switch (s) {
  | Drv(drv) =>
    Drv(
      switch (drv) {
      | Jdmt
      | Ctx
      | Prop
      | Exp => Exp(drv_exp(u(Drv(Exp))))
      | Pat => Pat(drv_pat(u(Drv(Pat))))
      | Typ => Typ(drv_typ(u(Drv(Typ))))
      | TPat => TPat(drv_tpat(u(Drv(TPat))))
      },
    )
  | Pat => Pat(pat(u(Pat)))
  | TPat => TPat(tpat(u(TPat)))
  | Typ => Typ(typ(u(Typ)))
  | Exp => Exp(exp(u(Exp)))
  | Rul => Rul(rul(u(Rul)))
  | Mod => Mod(mod_(u(Mod)))
  | Sig => Sig(sig_(u(Sig)))
  | MPat => MPat(mpat(u(MPat)))
  | Any =>
    let sort = Segment.sort_of(skel, seg);
    go_s(~left_bound, ~right_bound, sort == Any ? Exp : sort, skel, seg);
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
    | ([t], []) =>
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
      /* A standard Drv pair is parenthesised, so here we collapse
         [Parens(Tuple(e1, e2))] into [Pair(e1, e2)]. */
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
  | Op(([(_id, ([t], []))], [])) as tm =>
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
  | Op(([(_id, ([t], []))], [])) as tm =>
    switch (t) {
    | "Num" => ret(Num)
    | "Bool" => ret(Bool)
    | "1"
    | "Unit" => ret(Unit)
    | _ when Token.is_explicit_hole(t) => ret(TypHole)
    | _
        when
          Token.is_var(t)
          && String.length(t) > 1
          && String.sub(t, 0, 1) == "$" =>
      ret(Quote(t))
    | _ when Token.is_typ_var(t) => ret(Var(t))
    | _ => ret(hole(tm))
    }
  | Op(([(_id, (["(", ")"], [Drv(Typ(body))]))], [])) =>
    ret(Parens(body))
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
  | Op(([(_id, ([t], []))], []))
      when
        Token.is_var(t)
        && String.length(t) > 1
        && String.sub(t, 0, 1) == "$" =>
    ret(Quote(t))
  | Op(([(_id, ([t], []))], [])) when Token.is_typ_var(t) =>
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
      /* The [of_*] / [end] delimiters lift a Drv term back up to sort Exp
         by wrapping it in a [DrvQuote] node tagged with its derivation sort. */
      | (["of_jdmt", "end"], [Drv(Exp(j))]) =>
        ret(DrvQuote(Exp(j), Jdmt))
      | (["of_ctx", "end"], [Drv(Exp(c))]) => ret(DrvQuote(Exp(c), Ctx))
      | (["of_prop", "end"], [Drv(Exp(p))]) =>
        ret(DrvQuote(Exp(p), Prop))
      | (["of_alfa_exp", "end"], [Drv(Exp(e))]) =>
        ret(DrvQuote(Exp(e), Exp))
      | (["of_alfa_typ", "end"], [Drv(Typ(t))]) =>
        ret(DrvQuote(Typ(t), Typ))
      | (["of_alfa_pat", "end"], [Drv(Pat(p))]) =>
        ret(DrvQuote(Pat(p), Pat))
      | (["of_alfa_tpat", "end"], [Drv(TPat(tp))]) =>
        ret(DrvQuote(TPat(tp), TPat))
      | ([t], []) when is_hole_label(t) => ret(hole(tm))
      | ([t], []) when !is_hole_label(t) => ret(Invalid(t))
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
        | (["!"], []) => UnOp(Bool(Not), r)
        | (["fun", "->"], [Pat(pat)]) => Fun(pat, r, None, None)
        | (["forall", "->"], [Pat(pat)]) => Forall(pat, r)
        | (["fix", "->"], [Pat(pat)]) => FixF(pat, r, None)
        | (["typfun", "->"], [TPat(tpat)]) => TypFun(tpat, r, None)
        | (["let", "=", "in"], [Pat(pat), Exp(def)]) => Let(pat, def, r)
        | (["module", "=", "in"], [MPat(mp), Exp(def)]) =>
          ModuleExp(mp, def, r)
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
          | (["."], []) =>
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
        | (["()"], []) =>
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
        | (["(", ")"], [Pat(arg)]) => Ap(l, arg)
        | _ => hole(tm)
        },
      )
    | _ => ret(hole(tm))
    }
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
    | ([(_id, (["{", "}"], [Sig(body)]))], []) =>
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
        | ([t], []) when Token.is_empty_tuple(t) => Prod([])
        | ([t], []) when Token.is_empty_module(t) => Sig([])
        | (["Bool"], []) => Atom(Bool)
        | (["Int"], []) => Atom(Int)
        | (["SInt"], []) => Atom(SInt)
        | (["Float"], []) => Atom(Float)
        | (["String"], []) => Atom(String)
        | (["Nat"], []) => Atom(Nat)
        | (["Void"], []) => Sum([])
        | (["DrvJdmt"], []) => DrvQuoteTy(Jdmt)
        | (["DrvCtx"], []) => DrvQuoteTy(Ctx)
        | (["DrvProp"], []) => DrvQuoteTy(Prop)
        | (["ALFAExp"], []) => DrvQuoteTy(Exp)
        | (["DrvPat"], []) => DrvQuoteTy(Pat)
        | (["ALFATyp"], []) => DrvQuoteTy(Typ)
        | (["DrvTPat"], []) => DrvQuoteTy(TPat)
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
    | ([(_, (["+"], []))], []) =>
      adopted_ids := ids @ adopted_ids^;
      (Sum(t0), ids);
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
      let all_items =
        [Grammar.Mod(m1)]
        @ List.map(m => Grammar.Mod(m), between_kids)
        @ [Grammar.Mod(m2)];
      ret(MultiHole(all_items));
    | None => ret(hole(Bin(Mod(m1), tiles, Mod(m2))))
    }
  /* ModLet: let p = e - the pattern is inside the tile, expression is the body */
  | Pre(([(_id, (["let", "="], [Pat(p)]))], []), Exp(e)) =>
    ret(ModLet(p, e))
  /* ModuleMod: module M = e - MPat inside tile, expression is the body */
  | Pre(([(_id, (["module", "="], [MPat(mp)]))], []), Exp(e)) =>
    ret(ModuleMod(mp, e))
  /* ModType: type t = T - the tpat is inside the tile, type is the body */
  | Pre(([(_id, (["type", "="], [TPat(tp)]))], []), Typ(ty)) =>
    ret(ModType(tp, ty))
  /* Expression-level structures (binary ops, prefix, postfix) - wrap as ModExp */
  | Bin(Exp(_), _, Exp(_)) as tm => ret(ModExp(exp(tm)))
  | Pre(_, Exp(_)) as tm => ret(ModExp(exp(tm)))
  | Post(Exp(_), _) as tm => ret(ModExp(exp(tm)))
  | (Pre(_) | Post(_) | Bin(_) | Hole(_)) as tm => ret(hole(tm));
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
      | ([t], []) when is_hole_label(t) => ret(hole(tm))
      | ([t], []) => ret(Invalid(t))
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
  | Pre(([(_id, (["let"], []))], []), Pat(p)) => ret(SigLet(p))
  /* SigType: type t = T - the tpat is inside the tile, type is the body */
  | Pre(([(_id, (["type", "="], [TPat(tp)]))], []), Typ(ty)) =>
    ret(SigType(tp, ty))
  | (Pre(_) | Post(_) | Bin(_) | Hole(_)) as tm => ret(hole(tm));
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
    | ([(_id, ([t], []))], []) when Token.is_var(t) || Token.is_ctr(t) =>
      ret(Var(t))
    | ([(_id, ([t], []))], []) when is_hole_label(t) => ret(hole(tm))
    | ([(_id, ([t], []))], []) => ret(Invalid(t))
    | _ => ret(hole(tm))
    }
  | Bin(MPat(mp), tiles, Typ(ty)) as tm =>
    switch (tiles) {
    | ([(_id, ([":"], []))], []) => ret(Asc(mp, ty))
    | _ => ret(hole(tm))
    }
  | (Pre(_) | Post(_) | Bin(_) | Hole(_)) as tm => ret(hole(tm));
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

and unsorted =
    (
      ~left_bound=0,
      ~right_bound=?,
      sort: Sort.t,
      skel: Skel.t,
      seg: Segment.t,
    )
    : unsorted => {
  let right_bound = Option.value(right_bound, ~default=List.length(seg));

  /* Remove projectors. We do this here as opposed to removing
   * them in an external call to save a whole-syntax pass. */
  let tile_kids = (p: Piece.t): list(Any.t) =>
    switch (p) {
    | Secondary(_) => []
    | Projector({id, kind, model, syntax, _} as pr) =>
      let _ = log_projector(pr);
      let sort = Piece.sort(syntax) |> fst;
      let seg = Piece.unparenthesize(syntax);
      let skel = Segment.skel(seg);
      let inner = go_s(sort, skel, seg);
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
    | Tile({mold, shards, children, _}) =>
      Aba.aba_triples(Aba.mk(shards, children))
      |> List.map(((l, kid, r)) => {
           let s = l + 1 == r ? List.nth(mold.in_, l) : Sort.Any;
           let skel = Segment.skel(~sort=s, kid);
           go_s(s, skel, kid);
         })
    };

  /* Capture term ranges */
  record_term_data(sort, seg, skel);

  /* --- Hole secondary + ID assignment ---
   * For each Hole ref in the root, compute its secondary by scanning
   * the segment between its boundary tiles. Create a single ID used
   * both for secondary_map storage and for the fake tile entry.
   *
   * Boundary computation for Hole ref at position i in root refs:
   *   Left: i==0 → left_bound; else → right edge of Aba child[i-1]
   *   Right: i==last → right_bound; else → left edge of Aba child[i] */
  let root: Aba.t(Skel.piece_ref, Skel.t) = Skel.root(skel);
  let root_refs = Aba.get_as(root);
  let root_children = Aba.get_bs(root);
  let num_refs = List.length(root_refs);

  let hole_left_boundary = (i: int): int =>
    if (i == 0) {
      left_bound;
    } else {
      let child = List.nth(root_children, i - 1);
      switch (Skel.range(child)) {
      | Some((_, right_edge)) => right_edge
      | None =>
        switch (List.nth(root_refs, i - 1)) {
        | Skel.Piece(idx) => idx
        | Skel.Hole(_) => left_bound
        }
      };
    };

  let hole_right_boundary = (i: int): int =>
    if (i == num_refs - 1) {
      right_bound;
    } else {
      let child = List.nth(root_children, i);
      switch (Skel.range(child)) {
      | Some((left_edge, _)) => left_edge
      | None =>
        switch (List.nth(root_refs, i + 1)) {
        | Skel.Piece(idx) => idx
        | Skel.Hole(_) => right_bound
        }
      };
    };

  /* For each Hole ref, find the segment index of the right-adjacent piece.
   * This is the key used by assign_secondary to stash conflict-boundary
   * secondary in hole_secondary. */
  let right_adjacent_idx = (skel: Skel.t, i: int): int => {
    /* Look rightward through root refs for the first Piece */
    let rec scan_refs = (j: int): option(int) =>
      if (j >= num_refs) {
        None;
      } else {
        switch (List.nth(root_refs, j)) {
        | Skel.Piece(idx) => Some(idx)
        | Skel.Hole(_) =>
          /* Check child skel between j-1 and j */
          if (j > 0 && j - 1 < List.length(root_children)) {
            switch (Skel.range(List.nth(root_children, j - 1))) {
            | Some((left, _)) => Some(left)
            | None => scan_refs(j + 1)
            };
          } else {
            scan_refs(j + 1);
          }
        };
      };
    /* First try scanning within the root */
    switch (scan_refs(i + 1)) {
    | Some(idx) => idx
    | None =>
      /* Nothing in root; look at outer skel's right operand */
      switch (skel) {
      | Bin(_, _, r)
      | Pre(_, r) =>
        switch (Skel.range(r)) {
        | Some((left, _)) => left
        | None => right_bound
        }
      | Op(_)
      | Post(_, _) => right_bound
      }
    };
  };

  /* Create IDs for holes and look up their secondary from hole_secondary. */
  let hole_id_for_ref: ref(list((int, Id.t))) = ref([]);
  List.iteri(
    (i, ref) =>
      switch (ref) {
      | Skel.Hole(_) =>
        let id = Id.mk();
        let key = right_adjacent_idx(skel, i);
        let sec =
          switch (List.assoc_opt(key, hole_secondary^)) {
          | Some(run) => (run, [])
          | None => ([], [])
          };
        secondary_map := Id.Map.add(id, sec, secondary_map^);
        hole_id_for_ref := [(i, id), ...hole_id_for_ref^];
      | Skel.Piece(_) => ()
      },
    root_refs,
  );

  /* Resolve refs to pieces or holes with pre-assigned IDs */
  let resolved_refs =
    List.mapi(
      (i, ref) =>
        switch (ref) {
        | Skel.Piece(idx) => `Piece(List.nth(seg, idx))
        | Skel.Hole(_) => `Hole(List.assoc(i, hole_id_for_ref^))
        },
      root_refs,
    );

  /* Process Aba children (sub-skeletons between chained root refs).
   * Pass bounds so child skeletons know their extent in the segment. */
  let kid_bound_from_ref = (ref_idx: int): int =>
    switch (List.nth(root_refs, ref_idx)) {
    | Skel.Piece(idx) => idx
    | Skel.Hole(_) =>
      /* For a Hole ref, use its boundary that faces the child */
      if (ref_idx < List.length(root_children)) {
        /* This ref is to the LEFT of child[ref_idx], so use right boundary */
        hole_right_boundary(
          ref_idx,
        );
      } else {
        /* This ref is to the RIGHT of child[ref_idx-1], so use left boundary */
        hole_left_boundary(
          ref_idx,
        );
      }
    };

  let sorts_of_ref =
    fun
    | `Piece(p) =>
      switch (Piece.nibs(p)) {
      | Some((l, r)) => (l.sort, r.sort)
      | None => (Sort.Any, Sort.Any)
      }
    | `Hole(_) => (Sort.Any, Sort.Any);

  let processed_children =
    List.mapi(
      (child_idx, child_skel) => {
        /* child_idx is between ref[child_idx] and ref[child_idx + 1] */
        let lb = kid_bound_from_ref(child_idx);
        let rb = kid_bound_from_ref(child_idx + 1);
        let ref_l = List.nth(resolved_refs, child_idx);
        let ref_r = List.nth(resolved_refs, child_idx + 1);
        let (_, s_l) = sorts_of_ref(ref_l);
        let (s_r, _) = sorts_of_ref(ref_r);
        let s = s_l == s_r ? s_l : Sort.Any;
        go_s(~left_bound=lb, ~right_bound=rb, s, child_skel, seg);
      },
      root_children,
    );

  /* Build tiles Aba with processed children */
  let tiles =
    Aba.mk(resolved_refs, processed_children)
    |> Aba.map_a(
         fun
         | `Piece(p) => (Piece.id(p), Aba.mk(tokens(p), tile_kids(p)))
         | `Hole(id) => (id, Aba.mk([" "], [])),
       );

  /* Compute sorts for recursive child processing */
  let (l_sort, r_sort) = {
    let first_ref = List.hd(resolved_refs);
    let last_ref = ListUtil.last(resolved_refs);
    let l_sort =
      switch (first_ref) {
      | `Piece(p) =>
        let (l, _) = Option.get(Piece.nibs(p));
        l.sort;
      | `Hole(_) => Sort.Any
      };
    let r_sort =
      switch (last_ref) {
      | `Piece(p) =>
        let (_, r) = Option.get(Piece.nibs(p));
        r.sort;
      | `Hole(_) => Sort.Any
      };
    (l_sort, r_sort);
  };

  /* Compute root range for child bounds */
  let root_range = {
    let piece_indices =
      List.filter_map(
        fun
        | Skel.Piece(i) => Some(i)
        | Skel.Hole(_) => None,
        root_refs,
      );
    let child_ranges = List.filter_map(Skel.range, root_children);
    let all = List.map(i => (i, i), piece_indices) @ child_ranges;
    switch (all) {
    | [] => None
    | [(l, r), ...rest] =>
      Some(
        List.fold_left(
          ((min_l, max_r), (l, r)) => (min(min_l, l), max(max_r, r)),
          (l, r),
          rest,
        ),
      )
    };
  };

  /* Check for single-hole Op: return Hole directly instead of
   * creating a fake tile entry with is_hole_label */
  switch (skel, resolved_refs) {
  | (Op(_), [`Hole(id)]) => Hole(id)
  | (Op(_), _) => Op(tiles)
  | (Pre(_, r), _) =>
    let lb =
      switch (root_range) {
      | Some((_, rr)) => rr
      | None => left_bound
      };
    Pre(tiles, go_s(~left_bound=lb, ~right_bound, r_sort, r, seg));
  | (Post(l, _), _) =>
    let rb =
      switch (root_range) {
      | Some((rl, _)) => rl
      | None => right_bound
      };
    Post(go_s(~left_bound, ~right_bound=rb, l_sort, l, seg), tiles);
  | (Bin(l, _, r), _) =>
    let l_rb =
      switch (root_range) {
      | Some((rl, _)) => rl
      | None => right_bound
      };
    let r_lb =
      switch (root_range) {
      | Some((_, rr)) => rr
      | None => left_bound
      };
    Bin(
      go_s(~left_bound, ~right_bound=l_rb, l_sort, l, seg),
      tiles,
      go_s(~left_bound=r_lb, ~right_bound, r_sort, r, seg),
    );
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

/* Build reverse map from secondary_map: secondary piece ID → owning term ID */
let build_ws_to_term = (sec_map: secondary_map): Id.Map.t(Id.t) =>
  Id.Map.fold(
    (term_id, (before, after), acc) =>
      List.fold_left(
        (acc, s: Secondary.t) => Id.Map.add(s.id, term_id, acc),
        acc,
        before @ after,
      ),
    sec_map,
    Id.Map.empty,
  );

let go =
  Core.Memo.general(
    ~cache_size_bound=1000,
    seg => {
      map := TermMap.empty;
      term_data := Id.Map.empty;
      projectors := Id.Map.empty;
      projector_list := [];
      adopted_ids := [];
      /* Assign secondary to tiles and stash conflict runs for holes */
      secondary_map := Id.Map.empty;
      hole_secondary := [];
      assign_secondary(seg);
      let skel = Segment.skel(seg);
      let term = exp(unsorted(Exp, skel, seg));
      consolidate_adopted();
      {
        term,
        term_data: term_data^,
        terms: map^,
        projectors: projectors^,
        projector_list: projector_list^,
        ws_to_term: build_ws_to_term(secondary_map^),
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
        secondary_map := Id.Map.empty;
        hole_secondary := [];
        assign_secondary(seg);
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
        /* virtual-grout: holes are represented as structural Any tiles.
         * Keep them as Any rather than defaulting to Exp (dev's fallback)
         * so the hole-detection in CachedSyntax sees them. */
        | Any => Some(Any()) /* hole tiles */
        };
      };
    }
  );

let from_zip_for_sem = (z: Zipper.t, ~root) =>
  go(Dump.to_segment(z, ~root));

let from_zip_for_sem =
  Core.Memo.general(~cache_size_bound=1000, from_zip_for_sem);
