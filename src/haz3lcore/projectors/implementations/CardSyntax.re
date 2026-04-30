open Util;
open Language;
open OptUtil.Syntax;
open CardTypes;

let rec strip_wraps_exp = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(inner) =>
    switch (inner.term) {
    | Tuple(_) => e
    | _ => strip_wraps_exp(inner)
    }
  | _ => e
  };

let rec strip_wraps_pat = (p: Pat.t): Pat.t =>
  switch (p.term) {
  | Parens(inner) =>
    switch (inner.term) {
    | Tuple(_) => p
    | _ => strip_wraps_pat(inner)
    }
  | _ => p
  };

let string_to_suit = (s: string): option(suit) =>
  switch (s |> Sexplib.Sexp.of_string |> suit_of_sexp) {
  | s => Some(s)
  | exception _ => None
  };

let string_to_rank = (s: string): option(rank) =>
  switch (s |> Sexplib.Sexp.of_string |> rank_of_sexp) {
  | r => Some(r)
  | exception _ => None
  };

let rec exp_to_card = (term: Exp.t): option(card) =>
  switch (term.term) {
  | Parens(inner) => exp_to_card(inner)
  | Tuple([t1, t2]) =>
    switch (t1.term, t2.term) {
    | (Constructor(suit, _), Constructor(rank, _)) =>
      switch (string_to_suit(suit), string_to_rank(rank)) {
      | (Some(s), Some(r)) => Some((s, r))
      | _ => None
      }
    | _ => None
    }
  | _ => None
  };

let rec pat_to_card = (term: Pat.t): option(card) =>
  switch (term.term) {
  | Parens(p) => pat_to_card(p)
  | Tuple([p1, p2]) =>
    switch (p1.term, p2.term) {
    | (Constructor(suit, _), Constructor(rank, _)) =>
      let* s = string_to_suit(suit);
      let* r = string_to_rank(rank);
      Some((s, r));
    | (Constructor(suit, _), Wild) =>
      let* s = string_to_suit(suit);
      Some((s, UnknownR));
    | (Wild, Constructor(rank, _)) =>
      let* r = string_to_rank(rank);
      Some((UnknownS, r));
    | (Wild, Wild) => Some((UnknownS, UnknownR))
    | _ => None
    }
  | _ => None
  };

let any_to_state = (term: Any.t): option(state) =>
  switch (term) {
  | Exp(term) =>
    switch (strip_wraps_exp(term).term) {
    | ListLit(terms) =>
      let+ cards = terms |> List.map(exp_to_card) |> OptUtil.sequence;
      (Exp, Hand(cards));
    | _ =>
      let+ card = exp_to_card(term);
      (Exp, Card(card));
    }
  | Pat(term) =>
    switch (strip_wraps_pat(term).term) {
    | ListLit(terms) =>
      let+ cards = terms |> List.map(pat_to_card) |> OptUtil.sequence;
      (Pat, Hand(cards));
    | _ =>
      let+ card = pat_to_card(term);
      (Pat, Card(card));
    }
  | _ => None
  };

open IdTagged.FreshGrammar;

/* Double-wrap the tuple in Parens. ProjectorPerform.SetSyntax calls
   Segment.unparenthesize on whatever segment we hand it, stripping a
   single outer paren level before re-wrapping for the projector
   delimiter. With only one Parens here that strip leaves a bare
   `Hearts, Ace` inside the projector — a non-parenthesized top-level
   tuple — which downstream `seg_to_term` (`MakeTerm.for_projection`)
   rejects, crashing the card render the next time the projector
   re-evaluates. The extra Parens absorbs the strip so the projector's
   content stays `(Hearts, Ace)` and round-trips cleanly. */
let card_to_exp = ((suit, rank): card): exp =>
  Exp.parens(
    Exp.parens(
      Exp.tuple([
        Exp.constructor(Sexplib.Sexp.to_string(sexp_of_suit(suit)), None),
        Exp.constructor(Sexplib.Sexp.to_string(sexp_of_rank(rank)), None),
      ]),
    ),
  );

let card_to_pat = ((suit, rank): card): pat =>
  Pat.parens(
    Pat.parens(
      Pat.tuple([
        switch (suit) {
        | UnknownS => Pat.wild()
        | _ =>
          Pat.constructor(Sexplib.Sexp.to_string(sexp_of_suit(suit)), None)
        },
        switch (rank) {
        | UnknownR => Pat.wild()
        | _ =>
          Pat.constructor(Sexplib.Sexp.to_string(sexp_of_rank(rank)), None)
        },
      ]),
    ),
  );

let collection_to_exp = (collection: collection): exp =>
  switch (collection) {
  | Card(card) => card_to_exp(card)
  | Hand(hand) => Exp.list_lit(List.map(card_to_exp, hand))
  };

let collection_to_pat = (collection: collection): pat =>
  switch (collection) {
  | Card(card) => card_to_pat(card)
  | Hand(hand) => Pat.list_lit(List.map(card_to_pat, hand))
  };

let state_to_any = ((sort, collection): state): Any.t =>
  switch (sort) {
  | Exp => Exp(collection_to_exp(collection))
  | Pat => Pat(collection_to_pat(collection))
  };
