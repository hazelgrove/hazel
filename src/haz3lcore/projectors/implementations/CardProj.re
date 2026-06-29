open Util;
open ProjectorBase;
open Language;

/* Card projector logic: projects playing-card expressions/patterns as
   graphical cards. The model/action and syntax-domain types live at
   file level (outside the sealed module below) so that the web view
   (CardProjView) can reuse them. */

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Show
  | Choose(int)
  | Flipped;

[@deriving (show({with_path: false}), sexp, yojson)]
type model = {mode};
[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | SetMode(mode);

let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
  switch (model_of_sexp(sexp)) {
  | exception _ => {mode: Show}
  | m => m
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type suit =
  | UnknownS
  | Hearts
  | Diamonds
  | Clubs
  | Spades;

[@deriving (show({with_path: false}), sexp, yojson)]
type rank =
  | UnknownR
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King;

[@deriving (show({with_path: false}), sexp, yojson)]
type card = (suit, rank);

[@deriving (show({with_path: false}), sexp, yojson)]
type hand = list(card);

[@deriving (show({with_path: false}), sexp, yojson)]
type collection =
  | Card(card)
  | Hand(hand);

[@deriving (show({with_path: false}), sexp, yojson)]
type sort =
  | Exp
  | Pat;

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (sort, collection);

[@deriving (show({with_path: false}), sexp, yojson)]
type update =
  | ReplaceCard(card)
  | ReplaceCardInHand(int, card);

let update = ((sort, collection): state, update: update): state =>
  switch (update) {
  | ReplaceCard(new_card) =>
    switch (collection) {
    | Card(_) => (sort, Card(new_card))
    | Hand(_) => (sort, collection)
    }
  | ReplaceCardInHand(i, card) =>
    switch (collection) {
    | Card(_) => (sort, collection)
    | Hand(hand) => (sort, Hand(ListUtil.update_nth(i, hand, _ => card)))
    }
  };

let sort_of = (sort: Sort.t): sort =>
  switch (sort) {
  | Sort.Exp => Exp
  | Sort.Pat => Pat
  | _ => Exp
  };

let to_sort = (sort: sort): Sort.t =>
  switch (sort) {
  | Exp => Sort.Exp
  | Pat => Sort.Pat
  };

module SyntaxTerm = {
  let rec strip_wraps_pat = (p: Pat.t): Pat.t => {
    switch (p.term) {
    | Parens(inner) =>
      switch (inner.term) {
      | Tuple(_) => p
      | _ => strip_wraps_pat(inner)
      }
    | _ => p
    };
  };
  let rec strip_wraps_exp = (e: Exp.t): Exp.t => {
    switch (e.term) {
    | Parens(inner) =>
      switch (inner.term) {
      | Tuple(_) => e
      | _ => strip_wraps_exp(inner)
      }
    | _ => e
    };
  };

  open IdTagged.FreshGrammar;
  open OptUtil.Syntax;

  let card_to_exp = ((suit, rank): card): exp =>
    Exp.parens(
      Exp.tuple([
        Exp.constructor(Sexplib.Sexp.to_string(sexp_of_suit(suit)), None),
        Exp.constructor(Sexplib.Sexp.to_string(sexp_of_rank(rank)), None),
      ]),
    );

  let card_to_pat = ((suit, rank): card): pat =>
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
    );

  let syntax_to_any = ((sort, collection): state): Any.t => {
    let collection_to_exp = (collection: collection): Exp.t =>
      switch (collection) {
      | Card(card) => card_to_exp(card)
      | Hand(hand) => Exp.list_lit(List.map(card_to_exp, hand))
      };

    let collection_to_pat = (collection: collection): pat =>
      switch (collection) {
      | Card(card) => card_to_pat(card)
      | Hand(hand) => Pat.list_lit(List.map(card_to_pat, hand))
      };

    switch (sort) {
    | Exp => Exp(collection_to_exp(collection))
    | Pat => Pat(collection_to_pat(collection))
    };
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

  let rec exp_to_card = (term: Exp.t): option(card) => {
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
  };

  let rec pat_to_card = (term: pat): option(card) => {
    switch (term.term) {
    | Parens(pat) => pat |> pat_to_card
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
  };

  let any_to_syntax = (term: Any.t): option(state) => {
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
  };

  let put = (info, syntax): option(Base.segment) =>
    info.utility.lift_syntax(
      ~inline=true,
      _ => syntax_to_any(syntax),
      info.syntax,
    );

  let get_opt = (any: Any.t): option(state) =>
    switch (any |> any_to_syntax) {
    | Some(syntax) => Some(syntax)
    | None => None
    };

  let get = (info: info): state =>
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(syntax) =>
      switch (get_opt(syntax)) {
      | Some(syntax) => syntax
      | None => failwith("Cards: Get: not cards")
      }
    | None => failwith("Cards: Get: seg_to_term ")
    };

  let width_of_syntax = (syntax: state): int =>
    switch (syntax) {
    | (_, Card(_)) => 1
    | (_, Hand(hand)) => List.length(hand)
    };

  let width_of_any = (info: info): int =>
    switch (
      info.syntax
      |> info.utility.seg_to_term
      |> OptUtil.and_then(any_to_syntax)
    ) {
    | None => 0
    | Some((_, Card(_)))
    | Some((_, Hand([_]))) => 4
    | Some((_, Hand(hand))) =>
      Float.ceil(
        3.5 +. 81. /. 100. *. (Float.of_int(List.length(hand)) -. 1.),
      )
      |> Float.to_int
    };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type m = model;
[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector with type model = m and type action = a = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = m;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;
  let dynamics = false;
  let elaborate_syntax = false;

  let init = (info: TermBase.Any.t): option(model) =>
    SyntaxTerm.get_opt(info) != None ? Some({mode: Show}) : None;

  let placeholder = (_, info): ProjectorCore.Shape.t => {
    horizontal: SyntaxTerm.width_of_any(info),
    vertical: Tab(1),
  };

  let update = (_model, _, action) =>
    switch (action) {
    | SetMode(mode) => {mode: mode}
    };

  let error = (_, _): option(ProjectorBase.error) => None;
  let resolve = _ => None;
  let expand = (_, _) => None;
};
