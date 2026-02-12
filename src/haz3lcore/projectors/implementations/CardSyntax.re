open Language;
open IdTagged.FreshGrammar;

let string_to_suit = (s: string): option(CardTypes.suit) =>
  switch (s |> Sexplib.Sexp.of_string |> CardTypes.suit_of_sexp) {
  | s => Some(s)
  | exception _ => None
  };

let string_to_rank = (s: string): option(CardTypes.rank) =>
  switch (s |> Sexplib.Sexp.of_string |> CardTypes.rank_of_sexp) {
  | r => Some(r)
  | exception _ => None
  };

let card_to_exp = ((suit, rank): CardTypes.card): Exp.t =>
  Exp.parens(
    Exp.tuple([
      Exp.constructor(
        Sexplib.Sexp.to_string(CardTypes.sexp_of_suit(suit)),
        None,
      ),
      Exp.constructor(
        Sexplib.Sexp.to_string(CardTypes.sexp_of_rank(rank)),
        None,
      ),
    ]),
  );

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

let rec exp_to_card = (term: Exp.t): option(CardTypes.card) => {
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
