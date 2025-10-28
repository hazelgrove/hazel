open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* CardRenderer - Visual renderer for card and hand expressions in probed values */

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Show
  | Choose
  | Flipped;

[@deriving (show({with_path: false}), sexp, yojson)]
type m = {mode};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | SetMode(mode);

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
type state =
  | Card(card)
  | Hand(hand);

let suit_to_int = (suit: suit): int =>
  switch (suit) {
  | Hearts => 0
  | Clubs => 1
  | Diamonds => 2
  | Spades => 3
  | UnknownS => 4
  };

let rank_to_int = (rank: rank): int =>
  switch (rank) {
  | Two => 1
  | Three => 2
  | Four => 3
  | Five => 4
  | Six => 5
  | Seven => 6
  | Eight => 7
  | Nine => 8
  | Ten => 9
  | Jack => 10
  | Queen => 11
  | King => 12
  | Ace => 13
  | UnknownR => 14
  };

module SyntaxTerm = {
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

  let card_to_exp = ((suit, rank): card): exp =>
    Exp.parens(
      Exp.tuple([
        Exp.constructor(Sexplib.Sexp.to_string(sexp_of_suit(suit)), None),
        Exp.constructor(Sexplib.Sexp.to_string(sexp_of_rank(rank)), None),
      ]),
    );

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

  let any_to_state = (any: Any.t): option(state) => {
    switch (any) {
    | Exp(term) =>
      switch (strip_wraps_exp(term).term) {
      | ListLit(terms) =>
        switch (terms |> List.map(exp_to_card) |> OptUtil.sequence) {
        | Some(cards) => Some(Hand(cards))
        | None => None
        }
      | _ =>
        switch (exp_to_card(term)) {
        | Some(card) => Some(Card(card))
        | None => None
        }
      }
    | _ => None
    };
  };

  let state_to_any = (state: state): Any.t => {
    let collection_to_exp = (c: state): Exp.t =>
      switch (c) {
      | Card(card) => card_to_exp(card)
      | Hand(hand) => Exp.list_lit(List.map(card_to_exp, hand))
      };

    Exp(collection_to_exp(state));
  };

  let put = (info: info, new_state: state): option(Base.segment) =>
    info.utility.lift_syntax(_ => state_to_any(new_state), info.syntax);
};

module Card = {
  let width = 35;
  let height = 47;

  let card_to_offset = (_sort: Sort.t, (suit, rank): card): (int, int) => (
    rank_to_int(rank) * width,
    suit_to_int(suit) * height,
  );

  let background_offset = (~flipped, sort: Sort.t, card: card): Css_gen.t => {
    let (offset_x, offset_y) =
      flipped ? (0, height) : card_to_offset(sort, card);
    Css_gen.create(
      ~field="background-position",
      ~value=Printf.sprintf("%dpx %dpx", - offset_x, - offset_y),
    );
  };

  let side = (sort: Sort.t, card: card, ~flipped: bool, clss: string): Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["card-sprite", clss, Sort.show(sort)]),
        Attr.style(background_offset(~flipped, sort, card)),
      ],
      [],
    );

  let view = (sort: Sort.t, card: card) =>
    Node.div(
      ~attrs=[Attr.classes(["card-scene", Sort.show(sort)])],
      [
        side(sort, card, ~flipped=false, "front"),
        side(sort, card, ~flipped=true, "back"),
      ],
    );
};

module Chooser = {
  let col_width = 8;
  let row_height = 14;

  let grid =
    [
      [Hearts, Spades, Diamonds, Clubs]
      |> List.map(suit =>
           List.map(
             rank => (suit, rank),
             [
               Two,
               Three,
               Four,
               Five,
               Six,
               Seven,
               Eight,
               Nine,
               Ten,
               Jack,
               Queen,
               King,
               Ace,
             ],
           )
         )
      |> List.flatten,
      [UnknownS]
      |> List.map(suit =>
           List.map(
             rank => (suit, rank),
             [
               Two,
               Three,
               Four,
               Five,
               Six,
               Seven,
               Eight,
               Nine,
               Ten,
               Jack,
               Queen,
               King,
               Ace,
               UnknownR,
             ],
           )
         )
      |> List.flatten,
    ]
    |> List.map(row => List.map(card => card, row));

  let replace_card = (info: info, parent, card: card, _) => {
    switch (SyntaxTerm.put(info, Card(card))) {
    | Some(seg) => parent(SetSyntax(seg))
    | None => Effect.Ignore
    };
  };

  let card_pos = (col: int, row: int) =>
    Attr.create(
      "style",
      Printf.sprintf(
        "position: absolute; left: %dpx; top: %dpx;",
        col * col_width,
        row * row_height,
      ),
    );

  let card_wrapper =
      (replace_card, sort: Sort.t, col: int, row: int, card: card) =>
    Node.div(
      ~attrs=[Attr.on_mousedown(replace_card(card)), card_pos(col, row)],
      [Card.view(sort, card)],
    );

  let view = (info, parent, sort: Sort.t, current_card: card) =>
    Node.div(
      ~attrs=[Attr.classes(["chooser"])],
      List.mapi(
        (r, row) =>
          List.mapi(
            (col, c) =>
              card_wrapper(replace_card(info, parent), sort, col, r, c),
            row,
          ),
        grid,
      )
      |> List.concat,
    );
};

module Singleton = {
  let view = (info, mode, parent, sort: Sort.t, card: card, local) =>
    Node.div(
      ~attrs=[
        Attr.classes([
          "projector",
          "card",
          Sort.show(sort),
          switch (mode) {
          | Show => "show"
          | Flipped => "flipped"
          | Choose => "choose"
          },
        ]),
      ],
      [
        Node.div(
          ~attrs=[Attr.classes(["card-wrapper"])],
          [
            Card.view(sort, card),
            switch (mode) {
            | Choose => Chooser.view(info, parent, sort, card)
            | _ => Node.div([])
            },
          ],
        ),
      ],
    );
};

module Hand = {
  let view = (info, mode, parent, sort: Sort.t, hand: hand, local) =>
    Node.div(
      ~attrs=[
        Attr.classes([
          "projector",
          "card",
          Sort.show(sort),
          switch (mode) {
          | Show => "show"
          | Choose => "choose"
          | Flipped => "flipped"
          },
        ]),
      ],
      [
        Node.div(
          ~attrs=[Attr.classes(["hand"])],
          List.mapi(
            (i, card) =>
              Node.div(
                ~attrs=[
                  Attr.classes([
                    "card-wrapper",
                    switch (mode, i == 0) {
                    | (Choose, true) => "choose"
                    | (Choose, false) => "show"
                    | (Show, _) => "show"
                    | (Flipped, _) => "flipped"
                    },
                  ]),
                ],
                [Card.view(sort, card)],
              ),
            hand,
          ),
        ),
      ],
    );
};

let render =
    (
      ~info: info,
      ~exp: Exp.t,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: m,
      ~local: a => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      (),
    ) =>
  switch (SyntaxTerm.any_to_state(Exp(exp))) {
  | Some(Card(card)) =>
    Singleton.view(info, model.mode, parent, Sort.Exp, card, local)
  | Some(Hand(hand)) =>
    Hand.view(info, model.mode, parent, Sort.Exp, hand, local)
  | None => Node.text("Invalid card/hand")
  };

let update: (m, a) => m =
  (model, action) =>
    switch (action) {
    | SetMode(new_mode) => {mode: new_mode}
    };

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["card-badge"]),
      Attr.title("Click to view cards visually"),
    ],
    [Node.text("♠️")],
  );

let init = (exp: Exp.t) =>
  switch (SyntaxTerm.any_to_state(Exp(exp))) {
  | Some(_) => Some({mode: Show})
  | None => None
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;

[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;
