open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type suit =
  | Unknown(Piece.t)
  | Hearts
  | Diamonds
  | Clubs
  | Spades;

[@deriving (show({with_path: false}), sexp, yojson)]
type rank =
  | Unknown(Piece.t)
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
type syntax =
  | Card(card)
  | Hand(hand);

module Syntax = {
  let suit_of_piece = (p: Piece.t): suit =>
    switch (p) {
    | Tile({label: [str], _}) =>
      switch (str |> Sexplib.Sexp.of_string |> suit_of_sexp) {
      | exception _ => Unknown(p)
      | s => s
      }
    | _ => Unknown(p)
    };

  let rank_of_piece = (p: Piece.t): rank =>
    switch (p) {
    | Tile({label: [str], _}) =>
      switch (str |> Sexplib.Sexp.of_string |> rank_of_sexp) {
      | exception _ => Unknown(p)
      | r => r
      }
    | _ => Unknown(p)
    };

  let rm_secondary = (segment: Segment.t): Segment.t =>
    List.filter(p => !Piece.is_secondary(p), segment);

  let piece_to_card = (piece: Piece.t): option(card) =>
    switch (piece) {
    | Tile({label: ["(", ")"], children: [segment], _}) =>
      switch (rm_secondary(segment)) {
      | [left_child, Tile({label: [","], _}), right_child] =>
        Some((suit_of_piece(left_child), rank_of_piece(right_child)))
      | _ => None
      }
    | _ => None
    };

  let piece_to_hand = (piece: Piece.t): option(hand) => {
    switch (piece) {
    | Tile({label: ["[", "]"], children: [segment], _}) =>
      segment |> rm_secondary |> List.filter_map(piece_to_card) |> Option.some
    | _ => None
    };
  };

  let piece_to_syntax = (piece: Piece.t): option(syntax) =>
    switch (piece_to_hand(piece)) {
    | Some(hand) => Some(Hand(hand))
    | None =>
      open OptUtil.Syntax;
      let+ card = piece_to_card(piece);
      Card(card);
    };

  let mk_tuple = (children): Piece.t =>
    Tile({
      id: Id.mk(),
      label: ["(", ")"],
      mold: Mold.mk_op(Sort.Exp, [Exp]),
      shards: [0],
      children: [children],
    });

  let mk_text = (str): Piece.t =>
    Tile({
      id: Id.mk(),
      label: [str],
      mold: Mold.mk_op(Sort.Exp, []),
      shards: [0],
      children: [],
    });

  let piece_of_suit = (suit: suit): Piece.t =>
    switch (suit) {
    | Unknown(p) => p
    | _ => suit |> sexp_of_suit |> Sexplib.Sexp.to_string |> mk_text
    };

  let piece_of_rank = (rank: rank) =>
    switch (rank) {
    | Unknown(p) => p
    | _ => rank |> sexp_of_rank |> Sexplib.Sexp.to_string |> mk_text
    };

  let card_to_piece = ((suit, rank): card): Piece.t =>
    mk_tuple([
      piece_of_suit(suit),
      Piece.mk_tile(Form.get("comma_exp"), []),
      piece_of_rank(rank),
    ]);

  let hand_to_piece = (hand: hand): Piece.t =>
    mk_tuple(List.map(card_to_piece, hand));

  let syntax_to_piece = (syntax: syntax): Piece.t =>
    switch (syntax) {
    | Card(card) => card_to_piece(card)
    | Hand(hand) => hand_to_piece(hand)
    };

  let put = syntax_to_piece;

  let get_opt = piece_to_syntax;

  let get = (piece: Piece.t): syntax =>
    switch (get_opt(piece)) {
    | None => failwith("ERROR: Card: Not card or hand")
    | Some(syntax) => syntax
    };

  let width_of_syntax = (syntax: syntax): int =>
    switch (syntax) {
    | Card(_) => 1
    | Hand(hand) => List.length(hand)
    };

  let width_of_piece = (piece: Piece.t): int =>
    switch (piece_to_syntax(piece)) {
    | None => 0
    | Some(Card(_)) => 4
    | Some(Hand(hand)) => 4 + List.length(hand) / 2
    };
};

let suit_to_int = (suit: suit): int =>
  switch (suit) {
  | Hearts => 0
  | Clubs => 1
  | Diamonds => 2
  | Spades => 3
  | Unknown(_) => 0
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
  | Unknown(_) => 0
  };

module Card = {
  /* Card images are stored in a spritesheet. The sheet image
   * has four rows (hearts, clubs, diamonds, spades) and 14
   * columns (first is misc, then 2-10, then J Q K A) */

  let width = 35; /* Width of each card in pixels */
  let height = 47; /* Height of each card in pixels */

  let card_to_offset = ((suit, rank): card): (int, int) => (
    rank_to_int(rank) * width,
    suit_to_int(suit) * height,
  );

  let background_offset = (card: card): Css_gen.t => {
    let (offset_x, offset_y) = card_to_offset(card);
    Css_gen.create(
      ~field="background-position",
      ~value=Printf.sprintf("%dpx %dpx", - offset_x, - offset_y),
    );
  };

  let view = (card: card): Node.t =>
    Node.div(
      ~attrs=[
        Attr.class_("card-sprite"),
        Attr.style(background_offset(card)),
      ],
      [],
    );
};

module Hand = {
  // a card, but each subsequent card should be absoluted positioned 20px to the right of the last and higher in z-index:
  let card_wrapper = (index: int, card: card): Node.t =>
    Node.div(
      ~attrs=[
        Attr.class_("card-wrapper"),
        Attr.create(
          "style",
          Printf.sprintf(
            "position: absolute; left: %dpx; z-index: %d;",
            index * 8,
            100 + index,
          ),
        ),
      ],
      [Card.view(card)],
    );

  let view = (hand: hand): Node.t => {
    Node.div(~attrs=[Attr.class_("hand")], List.mapi(card_wrapper, hand));
  };
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = p => Syntax.get_opt(p) != None;
  let can_focus = false;
  let dynamics = false;
  let placeholder = (_, info) => Inline(Syntax.width_of_piece(info.syntax));
  let update = (model, _) => model;
  let view =
      (
        _,
        ~info,
        ~local as _,
        ~parent as _: external_action => Ui_effect.t(unit),
        ~utility as _,
      ) => {
    switch (Syntax.get(info.syntax)) {
    | Card(card) => Card.view(card)
    | Hand(hand) => Hand.view(hand)
    };
  };
  let focus = _ => ();
};
