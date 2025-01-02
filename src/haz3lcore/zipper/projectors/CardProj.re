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
type syntax =
  | Card(card)
  | Hand(list(card));

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

  let mk_tuple = (children): Piece.t =>
    Tile({
      id: Id.mk(),
      label: ["(", ")"],
      mold: Mold.mk_op(Sort.Exp, [Exp]),
      shards: [0],
      children: [children],
    });

  let piece_to_card = (piece: Piece.t): option(card) => {
    //TODO: generalize this or use Term
    switch (piece) {
    | Tile({
        label: ["(", ")"],
        children: [[left_child, Tile({label: [","], _}), right_child]],
        _,
      })
    | Tile({
        label: ["(", ")"],
        children:
          [
            [left_child, Tile({label: [","], _}), Secondary(_), right_child],
          ],
        _,
      }) =>
      Some((suit_of_piece(left_child), rank_of_piece(right_child)))
    | _ => None
    };
  };

  let card_to_piece = ((suit, rank): card): Piece.t =>
    mk_tuple([
      piece_of_suit(suit),
      Piece.mk_tile(Form.get("comma_exp"), []),
      piece_of_rank(rank),
    ]);

  let put = card_to_piece;

  let get_opt = piece_to_card;

  let get = (piece: Piece.t): card =>
    switch (get_opt(piece)) {
    | None => failwith("ERROR: Card: not integer literal")
    | Some(card) => card
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

  let view = (info: info): Node.t => {
    let card = Syntax.get(info.syntax);
    Node.div(
      ~attrs=[
        Attr.class_("card-sprite"),
        Attr.style(background_offset(card)),
      ],
      [],
    );
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
  let placeholder = (_, _) => Inline(4);
  let update = (model, _) => model;
  let view =
      (
        _,
        ~info,
        ~local as _,
        ~parent as _: external_action => Ui_effect.t(unit),
        ~utility as _,
      ) =>
    Card.view(info);
  let focus = _ => ();
};
