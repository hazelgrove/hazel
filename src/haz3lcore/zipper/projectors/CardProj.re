open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type suit =
  | Unknown
  | Hearts
  | Diamonds
  | Clubs
  | Spades;

[@deriving (show({with_path: false}), sexp, yojson)]
type rank =
  | Unknown
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

let string_to_suit = (str): suit =>
  switch (str |> Sexplib.Sexp.of_string |> suit_of_sexp) {
  | exception _ => Unknown
  | s => s
  };

// Helper to convert string to rank
let string_to_rank = (str): rank =>
  switch (str |> Sexplib.Sexp.of_string |> rank_of_sexp) {
  | exception _ => Unknown
  | r => r
  };

let piece_to_card = (piece: Piece.t): option(card) => {
  // Helper to convert string to suit (used sexp_to_suit)
  // Look for constructor application pattern in segment
  print_endline("piece_to_card: " ++ (piece |> Piece.show));
  switch (piece) {
  | Tile({
      label: ["(", ")"],
      children:
        [
          [
            Tile({label: suit_label, _}),
            Tile({label: [","], _}),
            Tile({label: rank_label, _}),
          ],
        ],
      _,
    })
  | Tile({
      label: ["(", ")"],
      children:
        [
          [
            Tile({label: suit_label, _}),
            Tile({label: [","], _}),
            Secondary(_),
            Tile({label: rank_label, _}),
          ],
        ],
      _,
    }) =>
    let suit =
      switch (suit_label) {
      | [suit_name] => string_to_suit(suit_name)
      | _ => Unknown
      };
    let rank =
      switch (rank_label) {
      | [rank_name] => string_to_rank(rank_name)
      | _ => Unknown
      };
    Some((suit, rank));
  | _ => None
  };
};

let suit_to_string = suit => suit |> sexp_of_suit |> Sexplib.Sexp.to_string;

let rank_to_string = rank => rank |> sexp_of_rank |> Sexplib.Sexp.to_string;

let card_to_piece = ((suit, rank): card): Piece.t => {
  // Create a tuple piece with the suit and rank
  let mk_text = (str): Piece.t =>
    Tile({
      id: Id.mk(),
      label: [str],
      mold: Mold.mk_op(Sort.Exp, []),
      shards: [0],
      children: [],
    });

  let mk_tuple = (children): Piece.t =>
    Tile({
      id: Id.mk(),
      label: ["(", ")"],
      mold: Mold.mk_op(Sort.Exp, [Exp]),
      shards: [0],
      children: [children],
    });

  mk_tuple([
    mk_text(suit_to_string(suit)),
    mk_text(","),
    mk_text(rank_to_string(rank)),
  ]);
};

let suit_to_int = (suit: suit): int =>
  switch (suit) {
  | Hearts => 0
  | Clubs => 1
  | Diamonds => 2
  | Spades => 3
  | Unknown => 0
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
  | Unknown => 0
  };

/* card images are stored in a single pixel sheet. this
 * returns two ints representing the pixel offset of cards
 * declare constants for W and H of each card; the image
 has four rows (hears, clubs, diamonds, spades) and 14
 columns (first is misc, then 2 thru 10, the J Q K A) */
let card_to_offset = (card: card): (int, int) => {
  let width = 35;
  let height = 47;
  let (suit, rank) = card;
  let row = suit |> suit_to_int;
  let col = rank |> rank_to_int;
  print_endline(
    "row/col: " ++ string_of_int(row) ++ "/" ++ string_of_int(col),
  );
  (col * width, row * height);
};

let view_card = (card: card): Node.t => {
  let (offset_x, offset_y) = card_to_offset(card);
  Node.div(
    ~attrs=[
      Attr.class_("card-sprite"),
      Attr.style(
        Css_gen.create(
          ~field="background-position",
          ~value=
            string_of_int(- offset_x)
            ++ "px "
            ++ string_of_int(- offset_y)
            ++ "px",
        ),
      ),
    ],
    [],
  );
};

let put = card_to_piece;

let get_opt = piece_to_card;

let get = (piece: Piece.t): card =>
  switch (get_opt(piece)) {
  | None => failwith("ERROR: Card: not integer literal")
  | Some(card) => card
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = p => get_opt(p) != None;
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
      ) => {
    let (suit, rank) = get(info.syntax);
    view_card((suit, rank));
  };
  let focus = _ => ();
};
