open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Show
  | Choose
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
type collection =
  | Card(card)
  | Hand(hand);

[@deriving (show({with_path: false}), sexp, yojson)]
type sort =
  | Exp
  | Pat;

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = (sort, collection);

let sort_of = (sort: Sort.t): sort =>
  switch (sort) {
  | Sort.Exp => Exp
  | Sort.Pat => Pat
  | _ => failwith("ERROR: Card: Invalid sort")
  };

let to_sort = (sort: sort): Sort.t =>
  switch (sort) {
  | Exp => Sort.Exp
  | Pat => Sort.Pat
  };

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

  let piece_to_syntax = (piece: Piece.t): option(syntax) => {
    let sort = piece |> Piece.sort |> fst |> sort_of;
    switch (piece_to_hand(piece)) {
    | Some(hand) => Some((sort, Hand(hand)))
    | None =>
      open OptUtil.Syntax;
      let+ card = piece_to_card(piece);
      (sort, Card(card));
    };
  };

  let mk_tuple = (sort: Sort.t, children): Piece.t =>
    Tile({
      id: Id.mk(),
      label: ["(", ")"],
      mold: Mold.mk_op(sort, [sort]),
      shards: [0, 1],
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

  let card_to_piece_exp = ((suit, rank): card): Piece.t =>
    mk_tuple(
      Sort.Exp,
      [
        piece_of_suit(suit),
        Piece.mk_tile(Form.get("comma_exp"), []),
        piece_of_rank(rank),
      ],
    );

  let card_to_piece_pat = ((suit, rank): card): Piece.t =>
    mk_tuple(
      Sort.Pat,
      [
        piece_of_suit(suit),
        Piece.mk_tile(Form.get("comma_pat"), []),
        piece_of_rank(rank),
      ],
    );

  let hand_to_piece_exp = (hand: hand): Piece.t =>
    mk_tuple(Sort.Exp, List.map(card_to_piece_exp, hand));

  let hand_to_piece_pat = (hand: hand): Piece.t =>
    mk_tuple(Sort.Pat, List.map(card_to_piece_pat, hand));

  let syntax_to_piece = (syntax: syntax): Piece.t =>
    switch (syntax) {
    | (Exp, Card(card)) => card_to_piece_exp(card)
    | (Pat, Card(card)) => card_to_piece_pat(card)
    | (Exp, Hand(hand)) => hand_to_piece_exp(hand)
    | (Pat, Hand(hand)) => hand_to_piece_pat(hand)
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
    | (_, Card(_)) => 1
    | (_, Hand(hand)) => List.length(hand)
    };

  let width_of_piece = (piece: Piece.t): int =>
    switch (piece_to_syntax(piece)) {
    | None => 0
    | Some((_, Card(_)))
    | Some((_, Hand([_]))) => 4
    | Some((_, Hand(hand))) =>
      //TODO: Better formula / card dimensions / offset
      4 + List.length(hand) - (List.length(hand) + 66) / 24
    };
};

let suit_to_int = (suit: suit): int =>
  switch (suit) {
  | Hearts => 0
  | Clubs => 1
  | Diamonds => 2
  | Spades => 3
  | Unknown(_) => 4
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
  | Unknown(_) => 14
  };

module Card = {
  /* Card images are stored in a spritesheet. The sheet image
   * has four rows (hearts, clubs, diamonds, spades) and 14
   * columns (first is misc, then 2-10, then J Q K A) */

  let width = 35; /* Width of each card in pixels */
  let height = 47; /* Height of each card in pixels */

  let card_to_offset = (_sort: Sort.t, (suit, rank): card): (int, int) => (
    rank_to_int(rank) * width,
    suit_to_int(suit) * height,
  );

  let background_offset = (~flipped, sort: Sort.t, card: card): Css_gen.t => {
    let (offset_x, offset_y) =
      flipped
        ? switch (sort_of(sort)) {
          | Exp => (0, 0)
          | Pat => (0, height)
          }
        : card_to_offset(sort, card);
    Css_gen.create(
      ~field="background-position",
      ~value=Printf.sprintf("%dpx %dpx", - offset_x, - offset_y),
    );
  };

  let view = (sort: Sort.t, card: card): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["card-scene", Sort.show(sort)])],
      [
        Node.div(
          ~attrs=[
            Attr.classes(["card-sprite", "front", Sort.show(sort)]),
            Attr.style(background_offset(~flipped=false, sort, card)),
          ],
          [],
        ),
        Node.div(
          ~attrs=[
            Attr.classes(["card-sprite", "back", Sort.show(sort)]),
            Attr.style(background_offset(~flipped=true, sort, card)),
          ],
          [],
        ),
      ],
    );
};

module Chooser = {
  let col_width = 8;
  let row_height = 14;

  let grid = (sort: sort): list(list(card)) => {
    let maybe_rank =
      switch (sort) {
      | Exp => []
      | Pat => [Unknown(Syntax.mk_text("_"))]
      };
    let maybe_suit: list(suit) =
      switch (sort) {
      | Exp => []
      | Pat => [Unknown(Syntax.mk_text("_"))]
      };
    let suits: list(suit) = [Hearts, Spades, Diamonds, Clubs] @ maybe_suit;
    let ranks: list(rank) =
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
      ]
      @ maybe_rank;
    List.map(
      (suit: suit) => List.map((rank: rank) => (suit, rank), ranks),
      suits,
    );
  };

  let card_wrapper =
      (~indicated, parent, sort: Sort.t, col: int, row: int, card: card)
      : Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["card-wrapper"] @ (indicated ? ["indicated"] : [])),
        Attr.on_click(_ =>
          parent(SetSyntax(Syntax.put((sort_of(sort), Card(card)))))
        ),
        Attr.create(
          "style",
          Printf.sprintf(
            "position: absolute; left: %dpx; top: %dpx; z-index: %d;",
            col * col_width,
            row * row_height,
            100 + row + col,
          ),
        ),
      ],
      [Card.view(sort, card)],
    );

  let view = (parent, sort: Sort.t, card: card): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["chooser", Sort.show(sort)])],
      List.mapi(
        (r, row) =>
          List.mapi(
            (col, c) =>
              card_wrapper(parent, ~indicated=c == card, sort, col, r, c),
            row,
          ),
        grid(sort_of(sort)),
      )
      |> List.concat,
    );
};

module Hand = {
  // a card, but each subsequent card should be absoluted positioned 20px to the right of the last and higher in z-index:
  let card_wrapper = (sort: Sort.t, index: int, card: card): Node.t =>
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
      [Card.view(sort, card)],
    );

  let view = (sort: Sort.t, hand: hand): Node.t => {
    Node.div(
      ~attrs=[Attr.classes(["hand", Sort.show(sort)])],
      List.mapi(card_wrapper(sort), hand),
    );
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type m = model;
[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = m;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;
  let init: model = {mode: Show};
  let can_project = p => Syntax.get_opt(p) != None;
  let can_focus = false;
  let dynamics = false;
  let placeholder = (_, info) => Inline(Syntax.width_of_piece(info.syntax));
  let update = (_model, action) =>
    switch (action) {
    | SetMode(mode) => {mode: mode}
    };
  let view =
      (
        model,
        ~info,
        ~local,
        ~parent: external_action => Ui_effect.t(unit),
        ~utility as _,
      ) => {
    switch (Syntax.get(info.syntax)) {
    | (sort, Card(card)) =>
      Node.div(
        ~attrs=[
          Attr.classes(
            ["outer"]
            @ (
              switch (model.mode) {
              | Show => []
              | Choose => ["choose"]
              | Flipped => ["flipped"]
              }
            ),
          ),
          Attr.on_click(evt =>
            switch (JsUtil.is_double_click(evt)) {
            | false =>
              switch (model.mode) {
              | Show => local(SetMode(Flipped))
              | Flipped => local(SetMode(Choose))
              | Choose => local(SetMode(Show))
              }
            | true =>
              switch (model.mode) {
              | Show => local(SetMode(Choose))
              | Choose => local(SetMode(Show))
              | Flipped => local(SetMode(Flipped))
              }
            }
          ),
        ],
        [
          switch (model.mode) {
          | Show => Card.view(to_sort(sort), card)
          | Choose => Chooser.view(parent, to_sort(sort), card)
          | Flipped => Card.view(to_sort(sort), card)
          },
        ],
      )
    | (sort, Hand(hand)) => Hand.view(to_sort(sort), hand)
    };
  };
  let focus = _ => ();
};
