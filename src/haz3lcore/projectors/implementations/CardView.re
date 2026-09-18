open Virtual_dom.Vdom;
open CardTypes;

module Card = {
  /* Card images are stored in a spritesheet. The sheet image
   * has four rows (hearts, clubs, diamonds, spades) and 14
   * columns (first is misc, then 2-10, then J Q K A) */

  let width = 35;
  let height = 47;

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

  let side: (Sort.t, card, ~flipped: bool, string) => Node.t =
    (sort, card, ~flipped, clss) =>
      Node.div(
        ~attrs=[
          Attr.classes(["card-sprite", clss, Sort.show(sort)]),
          Attr.style(background_offset(~flipped, sort, card)),
        ],
        [],
      );

  let view =
    Core.Memo.general((sort: Sort.t, card: card) =>
      Node.div(
        ~attrs=[Attr.classes(["card-scene", Sort.show(sort)])],
        [
          side(sort, card, ~flipped=false, "front"),
          side(sort, card, ~flipped=true, "back"),
        ],
      )
    );
};

/* silhouette occupying one card's footprint: the empty hand */
module Empty = {
  let view: Node.t =
    Node.div(
      ~attrs=[Attr.classes(["card-empty"])],
      [
        Node.div(
          ~attrs=[Attr.classes(["card-empty-badge"])],
          [Node.text({js|∅|js})],
        ),
      ],
    );
};

module Chooser = {
  let col_width = 8;
  let row_height = 14;

  let grid = (sort: sort): list(list(card)) => {
    let maybe_rank: list(rank) =
      switch (sort) {
      | Exp => []
      | Pat => [UnknownR]
      };
    let maybe_suit: list(suit) =
      switch (sort) {
      | Exp => []
      | Pat => [UnknownS]
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

  let card_pos = (col: int, row: int) =>
    Attr.create(
      "style",
      Printf.sprintf(
        "position: absolute; left: %dpx; top: %dpx; z-index: %d;",
        col * col_width,
        row * row_height,
        100 + row + col,
      ),
    );

  let card_wrapper =
      (
        ~on_pick: card => Ui_effect.t(unit),
        ~indicated: card,
        sort: Sort.t,
        col: int,
        row: int,
        c: card,
      )
      : Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(
          ["card-wrapper"] @ (c == indicated ? ["indicated"] : []),
        ),
        Attr.on_mousedown(_ => on_pick(c)),
        card_pos(col, row),
      ],
      [Card.view(sort, c)],
    );

  let view =
      (
        ~on_pick: card => Ui_effect.t(unit),
        ~indicated: card,
        sort: Sort.t,
        card_sort: sort,
      )
      : Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["chooser", Sort.show(sort)])],
      List.mapi(
        (r, row) =>
          List.mapi(
            (col, c) => card_wrapper(~on_pick, ~indicated, sort, col, r, c),
            row,
          ),
        grid(card_sort),
      )
      |> List.concat,
    );
};
