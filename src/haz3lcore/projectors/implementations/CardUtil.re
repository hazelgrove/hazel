open Util;
open Virtual_dom.Vdom;
open Css_gen;

module Card = {
  /* Card images are stored in a spritesheet. The sheet image
   * has four rows (hearts, clubs, diamonds, spades) and 14
   * columns (first is misc, then 2-10, then J Q K A) */

  let width = 35; /* Width of each card in pixels */
  let height = 47; /* Height of each card in pixels */

  let card_to_offset =
      (_sort: Sort.t, (suit, rank): CardTypes.card): (int, int) => (
    CardTypes.rank_to_int(rank) * width,
    CardTypes.suit_to_int(suit) * height,
  );

  let background_offset =
      (~flipped, _sort: Sort.t, card: CardTypes.card): Css_gen.t => {
    let (offset_x, offset_y) =
      flipped ? (0, 0) : card_to_offset(_sort, card);
    Css_gen.create(
      ~field="background-position",
      ~value=Printf.sprintf("%dpx %dpx", - offset_x, - offset_y),
    );
  };

  let side =
      (sort: Sort.t, card: CardTypes.card, ~flipped: bool, clss: string)
      : Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["card-sprite", clss, Sort.show(sort)]),
        Attr.style(background_offset(~flipped, sort, card)),
      ],
      [],
    );

  let view = (sort: Sort.t, card: CardTypes.card) =>
    Node.div(
      ~attrs=[Attr.classes(["card-scene", Sort.show(sort)])],
      [
        side(sort, card, ~flipped=false, "front"),
        side(sort, card, ~flipped=true, "back"),
      ],
    );
};
