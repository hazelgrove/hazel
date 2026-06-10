open Util;
open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

let suit_to_int = (suit: CardProj.suit): int =>
  switch (suit) {
  | CardProj.Hearts => 0
  | CardProj.Clubs => 1
  | CardProj.Diamonds => 2
  | CardProj.Spades => 3
  | CardProj.UnknownS => 4
  };

let rank_to_int = (rank: CardProj.rank): int =>
  switch (rank) {
  | CardProj.Two => 1
  | CardProj.Three => 2
  | CardProj.Four => 3
  | CardProj.Five => 4
  | CardProj.Six => 5
  | CardProj.Seven => 6
  | CardProj.Eight => 7
  | CardProj.Nine => 8
  | CardProj.Ten => 9
  | CardProj.Jack => 10
  | CardProj.Queen => 11
  | CardProj.King => 12
  | CardProj.Ace => 13
  | CardProj.UnknownR => 14
  };

module Card = {
  /* Card images are stored in a spritesheet. The sheet image
   * has four rows (hearts, clubs, diamonds, spades) and 14
   * columns (first is misc, then 2-10, then J Q K A) */

  let width = 35; /* Width of each card in pixels */
  let height = 47; /* Height of each card in pixels */

  let card_to_offset =
      (_sort: Sort.t, (suit, rank): CardProj.card): (int, int) => (
    rank_to_int(rank) * width,
    suit_to_int(suit) * height,
  );

  let background_offset =
      (~flipped, sort: Sort.t, card: CardProj.card): Css_gen.t => {
    let (offset_x, offset_y) =
      flipped
        ? switch (CardProj.sort_of(sort)) {
          | CardProj.Exp => (0, 0)
          | CardProj.Pat => (0, height)
          }
        : card_to_offset(sort, card);
    Css_gen.create(
      ~field="background-position",
      ~value=Printf.sprintf("%dpx %dpx", - offset_x, - offset_y),
    );
  };

  let side: (Sort.t, CardProj.card, ~flipped: bool, string) => Node.t =
    (sort, card, ~flipped, clss) =>
      Node.div(
        ~attrs=[
          Attr.classes(["card-sprite", clss, Sort.show(sort)]),
          Attr.style(background_offset(~flipped, sort, card)),
        ],
        [],
      );

  let view =
    Core.Memo.general((sort: Sort.t, card: CardProj.card) =>
      Node.div(
        ~attrs=[Attr.classes(["card-scene", Sort.show(sort)])],
        [
          side(sort, card, ~flipped=false, "front"),
          side(sort, card, ~flipped=true, "back"),
        ],
      )
    );
};

module Chooser = {
  let col_width = 8;
  let row_height = 14;

  let grid = (sort: CardProj.sort): list(list(CardProj.card)) => {
    let maybe_rank =
      switch (sort) {
      | CardProj.Exp => []
      | CardProj.Pat => [CardProj.UnknownR]
      };
    let maybe_suit: list(CardProj.suit) =
      switch (sort) {
      | CardProj.Exp => []
      | CardProj.Pat => [CardProj.UnknownS]
      };
    let suits: list(CardProj.suit) =
      [CardProj.Hearts, CardProj.Spades, CardProj.Diamonds, CardProj.Clubs]
      @ maybe_suit;
    let ranks: list(CardProj.rank) =
      [
        CardProj.Two,
        CardProj.Three,
        CardProj.Four,
        CardProj.Five,
        CardProj.Six,
        CardProj.Seven,
        CardProj.Eight,
        CardProj.Nine,
        CardProj.Ten,
        CardProj.Jack,
        CardProj.Queen,
        CardProj.King,
        CardProj.Ace,
      ]
      @ maybe_rank;
    List.map(
      (suit: CardProj.suit) =>
        List.map((rank: CardProj.rank) => (suit, rank), ranks),
      suits,
    );
  };

  let replace_card =
      (info, parent, card: CardProj.card, index: option(int), _) => {
    let action =
      switch (index) {
      | None => CardProj.ReplaceCard(card)
      | Some(index) => CardProj.ReplaceCardInHand(index, card)
      };
    switch (
      action
      |> CardProj.update(CardProj.SyntaxTerm.get(info))
      |> CardProj.SyntaxTerm.put(info)
    ) {
    | None => Effect.Ignore
    | Some(seg) => parent(SetSyntax(seg))
    };
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
        ~indicated,
        replace_card,
        sort: Sort.t,
        col: int,
        row: int,
        card: CardProj.card,
      )
      : Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["card-wrapper"] @ (indicated ? ["indicated"] : [])),
        Attr.on_mousedown(replace_card(card)),
        card_pos(col, row),
      ],
      [Card.view(sort, card)],
    );

  let view =
      (info, parent, sort: Sort.t, card: CardProj.card, index: option(int))
      : Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["chooser", Sort.show(sort)])],
      List.mapi(
        (r, row) =>
          List.mapi(
            (col, c) =>
              card_wrapper(
                replace_card(info, parent, _, index),
                ~indicated=c == card,
                sort,
                col,
                r,
                c,
              ),
            row,
          ),
        grid(CardProj.sort_of(sort)),
      )
      |> List.concat,
    );
};

module Singleton = {
  let view =
      (
        info,
        mode,
        parent,
        local: CardProj.action => Ui_effect.t(unit),
        sort: Sort.t,
        card: CardProj.card,
      )
      : Node.t => {
    let on_mousedown = evt =>
      switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
      | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) =>
        switch (mode) {
        | CardProj.Choose(_)
        | CardProj.Flipped => local(CardProj.SetMode(CardProj.Show))
        | CardProj.Show => local(CardProj.SetMode(CardProj.Choose(0)))
        }
      | _ =>
        switch (mode) {
        | CardProj.Flipped
        | CardProj.Choose(_) => local(CardProj.SetMode(CardProj.Show))
        | _ => local(CardProj.SetMode(CardProj.Flipped))
        }
      };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["card-wrapper"]
          @ (
            switch (mode) {
            | CardProj.Show => ["show"]
            | CardProj.Flipped => ["flipped"]
            | CardProj.Choose(_) => ["choose"]
            }
          ),
        ),
        Attr.on_mousedown(on_mousedown),
      ],
      [
        switch (mode) {
        | CardProj.Show => Card.view(sort, card)
        | CardProj.Choose(_) => Chooser.view(info, parent, sort, card, None)
        | CardProj.Flipped => Card.view(sort, card)
        },
      ],
    );
  };
};

module CardInHand = {
  let view =
      (
        info,
        mode,
        parent,
        local: CardProj.action => Ui_effect.t(unit),
        sort: Sort.t,
        card: CardProj.card,
        index: int,
      )
      : Node.t => {
    let on_mousedown = evt =>
      switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
      | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) =>
        switch (mode) {
        | CardProj.Choose(_)
        | CardProj.Flipped => local(CardProj.SetMode(CardProj.Show))
        | CardProj.Show => local(CardProj.SetMode(CardProj.Choose(index)))
        }
      | _ =>
        switch (mode) {
        | CardProj.Choose(_) => local(CardProj.SetMode(CardProj.Show))
        | _ => Effect.Ignore
        }
      };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["card-wrapper"]
          @ (
            switch (mode) {
            | CardProj.Show => ["show"]
            | CardProj.Flipped => ["flipped"]
            | CardProj.Choose(cidx) => cidx == index ? ["choose"] : []
            }
          ),
        ),
        Attr.on_mousedown(on_mousedown),
      ],
      [
        switch (mode) {
        | CardProj.Show => Card.view(sort, card)
        | CardProj.Choose(cidx) =>
          cidx == index
            ? Chooser.view(info, parent, sort, card, Some(index))
            : Card.view(sort, card)
        | CardProj.Flipped => Card.view(sort, card)
        },
      ],
    );
  };
};

module Hand = {
  let card_wrapper =
      (
        info,
        id,
        mode,
        parent: external_action => Ui_effect.t(unit),
        local: CardProj.action => Ui_effect.t(unit),
        sort: Sort.t,
        index: int,
        card: CardProj.card,
      )
      : Node.t =>
    Node.div(
      ~attrs=[
        Attr.id(Id.cls(id) ++ "card-index-" ++ string_of_int(index)),
        Attr.class_("card-wrapper"),
        Attr.create(
          "style",
          Printf.sprintf(
            "position: absolute; left: %fpx; z-index: %d;",
            mode == CardProj.Flipped ? 0. : float_of_int(index) *. 8.5,
            100 + index,
          ),
        ),
      ],
      [CardInHand.view(info, mode, parent, local, sort, card, index)],
    );

  let view =
      (info, mode, parent, local, sort: Sort.t, hand: CardProj.hand): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["hand", Sort.show(sort)])],
      List.mapi(
        card_wrapper(info, info.id, mode, parent, local, sort),
        hand,
      ),
    );
};

module V: ProjectorView = {
  module L = CardProj.M;

  let focusable = Focusable.non;

  let view =
      ({model, info, local, parent, _}: View.args(L.model, L.action))
      : View.t => {
    inline:
      switch (CardProj.SyntaxTerm.get(info)) {
      | (sort, CardProj.Card(card)) =>
        Singleton.view(
          info,
          model.mode,
          parent,
          local,
          CardProj.to_sort(sort),
          card,
        )
      | (sort, CardProj.Hand(hand)) =>
        Hand.view(
          info,
          model.mode,
          parent,
          local,
          CardProj.to_sort(sort),
          hand,
        )
      },
    offside: None,
    overlay: None,
    error: false,
  };
};
