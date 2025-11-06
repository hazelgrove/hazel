open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open CardTypes;

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
type state =
  | Card(card)
  | Hand(hand);

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;

/* Calculator actions that can be performed on the integer */
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;

[@deriving (show({with_path: false}), sexp, yojson)]
type value = state;

module SyntaxTerm = {
  open IdTagged.FreshGrammar;

  let strip_wraps_exp = CardSyntax.strip_wraps_exp;

  let card_to_exp = CardSyntax.card_to_exp;

  let exp_to_card = CardSyntax.exp_to_card;

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
      [CardUtil.Card.view(sort, card)],
    );

  let view = (info, parent, sort: Sort.t, _: card) =>
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
  let view = (info, mode, parent, sort: Sort.t, card: card, local) => {
    let on_mousedown = evt =>
      switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
      | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) =>
        switch (mode) {
        | Choose
        | Flipped => local(SetMode(Show))
        | Show => local(SetMode(Choose))
        }
      | _ =>
        switch (mode) {
        | Flipped => local(SetMode(Show))
        | _ => local(SetMode(Flipped))
        }
      };

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
          ~attrs=[
            Attr.classes(["card-wrapper"]),
            Attr.on_mousedown(on_mousedown),
          ],
          [
            CardUtil.Card.view(sort, card),
            switch (mode) {
            | Choose => Chooser.view(info, parent, sort, card)
            | _ => Node.div([])
            },
          ],
        ),
      ],
    );
  };
};

module Hand = {
  let view = (_, mode, _, sort: Sort.t, hand: hand, local) => {
    let on_mousedown = evt =>
      switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
      | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) =>
        switch (mode) {
        | Choose
        | Flipped => local(SetMode(Show))
        | Show => local(SetMode(Choose))
        }
      | _ =>
        switch (mode) {
        | Flipped => local(SetMode(Show))
        | _ => local(SetMode(Flipped))
        }
      };

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
                  Attr.on_mousedown(on_mousedown),
                  Attr.create(
                    "style",
                    Printf.sprintf(
                      "position: absolute; left: %fpx; z-index: %d;",
                      mode == Flipped ? 0. : float_of_int(i) *. 8.5,
                      100 + i,
                    ),
                  ),
                ],
                [CardUtil.Card.view(sort, card)],
              ),
            hand,
          ),
        ),
      ],
    );
  };
};

/* Parse an expression into card/hand value */
let parse = (exp: Exp.t) => SyntaxTerm.any_to_state(Exp(exp));

/* Initialize card renderer model from parsed value */
let init = (_: state) => {mode: Show};

/* Render the card renderer */
let render =
    (
      ~info: info,
      ~exp: Exp.t,
      ~value: value,
      ~view_seg as _: (Sort.t, Segment.t) => Node.t,
      ~model: m,
      ~local: a => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      (),
    ) =>
  switch (parse(exp) |> Option.get) {
  | Card(card) =>
    Singleton.view(info, model.mode, parent, Sort.Exp, card, local)
  | Hand(hand) => Hand.view(info, model.mode, parent, Sort.Exp, hand, local)
  };

let update: (m, a) => m =
  (_, action) =>
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
