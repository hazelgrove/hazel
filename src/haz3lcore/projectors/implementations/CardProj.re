open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open CardTypes;
open CardSyntax;
open CardUtil;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Show
  | Choose(int)
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
type collection =
  | Card(card)
  | Hand(hand);

[@deriving (show({with_path: false}), sexp, yojson)]
type sort =
  | Exp
  | Pat;

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (sort, collection);

[@deriving (show({with_path: false}), sexp, yojson)]
type update =
  | ReplaceCard(card)
  | ReplaceCardInHand(int, card);

let update = ((sort, collection): state, update: update): state =>
  switch (update) {
  | ReplaceCard(new_card) =>
    switch (collection) {
    | Card(_) => (sort, Card(new_card))
    | Hand(_) => (sort, collection)
    }
  | ReplaceCardInHand(i, card) =>
    switch (collection) {
    | Card(_) => (sort, collection)
    | Hand(hand) => (sort, Hand(ListUtil.update_nth(i, hand, _ => card)))
    }
  };

let sort_of = (sort: Sort.t): sort =>
  switch (sort) {
  | Sort.Exp => Exp
  | Sort.Pat => Pat
  | _ => Exp
  };

let to_sort = (sort: sort): Sort.t =>
  switch (sort) {
  | Exp => Sort.Exp
  | Pat => Sort.Pat
  };

module SyntaxTerm = {
  let rec strip_wraps_pat = (p: Pat.t): Pat.t => {
    switch (p.term) {
    | Parens(inner) =>
      switch (inner.term) {
      | Tuple(_) => p
      | _ => strip_wraps_pat(inner)
      }
    | _ => p
    };
  };
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
  open OptUtil.Syntax;

  let card_to_exp = card => CardSyntax.card_to_exp(card);

  let card_to_pat = ((suit, rank): card): pat =>
    Pat.parens(
      Pat.tuple([
        switch (suit) {
        | UnknownS => Pat.wild()
        | _ =>
          Pat.constructor(
            Sexplib.Sexp.to_string(CardTypes.sexp_of_suit(suit)),
            None,
          )
        },
        switch (rank) {
        | UnknownR => Pat.wild()
        | _ =>
          Pat.constructor(
            Sexplib.Sexp.to_string(CardTypes.sexp_of_rank(rank)),
            None,
          )
        },
      ]),
    );

  let syntax_to_any = ((sort, collection): state): Any.t => {
    let collection_to_exp = (collection: collection): Exp.t =>
      switch (collection) {
      | Card(card) => card_to_exp(card)
      | Hand(hand) => Exp.list_lit(List.map(card_to_exp, hand))
      };

    let collection_to_pat = (collection: collection): pat =>
      switch (collection) {
      | Card(card) => card_to_pat(card)
      | Hand(hand) => Pat.list_lit(List.map(card_to_pat, hand))
      };

    switch (sort) {
    | Exp => Exp(collection_to_exp(collection))
    | Pat => Pat(collection_to_pat(collection))
    };
  };

  let string_to_suit = CardSyntax.string_to_suit;

  let string_to_rank = CardSyntax.string_to_rank;

  let rec exp_to_card = CardSyntax.exp_to_card;

  let rec pat_to_card = (term: pat): option(card) => {
    switch (term.term) {
    | Parens(pat) => pat |> pat_to_card
    | Tuple([p1, p2]) =>
      switch (p1.term, p2.term) {
      | (Constructor(suit, _), Constructor(rank, _)) =>
        let* s = string_to_suit(suit);
        let* r = string_to_rank(rank);
        Some((s, r));
      | (Constructor(suit, _), Wild) =>
        let* s = string_to_suit(suit);
        Some((s, UnknownR));
      | (Wild, Constructor(rank, _)) =>
        let* r = string_to_rank(rank);
        Some((UnknownS, r));
      | (Wild, Wild) => Some((UnknownS, UnknownR))
      | _ => None
      }
    | _ => None
    };
  };

  let any_to_syntax = (term: Any.t): option(state) => {
    switch (term) {
    | Exp(term) =>
      switch (strip_wraps_exp(term).term) {
      | ListLit(terms) =>
        let+ cards = terms |> List.map(exp_to_card) |> OptUtil.sequence;
        (Exp, Hand(cards));
      | _ =>
        let+ card = exp_to_card(term);
        (Exp, Card(card));
      }
    | Pat(term) =>
      switch (strip_wraps_pat(term).term) {
      | ListLit(terms) =>
        let+ cards = terms |> List.map(pat_to_card) |> OptUtil.sequence;
        (Pat, Hand(cards));
      | _ =>
        let+ card = pat_to_card(term);
        (Pat, Card(card));
      }
    | _ => None
    };
  };

  let put = (info, syntax): option(Base.segment) =>
    info.utility.lift_syntax(_ => syntax_to_any(syntax), info.syntax);

  let get_opt = (any: Any.t): option(state) =>
    switch (any |> any_to_syntax) {
    | Some(syntax) => Some(syntax)
    | None => None
    };

  let get = (info: info): state =>
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(syntax) =>
      switch (get_opt(syntax)) {
      | Some(syntax) => syntax
      | None => failwith("Cards: Get: not cards")
      }
    | None => failwith("Cards: Get: seg_to_term ")
    };

  let width_of_syntax = (syntax: state): int =>
    switch (syntax) {
    | (_, Card(_)) => 1
    | (_, Hand(hand)) => List.length(hand)
    };

  let width_of_any = (info: info): int =>
    switch (
      info.syntax
      |> info.utility.seg_to_term
      |> OptUtil.and_then(any_to_syntax)
    ) {
    | None => 0
    | Some((_, Card(_)))
    | Some((_, Hand([_]))) => 4
    | Some((_, Hand(hand))) =>
      Float.ceil(
        3.5 +. 81. /. 100. *. (Float.of_int(List.length(hand)) -. 1.),
      )
      |> Float.to_int
    };
};

module Card = {
  /* Card images are stored in a spritesheet. The sheet image
   * has four rows (hearts, clubs, diamonds, spades) and 14
   * columns (first is misc, then 2-10, then J Q K A) */

  let width = 35; /* Width of each card in pixels */
  let height = 47; /* Height of each card in pixels */

  let card_to_offset = (_sort: Sort.t, (suit, rank): card): (int, int) => (
    CardTypes.rank_to_int(rank) * width,
    CardTypes.suit_to_int(suit) * height,
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

module Chooser = {
  let col_width = 8;
  let row_height = 14;

  let grid = (sort: sort): list(list(card)) => {
    let maybe_rank =
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

  let replace_card = (info, parent, card: card, index: option(int), _) => {
    let action =
      switch (index) {
      | None => ReplaceCard(card)
      | Some(index) => ReplaceCardInHand(index, card)
      };
    switch (action |> update(SyntaxTerm.get(info)) |> SyntaxTerm.put(info)) {
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
      (~indicated, replace_card, sort: Sort.t, col: int, row: int, card: card)
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
      (info, parent, sort: Sort.t, card: card, index: option(int)): Node.t =>
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
        grid(sort_of(sort)),
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
        local: action => Ui_effect.t(unit),
        sort: Sort.t,
        card: card,
      )
      : Node.t => {
    let on_mousedown = evt =>
      switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
      | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) =>
        switch (mode) {
        | Choose(_)
        | Flipped => local(SetMode(Show))
        | Show => local(SetMode(Choose(0)))
        }
      | _ =>
        switch (mode) {
        | Flipped
        | Choose(_) => local(SetMode(Show))
        | _ => local(SetMode(Flipped))
        }
      };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["card-wrapper"]
          @ (
            switch (mode) {
            | Show => ["show"]
            | Flipped => ["flipped"]
            | Choose(_) => ["choose"]
            }
          ),
        ),
        Attr.on_mousedown(on_mousedown),
      ],
      [
        switch (mode) {
        | Show => Card.view(sort, card)
        | Choose(_) => Chooser.view(info, parent, sort, card, None)
        | Flipped => Card.view(sort, card)
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
        local: action => Ui_effect.t(unit),
        sort: Sort.t,
        card: card,
        index: int,
      )
      : Node.t => {
    let on_mousedown = evt =>
      switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
      | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) =>
        switch (mode) {
        | Choose(_)
        | Flipped => local(SetMode(Show))
        | Show => local(SetMode(Choose(index)))
        }
      | _ =>
        switch (mode) {
        | Choose(_) => local(SetMode(Show))
        | _ => Effect.Ignore
        }
      };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["card-wrapper"]
          @ (
            switch (mode) {
            | Show => ["show"]
            | Flipped => ["flipped"]
            | Choose(cidx) => cidx == index ? ["choose"] : []
            }
          ),
        ),
        Attr.on_mousedown(on_mousedown),
      ],
      [
        switch (mode) {
        | Show => Card.view(sort, card)
        | Choose(cidx) =>
          cidx == index
            ? Chooser.view(info, parent, sort, card, Some(index))
            : Card.view(sort, card)
        | Flipped => Card.view(sort, card)
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
        local: action => Ui_effect.t(unit),
        sort: Sort.t,
        index: int,
        card: card,
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
            mode == Flipped ? 0. : float_of_int(index) *. 8.5,
            100 + index,
          ),
        ),
      ],
      [CardInHand.view(info, mode, parent, local, sort, card, index)],
    );

  let view = (info, mode, parent, local, sort: Sort.t, hand: hand): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["hand", Sort.show(sort)])],
      List.mapi(
        card_wrapper(info, info.id, mode, parent, local, sort),
        hand,
      ),
    );
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
  let focusable = Focusable.non;
  let dynamics = false;

  let init = (info: TermBase.Any.t): option(model) =>
    SyntaxTerm.get_opt(info) != None ? Some({mode: Show}) : None;

  let placeholder = (_, info): ProjectorCore.Shape.t => {
    horizontal: SyntaxTerm.width_of_any(info),
    vertical: Tab(1),
  };

  let update = (_model, _, action) =>
    switch (action) {
    | SetMode(mode) => {mode: mode}
    };

  let view =
      (
        model,
        info,
        ~local,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      )
      : View.t => {
    inline:
      switch (SyntaxTerm.get(info)) {
      | (sort, Card(card)) =>
        Singleton.view(info, model.mode, parent, local, to_sort(sort), card)
      | (sort, Hand(hand)) =>
        Hand.view(info, model.mode, parent, local, to_sort(sort), hand)
      },
    offside: None,
    overlay: None,
  };
};
