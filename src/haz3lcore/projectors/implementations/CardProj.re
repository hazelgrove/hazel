open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

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
type collection =
  | Card(card)
  | Hand(hand);

[@deriving (show({with_path: false}), sexp, yojson)]
type sort =
  | Exp
  | Pat;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Show
  | Choose(int)
  | Flipped;

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | SetMode(mode)
  | ReplaceCard(card)
  | ReplaceCardInHand(int, card);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

let sort_of = (sort: Sort.t): sort =>
  switch (sort) {
  | Exp => Exp
  | Pat => Pat
  | _ => Exp
  };

let to_sort = (sort: sort): Sort.t =>
  switch (sort) {
  | Exp => Exp
  | Pat => Pat
  };

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

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    mode,
    sort,
    collection,
  };

  let default_model: t = {
    mode: Show,
    sort: Exp,
    collection: Card((UnknownS, UnknownR)),
  };

  let model_of_sexp = (sexp: Sexplib.Sexp.t): t =>
    switch (t_of_sexp(sexp)) {
    | exception _ => default_model
    | m => m
    };

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
  open Sexplib;

  let card_to_exp = ((suit, rank): card): Term.Exp.t =>
    Exp.parens(
      Exp.tuple([
        Exp.constructor(Sexp.to_string(sexp_of_suit(suit)), None),
        Exp.constructor(Sexp.to_string(sexp_of_rank(rank)), None),
      ]),
    );

  let card_to_pat = ((suit, rank): card): Term.Pat.t =>
    Pat.parens(
      Pat.tuple([
        switch (suit) {
        | UnknownS => Pat.wild()
        | _ => Pat.constructor(Sexp.to_string(sexp_of_suit(suit)), None)
        },
        switch (rank) {
        | UnknownR => Pat.wild()
        | _ => Pat.constructor(Sexp.to_string(sexp_of_rank(rank)), None)
        },
      ]),
    );

  let collection_to_exp = (collection: collection): Term.Exp.t =>
    switch (collection) {
    | Card(card) => card_to_exp(card)
    | Hand(hand) => Exp.list_lit(List.map(card_to_exp, hand))
    };

  let collection_to_pat = (collection: collection): Term.Pat.t =>
    switch (collection) {
    | Card(card) => card_to_pat(card)
    | Hand(hand) => Pat.list_lit(List.map(card_to_pat, hand))
    };

  let to_term = (m: t): Term.Any.t => {
    switch (m.sort) {
    | Exp => Exp(collection_to_exp(m.collection))
    | Pat => Pat(collection_to_pat(m.collection))
    };
  };

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

  let rec exp_to_card = (term: Term.Exp.t): option(card) => {
    switch (term.term) {
    | Parens(inner) => exp_to_card(inner)
    | Tuple([t1, t2]) =>
      switch (t1.term, t2.term) {
      | (Constructor(suit, _), Constructor(rank, _)) =>
        let* s = string_to_suit(suit);
        let* r = string_to_rank(rank);
        Some((s, r));
      | _ => None
      }
    | _ => None
    };
  };

  let rec pat_to_card = (term: Term.Pat.t): option(card) => {
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

  let of_term = (term: Term.Any.t): option(t) => {
    switch (term) {
    | Exp(term) =>
      switch (strip_wraps_exp(term).term) {
      | ListLit(terms) =>
        let+ cards = terms |> List.map(exp_to_card) |> OptUtil.sequence;
        {
          sort: Exp,
          collection: Hand(cards),
          mode: Show,
        };
      | _ =>
        let+ card = exp_to_card(term);
        {
          sort: Exp,
          collection: Card(card),
          mode: Show,
        };
      }
    | Pat(term) =>
      switch (strip_wraps_pat(term).term) {
      | ListLit(terms) =>
        let+ cards = terms |> List.map(pat_to_card) |> OptUtil.sequence;
        {
          sort: Pat,
          collection: Hand(cards),
          mode: Show,
        };
      | _ =>
        let+ card = pat_to_card(term);
        {
          sort: Pat,
          collection: Card(card),
          mode: Show,
        };
      }
    | _ => None
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed_m) = Model.t;

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

  let replace_card = (local, card: card, index: option(int), _) => {
    let action =
      switch (index) {
      | None => ReplaceCard(card)
      | Some(index) => ReplaceCardInHand(index, card)
      };
    local(action);
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

  let view = (local, sort: Sort.t, card: card, index: option(int)): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["chooser", Sort.show(sort)])],
      List.mapi(
        (r, row) =>
          List.mapi(
            (col, c) =>
              card_wrapper(
                replace_card(local, _, index),
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
        mode,
        local: action('ed_a) => Ui_effect.t(unit),
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
        | Choose(_) => Chooser.view(local, sort, card, None)
        | Flipped => Card.view(sort, card)
        },
      ],
    );
  };
};
module CardInHand = {
  let view =
      (
        mode,
        local: action('ed_a) => Ui_effect.t(unit),
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
            ? Chooser.view(local, sort, card, Some(index))
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
        id,
        mode,
        local: action('ed_a) => Ui_effect.t(unit),
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
      [CardInHand.view(mode, local, sort, card, index)],
    );
  let view = (info, mode, local, sort: Sort.t, hand: hand): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["hand", Sort.show(sort)])],
      List.mapi(card_wrapper(info.id, mode, local, sort), hand),
    );
};

let update =
    (
      ~update_ed as _,
      ~common as _,
      ~sort as _,
      _,
      {mode, sort, collection} as old: model('ed_m),
      action,
    )
    : model('ed_m) =>
  switch (action) {
  | SetMode(mode) => {
      mode,
      sort,
      collection,
    }
  | ReplaceCard(new_card) =>
    switch (collection) {
    | Card(_) => {
        mode,
        sort,
        collection: Card(new_card),
      }
    | Hand(_) => old
    }
  | ReplaceCardInHand(i, card) =>
    switch (collection) {
    | Card(_) => old
    | Hand(hand) => {
        mode,
        sort,
        collection: Hand(ListUtil.update_nth(i, hand, _ => card)),
      }
    }
  };
let view =
    (
      ~common as _,
      ~ed_str as _,
      ~view_ed as _,
      ~view_editable as _,
      ~enter_ed as _,
      ~mk_ed as _,
      ~mk_term_ed as _,
      ~calculate_ed as _,
      ~local,
      ~parent as _,
      ~focus as _,
      ~focussed as _,
      m: model('ed),
      info,
    )
    : View.t => {
  inline:
    switch (m.collection) {
    | Card(card) => Singleton.view(m.mode, local, to_sort(m.sort), card)
    | Hand(hand) => Hand.view(info, m.mode, local, to_sort(m.sort), hand)
    },
  offside: None,
  overlay: None,
  enter_left: None,
  enter_right: None,
};

let methods = {
  init: (~copy_ed as _, term: TermBase.Any.t, _ed) => Model.of_term(term),
  dynamics: false,
  placeholder: (~ed_size as _, model: model('ed), _info) => {
    horizontal:
      switch (model.collection) {
      | Card(_)
      | Hand([_]) => 4
      | Hand(hand) =>
        Float.ceil(
          3.5 +. 81. /. 100. *. (Float.of_int(List.length(hand)) -. 1.),
        )
        |> Float.to_int
      },
    vertical: Tab(1),
  },
  update,
  mk_term: (~mk_term_ed as _, ~sort as _, ~prev as _, m) => (
    m,
    NewValue(Model.to_term(m)),
  ),
  view,
  calculate: Calculate.default,
  get_cursor_info: CursorInfo.default,
  sexp_of_model,
  model_of_sexp,
  yojson_of_model,
  model_of_yojson,
  sexp_of_action,
  action_of_sexp,
  yojson_of_action,
  action_of_yojson,
  sexp_of_focus,
  focus_of_sexp,
  focus_of_yojson,
  yojson_of_focus,
};
