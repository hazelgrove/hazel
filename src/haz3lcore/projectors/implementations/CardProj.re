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
  | UnknownS(string)
  | Hearts
  | Diamonds
  | Clubs
  | Spades;

[@deriving (show({with_path: false}), sexp, yojson)]
type rank =
  | UnknownR(string)
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
type state = (sort, collection);

let sort_of = (sort: Sort.t): sort =>
  switch (sort) {
  | Sort.Exp => Exp
  | Sort.Pat => Pat
  | _ =>
    print_endline("WARNING:Card: Invalid sort: " ++ Sort.show(sort));
    Exp;
  };

let to_sort = (sort: sort): Sort.t =>
  switch (sort) {
  | Exp => Sort.Exp
  | Pat => Sort.Pat
  };

module SyntaxTerm = {
  open SyntaxUtil;
  let card_to_exp = ((suit, rank): card): Term.Exp.t =>
    Exp.mk_wrapped_tuple([
      Exp.constr_of_sexp(sexp_of_suit(suit)),
      Exp.constr_of_sexp(sexp_of_rank(rank)),
    ]);

  let card_to_pat = ((suit, rank): card): Term.Pat.t =>
    Pat.mk_wrapped_tuple([
      Pat.constr_of_sexp(sexp_of_suit(suit)),
      Pat.constr_of_sexp(sexp_of_rank(rank)),
    ]);

  let syntax_to_any = ((sort, collection): state): Term.Any.t => {
    let collection_to_exp = (collection: collection): Term.Exp.t =>
      switch (collection) {
      | Card(card) => card_to_exp(card)
      | Hand(hand) => Exp.mk_listlit(List.map(card_to_exp, hand))
      };

    let collection_to_pat = (collection: collection): Term.Pat.t =>
      switch (collection) {
      | Card(card) => card_to_pat(card)
      | Hand(hand) => Pat.mk_listlit(List.map(card_to_pat, hand))
      };

    switch (sort) {
    | Exp => Exp(collection_to_exp(collection))
    | Pat => Pat(collection_to_pat(collection))
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

  open OptUtil.Syntax;

  let rec exp_to_card = (term: Term.Exp.t): option(card) => {
    switch (term.term) {
    | Parens(inner) => exp_to_card(inner)
    | Tuple([t1, t2]) =>
      switch (t1.term, t2.term) {
      | (Constructor(suit, _), Constructor(rank, _)) =>
        switch (string_to_suit(suit), string_to_rank(rank)) {
        | (Some(s), Some(r)) => Some((s, r))
        | _ => None
        }
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
        Some((s, UnknownR("")));
      | (Wild, Constructor(rank, _)) =>
        let* r = string_to_rank(rank);
        Some((UnknownS(""), r));
      | (Wild, Wild) => Some((UnknownS(""), UnknownR("")))
      | _ => None
      }
    | _ => None
    };
  };

  let any_to_syntax = (term: Term.Any.t): option(state) => {
    switch (term) {
    | Exp(term) =>
      switch (Exp.strip_wraps(term).term) {
      | ListLit(terms) =>
        let+ cards = terms |> List.map(exp_to_card) |> OptUtil.sequence;
        (Exp, Hand(cards));
      | _ =>
        let+ card = exp_to_card(term);
        (Exp, Card(card));
      }
    | Pat(term) =>
      switch (Pat.strip_wraps(term).term) {
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
      //TODO: Better formula / card dimensions / offset
      let handlen: float = List.length(hand) |> Float.of_int;
      Float.ceil(3.5 +. 81. /. 100. *. (handlen -. 1.)) |> Float.to_int;
    };
};

let suit_to_int = (suit: suit): int =>
  switch (suit) {
  | Hearts => 0
  | Clubs => 1
  | Diamonds => 2
  | Spades => 3
  | UnknownS(_) => 4
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
  | UnknownR(_) => 14
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

  let view =
    Core.Memo.general((sort: Sort.t, card: card) =>
      (
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
        ): Node.t
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
      | Pat => [UnknownR("_")] //TODO
      };
    let maybe_suit: list(suit) =
      switch (sort) {
      | Exp => []
      | Pat => [UnknownS("_")] //TODO
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
      (info, ~indicated, parent, sort: Sort.t, col: int, row: int, card: card)
      : Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["card-wrapper"] @ (indicated ? ["indicated"] : [])),
        Attr.on_mousedown(_ => {
          //TODO: make this work for hands
          switch (SyntaxTerm.put(info, (sort_of(sort), Card(card)))) {
          | None => Effect.Ignore
          | Some(seg) =>
            print_endline("putting seg:" ++ Segment.show(seg));
            Effect.Many([parent(SetSyntax(seg))]);
          // Effect.Prevent_default,
          // Effect.Stop_propagation,
          }
        }),
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

  let view = (info, parent, sort: Sort.t, card: card): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["chooser", Sort.show(sort)])],
      List.mapi(
        (r, row) =>
          List.mapi(
            (col, c) =>
              card_wrapper(
                info,
                parent,
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
        | Choose
        | Flipped => local(SetMode(Show))
        | Show => local(SetMode(Choose))
        }
      | _ =>
        switch (mode) {
        | Flipped
        | Choose => local(SetMode(Show))
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
            | Choose => ["choose"]
            }
          ),
        ),
        Attr.on_mousedown(on_mousedown),
      ],
      [
        switch (mode) {
        | Show => Card.view(sort, card)
        | Choose => Chooser.view(info, parent, sort, card)
        | Flipped => Card.view(sort, card)
        },
      ],
    );
  };
};

module CardInHand = {
  let view =
      (
        _info,
        _elem_ids,
        mode,
        _parent,
        local: action => Ui_effect.t(unit),
        sort: Sort.t,
        card: card,
      )
      : Node.t => {
    let on_mousedown = evt =>
      switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
      | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) =>
        switch (mode) {
        | Choose
        | Flipped => local(SetMode(Show))
        | Show => local(SetMode(Choose))
        }
      | _ => Effect.Ignore
      };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["card-wrapper"]
          @ (
            switch (mode) {
            | Show => ["show"]
            | Flipped => ["flipped"]
            | Choose => ["choose"]
            }
          ),
        ),
        Attr.on_mousedown(on_mousedown),
      ],
      [
        switch (mode) {
        | Show => Card.view(sort, card)
        | Choose => Card.view(sort, card)
        //TODO: choosing for hands
        //Chooser.view(info, parent, sort, card)
        | Flipped => Card.view(sort, card)
        },
      ],
    );
  };
};

let hand_elem_ids = (id, hand: hand): list(string) =>
  List.mapi(
    (i, _) => Id.cls(id) ++ "card-index-" ++ string_of_int(i),
    hand,
  );

module Hand = {
  // a card, but each subsequent card should be absoluted positioned 20px to the right of the last and higher in z-index:
  let card_wrapper =
      (
        info,
        id,
        elem_ids,
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
      [CardInHand.view(info, elem_ids, mode, parent, local, sort, card)],
    );

  let view = (info, mode, parent, local, sort: Sort.t, hand: hand): Node.t => {
    Node.div(
      ~attrs=[Attr.classes(["hand", Sort.show(sort)])],
      List.mapi(
        card_wrapper(
          info,
          info.id,
          hand_elem_ids(info.id, hand),
          mode,
          parent,
          local,
          sort,
        ),
        hand,
      ),
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
  let can_project = (info: TermBase.Any.t) =>
    SyntaxTerm.get_opt(info) != None;
  let can_focus = false;
  let dynamics = false;
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
        |> Option.some
      | (sort, Hand(hand)) =>
        Hand.view(info, model.mode, parent, local, to_sort(sort), hand)
        |> Option.some
      },
    offside: None,
    overlay: None,
    underlay: None,
  };

  let focus = _ => ();
};
