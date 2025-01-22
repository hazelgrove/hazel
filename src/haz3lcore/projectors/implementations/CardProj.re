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
  | Unknown(string)
  | Hearts
  | Diamonds
  | Clubs
  | Spades;

[@deriving (show({with_path: false}), sexp, yojson)]
type rank =
  | Unknown(string)
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
  module Exp = {
    let get_wrap = (term: Term.Exp.t): option(Term.Exp.t) =>
      switch (term) {
      | {term: Wrap(term, _), _} => Some(term)
      | _ => None
      };

    let get_tuple = (term: Term.Exp.t): option(list(Term.Exp.t)) =>
      switch (term) {
      | {term: Tuple(terms), _} => Some(terms)
      | _ => None
      };

    let get_two_tuple = (term: Term.Exp.t): option((Term.Exp.t, Term.Exp.t)) =>
      switch (get_tuple(term)) {
      | Some([term1, term2]) => Some((term1, term2))
      | _ => None
      };

    let get_constructor = (term: Term.Exp.t): option(string) =>
      switch (term) {
      | {term: Constructor(str, _), _} => Some(str)
      | _ => None
      };

    let get_listlit = (term: Term.Exp.t): option(list(Term.Exp.t)) =>
      switch (term) {
      | {term: ListLit(terms), _} => Some(terms)
      | _ => None
      };

    let mk_constructor = (str: string): Term.Exp.t =>
      IdTagged.fresh(
        Constructor(str, Unknown(Internal) |> Typ.temp): Term.Exp.term,
      );

    let mk_tuple = (children: list(Term.Exp.t)): Term.Exp.t =>
      IdTagged.fresh(Tuple(children): Term.Exp.term);

    let mk_listlit = (children: list(Term.Exp.t)): Term.Exp.t =>
      IdTagged.fresh(ListLit(children): Term.Exp.term);

    let mk_wrap = (term: Term.Exp.t): Term.Exp.t =>
      IdTagged.fresh(Wrap(term, Paren): Term.Exp.term);
  };

  module Pat = {
    let get_wrap = (term: Term.Pat.t): option(Term.Pat.t) =>
      switch (term) {
      | {term: Wrap(term, _), _} => Some(term)
      | _ => None
      };

    let get_tuple = (term: Term.Pat.t): option(list(Term.Pat.t)) =>
      switch (term) {
      | {term: Tuple(terms), _} => Some(terms)
      | _ => None
      };

    let get_two_tuple = (term: Term.Pat.t): option((Term.Pat.t, Term.Pat.t)) =>
      switch (get_tuple(term)) {
      | Some([term1, term2]) => Some((term1, term2))
      | _ => None
      };

    let get_constructor = (term: Term.Pat.t): option(string) => {
      switch (term) {
      | {term: Constructor(str, _), _} => Some(str)
      | {term: Var(str), _} => Some(str)
      | {term: Wild, _} => Some("_")
      | _ => None
      };
    };

    let get_listlit = (term: Term.Pat.t): option(list(Term.Pat.t)) =>
      switch (term) {
      | {term: ListLit(terms), _} => Some(terms)
      | _ => None
      };

    let mk_constructor = (str: string): Term.Pat.t =>
      IdTagged.fresh(
        Constructor(str, Unknown(Internal) |> Typ.temp): Term.Pat.term,
      );

    let mk_tuple = (children: list(Term.Pat.t)): Term.Pat.t =>
      IdTagged.fresh(Tuple(children): Term.Pat.term);

    let mk_listlit = (children: list(Term.Pat.t)): Term.Pat.t =>
      IdTagged.fresh(ListLit(children): Term.Pat.term);

    let mk_wrap = (term: Term.Pat.t): Term.Pat.t =>
      IdTagged.fresh(Wrap(term, Paren): Term.Pat.term);
  };

  let suit_of_exp = (suit): option(suit) =>
    switch (suit |> Sexplib.Sexp.of_string |> suit_of_sexp) {
    | exception _ => None
    | s => Some(s)
    };
  let rank_of_exp = (rank): option(rank) =>
    switch (rank |> Sexplib.Sexp.of_string |> rank_of_sexp) {
    | exception _ => None
    | r => Some(r)
    };
  let suit_of_pat = (suit): option(suit) =>
    switch (suit |> Sexplib.Sexp.of_string |> suit_of_sexp) {
    | exception _ => Some(Unknown(suit))
    | s => Some(s)
    };
  let rank_of_pat = (rank): option(rank) =>
    switch (rank |> Sexplib.Sexp.of_string |> rank_of_sexp) {
    | exception _ => Some(Unknown(rank))
    | r => Some(r)
    };

  let exp_to_card = (term: Term.Exp.t): option(card) => {
    open OptUtil.Syntax;
    let* tuple = Exp.get_wrap(term);
    let* (t1, t2) = Exp.get_two_tuple(tuple);
    let* c1 = Exp.get_constructor(t1);
    let* c2 = Exp.get_constructor(t2);
    let* suit = suit_of_exp(c1);
    let* rank = rank_of_exp(c2);
    Some((suit, rank));
  };

  let pat_to_card = (term: Term.Pat.t): option(card) => {
    open OptUtil.Syntax;
    let* tuple = Pat.get_wrap(term);
    let* (t1, t2) = Pat.get_two_tuple(tuple);
    let* c1 = Pat.get_constructor(t1);
    let* c2 = Pat.get_constructor(t2);
    let* suit = suit_of_pat(c1);
    let* rank = rank_of_pat(c2);
    Some((suit, rank));
  };

  let any_to_syntax = (any: Any.t): option(syntax) => {
    OptUtil.Syntax.(
      switch (any) {
      | Exp(term) =>
        let term = Term.Exp.strip_wraps(term);
        switch (exp_to_card(term)) {
        | Some(card) => Some((Exp, Card(card)))
        | None =>
          let+ listlit = Exp.get_listlit(term);
          let cards = List.filter_map(exp_to_card, listlit);
          (Exp, Hand(cards));
        };
      | Pat(term) =>
        let term = Term.Pat.strip_wraps(term);
        switch (pat_to_card(term)) {
        | Some(card) => Some((Exp, Card(card)))
        | None =>
          let+ listlit = Pat.get_listlit(term);
          let cards = List.filter_map(pat_to_card, listlit);
          (Exp, Hand(cards));
        };
      | _ => None
      }
    );
  };

  let suit_to_exp = (suit: suit): Term.Exp.t =>
    Exp.mk_constructor(suit |> sexp_of_suit |> Sexplib.Sexp.to_string);

  let rank_to_exp = (rank: rank): Term.Exp.t =>
    Exp.mk_constructor(rank |> sexp_of_rank |> Sexplib.Sexp.to_string);

  let card_to_exp = ((suit, rank): card): Term.Exp.t =>
    Exp.mk_tuple([suit_to_exp(suit), rank_to_exp(rank)]);

  let hand_to_exp = (hand: hand): Term.Exp.t =>
    Exp.mk_listlit(List.map(card_to_exp, hand));

  let suit_to_pat = (suit: suit): Term.Pat.t =>
    Pat.mk_constructor(suit |> sexp_of_suit |> Sexplib.Sexp.to_string);

  let rank_to_pat = (rank: rank): Term.Pat.t =>
    Pat.mk_constructor(rank |> sexp_of_rank |> Sexplib.Sexp.to_string);

  let card_to_pat = ((suit, rank): card): Term.Pat.t =>
    Pat.mk_tuple([suit_to_pat(suit), rank_to_pat(rank)]);

  let hand_to_pat = (hand: hand): Term.Pat.t =>
    Pat.mk_listlit(List.map(card_to_pat, hand));

  let syntax_to_any = (syntax: syntax): Term.Any.t =>
    switch (syntax) {
    | (Exp, Card(card)) => Exp(card_to_exp(card))
    | (Exp, Hand(hand)) => Exp(hand_to_exp(hand))
    | (Pat, Card(card)) => Pat(card_to_pat(card))
    | (Pat, Hand(hand)) => Pat(hand_to_pat(hand))
    };

  let put = (info, syntax): Piece.t =>
    info.utility.lift_syntax(_ => syntax_to_any(syntax), info.syntax);

  let get_opt = (any: Any.t): option(syntax) =>
    switch (any |> any_to_syntax) {
    | Some(syntax) => Some(syntax)
    | None => None
    };

  let get = (info: info): syntax =>
    switch ([info.syntax] |> info.utility.seg_to_term |> get_opt) {
    | Some(syntax) => syntax
    | None => failwith("Cards: Get: not cards")
    };

  let width_of_syntax = (syntax: syntax): int =>
    switch (syntax) {
    | (_, Card(_)) => 1
    | (_, Hand(hand)) => List.length(hand)
    };

  let width_of_any = (info: info): int =>
    switch (any_to_syntax([info.syntax] |> info.utility.seg_to_term)) {
    | None => 0
    | Some((_, Card(_)))
    | Some((_, Hand([_]))) => 4
    | Some((_, Hand(hand))) =>
      //TODO: Better formula / card dimensions / offset
      4 + List.length(hand) - (List.length(hand) + 66) / 24
    };
};

// module Syntax = {
//   let suit_of_piece = (p: Piece.t): suit =>
//     switch (p) {
//     | Tile({label: [str], _}) =>
//       switch (str |> Sexplib.Sexp.of_string |> suit_of_sexp) {
//       | exception _ => Unknown(p)
//       | s => s
//       }
//     | _ => Unknown(p)
//     };

//   let rank_of_piece = (p: Piece.t): rank =>
//     switch (p) {
//     | Tile({label: [str], _}) =>
//       switch (str |> Sexplib.Sexp.of_string |> rank_of_sexp) {
//       | exception _ => Unknown(p)
//       | r => r
//       }
//     | _ => Unknown(p)
//     };

//   let rm_secondary = (segment: Segment.t): Segment.t =>
//     List.filter(p => !Piece.is_secondary(p), segment);

//   let piece_to_card =
//     Core.Memo.general(~cache_size_bound=1000, (piece: Piece.t) =>
//       (
//         switch (piece) {
//         | Tile({
//             label: ["(", ")"],
//             children:
//               [[Tile({label: ["(", ")"], children: [segment], _})]],
//             _,
//           })
//         //TODO: better unwrapping
//         | Tile({label: ["(", ")"], children: [segment], _}) =>
//           switch (rm_secondary(segment)) {
//           | [left_child, Tile({label: [","], _}), right_child] =>
//             Some((suit_of_piece(left_child), rank_of_piece(right_child)))
//           | _ => None
//           }
//         | _ => None
//         }:
//           option(card)
//       )
//     );

//   let piece_to_hand = (piece: Piece.t): option(hand) => {
//     switch (piece) {
//     | Tile({
//         label: ["(", ")"],
//         children: [[Tile({label: ["[", "]"], children: [segment], _})]],
//         _,
//       })
//     | Tile({label: ["[", "]"], children: [segment], _}) =>
//       segment |> rm_secondary |> List.filter_map(piece_to_card) |> Option.some
//     | _ => None
//     };
//   };

//   let piece_to_syntax = (piece: Piece.t): option(syntax) => {
//     let sort = piece |> Piece.sort |> fst |> sort_of;
//     switch (piece_to_hand(piece)) {
//     | Some(hand) => Some((sort, Hand(hand)))
//     | None =>
//       open OptUtil.Syntax;
//       let+ card = piece_to_card(piece);
//       (sort, Card(card));
//     };
//   };

//   let mk_tuple = (sort: Sort.t, children): Piece.t =>
//     Tile({
//       id: Id.mk(),
//       label: ["(", ")"],
//       mold: Mold.mk_op(sort, [sort]),
//       shards: [0, 1],
//       children: [children],
//     });

//   let mk_text = (str): Piece.t =>
//     Tile({
//       id: Id.mk(),
//       label: [str],
//       mold: Mold.mk_op(Sort.Exp, []),
//       shards: [0],
//       children: [],
//     });

//   let piece_of_suit = (suit: suit): Piece.t =>
//     switch (suit) {
//     | Unknown(p) => p
//     | _ => suit |> sexp_of_suit |> Sexplib.Sexp.to_string |> mk_text
//     };

//   let piece_of_rank = (rank: rank) =>
//     switch (rank) {
//     | Unknown(p) => p
//     | _ => rank |> sexp_of_rank |> Sexplib.Sexp.to_string |> mk_text
//     };

//   let card_to_piece_exp = ((suit, rank): card): Piece.t =>
//     [
//       piece_of_suit(suit),
//       Piece.mk_tile(Form.get("comma_exp"), []),
//       piece_of_rank(rank),
//     ]
//     |> mk_tuple(Sort.Exp)
//     |> (x => [x])
//     |> mk_tuple(Sort.Exp);

//   let card_to_piece_pat = ((suit, rank): card): Piece.t =>
//     [
//       piece_of_suit(suit),
//       Piece.mk_tile(Form.get("comma_pat"), []),
//       piece_of_rank(rank),
//     ]
//     |> mk_tuple(Sort.Pat)
//     |> (x => [x])
//     |> mk_tuple(Sort.Pat);

//   let hand_to_piece_exp = (hand: hand): Piece.t =>
//     mk_tuple(Sort.Exp, List.map(card_to_piece_exp, hand));

//   let hand_to_piece_pat = (hand: hand): Piece.t =>
//     mk_tuple(Sort.Pat, List.map(card_to_piece_pat, hand));

//   let syntax_to_piece = (syntax: syntax): Piece.t =>
//     switch (syntax) {
//     | (Exp, Card(card)) => card_to_piece_exp(card)
//     | (Pat, Card(card)) => card_to_piece_pat(card)
//     | (Exp, Hand(hand)) => hand_to_piece_exp(hand)
//     | (Pat, Hand(hand)) => hand_to_piece_pat(hand)
//     };

//   let put = syntax_to_piece;

//   let get_opt = piece_to_syntax;

//   let get = (piece: Piece.t): syntax =>
//     switch (get_opt(piece)) {
//     | None => failwith("ERROR: Card: Not card or hand")
//     | Some(syntax) => syntax
//     };

//   let width_of_syntax = (syntax: syntax): int =>
//     switch (syntax) {
//     | (_, Card(_)) => 1
//     | (_, Hand(hand)) => List.length(hand)
//     };

//   let width_of_piece = (piece: Piece.t): int =>
//     switch (piece_to_syntax(piece)) {
//     | None => 0
//     | Some((_, Card(_)))
//     | Some((_, Hand([_]))) => 4
//     | Some((_, Hand(hand))) =>
//       //TODO: Better formula / card dimensions / offset
//       4 + List.length(hand) - (List.length(hand) + 66) / 24
//     };
// };

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
      | Pat => [Unknown("_")] //TODO
      };
    let maybe_suit: list(suit) =
      switch (sort) {
      | Exp => []
      | Pat => [Unknown("_")] //TODO
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
          print_endline("setting syntax");
          //TODO: make this work for hands
          Effect.Many([
            parent(
              SetSyntax(
                SyntaxTerm.put(info, (sort_of(sort), Card(card))),
              ),
            ),
            // Effect.Prevent_default,
            // Effect.Stop_propagation,
          ]);
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
      switch (JsUtil.is_double_click(evt)) {
      | _ when JsUtil.shift_held(evt) =>
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
        info,
        _elem_ids,
        mode,
        parent,
        local: action => Ui_effect.t(unit),
        sort: Sort.t,
        card: card,
      )
      : Node.t => {
    let on_mousedown = evt =>
      switch (JsUtil.is_double_click(evt)) {
      | _ when JsUtil.shift_held(evt) =>
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
        | Choose => Chooser.view(info, parent, sort, card)
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
            "position: absolute; left: %dpx; z-index: %d;",
            mode == Flipped ? 0 : index * 8,
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
  let can_project = (_, info) => SyntaxTerm.get_opt(info) != None;
  let can_focus = false;
  let dynamics = false;
  let placeholder = (_, info): ProjectorCore.shape => {
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
      ) => {
    switch (SyntaxTerm.get(info)) {
    | (sort, Card(card)) =>
      Singleton.view(info, model.mode, parent, local, to_sort(sort), card)
    | (sort, Hand(hand)) =>
      Hand.view(info, model.mode, parent, local, to_sort(sort), hand)
    };
  };
  let offside_view = None;
  let overlay_view = None;
  let underlay_view = None;
  let focus = _ => ();
};
