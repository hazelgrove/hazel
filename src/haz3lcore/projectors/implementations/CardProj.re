open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open CardTypes;

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

module SyntaxTerm = {
  let put = (info, syntax): option(Base.segment) =>
    info.utility.lift_syntax(
      ~inline=true,
      _ => CardSyntax.state_to_any(syntax),
      info.syntax,
    );

  let get_opt = (any: Any.t): option(state) => CardSyntax.any_to_state(any);

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

  /* A hand is a FAN: each card after the first advances 8.5px (see
     card_wrapper), ~0.817 columns at default zoom. The base covers the
     top card (37px incl. border ~3.56 cols) plus the tab-extent's
     chevron insets (2 x 0.4 col) and a hair of slack. */
  let width_of_any = (info: info): int =>
    switch (
      info.syntax
      |> info.utility.seg_to_term
      |> OptUtil.and_then(CardSyntax.any_to_state)
    ) {
    | None => 0
    | Some((_, Card(_)))
    | Some((_, Hand([_]))) => 5
    | Some((_, Hand(hand))) =>
      Float.ceil(
        4.6 +. 817. /. 1000. *. (Float.of_int(List.length(hand)) -. 1.),
      )
      |> Float.to_int
    };
};

module Chooser = {
  let on_pick =
      (info, parent, index: option(int), card: card): Ui_effect.t(unit) => {
    let act =
      switch (index) {
      | None => ReplaceCard(card)
      | Some(index) => ReplaceCardInHand(index, card)
      };
    switch (act |> update(SyntaxTerm.get(info)) |> SyntaxTerm.put(info)) {
    | None => Effect.Ignore
    | Some(seg) => parent(SetSyntax(seg))
    };
  };

  let view =
      (info, parent, sort: Sort.t, card: card, index: option(int)): Node.t =>
    CardView.Chooser.view(
      ~on_pick=on_pick(info, parent, index),
      ~indicated=card,
      sort,
      sort_of(sort),
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
        | Show => CardView.Card.view(sort, card)
        | Choose(_) => Chooser.view(info, parent, sort, card, None)
        | Flipped => CardView.Card.view(sort, card)
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
        | Show => CardView.Card.view(sort, card)
        | Choose(cidx) =>
          cidx == index
            ? Chooser.view(info, parent, sort, card, Some(index))
            : CardView.Card.view(sort, card)
        | Flipped => CardView.Card.view(sort, card)
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
      ~attrs=[
        Attr.classes(["hand", Sort.show(sort)]),
        /* fan footprint: cards advance 8.5px, top card is full width */
        Attr.create(
          "style",
          Printf.sprintf(
            "width: %fpx;",
            hand == []
              ? 37. : 8.5 *. float_of_int(List.length(hand) - 1) +. 37.,
          ),
        ),
      ],
      hand == []
        ? [CardView.Empty.view]
        : List.mapi(
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
  let elaborate_syntax = false;

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

  let error = (_, _): option(ProjectorBase.error) => None;

  let view =
      ({model, info, local, parent, _}: View.args(model, action)): View.t => {
    inline:
      switch (SyntaxTerm.get(info)) {
      | (sort, Card(card)) =>
        Singleton.view(info, model.mode, parent, local, to_sort(sort), card)
      | (sort, Hand(hand)) =>
        Hand.view(info, model.mode, parent, local, to_sort(sort), hand)
      },
    offside: None,
    overlay: None,
    below: None,
    error: false,
  };
};
