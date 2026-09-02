open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open CardTypes;

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
type model = m;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = collection;

let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
  switch (model_of_sexp(sexp)) {
  | exception _ => {mode: Show}
  | m => m
  };

let parse = (_sort: Sort.t, exp: Exp.t): option(value) =>
  switch (CardSyntax.any_to_state(Exp(exp))) {
  | Some((Exp, c)) => Some(c)
  | _ => None
  };

let init = (_: value) => {mode: Show};
let empty = {mode: Show};

/* Card sprites are 47px tall (~3 editor rows); a hand fans in one row
   of cards regardless of count. */
let drawer_rows = (_: value): int => 4;

let update: (m, a) => m =
  (_, action) =>
    switch (action) {
    | SetMode(new_mode) => {mode: new_mode}
    };

let put = (info: info, card: card): option(Base.segment) =>
  info.utility.lift_syntax(
    ~inline=true,
    _: Any.t => Exp(CardSyntax.card_to_exp(card)),
    info.syntax,
  );

let on_pick =
    (info: info, parent: external_action => Ui_effect.t(unit), card: card)
    : Ui_effect.t(unit) =>
  switch (put(info, card)) {
  | None => Effect.Ignore
  | Some(seg) => parent(SetSyntax(seg))
  };

let mode_click = (mode, can_choose: bool, local, evt) =>
  switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
  | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) && can_choose =>
    switch (mode) {
    | Choose
    | Flipped => local(SetMode(Show))
    | Show => local(SetMode(Choose))
    }
  | _ =>
    switch (mode) {
    | Choose
    | Flipped => local(SetMode(Show))
    | Show => local(SetMode(Flipped))
    }
  };

let mode_class = (mode: mode) =>
  switch (mode) {
  | Show => "show"
  | Flipped => "flipped"
  | Choose => "choose"
  };

let projector_attrs = (mode: mode, sort: Sort.t) =>
  Attr.classes(["projector", "card", Sort.show(sort), mode_class(mode)]);

module Singleton = {
  let view =
      (
        info: info,
        mode: mode,
        parent: external_action => Ui_effect.t(unit),
        local: a => Ui_effect.t(unit),
        sort: Sort.t,
        card: card,
      )
      : Node.t =>
    Node.div(
      ~attrs=[projector_attrs(mode, sort)],
      [
        Node.div(
          ~attrs=[
            Attr.classes(["card-wrapper", mode_class(mode)]),
            Attr.on_mousedown(mode_click(mode, true, local)),
          ],
          [
            switch (mode) {
            | Show
            | Flipped => CardView.Card.view(sort, card)
            | Choose =>
              CardView.Chooser.view(
                ~on_pick=on_pick(info, parent),
                ~indicated=card,
                sort,
                sort_of(sort),
              )
            },
          ],
        ),
      ],
    );
};

module Hand = {
  let card_wrapper =
      (mode: mode, sort: Sort.t, index: int, card: card): Node.t =>
    Node.div(
      ~attrs=[
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
      [CardView.Card.view(sort, card)],
    );

  let view =
      (mode: mode, local: a => Ui_effect.t(unit), sort: Sort.t, hand: hand)
      : Node.t => {
    let n = List.length(hand);
    let width =
      mode == Flipped || n == 0
        ? CardView.Card.width
        : Float.to_int(Float.ceil(Float.of_int(n - 1) *. 8.5))
          + CardView.Card.width;
    Node.div(
      ~attrs=[
        projector_attrs(mode, sort),
        Attr.on_mousedown(mode_click(mode, false, local)),
      ],
      [
        Node.div(
          ~attrs=[
            Attr.classes(["hand", Sort.show(sort)]),
            Attr.create(
              "style",
              Printf.sprintf(
                "width: %dpx; height: %dpx;",
                width,
                CardView.Card.height,
              ),
            ),
          ],
          hand == []
            ? [CardView.Empty.view]
            : List.mapi(card_wrapper(mode, sort), hand),
        ),
      ],
    );
  };
};

let render =
    (
      ~info: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg as _: (Sort.t, Segment.t) => Node.t,
      ~model: m,
      ~local: a => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort as _: Sort.t,
      (),
    )
    : Node.t =>
  switch (value) {
  | Card(card) =>
    Singleton.view(info, model.mode, parent, local, Sort.Exp, card)
  | Hand(hand) => Hand.view(model.mode, local, Sort.Exp, hand)
  };

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["card-badge"]),
      Attr.title("Click to view cards visually"),
    ],
    [Node.text({js|♠️|js})],
  );
