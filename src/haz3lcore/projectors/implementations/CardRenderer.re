open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open CardTypes;

/* CardRenderer is the rich-probe view for card-shaped sample values.
   The separate CardProj.re owns the alt-l inline projector with its
   own click-to-flip / shift-click-to-choose UX; this renderer is
   purely structural — it shows the sample's value and nothing more,
   so there's no model state and no actions. */

[@deriving (show({with_path: false}), sexp, yojson)]
type m = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type a = unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = collection;

let parse = (_sort: Sort.t, exp: Exp.t): option(value) =>
  switch (CardSyntax.any_to_state(Exp(exp))) {
  | Some((Exp, c)) => Some(c)
  | _ => None
  };

let init = (_: value) => ();

let update = ((), ()) => ();

/* Match CardProj.placeholder (Tab(1)) so a line that already hosts a
   CardProj — which reserves 1 deferred linebreak for its own card art
   — doesn't get extra rows tacked on by the probe's reservation. The
   refractor and projector pipelines both feed DeferredLinebreaks.update,
   which max-merges, so equal values mean the bigger of the two wins
   without compounding. */
let placeholder = (_: value, _: m): ProjectorCore.Shape.t =>
  ProjectorCore.Shape.{
    vertical: Tab(1),
    horizontal: 0,
  };

let projector_attrs = (sort: Sort.t) =>
  Attr.classes(["projector", "card", Sort.show(sort)]);

module Singleton = {
  let view = (sort: Sort.t, card: card): Node.t =>
    Node.div(
      ~attrs=[projector_attrs(sort)],
      [
        Node.div(
          ~attrs=[Attr.classes(["card-wrapper"])],
          [CardView.Card.view(sort, card)],
        ),
      ],
    );
};

module Hand = {
  let card_wrapper = (sort: Sort.t, index: int, card: card): Node.t =>
    Node.div(
      ~attrs=[
        Attr.class_("card-wrapper"),
        Attr.create(
          "style",
          Printf.sprintf(
            "position: absolute; left: %fpx; z-index: %d;",
            float_of_int(index) *. 8.5,
            100 + index,
          ),
        ),
      ],
      [CardView.Card.view(sort, card)],
    );

  let view = (sort: Sort.t, hand: hand): Node.t => {
    let n = List.length(hand);
    let width =
      Float.to_int(Float.ceil(Float.of_int(n - 1) *. 8.5))
      + CardView.Card.width;
    Node.div(
      ~attrs=[projector_attrs(sort)],
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
          List.mapi(card_wrapper(sort), hand),
        ),
      ],
    );
  };
};

let render =
    (
      ~info as _: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg as _: (Sort.t, Segment.t) => Node.t,
      ~model as _: m,
      ~local as _: a => Ui_effect.t(unit),
      ~parent as _: external_action => Ui_effect.t(unit),
      ~sort as _: Sort.t,
      (),
    )
    : Node.t =>
  switch (value) {
  | Card(card) => Singleton.view(Sort.Exp, card)
  | Hand(hand) => Hand.view(Sort.Exp, hand)
  };

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["card-badge"]),
      Attr.title("Click to view cards visually"),
    ],
    [Node.text({js|♠️|js})],
  );
