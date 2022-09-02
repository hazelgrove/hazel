open Virtual_dom.Vdom;
open Node;

let card_option_view = (id: int, {name, _}: CardInfo.t): Node.t =>
  option(~attr=Attr.value(string_of_int(id)), [text(name)]);

let card_select =
    (~inject: ModelAction.t => Effect.t(_), cards_info: list(CardInfo.t)) => {
  let load_card = (_, id) =>
    inject(ModelAction.LoadCard(int_of_string(id)));
  select(
    ~attr=Attr.on_change(load_card),
    List.mapi(card_option_view, cards_info),
  );
};

let cardstack_option_view = (id: int, cardstack: CardstackInfo.t): Node.t =>
  option(~attr=Attr.value(string_of_int(id)), [text(cardstack.title)]);

let cardstack_select = (~inject: ModelAction.t => Effect.t(_)) => {
  let load_cardstack = (_, id) =>
    inject(ModelAction.LoadCardstack(int_of_string(id)));
  select(
    ~attr=Attr.on_change(load_cardstack),
    List.mapi(cardstack_option_view, Model.cardstack_info),
  );
};

let prev_card_button = (~inject, cardstack): Node.t => {
  let disabled = Cardstack.has_prev(cardstack) ? [] : [Attr.disabled];
  button(
    ~attr=
      Attr.many([
        Attr.id("cardstack-prev-button"),
        Attr.on_click(_ => inject(ModelAction.PrevCard)),
        ...disabled,
      ]),
    [text("<")],
  );
};

let next_card_button = (~inject, cardstack): Node.t => {
  let disabled = Cardstack.has_next(cardstack) ? [] : [Attr.disabled];
  button(
    ~attr=
      Attr.many([
        Attr.id("cardstack-next-button"),
        Attr.on_click(_ => inject(ModelAction.NextCard)),
        ...disabled,
      ]),
    [text(">")],
  );
};

let view = (~inject: ModelAction.t => Ui_effect.t(_), ~model: Model.t) => {
  let cardstack = Model.get_cardstack(model);
  let cards_info = Model.get_cards_info(model);
  div(
    ~attr=Attr.id("card-controls"),
    [
      cardstack_select(~inject),
      card_select(~inject, cards_info),
      prev_card_button(~inject, cardstack),
      next_card_button(~inject, cardstack),
    ],
  );
};
