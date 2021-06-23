open Virtual_dom.Vdom;
open Node;

let card_to_option = (id, {name, _}: CardInfo.t): Node.t =>
  option([Attr.value(string_of_int(id))], [text(name)]);

let card_select =
    (
      ~inject: ModelAction.t => Event.t,
      cardstacks,
      cardstack_info: list(CardstackInfo.t),
    ) => {
  let cards =
    switch (cardstacks |> ZList.prefix_length |> List.nth_opt(cardstack_info)) {
    | None => []
    | Some({cards, _}) => cards
    };
  Node.select(
    [
      Attr.on_change((_, id) =>
        inject(ModelAction.LoadCard(int_of_string(id)))
      ),
    ],
    List.mapi(card_to_option, cards),
  );
};

let cardstacks_select =
    (~inject: ModelAction.t => Event.t, cardstacks: list(CardstackInfo.t)) => {
  let cardstack_options =
    List.mapi(
      (i, cardstack: CardstackInfo.t) => {
        let example_idx = string_of_int(i);
        Node.option(
          [Attr.value(example_idx)],
          [Node.text(cardstack.title)],
        );
      },
      cardstacks,
    );
  Node.select(
    [
      Attr.on_change((_, example_idx) =>
        inject(ModelAction.LoadCardstack(int_of_string(example_idx)))
      ),
    ],
    cardstack_options,
  );
};

let prev_card_button = (~inject, cardstack) => {
  let show_prev = Cardstack.has_prev(cardstack) ? [] : [Attr.disabled];
  Node.button(
    [
      Attr.id("cardstack-prev-button"),
      Attr.on_click(_ => inject(ModelAction.PrevCard)),
      ...show_prev,
    ],
    [Node.text("<")],
  );
};

let next_card_button = (~inject, cardstack) => {
  let show_next = Cardstack.has_next(cardstack) ? [] : [Attr.disabled];
  Node.button(
    [
      Attr.id("cardstack-next-button"),
      Attr.on_click(_ => inject(ModelAction.NextCard)),
      ...show_next,
    ],
    [Node.text(">")],
  );
};

let view = (~inject: ModelAction.t => Ui_event.t, ~model: Model.t) => {
  let cardstack_info = Model.cardstack_info;
  let cardstack = Model.get_cardstack(model);
  let cardstacks = model.cardstacks;
  div(
    [Attr.id("card-controls")],
    [
      cardstacks_select(~inject, cardstack_info),
      card_select(~inject, cardstacks, cardstack_info),
      prev_card_button(~inject, cardstack),
      next_card_button(~inject, cardstack),
    ],
  );
};
