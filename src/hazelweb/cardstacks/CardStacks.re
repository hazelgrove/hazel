type cardstacks = list(CardStack.t);
let cardstacks: cardstacks = [
  TutorialCards.cardstack,
  // RCStudyCards.cardstack,
];
type edit_state = Statics.edit_state;

type card_state = {
  card: Card.t,
  edit_state,
};

type cardstack_state = {
  cardstack: CardStack.t,
  zcards: ZList.t(card_state, card_state),
};

type cardstacks_state = ZList.t(cardstack_state, cardstack_state);
let mk_cardstack_state = (cardstack: CardStack.t) => {
  let card_states =
    List.map(
      card =>
        {
          card,
          edit_state:
            card.init_zexp
            |> Statics.Exp.fix_and_renumber_holes_z(Contexts.empty),
        },
      cardstack.cards,
    );
  let zcards =
    OptUtil.get(_ => failwith("no cards"), ZList.split_at(0, card_states));
  {cardstack, zcards};
};

let mk_cardstacks_state = cardstacks => {
  let cardstack_states = List.map(mk_cardstack_state, cardstacks);
  OptUtil.get(
    _ => failwith("no cardstacks"),
    ZList.split_at(0, cardstack_states),
  );
};
