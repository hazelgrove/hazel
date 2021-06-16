[@deriving sexp]
type t = {
  info: CardstackInfo.t,
  zcards: ZList.t(ZCard.t, Card.t),
};

let mk = (~width, info: CardstackInfo.t): t => {
  let cards = info.cards |> List.map(Card.mk);
  assert(List.length(cards) != 0);
  let zcards =
    cards
    |> ListUtil.split_nth_opt(0)
    |> Option.get
    |> ZList.map_z(ZCard.mk(~width));
  {info, zcards};
};

let get_z = (cardstack: t): ZCard.t => cardstack.zcards |> ZList.prj_z;
let put_z = (zcard: ZCard.t, cardstack: t): t => {
  ...cardstack,
  zcards: cardstack.zcards |> ZList.replace_z(zcard),
};

let get_program = cardstack => cardstack |> get_z |> ZCard.get_program;
let put_program = (program, cardstack) => {
  let new_zcard = cardstack |> get_z |> ZCard.put_program(program);
  cardstack |> put_z(new_zcard);
};

let has_prev = (cardstack: t): bool => {
  let (prefix, _, _) = cardstack.zcards;
  !ListUtil.is_empty(prefix);
};
let has_next = (cardstack: t): bool => {
  let (_, _, suffix) = cardstack.zcards;
  !ListUtil.is_empty(suffix);
};

let prev_card = (cardstack: t): t => {
  let width = get_program(cardstack).width;
  switch (cardstack.zcards |> ZList.map_z(ZCard.erase) |> ZList.shift_prev) {
  | None => cardstack
  | Some(shifted) => {
      ...cardstack,
      zcards: shifted |> ZList.map_z(ZCard.mk(~width)),
    }
  };
};
let next_card = (cardstack: t): t => {
  let width = get_program(cardstack).width;
  switch (cardstack.zcards |> ZList.map_z(ZCard.erase) |> ZList.shift_next) {
  | None => cardstack
  | Some(shifted) => {
      ...cardstack,
      zcards: shifted |> ZList.map_z(ZCard.mk(~width)),
    }
  };
};
