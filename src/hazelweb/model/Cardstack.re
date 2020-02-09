type t = {
  info: CardstackInfo.t,
  zcards: ZList.t(Card.t, Card.t),
};

let mk = (info: CardstackInfo.t): t => {
  let cards = info.cards |> List.map(Card.mk);
  assert(List.length(cards) != 0);
  let zcards = cards |> ZList.split_at(0) |> Option.get;
  {info, zcards};
};

let get_z = (cardstack: t): Card.t => cardstack.zcards |> ZList.prj_z;
let put_z = (zcard: Card.t, cardstack: t): t => {
  ...cardstack,
  zcards: cardstack.zcards |> ZList.replace_z(zcard),
};

let get_program = cardstack => cardstack |> get_z |> Card.get_program;
let put_program = (program, cardstack) => {
  let new_zcard = cardstack |> get_z |> Card.put_program(program);
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

let prev_card = (cardstack: t): t =>
  switch (cardstack.zcards |> ZList.shift_prev) {
  | None => cardstack
  | Some(zcards) => {...cardstack, zcards}
  };
let next_card = (cardstack: t): t =>
  switch (cardstack.zcards |> ZList.shift_next) {
  | None => cardstack
  | Some(zcards) => {...cardstack, zcards}
  };
