/**
 * All cardstacks along with current cardstack,
 * current cardstack contains current `Program`
 */
[@deriving sexp]
type t = ZList.t(Cardstack.t, Cardstack.t);

let mk = (~width, info: list(CardstackInfo.t)): t => {
  assert(List.length(info) != 0);
  let cardstacks = info |> List.map(Cardstack.mk(~width));
  cardstacks |> ZList.split_at(0) |> Option.get;
};

let get_z = ZList.prj_z;
let put_z = ZList.replace_z;
let map_z = ZList.map_z;

let get_program = cardstacks => cardstacks |> get_z |> Cardstack.get_program;
let put_program = (program, cardstacks): t => {
  let new_cardstack = cardstacks |> get_z |> Cardstack.put_program(program);
  cardstacks |> put_z(new_cardstack);
};

let load_cardstack = (i: int, cardstacks: t): t => {
  let cardstacks_list = ZList.erase(cardstacks, x => x);
  cardstacks_list |> ZList.split_at(i) |> Option.get;
};

let get_current_cardstack_index = ZList.get_index;
let get_current_card_index = zcardstacks =>
  zcardstacks |> get_z |> ((cs: Cardstack.t) => cs.zcards) |> ZList.get_index;
