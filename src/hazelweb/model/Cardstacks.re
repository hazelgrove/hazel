type t = ZList.t(Cardstack.t, Cardstack.t);

let mk = (info: list(CardstackInfo.t)): t => {
  assert(List.length(info) != 0);
  let cardstacks = info |> List.map(Cardstack.mk);
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

let follow_cursor_path = (cursor_path: CursorPath.t, cardstacks: t): t => {
  let cardstack = cardstacks |> get_z;
  let new_ze =
    (cardstacks |> get_z |> Cardstack.get_z).info.init_zexp
    |> ZExp.erase
    |> CursorPath.Exp.follow(cursor_path);
  let new_cardstack =
    switch (new_ze) {
    | None =>
      JSUtil.log("fail follow");
      cardstack;

    | Some(new_ze') => Cardstack.put_zexp(new_ze', cardstack)
    };
  put_z(new_cardstack, cardstacks);
};
