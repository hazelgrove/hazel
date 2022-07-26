[@deriving sexp]
type current =
  | ResultOk(ProgramResult.t)
  | ResultFail
  | ResultTimedOut
  | ResultPending;

[@deriving sexp]
type t = {
  prev: ProgramResult.t,
  current,
};

let init = {prev: ProgramResult.empty, current: ResultFail};

let get_prev_result = ({prev, _}) => prev;
let put_prev_result = (prev, cr) => {...cr, prev};
let get_prev_dhexp = cr => cr |> get_prev_result |> ProgramResult.get_dhexp;

let get_current = ({current, _}) => current;
let put_current = (current, cr) => {...cr, current};

let get_current_result = cr =>
  switch (cr |> get_current) {
  | ResultOk(r) => Some(r)
  | ResultFail
  | ResultTimedOut
  | ResultPending => None
  };
let get_current_dhexp = cr =>
  cr |> get_current_result |> Option.map(ProgramResult.get_dhexp);

let get_current_or_prev_result = cr =>
  cr |> get_current_result |> OptUtil.get(() => cr |> get_prev_result);
let get_current_or_prev_dhexp = cr =>
  cr |> get_current_dhexp |> OptUtil.get(() => cr |> get_prev_dhexp);
let update = (current, cr) => {
  let cr =
    switch (cr.current) {
    | ResultOk(r) => put_prev_result(r, cr)
    | ResultFail
    | ResultTimedOut
    | ResultPending => cr
    };

  put_current(current, cr);
};
