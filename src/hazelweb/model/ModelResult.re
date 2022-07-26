[@deriving sexp]
type previous = ProgramResult.t;

[@deriving sexp]
type current =
  | ResultOk(ProgramResult.t)
  | ResultFail(ProgramEvaluator.evaluation_exn)
  | ResultTimedOut
  | ResultPending;

[@deriving sexp]
type t = {
  previous,
  current,
};

let init = {previous: ProgramResult.empty, current: ResultPending};

let get_previous_result = ({previous, _}) => previous;
let put_previous_result = (previous, cr) => {...cr, previous};
let get_previous_dhexp = cr =>
  cr |> get_previous_result |> ProgramResult.get_dhexp;

let get_current = ({current, _}) => current;
let put_current = (current, cr) => {...cr, current};

let get_current_result = cr =>
  switch (cr |> get_current) {
  | ResultOk(r) => Some(r)
  | ResultFail(_)
  | ResultTimedOut
  | ResultPending => None
  };
let get_current_dhexp = cr =>
  cr |> get_current_result |> Option.map(ProgramResult.get_dhexp);

let get_current_or_previous_result = cr =>
  cr |> get_current_result |> OptUtil.get(() => cr |> get_previous_result);
let get_current_or_previous_dhexp = cr =>
  cr |> get_current_dhexp |> OptUtil.get(() => cr |> get_previous_dhexp);
let update = (current, cr) => {
  let cr =
    switch (cr.current) {
    | ResultOk(r) => put_previous_result(r, cr)
    | ResultFail(_)
    | ResultTimedOut
    | ResultPending => cr
    };

  put_current(current, cr);
};
