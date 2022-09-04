[@deriving sexp]
type previous = ProgramResult.t;

[@deriving sexp]
type current =
  | ResultOk(ProgramResult.t)
  | ResultFail(ProgramEvaluator.exn_error)
  | ResultTimeout
  | ResultPending;

[@deriving sexp]
type t = {
  previous,
  current,
};

let init = previous => {previous, current: ResultPending};

let get_previous = ({previous, _}) => previous;
let put_previous = (previous, cr) => {...cr, previous};
let get_previous_dhexp = cr => cr |> get_previous |> ProgramResult.get_dhexp;

let get_current = ({current, _}) => current;

let update_current = (current, res) => {
  let res =
    switch (res.current) {
    | ResultOk(r) => put_previous(r, res)
    | ResultFail(_)
    | ResultTimeout
    | ResultPending => res
    };

  let res = {...res, current};
  res;
};
