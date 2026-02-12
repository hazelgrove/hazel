[@deriving (show({with_path: false}), sexp, yojson)]
type op =
  | Add
  | Subtract
  | Multiply
  | Divide;

[@deriving (show({with_path: false}), sexp, yojson)]
type cal_state = {
  operation: op,
  operand: option(Bigint.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type m = option(cal_state);

/* Calculator actions */
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | SelectOp(op)
  | SetOperand(Bigint.t)
  | Clear;

include RichProbe.RichProbe with type model = m and type action = a;
