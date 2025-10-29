open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type op =
  | Add
  | Subtract
  | Multiply
  | Divide;

[@deriving (show({with_path: false}), sexp, yojson)]
type cal_state = {
  operation: op,
  operand: option(int),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type m = option(cal_state);

/* Calculator actions */
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | SelectOp(op)
  | SetOperand(int)
  | Clear;

include RichProbe.RichProbe with type model = m and type action = a;
