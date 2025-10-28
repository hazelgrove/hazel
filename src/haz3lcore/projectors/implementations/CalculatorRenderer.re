open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* CalculatorRenderer - An interactive calculator for integer arithmetic operations */

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

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;

/* Calculator actions that can be performed on the integer */
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;

/* Reusable UI components */
let rec display_value = (value: int, pending_op: option(cal_state)) =>
  switch (pending_op) {
  | None => string_of_int(value)
  | Some({operation, operand: None}) =>
    string_of_int(value) ++ " " ++ op_to_string(operation) ++ " ?"
  | Some({operation, operand: Some(op_val)}) =>
    string_of_int(value)
    ++ " "
    ++ op_to_string(operation)
    ++ " "
    ++ string_of_int(op_val)
  }
and op_to_string = op =>
  switch (op) {
  | Add => "+"
  | Subtract => "-"
  | Multiply => "*"
  | Divide => "/"
  };

/* Calculator detection from expressions - detect integer atoms */
let is_int_exp = (exp: Exp.t) =>
  switch (exp.term) {
  | Atom(atom) =>
    switch (atom) {
    | Atom.Int(_)
    | Atom.SInt(_) => true
    | _ => false
    }
  | _ => false
  };

/* Core transformation functions for applying arithmetic operations */
let apply_arithmetic_operation = (info: info, operation: op, operand: int) =>
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            BinOp(
              switch (operation) {
              | Add => Int(Operators.Plus)
              | Subtract => Int(Operators.Minus)
              | Multiply => Int(Operators.Times)
              | Divide => Int(Operators.Divide)
              },
              exp_term |> DHExp.fresh,
              Atom(Atom.SInt(operand)) |> DHExp.fresh,
            )
            |> DHExp.fresh,
          )
        | _ =>
          failwith(
            "CalculatorRenderer: apply_arithmetic_operation: not an expression",
          ),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None =>
      failwith("CalculatorRenderer: apply_arithmetic_operation: lift failed")
    }
  );

let apply_operation = (info, operation, operand, local, parent) => {
  let segment = apply_arithmetic_operation(info, operation, operand);
  Effect.Many([local(Clear), parent(ProjectorBase.SetSyntax(segment))]);
};

let get_int_value = (exp: Exp.t): option(int) =>
  switch (exp.term) {
  | Atom(atom) =>
    switch (atom) {
    | Atom.SInt(value) => Some(value)
    | Atom.Int(bigint) =>
      switch (Bigint.to_int(bigint)) {
      | Some(i) => Some(i)
      | None => None /* too big for int */
      }
    | _ => None
    }
  | _ => None
  };

/* Calculator buttons */
let digit_buttons = (local, current_operand) =>
  List.map(
    digit =>
      Node.button(
        ~attrs=[
          Attr.classes(["calculator-digit"]),
          Attr.on_click(_ =>
            switch (current_operand) {
            | None => local(SetOperand(digit))
            | Some(current) => local(SetOperand(current * 10 + digit))
            }
          ),
        ],
        [Node.text(string_of_int(digit))],
      ),
    [7, 8, 9, 4, 5, 6, 1, 2, 3, 0],
  );

let op_buttons = local =>
  [
    ("+", _ => local(SelectOp(Add))),
    ("-", _ => local(SelectOp(Subtract))),
    ("*", _ => local(SelectOp(Multiply))),
    ("/", _ => local(SelectOp(Divide))),
  ]
  |> List.map(((text, action)) =>
       Node.button(
         ~attrs=[Attr.classes(["calculator-op"]), Attr.on_click(action)],
         [Node.text(text)],
       )
     );

let control_buttons = (info, local, can_apply, state_opt, parent) => [
  Node.button(
    ~attrs=[
      Attr.classes(["calculator-clear"]),
      Attr.on_click(_ => local(Clear)),
    ],
    [Node.text("C")],
  ),
  Node.button(
    ~attrs=[
      Attr.classes(["calculator-apply"] @ (can_apply ? [] : ["disabled"])),
      Attr.on_click(_ =>
        switch (state_opt) {
        | Some({operation, operand: Some(operand)}) =>
          apply_operation(info, operation, operand, local, parent)
        | _ => Effect.Ignore
        }
      ),
    ],
    [Node.text("=")],
  ),
];

/* Main calculator rendering function */
let render = (~info, ~exp, ~view_seg, ~model, ~local, ~parent, ()) => {
  let current_value = get_int_value(exp) |> Option.value(~default=0);
  let state_opt =
    switch (model) {
    | Some(s) when Option.is_some(s.operand) => Some(s)
    | _ => None
    };
  let can_apply = Option.is_some(state_opt);

  Node.div(
    ~attrs=[Attr.classes(["calculator"])],
    [
      Node.div(
        ~attrs=[Attr.classes(["calculator-display"])],
        [Node.text(display_value(current_value, model))],
      ),
      Node.div(
        ~attrs=[Attr.classes(["calculator-keypad"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["calculator-digits"])],
            digit_buttons(
              local,
              Option.bind(model, ({operand, _}) => operand),
            ),
          ),
          Node.div(
            ~attrs=[Attr.classes(["calculator-ops"])],
            op_buttons(local),
          ),
          Node.div(
            ~attrs=[Attr.classes(["calculator-controls"])],
            control_buttons(info, local, can_apply, state_opt, parent),
          ),
        ],
      ),
    ],
  );
};

let update: (model, action) => model =
  (model, action) => {
    switch (action, model) {
    | (SelectOp(operation), _) =>
      Some({
        operation,
        operand: None,
      })
    | (SetOperand(new_operand), Some({operation, _} as state)) =>
      Some({
        ...state,
        operand: Some(new_operand),
      })
    | (Clear, _) => None
    | _ => model
    };
  };

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["calculator-badge"]),
      Attr.title("Click to open calculator"),
    ],
    [Node.text("🧮")],
  );

let init = exp => {
  print_endline("CalculatorRenderer: init called");
  print_endline("Expression: " ++ Exp.show(exp));
  if (is_int_exp(exp)) {
    Some(None);
  } else {
    None;
  };
};

module M = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = m;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;

  let update = update;
  let init = init;
  let badge = badge;
  let render = render;
};
