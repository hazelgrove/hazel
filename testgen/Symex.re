open Haz3lmenhir;
[@deriving (show({with_path: false}), eq)]
module VariableState = {
  include Map.Make({
    type t = string;
    let compare = compare;
  });

  let pp =
      (
        fmt_a: (Format.formatter, 'a) => unit,
        formatter: Format.formatter,
        m: t('a),
      ) => {
    Format.fprintf(formatter, "{");
    iter(
      (key, value) => {
        Format.fprintf(formatter, "%s: ", key);
        fmt_a(formatter, value);
      },
      m,
    );
    Format.fprintf(formatter, "}");
  };
};

[@deriving (show({with_path: false}), eq)]
type symbolic_variable_state = VariableState.t(AST.exp(unit));

[@deriving (show({with_path: false}), eq)]
type assumptions = list(AST.exp(unit));
[@deriving (show({with_path: false}), eq)]
type symex_state = {
  symbolic_variable_state,
  assumptions,
};
let initial_state: symbolic_variable_state = VariableState.empty;
let initial_assumptions: assumptions = [];

let initial_symex_state: symex_state = {
  symbolic_variable_state: initial_state,
  assumptions: initial_assumptions,
};

[@deriving (show({with_path: false}), eq)]
type symex_exp = AST.Annotated.t(AST.exp(symex_state), symex_state);
let rec symbolic_execution =
        (~state: symex_state, exp: AST.exp(unit)): symex_exp => {
  switch (exp) {
  | If(cond, then_branch, else_branch) =>
    // TODO When do we substitute variables and such?
    let cond': symex_exp = symbolic_execution(~state, cond.term);
    let then' =
      symbolic_execution(
        ~state={
          symbolic_variable_state: state.symbolic_variable_state,
          assumptions: [cond.term, ...state.assumptions],
        },
        then_branch.term,
      );
    let else' =
      symbolic_execution(
        ~state={
          symbolic_variable_state: state.symbolic_variable_state,
          assumptions: [UnOp(Bool(Not), cond), ...state.assumptions],
        },
        else_branch.term,
      );

    let foo: AST.exp(symex_state) = If(cond', then', else');
    {
      term: foo,
      annotation: state,
    };
  | BinExp(left, op, right) =>
    let left' = symbolic_execution(~state, left.term);
    let right' = symbolic_execution(~state, right.term);
    {
      term: BinExp(left', op, right'),
      annotation: state,
    };
  | Var(name) => {
      term: Var(name),
      annotation: state,
    }
  | IndicationExp(e) =>
    let indicated = symbolic_execution(~state, e.term);
    {
      term: IndicationExp(indicated),
      annotation: state,
    };
  | Atom(atom) => {
      term: Atom(atom),
      annotation: state,
    }
  | _ =>
    raise(
      Failure(
        "Unsupported expression" ++ [%derive.show: AST.exp(unit)](exp),
      ),
    )
  };
};
