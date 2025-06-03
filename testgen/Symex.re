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

let rec substitute =
        (
          var_name: string,
          original_expression: AST.exp(unit),
          new_expression: AST.exp(unit),
        ) => {
  let go:
    AST.Annotated.t(AST.exp(unit), unit) =>
    AST.Annotated.t(AST.exp(unit), unit) =
    x => {
      {
        term: substitute(var_name, x.term, new_expression),
        annotation: (),
      };
    };
  // Subst
  switch (original_expression) {
  | Var(name) when name == var_name => new_expression
  | Var(_) => original_expression
  | If(cond, then_branch, else_branch) =>
    If(go(cond), go(then_branch), go(else_branch))
  | BinExp(left, op, right) => BinExp(go(left), op, go(right))
  | UnOp(op, x) => UnOp(op, go(x))
  | Atom(_) => original_expression
  | IndicationExp(x) => IndicationExp(go(x))
  | _ =>
    raise(
      Failure(
        "Unsupported expression"
        ++ [%derive.show: AST.exp(unit)](original_expression),
      ),
    )
  };
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
  | Let({term: VarPat(var_name), _}, x, body) =>
    // Add var -> x to the symbolic variable state in the body

    let x': symex_exp = symbolic_execution(~state, x.term);
    let x_term: AST.exp(unit) = AST.map_exp_annotation(__ => (), x').term;
    let body' = symbolic_execution(~state, body.term);

    let body'' =
      AST.map_exp_annotation(
        state => {
          {
            symbolic_variable_state: state.symbolic_variable_state,
            assumptions:
              List.map(substitute(var_name, _, x_term), state.assumptions),
          }
        },
        body',
      );
    {
      term:
        Let(
          {
            term: VarPat(var_name),
            annotation: x'.annotation,
          },
          x',
          body'',
        ),
      annotation: state,
    };
  | UnOp(op, x) =>
    let x' = symbolic_execution(~state, x.term);
    {
      term: UnOp(op, x'),
      annotation: state,
    };
  | _ =>
    raise(
      Failure(
        "Unsupported expression" ++ [%derive.show: AST.exp(unit)](exp),
      ),
    )
  };
};
