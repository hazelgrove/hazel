// This may return closures
let evaluate': (~env: Environment.t, Exp.t) => (Exp.t, EvaluatorState.t);

// INVARIANT: this evaluate function should never return an expression with closures.

let evaluate: (~env: Environment.t, Exp.t) => (Exp.t, EvaluatorState.t);
