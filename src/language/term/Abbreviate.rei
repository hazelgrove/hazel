/* Shortens a term, typically a value, to fit a width budget for display,
   eliding what doesn't fit with an ellipsis. */

/** The shortened term and the columns it takes. `~available` defaults to
    12 columns. With `~see_through=true` the walk passes through ascriptions
    (in expressions and patterns) and projectors (in expressions), as if
    DHExp.strip_ascriptions and Exp.strip_projectors had run first, but
    touching only what the budget reaches. */
let abbreviate_exp:
  (~see_through: bool=?, ~available: int=?, Exp.t) => (Exp.t, int);
let abbreviate_pat: (~available: int=?, Pat.t) => (Pat.t, int);

/* Exported for Test_Abbreviate. */
module AbbrevBudget: {
  let split_evenly: (~total: int, ~parts: int) => list(int);
};
