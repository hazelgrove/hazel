/* Shortens an expression to fit a width budget, for the probe and table
   projectors. Every per-form abbreviator is private, as is the rest of the
   budget arithmetic -- `AbbrevBudget.split_evenly` is exported only because
   Test_Abbreviate covers it directly. */

let abbreviate_exp: (~available: int=?, Exp.t) => (Exp.t, int);
let abbreviate_pat: (~available: int=?, Pat.t) => (Pat.t, int);

module AbbrevBudget: {
  let split_evenly: (~total: int, ~parts: int) => list(int);
};
