type filter = {
  act: FilterAction.t,
  pat: Exp.t,
};
type t = list(filter);
let extends = (flt: filter, env: list(filter)): list(filter) => [
  flt,
  ...env,
];
