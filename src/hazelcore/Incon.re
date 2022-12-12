let rec is_reduntant_int = (xs: list(int)): bool =>
  switch (xs) {
  | [] => false
  | [_, ...xis] => is_reduntant_int(xis)
  };
let rec is_reduntant_float = (xs: list(float)): bool =>
  switch (xs) {
  | [] => false
  | [_, ...xis] => is_reduntant_float(xis)
  };
