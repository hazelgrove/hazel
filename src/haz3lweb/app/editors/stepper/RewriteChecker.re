open Haz3lcore;

// underscores indicate unused arguments
let check_rewrite = (_from: Exp.t, _to: Exp.t): bool => {
  let random = Random.float(1.0);
  random >= 0.5;
};
