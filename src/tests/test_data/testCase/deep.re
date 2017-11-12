switch (
  switch v {
  | HazelPrelude.L el => el
  | HazelPrelude.R er => er
  }: HazelPrelude.hole
) {
| HazelPrelude.L l =>
  switch l {
  | HazelPrelude.L ll => ll
  | HazelPrelude.R lr => lr
  }
| HazelPrelude.R r =>
  switch r {
  | HazelPrelude.L rl => rl
  | HazelPrelude.R rr => rr
  }
};
