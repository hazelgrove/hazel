type t =
  | Pat(Shard_pat.t)
  | Exp(Shard_exp.t);

let mk_text: (Sort.t, string) => Shard.t;