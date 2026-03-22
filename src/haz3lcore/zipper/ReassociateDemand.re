open Util;

module TokenKey = {
  type t = Token.t;
  let compare = compare;
};

module TokenSet = Set.Make(TokenKey);

module TokenDemand = Map.Make(TokenKey);

type t = {
  token_demand: TokenDemand.t(int),
  target_tokens: TokenSet.t,
};

let is_multidelimiter_label = (label: Label.t): bool => List.length(label) > 1;

let token_demand_of_tokens = (tokens: list(Token.t)): TokenDemand.t(int) =>
  List.fold_left(
    (acc, tok) =>
      TokenDemand.update(
        tok,
        opt => Some(switch (opt) { | Some(n) => n + 1 | None => 1 }),
        acc,
      ),
    TokenDemand.empty,
    tokens,
  );

let token_demand_of_shards = (shards: list(Tile.t)): TokenDemand.t(int) =>
  shards
  |> List.filter_map((shard: Tile.t) =>
       switch (Tile.effective_label(shard)) {
       | [tok] => Some(tok)
       | _ => None
       }
     )
  |> token_demand_of_tokens;

let token_set_of_token_demand = (demand: TokenDemand.t(int)): TokenSet.t =>
  TokenDemand.fold((tok, _, acc) => TokenSet.add(tok, acc), demand, TokenSet.empty);

let token_demand_contains =
    (demand: TokenDemand.t(int), tok: Token.t): bool => TokenDemand.mem(tok, demand);

let token_demand_remove =
    (demand: TokenDemand.t(int), tok: Token.t): TokenDemand.t(int) =>
  switch (TokenDemand.find_opt(tok, demand)) {
  | Some(n) when n > 1 => TokenDemand.add(tok, n - 1, demand)
  | Some(_) => TokenDemand.remove(tok, demand)
  | None => demand
  };

let token_demand_subtract_tokens =
    (demand: TokenDemand.t(int), tokens: list(Token.t)): TokenDemand.t(int) =>
  List.fold_left(token_demand_remove, demand, tokens);

let rec deep_local_missing_shards_segment =
    (~side: Direction.t, seg: Segment.t): list(Tile.t) =>
  List.concat_map(
    fun
    | Piece.Tile(t) => {
        let self =
          if (!Tile.is_complete(t) && is_multidelimiter_label(t.label)) {
            switch (side) {
            | Left => Tile.right_missing_shards(t)
            | Right => Tile.left_missing_shards(t)
            };
          } else {
            [];
          };
        self
        @ List.concat_map(deep_local_missing_shards_segment(~side), t.children);
      }
    | _ => [],
    seg,
  );

let of_relatives = ({siblings: (pre, suf), ancestors}: Relatives.t): option(t) => {
  let target_shards =
    deep_local_missing_shards_segment(~side=Left, pre)
    @ deep_local_missing_shards_segment(~side=Right, suf)
    @ Ancestors.local_missing_shards(ancestors);
  switch (target_shards) {
  | [] => None
  | shards =>
    let token_demand = token_demand_of_shards(shards);
    Some({
        token_demand,
        target_tokens: token_set_of_token_demand(token_demand),
      })
  };
};

let touches_ancestor = (demand: t, ancestor: Ancestor.t): bool =>
  List.exists(token_demand_contains(demand.token_demand), ancestor.label);

let is_satisfied = (demand: t): bool => TokenDemand.is_empty(demand.token_demand);

let cover_by_label = (demand: t, label: Label.t): t => {
  ...demand,
  token_demand: token_demand_subtract_tokens(demand.token_demand, label),
};

let tile_has_target_token_demand =
    (~side: Direction.t, ~target_tokens: TokenSet.t, t: Tile.t): bool =>
  (
    switch (side) {
    | Left => Tile.right_missing_shards(t)
    | Right => Tile.left_missing_shards(t)
    }
  )
  |> List.exists(shard =>
       switch (Tile.effective_label(shard)) {
       | [tok] => TokenSet.mem(tok, target_tokens)
       | _ => false
       }
     );

let shard_pieces = (t: Tile.t) =>
  List.map(
    s =>
      Piece.Tile({
        ...t,
        shards: [s],
        children: [],
      }),
    t.shards,
  );

/* Recursively crack only along paths that lead to the currently
   relevant unresolved delimiter demand. This is narrower than
   "any incomplete descendant": unrelated complete tiles stay intact,
   but complete wrappers on the path to relevant demand still crack. */
let rec flatten_tiles_with_relevant_incomplete =
    (~side: Direction.t, ~target_tokens: TokenSet.t, seg: Segment.t)
    : (bool, Segment.t) =>
  List.fold_right(
    (p, (acc_has_relevant, acc_seg)) =>
      switch (p) {
      | Piece.Tile(t) =>
        let child_results =
          List.map(
            flatten_tiles_with_relevant_incomplete(~side, ~target_tokens),
            t.children,
          );
        let child_has_relevant =
          List.exists(((has_relevant, _)) => has_relevant, child_results);
        let self_has_relevant =
          !Tile.is_complete(t)
          && is_multidelimiter_label(t.label)
          && tile_has_target_token_demand(~side, ~target_tokens, t);
        let has_relevant = self_has_relevant || child_has_relevant;
        if (
          Tile.is_complete(t)
          && is_multidelimiter_label(t.label)
          && child_has_relevant
        ) {
          let flattened_children = List.map(snd, child_results);
          let cracked =
            Aba.mk(shard_pieces(t), flattened_children)
            |> Aba.join(p => [p], Fun.id)
            |> List.flatten;
          (has_relevant || acc_has_relevant, cracked @ acc_seg);
        } else {
          (has_relevant || acc_has_relevant, [p, ...acc_seg]);
        };
      | _ => (acc_has_relevant, [p, ...acc_seg])
      },
    seg,
    (false, []),
  );

let flatten_tiles_with_target_demand =
    (~side: Direction.t, ~target_tokens: TokenSet.t, seg: Segment.t)
    : Segment.t =>
  flatten_tiles_with_relevant_incomplete(~side, ~target_tokens, seg) |> snd;

let crack_siblings = (~demand: t, (pre, suf): Siblings.t): Siblings.t => (
  flatten_tiles_with_target_demand(
    ~side=Left,
    ~target_tokens=demand.target_tokens,
    pre,
  ),
  flatten_tiles_with_target_demand(
    ~side=Right,
    ~target_tokens=demand.target_tokens,
    suf,
  ),
);
