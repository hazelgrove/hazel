open Util;

module TokenKey = {
  type t = Token.t;
  let compare = compare;
};

module TokenSet = Set.Make(TokenKey);

type t = {
  left_obligations: list(Token.t),
  right_obligations: list(Token.t),
  left_target_tokens: TokenSet.t,
  right_target_tokens: TokenSet.t,
};

let is_multidelimiter_label = (label: Label.t): bool => List.length(label) > 1;

let token_of_shard = (shard: Tile.t): option(Token.t) =>
  switch (Tile.effective_label(shard)) {
  | [tok] => Some(tok)
  | _ => None
  };

let tokens_of_shards = (shards: list(Tile.t)): list(Token.t) =>
  List.filter_map(token_of_shard, shards);

let token_set_of_tokens = (tokens: list(Token.t)): TokenSet.t =>
  List.fold_left((acc, tok) => TokenSet.add(tok, acc), TokenSet.empty, tokens);

let rec deep_local_missing_shards_segment =
    (~side: Direction.t, seg: Segment.t): list(Tile.t) =>
  (
    switch (side) {
    | Left => List.rev(seg)
    | Right => seg
    }
  )
  |> List.concat_map(
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
           let children =
             switch (side) {
             | Left => List.rev(t.children)
             | Right => t.children
             }
             |> List.concat_map(deep_local_missing_shards_segment(~side));
           self @ children;
         }
       | _ => [],
     );

let left_target_tokens = (demand: t): TokenSet.t => demand.left_target_tokens;

let right_target_tokens = (demand: t): TokenSet.t => demand.right_target_tokens;

let rec tokens_of_segment = (seg: Segment.t): list(Token.t) =>
  List.concat_map(tokens_of_piece, seg)

and tokens_of_piece = (p: Piece.t): list(Token.t) =>
  switch (p) {
  | Piece.Tile(t) when List.length(t.shards) == 1 && t.children == [] =>
    tokens_of_shards([t])
  | Piece.Tile(t) => tokens_of_segment(Tile.disassemble(t))
  | _ => []
  };

let consume_tokens_in_order =
    (obligations: list(Token.t), available: list(Token.t)): list(Token.t) => {
  let rec go = (obligations, available) =>
    switch (obligations, available) {
    | ([], _) => []
    | (_, []) => obligations
    | ([need, ...rest_need] as obligations, [tok, ...rest_available]) =>
      need == tok ? go(rest_need, rest_available) : go(obligations, rest_available)
    };
  go(obligations, available);
};

let of_relatives = ({siblings: (pre, suf), ancestors}: Relatives.t): option(t) => {
  let left_obligations =
    tokens_of_shards(deep_local_missing_shards_segment(~side=Left, pre))
    @ tokens_of_shards(Ancestors.local_missing_shards(ancestors));
  let right_obligations =
    tokens_of_shards(deep_local_missing_shards_segment(~side=Right, suf));
  switch (left_obligations, right_obligations) {
  | ([], []) => None
  | _ =>
    Some({
        left_obligations,
        right_obligations,
        left_target_tokens: token_set_of_tokens(left_obligations),
        right_target_tokens: token_set_of_tokens(right_obligations),
      })
  };
};

let touches_generation = (demand: t, ((ancestor, parent_sibs): Ancestors.generation)): bool => {
  let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
  let left_available = tokens_of_segment(fst(parent_sibs) @ left_dis);
  let right_available = tokens_of_segment(right_dis @ snd(parent_sibs));
  List.exists(tok => TokenSet.mem(tok, left_target_tokens(demand)), right_available)
  || List.exists(tok => TokenSet.mem(tok, right_target_tokens(demand)), left_available);
};

let is_satisfied = (demand: t): bool =>
  switch (demand.left_obligations, demand.right_obligations) {
  | ([], []) => true
  | _ => false
  };

let cover_by_generation = (demand: t, ((ancestor, parent_sibs): Ancestors.generation)): t => {
  let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
  let left_available = tokens_of_segment(fst(parent_sibs) @ left_dis);
  let right_available = tokens_of_segment(right_dis @ snd(parent_sibs));
  let left_obligations =
    consume_tokens_in_order(demand.left_obligations, right_available);
  let right_obligations =
    consume_tokens_in_order(demand.right_obligations, left_available);
  {
    left_obligations,
    right_obligations,
    left_target_tokens: token_set_of_tokens(left_obligations),
    right_target_tokens: token_set_of_tokens(right_obligations),
  };
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
    ~target_tokens=left_target_tokens(demand),
    pre,
  ),
  flatten_tiles_with_target_demand(
    ~side=Right,
    ~target_tokens=right_target_tokens(demand),
    suf,
  ),
);
