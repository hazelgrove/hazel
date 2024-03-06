let shard_left_shape = (t: Tile.t): Nib.Shape.t => {
  switch (t.shards) {
  | [idx, ..._] =>
    if (idx == 0) {
      fst(t.mold.nibs).shape;
    } else if (idx == List.length(t.label) - 2) {
      snd(t.mold.nibs).shape;
    } else {
      Nib.Shape.Concave(0);
    }
  | _ => Convex
  };
};

/* Suggest the token at the top of the backpack, if we can put it down */
let suggest = (z: Zipper.t): list(Suggestion.t) => {
  /* Note: Sort check unnecessary here as wouldn't be able to put down */
  switch (z.backpack) {
  | [] => []
  | [{content, _}, ..._] =>
    switch (content) {
    | [Tile({label, shards: [idx], _} as t)] when Zipper.can_put_down(z) => [
        {
          content: List.nth(label, idx),
          strategy: Any(FromBackpack(shard_left_shape(t))),
        },
      ]
    | _ => []
    }
  };
};

/* precondition: backpack contains only 1-shards. */
let rec to_token_list = (backpack: Backpack.t): list(Token.t) => {
  switch (backpack) {
  | [] => []
  | [{content, _}, ...backpack] =>
    switch (content) {
    | [Tile({label, shards: [idx], _})] => [
        List.nth(label, idx),
        ...to_token_list(backpack),
      ]
    | _ => []
    }
  };
};

let is_convex = ({strategy, _} as s: Suggestion.t) =>
  switch (strategy) {
  | Any(FromBackpack(shape)) when shape == Convex => Some(s)
  | _ => None
  };

let is_concave = ({strategy, _} as s: Suggestion.t) =>
  switch (strategy) {
  | Any(FromBackpack(shape)) when shape != Convex => Some(s)
  | _ => None
  };
