[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = list(RFrame.t);

let empty: t = [];

let push = (~onto: Dir.t, r: Regex.t, ctx: t): t => {
  let rs =
    switch (r) {
    | Seq(rs) => rs
    | _ => [r]
    };
  switch (onto, ctx) {
  | (L, [Seq_(ls, r), ...uz]) => [Seq_(ls @ rs, r), ...uz]
  | (R, [Seq_(ls, r), ...uz]) => [Seq_(ls, rs @ r), ...uz]
  | (L, _) => [Seq_(rs, []), ...ctx]
  | (R, _) => [Seq_([], rs), ...ctx]
  };
};
let push_seq = (~onto: Dir.t, rs: list(Regex.t), ctx: t) => {
  let push = push(~onto);
  switch (onto) {
  | L => List.fold_left(Fun.flip(push), ctx, rs)
  | R => List.fold_right(push, rs, ctx)
  };
};

let nullable = (side: Dir.t) => List.for_all(RFrame.nullable(side));
