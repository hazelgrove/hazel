include Chain;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Frame.Open.t, Frame.Closed.t);

let empty = Chain.unit(Frame.Open.empty);

let link = (~slopes=Frame.Open.empty) => link(slopes);

let fold = Chain.fold_left;

let face = (~side: Dir.t, ctx: t) => {
  open Util.OptUtil.Syntax;
  let/ () = Frame.Open.face(~side, fst(ctx));
  let+ (_, (l, r), _) = Result.to_option(unlink(ctx));
  Terr.face(Dir.pick(side, (l, r)));
};

let extend = (~side as d: Dir.t, tl, ctx) => {
  switch (Frame.Open.extend(~side=d, tl, fst(ctx))) {
  | Some(hd) => Some(put_fst(hd, ctx))
  | None =>
    open Util.OptUtil.Syntax;
    let+ (slopes, terrs, ctx) = Result.to_option(unlink(ctx));
    let (t_d, t_b) = Dir.order(d, terrs);
    let terrs = Dir.order(d, (Terr.extend(tl, t_d), t_b));
    link(~slopes, terrs, ctx);
  };
};
