open Util;

module Walk = Walker.Walk;

let fill_eq = (fill: list(Meld.t), sort: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (l, r) =
    switch (fill) {
    | [] => Molded.Label.(space, space)
    | [_, ..._] =>
      Meld.(face(L, List.hd(fill)), face(R, ListUtil.last(fill)))
    };
  let+ w_l = ListUtil.hd_opt(Walker.walk_into(~from=L, sort, Node(l)))
  and+ w_r = ListUtil.hd_opt(Walker.walk_into(~from=R, sort, Node(r)));
  let (h_l, h_r) = (Walk.height(w_l) > 2, Walk.height(w_r) > 2);
  let (i_l, i_r) = Walk.(intermediates(w_l) > 0, intermediates(w_r) > 0);
  Cell.fill(~req=i_l || i_r, ~l=h_l, ~r=h_r, fill, sort);
};

let fill_lt = (m: Meld.t, bound: Bound.t(Molded.Sort.t), sort: Molded.Sort.t) => {
  open OptUtil.Syntax;
  let (l, r) = Meld.faces(m);
  let+ _w_l = ListUtil.hd_opt(Walker.walk_into(~from=L, bound, l))
  and+ w_r = ListUtil.hd_opt(Walker.walk_into(~from=R, Node(sort), r));
  Meld.grout(~fill, ~r=Walk.height(w_r) > 2, sort);
};

let fill_gt = (m: Meld.t, sort: Molded.Sort.t, bound: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (l, r) = Meld.faces(m);
  let+ w_l = ListUtil.hd_opt(Walker.walk_into(~from=L, Node(sort), l))
  and+ _w_r = ListUtil.hd_opt(Walker.walk_into(~from=R, bound, r));
  Meld.grout(~l=Walk.height(w_l) > 2, m, sort);
};

let fill = (m: Meld.t, bounds) =>
  switch (bounds) {
  | Eq(sort) =>
    let l =
      lt_prime(sort, m)
        ? Some(false)
        : tile_sort(sort)
          |> Option.map(s => lt_prime(Node(Molded.Sort.grout(s)), m));
    let r =
      gt_prime(m, sort)
        ? Some(false)
        : tile_sort(sort)
          |> Option.map(s => gt_prime(m, Node(Molded.Sort.grout(s))));
    let meld = Meld.wrap(~sort);
    ();
  };

let fill = (m: Meld.t, bounds) => {
  let (l, r) = Meld.ends(l, r);
  switch (bounds) {
  | Eq(sort) =>
    let ent_l = Walker.enter(~from=L, sort, l);
    let ent_r = Walker.enter(~from=R, sort, r);
    switch (ent_l, ent_r) {
    | ([], _)
    | (_, []) =>
      open OptUtil.Syntax;
      let* s =
        switch (sort) {
        | Root => Some(Sort.root)
        | Node({mtrl: Tile(s), _}) => Some(s)
        | Node({mtrl: Grout(_) | Space, _}) => None
        };
      let+ l =
        switch (ent_l, Walker.enter_grout(~from=L, s, l)) {
        | ([_, ..._], _) => Some(false)
        | ([], [_, ..._]) => Some(true)
        | ([], []) => None
        }
      and+ r =
        switch (ent_r, Walker.enter_grout(~from=R, s, r)) {
        | ([_, ..._], _) => Some(false)
        | ([], [_, ..._]) => Some(true)
        | ([], []) => None
        };
      let meld = Meld.wrap_grout(~sort=s, ~l, m, ~r);
      Cell.mk(~meld, sort);
    | ([_, ..._], [_, ..._]) => failwith("todo")
    };
  };
};

module Eq = {
  let walk = (~from, src, dst) => {
    let (src, dst) = (Wald.hd(src), Wald.hd(dst));
    Walker.walk_eq(~from, Node(src), Node(dst));
  };

  let bake =
      (~src: Wald.t, ~fill=[], ~dst: Wald.t, w: Walk.t): option(Wald.t) => {};

  let eq = (~from: Dir.t, l: Wald.t, ~fill=[], r: Wald.t): option(Wald.t) => {
    let (src, dst) = Dir.choose(from, l, r);
    let rec go = (~init=false, src, fill) => {
      open OptUtil.Syntax;
      let/ () =
        // try removing ghost
        switch (Wald.unlink(src)) {
        | Ok((hd, cell, tl)) when Token.is_ghost(hd) =>
          let fill = Option.to_list(cell.content) @ fill;
          go(tl, fill) |> Effects.perform_if(Remove(hd));
        | _ => None
        };
      walk(~from, src, dst) |> Oblig.Delta.minimize(bake(~src, ~fill, ~dst));
    };
    go(~init=true, src, Dir.choose(from, fill, List.rev(fill)));
  };
};

module Wald = {
  let hd = Bound.map(Wald.hd);

  let meld =
      (~from: Dir.t, l: Wald.Bound.t, ~fill=[], r: Wald.Bound.t)
      : option(Chain.t(Chain.t(Cell.t, Token.t), unit)) => {
    let (src, dst) = Dir.choose(from, l, r);
    let fill = Dir.choose(from, fill, List.rev(fill));
    let rec go = (~init=false, src, fill) => {
      open OptUtil.Syntax;
      let/ () =
        // try removing ghost
        switch (Wald.unlink(src)) {
        | Ok((hd, cell, tl)) when Token.is_ghost(hd) =>
          let fill = Option.to_list(cell.content) @ fill;
          go(tl, fill) |> Effects.perform_if(Remove(hd));
        | _ => None
        };
      walk(~from, src, dst)
      // require eq if ghost has been removed
      |> (init ? Fun.id : List.filter(Walk.is_eq))
      |> Oblig.Delta.minimize(bake(~fill));
    };
    go(~init=true, src, fill);
  };
};

module Slope = {
  module Dn = {
    let rec push =
            (~top=Bound.Root, dn: Slope.Dn.t, ~fill=?, w: Wald.t)
            : Result.t(Slope.Dn.t, option(Meld.t)) =>
      switch (dn) {
      | [] => Wald.walk_eq(~from, top, Node(w))
      | [hd, ...tl] =>
        switch (
          Wald.meld(
            ~from=L,
            Node(hd.wald),
            ~fill=Option.to_list(fill),
            Node(w),
          )
        ) {
        | Some(lvls) => failwith("todo")
        }
      };
  };
};

module Slope = {
  module Dn = {
    let rec push_wald =
            (~top=Bound.Root, dn: Slope.Dn.t, ~cell=Cell.empty, w: Wald.t)
            : Result.t(Slope.Dn.t, Cell.t) =>
      switch (dn) {
      | [] => Wald.lt(top, ~cell, w) |> Result.of_option(~error=cell)
      | [hd, ...tl] =>
        switch (
          Wald.eq(hd.wald, ~cell, w),
          Wald.lt(Node(hd.wald), ~cell, w),
          Wald.gt(hd.wald, ~cell, Node(w)),
        ) {
        | (Some(eq), _, _) => Ok([{...hd, wald: eq}, ...tl])
        | (_, Some(lt), _) => Ok(Slope.Dn.cat(dn, lt))
        | (_, _, Some(gt)) =>
          let cell = Slope.Up.roll(~cell=hd.cell, gt);
          push_wald(~top, tl, ~cell, w);
        | (None, None, None) =>
          open Result.Syntax;
          let g = Token.mk(Grout((Concave, Concave)));
          let* dn = push_wald(~top, dn, ~cell, Wald.unit(g));
          push_wald(~top, dn, w);
        }
      };

    let push = (~top=Bound.Root, dn, ~cell=Cell.empty, t) =>
      push_wald(~top, dn, ~cell, Wald.unit(t));
  };
};

module Ctx = {
  open OptUtil.Syntax;

  let push =
      (~onto: Dir.t, w: Wald.t, ~cell=Cell.empty, ctx: Ctx.t): option(Ctx.t) =>
    switch (onto, Ctx.unlink(ctx)) {
    | (L, Error((dn, up))) =>
      let+ dn = Slope.Dn.push(dn, ~slot, w);
      Ctx.unit((dn, up));
    | (L, Ok(((dn, up), (l, r), ctx))) =>
      switch (Slope.Dn.push(~top=Node(l.wald), dn, ~cell, w)) {
      | Some(dn) => Some(Ctx.unit((dn, up)))
      | None =>
        let+ w = Wald.eq(l.wald, ~cell=Slope.Dn.roll(dn), w);
        let (dn, up) = ([{...l, wald: w}], [r, ...up]);
        Ctx.map_fst(Frame.Open.cat((dn, up)), ctx);
      }
    | (R, _) => failwith("todo: symmetric to L")
    };

  let bridge = (~sel=Ziggurat.empty, well: Stepwell.t): Stepwell.t => {
    print_endline("Stepwell.assemble");
    let (pre, suf) = get_slopes(well);
    // separate siblings that belong to the selection
    let (pre_lt_sel, pre_geq_sel) = Ziggurat.split_lt(pre, sel);
    let (sel_leq_suf, sel_gt_suf) = Ziggurat.split_gt(sel, suf);
    well
    |> Stepwell.put_slopes(Slopes.empty)
    |> bridge_slopes((pre_lt_sel, sel_gt_suf))
    |> push_slopes((pre_geq_sel, sel_leq_suf));
  };

  let pull_lexeme = (~char=false, ~from: Dir.t, well: Stepwell.t) =>
    switch (Slopes.pull_lexeme(~char, ~from, Stepwell.get_slopes(well))) {
    | Some((a, sib)) => Some((a, Stepwell.put_slopes(sib, well)))
    | None =>
      open OptUtil.Syntax;
      let+ (slopes, bridge, well) = Chain.unlink(well);
      let (lx, slopes') = Bridge.pull_lexeme(~char, ~from, bridge);
      let well =
        well |> push_slopes(Slopes.cat(slopes, slopes')) |> assemble;
      (lx, well);
    };
};
