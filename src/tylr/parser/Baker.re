module Baked = {
  type t = Chain.t(Cell.t, Token.t);
  let cat = Chain.cat((cell, _) => cell);
};

let bake_padding = (~entered=?, side: Dir.t, step: Step.t): Cell.t => {
  let mold = Mold.push(~onto=Dir.toggle(side), step.mtrl, step.mold);
  let sort = Molded.{mtrl: Mtrl.Space, mold};
  let bounds =
    switch (entered) {
    | None => Cell.Bounds.Eq(sort)
    | Some(s) =>
      // note: assuming s is Bound.t(_)
      switch (side) {
      | L => Cell.Bounds.Lt(s, sort)
      | R => Cell.Bounds.Gt(sort, s)
      };
    };
  Cell.empty(bounds);
};

let bake_token = _ => failwith("todo");
let bake_cell = (~entered=?, _) => failwith("todo");

let space_sort = (side: Dir.t, step: Step.t): Molded.Sort.t => {
  let mold = Mold.push(~onto=Dir.toggle(side), step.mtrl, step.mold);
  Molded.{mtrl: Mtrl.Space, mold};
};

let bake_step = (~src: Dir.t, ~entered=?, step, baked: 'baked): 'baked =>
  switch (Molded.Sym.is_sort(step)) {
  | None =>
    let baked =
      switch (entered) {
      | None => baked
      | Some(s) =>
        baked
        |> Chain.put_fst({
          let space = space_sort(src, step);
          let bounds =
            Cell.Bounds.(Dir.choose(src, Lt(s, space), Gt(space, s)));
          Cell.empty(bounds);
        });
      };
    let tok = bake_token(step);
    let cell = Cell.empty(Eq(space_sort(Dir.toggle(src), step)));
    Chain.link(cell, tok, baked);
  | Some(sort) =>
    let bounds =
      switch (entered) {
      | None => Cell.Bounds.Eq(sort)
      | Some(s) =>
        Cell.Bounds.(Dir.choose(src, Lt(s, sort), Gt(sort, s)))
      };
    // note: fill creates necessary grout
    Chain.put_fst(Cell.fill(bounds), baked);
  };

let bake_steps = (~src: Dir.t, steps: list(Walk.Step.t)): ('acc => 'acc) =>
  List.fold_right(
    (step, (entered, baked)) =>
      (None, bake_step(~src, ~entered?, baked)),
    steps,
  );

let bake_walk = (~src: Dir.t, walk: Walk.t, init: Baked.t): Baked.t => {
  Walk.group_enters(walk)
  |> Chain.fold_right(
    (steps, enters, baked) => {
      let sort = Chain.lst(enters);


      let entered =
        switch (entered) {
        | Some(_) => entered
        | None => Some(enter)
        };
      bake_steps(~src, steps, (entered, baked));
    },
    steps => bake_steps(~src, steps, init),
  );
};

let fill = (meld: Meld.t, cell: Cell.t) =>
  switch (cell, Meld.sort(meld)) {
  | (Empty, Space) => Some(Cell.put(meld, cell))
  | (Empty, _) => None
  | (Full({sorts, meld, _}), _) =>
    let (l, r) = sorts;
    Walker.bounds(l, meld, r) |> Option.map(Cell.full(~sorts));
  };


module Step = {
  type t = Sym.t(Token.t, Cell.t);

  let bake = (~container=?, d: Dir.t, step: Walker.Step.t): t =>
    switch (step) {
    | (NT(msrt), mold) => failwith("todo")
    | (T(mlbl), mold) => failwith("todo")
    };
};

module Walk = {
  type t = Chain.t(list(Step.t), unit);

  let mk = Chain.of_loops;

  let bake = (d: Dir.t, w: Walker.Walk.t): t => {
    let (lst, rest) = Chain.split_lst(w);
    let baked_lst = List.map(Step.bake(d), lst);
    let baked_rest =
      List.combine(rest)
      |> List.rev_map(((msort, steps)) =>
           steps
           |> List.mapi((i, step) => {
                let container =
                  i == List.length(steps) - 1 ? Some(msort) : None;
                Step.bake(~container?, d, step);
              })
         );
    mk(baked_rest @ [baked_lst]);
  };
};

module Wald = {
  include Wald;

  let to_chain = (walk: list(Step.t)): Chain.t(Cell.t, Token.t) =>
    List.fold_right(
      (step, chain) =>
        switch (step) {
        | T(tok) => Chain.link(Cell.empty, tok, chain)
        | NT(cell) => Chain.put_fst(cell, chain)
        },
      walk,
      Chain.of_loop(Cell.empty),
    );

  let fill_min =
      (meld: Meld.t, chain: Chain.t(Cell.t, Token.t))
      : option(Chain.t(Cell.t, Token.t)) => {
    Chain.unzip(chain)
    |> Oblig.Delta.minimize(((pre, cell, suf)) =>
         fill(meld, cell)
         |> Option.map(filled => Chain.zip(pre, filled, suf))
       );
  };

  let arrange = (~src, ~dst, steps: Chain.t(Cell.t, Token.t)): Wald.t => {
    let (cell, tl) = Chain.split_fst(steps);
    let tl_dst =
      List.fold_right(
        ((tok, cell), wald) => Wald.link(tok, ~cell, wald),
        tl,
        dst,
      );
    Wald.zip(src, cell, tl_dst);
  };

  let mk =
      (
        ~src: Wald.t,
        ~fill=?,
        ~dst: Wald.t,
        d: Dir.t,
        steps: list(Walker.Step.t),
      )
      : option(Wald.t) =>
    steps
    |> List.map(Step.bake(d))
    |> to_chain
    |> (
      switch (fill) {
      | None => Option.some
      | Some(meld) => fill_min(meld)
      }
    )
    |> Option.map(arrange(~src, ~dst));
};
