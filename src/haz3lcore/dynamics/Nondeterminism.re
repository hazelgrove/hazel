open Util;

// Defines search combinators, based upon the Sequence.t monad.
// https://www.cambridge.org/core/services/aop-cambridge-core/content/view/AB57FF99CEA76C1C31A336B560D6FD3C/S0956796809007321a.pdf/algebras-for-combinatorial-search.pdf
// https://okmij.org/ftp/Computation/LogicT.pdf
// http://www.cs.ox.ac.uk/files/6043/Seres99%20-%20Algebra.pdf
module type Search = {
  type t('a);

  let return: 'a => t('a);

  // Monadic operators
  // Conjunction / bind
  let bind: (t('a), ~f: 'a => t('b)) => t('b);

  // Fair conjunction
  let fbind: (t('a), ~f: 'a => t('b)) => t('b);

  // Mapping
  let map: (t('a), ~f: 'a => 'b) => t('b);
  let join: t(t('a)) => t('a);
  // x >>= f is join(x >>| f)
  // x >>| f is x >>= (a => return(f(a)))
  // join m = (m >>= id)

  let fjoin: t(t('a)) => t('a);

  // Nondeterminism:
  let fail: t('a);

  // Disjunction
  let choice: (t('a), t('a)) => t('a);

  // Infinite disjunction
  let concat: Sequence.t(t('a)) => t('a);

  // Fair disjunction
  let fchoice: (t('a), t('a)) => t('a);

  // Infinite fair disjuction / interleaving
  let interleave: Sequence.t(t('a)) => t('a);
  // interleave(s) is fold(fchoice, fail, s) but fully fair

  let wrap: t('a) => t('a);

  // Pruning
  let once: t('a) => option('a);
  // Mercury if then else construct: produces the else branch upon failure
  let ifte: (t('a), ~thn: 'a => t('a), ~els: t('a)) => t('a);
  // guard(b) returns fail if b is false, otherwise return ().
  let guard: bool => t(unit);

  // Retrieving answers
  let run: t('a) => Sequence.t('a);
  let run_n: (~solutions: int, t('a)) => list('a);

  module Infix: {
    // Conjunction
    let (>>=): (t('a), 'a => t('b)) => t('b);
    // Fair conjunction
    let (>>-): (t('a), 'a => t('b)) => t('b);
    // Map
    let (>>|): (t('a), 'a => 'b) => t('b);
    // Choice
    let (<||>): (t('a), t('a)) => t('a);
    // Fair Choice
    let (<|>): (t('a), t('a)) => t('a);
  };

  // Let binding syntax
  module Syntax: {
    // Bind/flatmap
    let ( let* ): (t('a), 'a => t('b)) => t('b);
    // Fair Bind
    let (let.): (t('a), 'a => t('b)) => t('b);

    // Map
    let (let+): (t('a), 'a => 'b) => t('b);
  };
};

module DFS: Search = {
  include Sequence;

  let fail = empty;

  let choice = append;
  let fchoice = (s1, s2) => [s1, s2] |> of_list |> interleave;
  let fbind = (s, ~f) => interleave(s >>| f);
  let wrap = x => x;

  let fjoin = interleave;

  let once = hd;
  let ifte = (s, ~thn, ~els) =>
    switch (next(s)) {
    | None => els
    | Some((x, xs)) => choice(thn(x), xs >>= thn)
    };
  let guard = b => b ? return() : fail;

  let run = x => x;
  let run_n = (~solutions, s) => take(s, solutions) |> to_list;

  module Infix = {
    // Conjunction
    let (>>=) = (m, f) => bind(m, ~f);
    // Fair conjunction
    let (>>-) = (m, f) => fbind(m, ~f);
    // Map
    let (>>|) = (m, f) => map(m, ~f);
    // Choice
    let (<||>) = choice;
    // Fair Choice
    let (<|>) = fchoice;
  };

  module Syntax = {
    // Bind/flatmap
    let ( let* ) = Infix.(>>=);
    let ( and* ) = zip;

    // Fair bind
    let (let.) = Infix.(>>-);
    let (and.) = zip;

    // Map
    let (let+) = Infix.(>>|);
    let (and+) = zip;
  };
};

module BFS = {
  open Sequence;
  // These inner sequences are _bags_.
  // Their order of elements doe not matter.
  type t('a) = Sequence.t(Sequence.t('a));
  let fair_fold = fold;

  let return = x => return(return(x));
  let fail = empty;

  let choice = (s1, s2) =>
    zip_full(s1, s2)
    >>| (
      fun
      | `Both(b1, b2) => append(b1, b2)
      | `Left(b)
      | `Right(b) => b
    );
  let fchoice = (s1, s2) =>
    zip_full(s1, s2)
    >>| (
      fun
      | `Both(b1, b2) => [b1, b2] |> of_list |> interleave
      | `Left(b)
      | `Right(b) => b
    );

  let concat = fold(~init=fail, ~f=choice);

  let interleave = fold(~init=fail, ~f=fchoice); // TEST

  let rec bind_gen = (m, f) =>
    switch (next(m)) {
    | None => Generator.return()
    | Some((d, ds)) =>
      switch (next(concat(d >>| f))) {
      | None =>
        Generator.(bind)(Generator.yield(empty), ~f=() => bind_gen(ds, f))
      | Some((d', ds')) =>
        Generator.(bind)(Generator.yield(d'), ~f=() =>
          Generator.of_sequence(choice(ds', bind(ds, ~f)))
        )
      }
    }
  and bind = (m, ~f) => Generator.run(bind_gen(m, f));

  let rec fbind_gen = (m, f) =>
    switch (next(m)) {
    | None => Generator.return()
    | Some((d, ds)) =>
      switch (next(interleave(d >>| f))) {
      | None =>
        Generator.(bind)(Generator.yield(empty), ~f=() => fbind_gen(ds, f))
      | Some((d', ds')) =>
        Generator.(bind)(Generator.yield(d'), ~f=() =>
          Generator.of_sequence(fchoice(ds', fbind(ds, ~f)))
        )
      }
    }
  and fbind = (m, ~f) => Generator.run(fbind_gen(m, f));

  let map = (m, ~f) => m >>| (m' => m' >>| f);

  let join: t(t('a)) => t('a) = m => bind(m, ~f=x => x);
  let fjoin = m => fbind(m, ~f=x => x);

  let wrap = m => append(empty, m);

  let once = m =>
    m |> find(~f=b => !is_empty(b)) |> Option.map(hd) |> Option.join;
  let ifte = (m, ~thn, ~els) =>
    switch (once(m)) {
    | None => els
    | Some(_) => bind(m, ~f=thn)
    };
  let guard = b => b ? return() : fail;

  let run = Sequence.interleave;
  let run_n = (~solutions, m) => take(Sequence.interleave(m), solutions);

  module Infix = {
    // Conjunction
    let (>>=) = (m, f) => bind(m, ~f);
    // Fair conjunction
    let (>>-) = (m, f) => fbind(m, ~f);
    // Map
    let (>>|) = (m, f) => map(m, ~f);
    // Choice
    let (<||>) = choice;
    // Fair Choice
    let (<|>) = fchoice;
  };

  module Syntax = {
    // Bind/flatmap
    let ( let* ) = Infix.(>>=);
    let ( and* ) = zip;

    // Fair bind
    let (let.) = Infix.(>>-);
    let (and.) = zip;

    // Map
    let (let+) = Infix.(>>|);
    let (and+) = zip;
  };
};

// Optional bound limits
type bounds = {
  width: int,
  depth: int,
};
let zero_bound = {width: 0, depth: 0};

module type BoundsConfig = {
  let init: bounds; // Initial bounds
  // Returns none if iteration should stop after some bound
  let inc: bounds => option(bounds);
};

module ConstIncrConfig =
       (Incr: {
          let incr_const: int;
          let init: int;
        })
       : BoundsConfig => {
  let init = {width: Incr.init, depth: Incr.init};
  let inc = ({width, depth}) =>
    Some({width: width + Incr.incr_const, depth: depth + Incr.incr_const});
};

let const_incr_config = (~init, ~inc) => {
  module Incr = {
    let incr_const = inc;
    let init = init;
  };
  ((module ConstIncrConfig(Incr)): (module BoundsConfig));
};

// Search bounded in width and depth
module Bounded = (Config: BoundsConfig) : Search => {
  type t('a) = bounds => list(('a, bounds));

  let return = (x, bound) => [(x, bound)];
  let fail = _bound => [];

  let bind = (m, ~f, bound) =>
    m(bound)
    |> List.fold_left((acc, (x, rem_bound)) => acc @ f(x, rem_bound), []);
  let fbind = bind;

  let choice = (m1, m2, bound) => m1(bound) @ m2(bound);
  let fchoice = choice;

  let wrap = m =>
    fun
    | {width: 0, depth: 0} => []
    | {width: 0, depth} => m({width: 0, depth: depth - 1})
    | {width, depth: 0} => m({width: width - 1, depth: 0})
    // Very inefficient, but space complexity still better than BFS
    // Could extend to model bound minimums to improve this
    | {width, depth} =>
      m({width: width - 1, depth}) @ m({width, depth: depth - 1});

  let map: type a b. (t(a), ~f: a => b) => t(b) =
    (m, ~f) => bind(m, ~f=x => return(f(x)));
  let join = mm => bind(mm, ~f=x => x);
  let fjoin = join;

  let concat = (ms, bound) =>
    ms |> Sequence.to_list |> List.fold_left((acc, m) => acc @ m(bound), []);
  let interleave = concat;

  let (-) = ({width: w, depth: d}, {width: w', depth: d'}) => {
    width: w - w',
    depth: d - d',
  };
  let (>) = ({width: w, depth: d}, {width: w', depth: d'}) =>
    w > w' || d > d';
  let max = ({width: w, depth: d}, {width: w', depth: d'}) => {
    width: max(w, w'),
    depth: max(d, d'),
  };
  let run = m =>
    Sequence.unfold(
      ~init=(zero_bound, Some(Config.init)),
      ~f=
        fun
        | (_, None) => None
        | (prev_bound, Some(curr_bound)) =>
          Some((
            m(curr_bound)
            |> List.filter(((_, rem_bound)) =>
                 curr_bound - rem_bound > prev_bound
               )
            |> List.map(fst)
            |> Sequence.of_list,
            (curr_bound, Config.inc(curr_bound)),
          )),
    )
    |> Sequence.concat;

  let run_n = (~solutions, m) =>
    Sequence.(take(run(m), solutions) |> to_list);

  let once = m => m |> run_n(~solutions=1) |> Util.ListUtil.hd_opt;
  let ifte = (m, ~thn, ~els) =>
    switch (once(m)) {
    | None => els
    | Some(_) => bind(m, ~f=thn)
    };
  let guard = b => b ? return() : fail;

  module Infix = {
    // Conjunction
    let (>>=) = (m, f) => bind(m, ~f);
    // Fair conjunction
    let (>>-) = (m, f) => fbind(m, ~f);
    // Map
    let (>>|) = (m, f) => map(m, ~f);
    // Choice
    let (<||>) = choice;
    // Fair Choice
    let (<|>) = fchoice;
  };

  module Syntax = {
    // Bind/flatmap
    let ( let* ) = Infix.(>>=);

    // Fair bind
    let (let.) = Infix.(>>-);

    // Map
    let (let+) = Infix.(>>|);
  };
};
