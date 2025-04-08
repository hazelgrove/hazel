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

  // Lazily apply a function, used to avoid OCaml strictness causing infinite overflow
  // apply(f, x) is return(x) >>= x => f(x)
  let apply: ('a => t('b), 'a) => t('b);

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

  // Folding disjunction
  let concat: list(t('a)) => t('a);

  // Fair disjunction
  let fchoice: (t('a), t('a)) => t('a);

  // Folding fair disjuction / interleaving
  let interleave: list(t('a)) => t('a);
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
  let run_n: (t('a), ~solutions: int) => list('a);

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
  let apply = (f, x) => bind(return(x), ~f);

  let fjoin = interleave;
  let concat = s => s |> of_list |> concat;
  let interleave = s => s |> of_list |> interleave;

  let once = hd;
  let ifte = (s, ~thn, ~els) =>
    switch (next(s)) {
    | None => els
    | Some((x, xs)) => choice(thn(x), xs >>= thn)
    };
  let guard = b => b ? return() : fail;

  let run = x => x;
  let run_n = (s, ~solutions) => take(s, solutions) |> to_list;

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

module BFS: Search = {
  open Sequence;
  // These inner sequences are _bags_.
  // Their order of elements doe not matter.
  type t('a) = Sequence.t(list('a));
  let fair_fold = fold;

  let return = x => return([x]);
  let fail = empty;

  let choice = (s1, s2) =>
    zip_full(s1, s2)
    >>| (
      fun
      | `Both(b1, b2) => b1 @ b2
      | `Left(b)
      | `Right(b) => b
    );
  let fchoice = choice;

  let concat = l => List.fold_right(choice, l, fail);

  let interleave = l => List.fold_right(fchoice, l, fail);

  let rec bind_gen =
          (m: t('a), f: 'a => t('b)): Generator.t(unit, list('b)) =>
    switch (next(m)) {
    | None => Generator.return()
    | Some((d, ds)) =>
      switch (next(List.fold_right(choice, d |> List.map(f), fail))) {
      | None =>
        Generator.(bind)(Generator.yield([]), ~f=() => bind_gen(ds, f))
      | Some((d', ds')) =>
        Generator.(bind)(Generator.yield(d'), ~f=() =>
          Generator.of_sequence(choice(ds', bind(ds, ~f)))
        )
      }
    }
  and bind = (m: t('a), ~f: 'a => t('b)): t('b) =>
    Generator.run(bind_gen(m, f));

  let fbind = bind;

  let map = (m, ~f) => m >>| List.map(f);

  let join: t(t('a)) => t('a) = m => bind(m, ~f=x => x);
  let fjoin = m => fbind(m, ~f=x => x);

  let wrap = m => append(empty, m);
  let apply = (f, x) => bind(return(x), ~f);

  let once = m =>
    m |> find(~f=b => !List.is_empty(b)) |> Option.map(List.hd);
  let ifte = (m, ~thn, ~els) =>
    switch (once(m)) {
    | None => els
    | Some(_) => bind(m, ~f=thn)
    };
  let guard = b => b ? return() : fail;

  let run = m => m |> Sequence.map(~f=Sequence.of_list) |> Sequence.concat;
  let run_n = (m, ~solutions) =>
    Sequence.take(run(m), solutions) |> Sequence.to_list;

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
type bound = {depth: int};
let zero_bound = {depth: 0};

module type BoundsConfig = {
  let init: bound; // Initial bounds
  // Returns none if iteration should stop after some bound
  let inc: bound => option(bound);
};

module ConstIncrConfig =
       (Incr: {
          let incr_const: int;
          let init: int;
        })
       : BoundsConfig => {
  let init = {depth: Incr.init};
  let inc = ({depth}) => Some({depth: depth + Incr.incr_const});
};

let const_incr_config = (~init, ~inc) => {
  module Incr = {
    let incr_const = inc;
    let init = init;
  };
  ((module ConstIncrConfig(Incr)): (module BoundsConfig));
};

// DFS Search bounded in depth
module Bounded = (Config: BoundsConfig) : Search => {
  type t('a) = bound => list(('a, bound));

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
    | {depth: 0} => []
    | {depth} => m({depth: depth - 1});
  let apply = (f, x) => bind(return(x), ~f);

  let map: type a b. (t(a), ~f: a => b) => t(b) =
    (m, ~f) => bind(m, ~f=x => return(f(x)));
  let join = mm => bind(mm, ~f=x => x);
  let fjoin = join;

  let concat = (ms, bound) =>
    ms |> List.fold_left((acc, m) => acc @ m(bound), []);
  let interleave = concat;

  let (-) = ({depth: d}, {depth: d'}) => {depth: d - d'};
  let (>) = ({depth: d}, {depth: d'}) => d > d';
  let max = ({depth: d}, {depth: d'}) => {depth: max(d, d')};
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

  let run_n = (m, ~solutions) =>
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
