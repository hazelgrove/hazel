open Util;

// Defines nondeterministic search combinators, based upon a monadic tree model
module type Search = {
  type t('a);

  let return: 'a => t('a);

  // Monadic operators
  // Conjunction / bind
  let bind: (t('a), ~f: 'a => t('b)) => t('b);

  // Lazily apply a function, used to avoid OCaml strictness causing infinite overflow
  // apply(f, x) is return(x) >>= x => f(x)
  let apply: ('a => t('b), 'a) => t('b);

  // Mapping
  let map: (t('a), ~f: 'a => 'b) => t('b);
  let join: t(t('a)) => t('a);
  // x >>= f is join(x >>| f)
  // x >>| f is x >>= (a => return(f(a)))
  // join m = (m >>= id)

  // Nondeterminism:
  let fail: t('a);

  // Disjunction
  let choice: (t('a), t('a)) => t('a);

  // Folding disjunction
  let concat: list(t('a)) => t('a);

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
    // Map
    let (>>|): (t('a), 'a => 'b) => t('b);
    // Choice
    let (<|>): (t('a), t('a)) => t('a);

    // Lazy Application
    let (@@): ('a => t('b), 'a) => t('b);
    let (|>-): ('a, 'a => t('b)) => t('b);
  };

  // Let binding syntax
  module Syntax: {
    // Bind/flatmap
    let ( let* ): (t('a), 'a => t('b)) => t('b);

    // Map
    let (let+): (t('a), 'a => 'b) => t('b);
  };
};

module DFS: Search = {
  include Sequence;

  let fail = empty;
  let apply = (f, x) => bind(return(x), ~f);

  let choice = append;
  let wrap = x => x;

  let concat = s => s |> of_list |> concat;

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
    // Map
    let (>>|) = (m, f) => map(m, ~f);
    // Choice
    let (<|>) = choice;

    // Lazy Application
    let (@@) = (f, x) => apply(f, x);
    let (|>-) = (x, f) => apply(f, x);
  };

  module Syntax = {
    // Bind/flatmap
    let ( let* ) = Infix.(>>=);
    let ( and* ) = zip;

    // Map
    let (let+) = Infix.(>>|);
    let (and+) = zip;
  };
};

// Interleaved DFS, technically not an associative monad
module IDFS: Search = {
  include Sequence;

  let fail = empty;
  let choice = (s1, s2) => round_robin([s1, s2]);
  let bind = (s, ~f) => interleave(s >>| f);
  let apply = (f, x) => bind(return(x), ~f);
  let wrap = x => x;

  let concat = round_robin;

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
    // Map
    let (>>|) = (m, f) => map(m, ~f);
    // Choice
    let (<|>) = choice;

    // Lazy Application
    let (@@) = (f, x) => apply(f, x);
    let (|>-) = (x, f) => apply(f, x);
  };

  module Syntax = {
    // Bind/flatmap
    let ( let* ) = Infix.(>>=);
    let ( and* ) = zip;

    // Map
    let (let+) = Infix.(>>|);
    let (and+) = zip;
  };
};

module BFS: Search = {
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

  // Choices a sequence of forests:
  // - Gets the sequence of root levels and concatenates them
  // - Takes the sequence of sequences of remaining levels and defers the choice of these to the next step
  let choice_n: Sequence.t(t('a)) => t('a) =
    ms =>
      unfold(~init=ms, ~f=ms =>
        is_empty(ms)
          ? None
          : ms
            |> filter_map(~f=next)
            |> (ms => Some((ms >>| fst |> concat, ms >>| snd)))
      );

  let concat = l => List.fold_left(choice, fail, l);

  let rec bind = (m: t('a), ~f: 'a => t('b)): t('b) =>
    unfold(~init=(m, fail), ~f=((m, acc)) =>
      switch (Sequence.next(m), Sequence.next(acc)) {
      | (None, None) => None
      | (None, Some((ac, acs))) => Some((ac, (fail, acs)))
      | (Some((b, bs)), None) =>
        let bound = b >>| f |> choice_n;
        let (hd, bound) =
          next(bound) |> Option.value(~default=(empty, fail));
        Some((hd, (bs, bound)));
      | (Some((b, bs)), Some((ac, acs))) =>
        let bound = b >>| f |> choice_n;
        let (hd, bound) =
          next(bound) |> Option.value(~default=(empty, fail));
        Some((append(ac, hd), (bs, choice(acs, bound))));
      }
    );

  let map = (m, ~f) => m >>| Sequence.map(~f);

  let join: t(t('a)) => t('a) = m => bind(m, ~f=x => x);

  let wrap = m => append(singleton(empty), m);
  let apply = (f, x) => bind(return(x), ~f);

  let once = m =>
    m |> find(~f=b => !Sequence.is_empty(b)) |> Option.map(Sequence.hd_exn);
  let ifte = (m, ~thn, ~els) =>
    switch (once(m)) {
    | None => els
    | Some(_) => bind(m, ~f=thn)
    };
  let guard = b => b ? return() : fail;

  let run = Sequence.concat;
  let run_n = (m, ~solutions) =>
    Sequence.take(run(m), solutions) |> Sequence.to_list;

  module Infix = {
    // Conjunction
    let (>>=) = (m, f) => bind(m, ~f);
    // Map
    let (>>|) = (m, f) => map(m, ~f);
    // Choice
    let (<|>) = choice;

    // Lazy Application
    let (@@) = (f, x) => apply(f, x);
    let (|>-) = (x, f) => apply(f, x);
  };

  module Syntax = {
    // Bind/flatmap
    let ( let* ) = Infix.(>>=);
    let ( and* ) = zip;

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
  // Bool logs if the depth bound was ever reached
  type t('a) = bound => (bool, list(('a, bound)));

  let return = (x, bound) => (bound == zero_bound, [(x, bound)]);
  let fail = _bound => (false, []);

  let bind = (m: t('a), ~f: 'a => t('b)): t('b) =>
    bound =>
      m(bound)
      |> (
        ((reached, sols)) =>
          List.fold_left(
            ((acc_reached, acc_sols), (sol, rem_bound)) =>
              f(sol, rem_bound)
              |> (
                ((b_reached, b_sols)) => (
                  acc_reached || b_reached,
                  acc_sols @ b_sols,
                )
              ),
            (reached, []),
            sols,
          )
      );

  let choice = (m1, m2, bound) =>
    (m1(bound), m2(bound))
    |> (
      (((m1_reached, m1_sols), (m2_reached, m2_sols))) => (
        m1_reached || m2_reached,
        m1_sols @ m2_sols,
      )
    );

  let wrap = m =>
    fun
    | {depth: 0} => (false, [])
    | {depth} => m({depth: depth - 1});
  let apply = (f, x) => bind(return(x), ~f);

  let map: type a b. (t(a), ~f: a => b) => t(b) =
    (m, ~f) => bind(m, ~f=x => return(f(x)));
  let join = mm => bind(mm, ~f=x => x);

  let concat = (ms, bound) =>
    ms
    |> List.fold_left(
         ((acc_reached, acc_sols), m) =>
           m(bound)
           |> (
             ((reached, sols)) => (acc_reached || reached, acc_sols @ sols)
           ),
         (false, []),
       );

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
          Some(
            m(curr_bound)
            |> (
              ((reached, sols)) => (
                sols
                |> List.filter(((_, rem_bound)) =>
                     curr_bound - rem_bound > prev_bound
                   )
                |> List.map(fst)
                |> Sequence.of_list,
                (curr_bound, !reached ? None : Config.inc(curr_bound)),
              )
            ),
          ),
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
    // Map
    let (>>|) = (m, f) => map(m, ~f);
    // Choice
    let (<|>) = choice;

    // Lazy Application
    let (@@) = (f, x) => apply(f, x);
    let (|>-) = (x, f) => apply(f, x);
  };

  module Syntax = {
    // Bind/flatmap
    let ( let* ) = Infix.(>>=);

    // Map
    let (let+) = Infix.(>>|);
  };
};
