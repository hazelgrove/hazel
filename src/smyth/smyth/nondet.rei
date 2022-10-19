/** Nondeterministic computations via collections semantics. */;

/** The type of a nondeterministic computation; conceptually, a list. */

type t('a);

/** {1:construction Construction} */;

/** The empty nondeterministic computation (i.e., one with no possible
    outcomes); useful for cutting off a nondeterministic computation. */

let none: t('a);

/** Treats each entry in the list as a possible nondeterministic outcome. */

let from_list: list('a) => t('a);

/** {1:collection Collection} */;

/** Collects all possible nondeterministic outputs. */

let to_list: t('a) => list('a);

/** {1:core Core functions} */;

let map: ('a => 'b, t('a)) => t('b);
let pure: 'a => t('a);
let join: t(t('a)) => t('a);

/** {1:generic Generic library functions} */;

let pure_bind: (t('a), 'a => 'b) => t('b);
let bind: (t('a), 'a => t('b)) => t('b);
let and_then: ('a => t('b), t('a)) => t('b);
let guard: bool => t(unit);

/** {1:specific Specific library functions} */;

/** Takes a list of nondeterministic computations and (conceptually)
    nondeterministically chooses {i just one} of the computations to perform;
    the "sum" operation. */

let union: list(t('a)) => t('a);

/** Takes a list of nondeterministic computations and (conceptually) chooses
    one computation {i from each} to perform; the "product" operation. */

let one_of_each: list(t('a)) => t(list('a));

/** Checks if a nondeterministic computation is empty (no possible outcomes). */

let is_empty: t('a) => bool;

/** Filters all possible nondeterministic outcomes with a predicate. */

let filter: ('a => bool, t('a)) => t('a);

/** Deduplicates nondeterministic outcomes. */

let dedup: t('a) => t('a);

/** Filters all possible nondeterministic outcomes to include only those that
    are [Some]thing. */

let collapse_option: t(option('a)) => t('a);

/** [take n nd] limits the nondeterministic computation [nd] to at most [n]
    possible outcomes. */

let take: (int, t('a)) => t('a);

/** [curb_overflow n nd] entirely eliminates {i all} possible outcomes of the
    nondeterministic computation [nd] if it has more than [n] possible outcomes;
    useful for eliminating pathological branches of computation. */

let curb_overflow: (int, t('a)) => t('a);

/** {1:lifting Lifting} */;

let lift_option: option('a) => t('a);
let lift_result: result('a, 'e) => t('a);

/** {1:syntax Syntax} */;

module Syntax: {
  let (let+): (t('a), 'a => 'b) => t('b);
  let ( let* ): (t('a), 'a => t('b)) => t('b);
};
