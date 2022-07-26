type next('a) = 'a => unit;
type complete = unit => unit;

type observer('a) = {
  next: next('a),
  complete,
};

type t('a);

/**
  [create ()] is an observable.
 */
let create: unit => t('a);

/**
  [next o v] pushes [v] to the observable.
 */
let next: (t('a), 'a) => unit;

/**
  [complete o] pushes the completion signal to the observable. The observable's
  execution then ends.
 */
let complete: t('a) => unit;

/**
  [subscribe o {next, complete}] attaches an observer to the observable.
 */
let subscribe: (t('a), observer('a)) => unit;
