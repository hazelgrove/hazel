type next('a) = 'a => unit;
type complete = unit => unit;

/**
  The type for an observer configuration.
 */
type observer('a) = {
  next: next('a),
  complete,
};

/**
  The type for a subscription.
 */
type subscription('a);

/**
  The type for an observable.
 */
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
  [subscribe o {next, complete}] subscribes the observable.
 */
let subscribe: (t('a), observer('a)) => subscription('a);

/**
  [unsubscribe s] removes a subscription.
 */
let unsubscribe: subscription('a) => unit;
