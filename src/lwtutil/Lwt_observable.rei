type next('a) = 'a => unit;
type complete = unit => unit;

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
  [subscribe o {next, complete}] subscribes the observable. [unsubscribe] may
  be used to unsubscribe the observer.

  If the observable is already completed, a completion signal is forwarded to
  the observer.
 */
let subscribe: (t('a), next('a), complete) => subscription('a);

/**
  [subscribe' o next] is [subscribe o next (() => ())].
 */
let subscribe': (t('a), next('a)) => subscription('a);

/**
  [wait o] is a promise that resolves when the observable's execution
  completes. There is no guarantee that all observers have received a
  completion signal by then.
 */
let wait: t('a) => Lwt.t(unit);

/**
  [unsubscribe s] removes a subscription.
 */
let unsubscribe: subscription('a) => unit;
