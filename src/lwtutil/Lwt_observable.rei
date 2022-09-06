/**
  Asynchronous reactive streams, in the style of {{: https://rxjs.dev/} RxJS}.

  {{: https://rxjs.dev/guide/overview} Their guide} might be helpful.
  Also see the tests ([Test.re]) for usage examples.
 */
type t('a);

/**
  The type for a subscription.
 */
type subscription('a);

/**
  The type of a subscription callback.
 */
type next('a) = 'a => unit;

/**
  The type of a subscription completion callback.
 */
type complete = unit => unit;

/**
  [of_stream stream] is an observable that uses [stream].
 */
let of_stream: Lwt_stream.t('a) => t('a);

/**
  [create f] is an observable. [f] is a function which takes [next]; [next v]
  pushes [v] to the stream. When the promise [f next] resolves, the stream is
  completed. Note that any failures in the promise are discarded.
 */
let create: unit => (t('a), next('a), complete);

/**
  [subscribe o {next, complete}] subscribes to the observable. [unsubscribe]
  may be used to unsubscribe the observer.

  If the observable is already completed, a completion signal is forwarded to
  the observer.
 */
let subscribe: (t('a), next('a), complete) => subscription('a);

/**
  [subscribe' o next] is [subscribe o next (() => ())].
 */
let subscribe': (t('a), next('a)) => subscription('a);

/**
  [unsubscribe s] removes a subscription.
 */
let unsubscribe: subscription('a) => unit;

/**
  [wait o] is a promise that resolves when the observable's execution
  completes. There is no guarantee that all observers have received a
  completion signal by then.
 */
let wait: t('a) => Lwt.t(unit);

/**
  [pipe f o] is a new observable created by mapping the stream via [f].
 */
let pipe: (Lwt_stream.t('a) => Lwt_stream.t('b), t('a)) => t('b);
