/* TODO: Combinators. */
open Lwt.Infix;
open Lwt.Syntax;

module Subscriptions = Map.Make(Int);

type next('a) = 'a => unit;
type complete = unit => unit;

type observer('a) = {
  next: next('a),
  complete,
};

type t('a) = {
  stream: Lwt_stream.t('a),
  push: option('a) => unit,
  observers: ref(Subscriptions.t(observer('a))),
  count: ref(int),
  is_complete: ref(bool),
}

and subscription('a) = {
  id: int,
  observable: ref(t('a)),
};

/** [forward f o] applies [f] to all the observers of [o]. */
let forward = (f, {observers, _}: t('a)) => {
  let rec forward' =
    fun
    | [] => ()
    | [ob, ...observers] => {
        f(ob);
        forward'(observers);
      };

  let observers = observers^ |> Subscriptions.bindings |> List.map(snd);
  forward'(observers);
};

/** [forward v o] calls [next v] for all observers of [o]. */
let forward_next = v => forward((ob: observer('a)) => ob.next(v));
/** [forward_complete () o] calls [complete ()] for all observers of [o]. */
let forward_complete = () => forward((ob: observer('a)) => ob.complete());

/**
  [next o] consumes the next stream item and forwards it to the observers
  of [o].
 */
let next = ({stream, _} as o: t('a)) => {
  let q = Lwt_stream.get(stream);

  Lwt.on_any(
    q,
    fun
    | Some(v) => o |> forward_next(v)
    | None => o |> forward_complete(),
    /* Failures from [q] should not be possible (according to Lwt_stream docs). */
    ignore,
  );

  Lwt.try_bind(
    () => q,
    v => v |> Option.is_some |> Lwt.return,
    _exn => Lwt.return_false,
  );
};

/**
  [loop o] calls [next o] until the stream is complete.
 */
let loop = (o: t('a)) => {
  let rec loop' = () =>
    next(o)
    >>= (
      continue =>
        if (continue) {
          loop'();
        } else {
          Lwt.return_unit;
        }
    );

  let* () = loop'();
  o.is_complete := true;

  Lwt.return_unit;
};

let create = () => {
  let (stream, push) = Lwt_stream.create();
  let o = {
    stream,
    push,
    observers: ref(Subscriptions.empty),
    count: ref(0),
    is_complete: ref(false),
  };

  let _ = loop(o);
  o;
};

let next = ({push, _}: t('a), v: 'a) => push(Some(v));
let complete = ({push, _}: t('a)) => push(None);

let subscribe =
    (
      {observers, count, is_complete, _} as o: t('a),
      next: next('a),
      complete: complete,
    ) => {
  let ob = {next, complete};
  let s = {id: count^, observable: ref(o)};

  if (is_complete^) {
    ob.complete();
  } else {
    observers := Subscriptions.add(s.id, ob, observers^);
    count := count^ + 1;
  };
  s;
};

let subscribe' = (o: t('a), next: next('a)) => subscribe(o, next, () => ());

let wait = (o: t('a)) => {
  let (q, r) = Lwt.wait();
  let _ = subscribe(o, ignore, () => Lwt.wakeup_later(r, ()));
  q;
};

let unsubscribe = ({id, observable}: subscription('a)) => {
  observable^.observers := Subscriptions.remove(id, observable^.observers^);
};
