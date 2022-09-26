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
  observers: ref(Subscriptions.t(observer('a))),
  count: ref(int),
  is_complete: ref(bool),
}

and subscription('a) = {
  id: int,
  observable: ref(t('a)),
};

/** [forward f o] applies [f] to all the observers of [o]. */
let forward = (f, {observers, _}: t('a)) =>
  observers^
  |> Subscriptions.bindings
  |> List.map(snd)
  |> Lwt_list.map_s(ob => Lwt.wrap(() => f(ob)));

/** [forward v o] calls [next v] for all observers of [o]. */
let forward_next = v => forward((ob: observer('a)) => ob.next(v));
/** [forward_complete () o] calls [complete ()] for all observers of [o]. */
let forward_complete = () => forward((ob: observer('a)) => ob.complete());

/**
  [next_and_forward o] consumes the next stream item and forwards it to the
  observers of [o].
 */
let next_and_forward = ({stream, _} as o: t('a)) => {
  let q = Lwt_stream.get(stream);

  Lwt.on_any(
    q,
    fun
    | Some(v) => o |> forward_next(v) |> ignore
    | None => o |> forward_complete() |> ignore,
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
    next_and_forward(o)
    >>= (
      not_complete =>
        if (not_complete) {
          loop'();
        } else {
          Lwt.return_unit;
        }
    );

  let* () = loop'();
  o.is_complete := true;

  Lwt.return_unit;
};

let of_stream = (stream: Lwt_stream.t('a)) => {
  let o = {
    stream,
    observers: ref(Subscriptions.empty),
    count: ref(0),
    is_complete: ref(false),
  };

  /* Initialize listener. */
  let _ = loop(o);

  o;
};

let create = () => {
  let (stream, push) = Lwt_stream.create();
  let o = of_stream(stream);

  let next = v => push(Some(v));
  let complete = () => push(None);
  (o, next, complete);
};

let subscribe =
    (
      {observers, count, is_complete, _} as o: t('a),
      next: next('a),
      complete: complete,
    ) => {
  let ob = {next, complete};
  let s = {id: count^, observable: ref(o)};

  if (is_complete^) {
    /* If execution is already complete, send signal to new observer without
     * adding a new subscription. */
    ob.complete();
  } else {
    /* Otherwise, add the new subscription. */
    observers := Subscriptions.add(s.id, ob, observers^);
    count := count^ + 1;
  };
  s;
};

let subscribe' = (o: t('a), next: next('a)) => subscribe(o, next, () => ());

let wait = ({stream, _}: t('a)) => Lwt_stream.closed(stream);

let unsubscribe = ({id, observable}: subscription('a)) => {
  observable^.observers := Subscriptions.remove(id, observable^.observers^);
};

let pipe =
    (
      f: Lwt_stream.t('a) => Lwt_stream.t('b),
      {stream, observers: _, count: _, is_complete: _}: t('a),
    ) =>
  stream |> f |> of_stream;
