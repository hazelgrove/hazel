open Lwt.Infix;
open Lwt.Syntax;

/** Create a fresh pool member. */
type create('a) = unit => Lwt.t('a);
/** Dispose of a pool member. */
type dispose('a) = 'a => Lwt.t(unit);
/** Check the validity of a member when [use] resulted in a failed computation. */
type check('a) = 'a => Lwt.t(bool);
/** Validate an existing available member before use. */
type validate('a) = 'a => Lwt.t(bool);

type t('a) = {
  /** Maximum size of the pool. */
  max: int,
  /** Size of the pool. */
  mutable count: int,
  /** Queue of available members. */
  queue: Queue.t('a),
  /** Resolvers waiting for an available member. */
  waiters: Lwt_dllist.t(Lwt.u('a)),
  create: create('a),
  dispose: dispose('a),
  check: check('a),
  validate: validate('a),
};

let init = (~max, ~create, ~dispose, ~check, ~validate) => {
  let count = 0;
  let queue = Queue.create();
  let waiters = Lwt_dllist.create();
  {max, count, queue, waiters, create, dispose, check, validate};
};

/**
  Create a fresh pool member.
 */
let create = pool =>
  Lwt.catch(
    () => {
      pool.count = pool.count + 1;
      pool.create();
    },
    exn => {
      pool.count = pool.count - 1;
      Lwt.fail(exn);
    },
  );

/**
  Dispose of a pool member.
 */
let dispose = (pool, c) => {
  let* () = pool.dispose(c);
  pool.count = pool.count - 1;
  Lwt.return_unit;
};

/**
  Release a pool member.
 */
let release = (pool, c) => {
  /* If there are waiters, fulfill the oldest; otherwise, restore back to
   * queue. */
  switch (Lwt_dllist.take_opt_l(pool.waiters)) {
  | Some(r) => Lwt.wakeup_later(r, c)
  | None => Queue.push(c, pool.queue)
  };
};

/**
  Check a member after [use] resulted in a failed computation and release it if
  it is still valid.
 */
let check_release = (pool, c) => {
  let* ok = pool.check(c);
  if (ok) {
    dispose(pool, c);
  } else {
    release(pool, c);
    Lwt.return_unit;
  };
};

let replace_disposed = pool =>
  switch (Lwt_dllist.take_opt_l(pool.waiters)) {
  | None => ()
  | Some(r) =>
    Lwt.on_any(
      Lwt.apply(pool.create, ()),
      c => Lwt.wakeup_later(r, c),
      exn => Lwt.wakeup_later_exn(r, exn),
    )
  };

/**
  Validate a member is still valid before using it.
 */
let validate_return = (pool, c) =>
  Lwt.try_bind(
    () => pool.validate(c),
    fun
    /* Validation ok; return. */
    | true => c |> Lwt.return
    /* Validation failed; create a new one. */
    | false => dispose(pool, c) >>= (() => create(pool)),
    /* Validation failed; create a new one if there is a waiter. */
    exn => {
      let* () = dispose(pool, c);
      replace_disposed(pool);
      Lwt.fail(exn);
    },
  );

/**
  Acquire an available pool member.
 */
let acquire = pool =>
  /* Try to take from the available queue. */
  switch (Queue.take_opt(pool.queue)) {
  /* Validate the available member. */
  | Some(c) => c |> validate_return(pool)
  | None =>
    /* No available members. */
    if (pool.count < pool.max) {
      /* Capacity available; create a fresh member. */
      create(pool);
    } else {
      /* Capacity reached; wait. */
      let (q, r) = Lwt.task();
      let node = Lwt_dllist.add_r(r, pool.waiters);
      Lwt.on_cancel(q, () => Lwt_dllist.remove(node));
      q >>= validate_return(pool);
    }
  };

let use = (pool, timeout, f) => {
  /* Acquire a member. */
  let* c = acquire(pool);

  /* Run [f] with the member and wrap a timeout. */
  let (q, c) = f(c);
  let q =
    Lwt.catch(
      () => q |> TimedLwt.wrap(timeout),
      /* If failure, check for validaty and release. */
      exn => check_release(pool, c) >>= (() => Lwt.fail(exn)),
    );

  let* x = q;
  switch (x) {
  /* Succeeded, release the member and return. */
  | Some(x) =>
    release(pool, c);
    x |> Lwt.return_some;
  /* Timed out; dispose of the member. */
  | None => dispose(pool, c) >>= (_ => Lwt.return_none)
  };
};

let add = pool =>
  /* If capacity available, create a fresh member and release it. */
  if (pool.count < pool.max) {
    create(pool) >|= release(pool);
  } else {
    Lwt.return_unit;
  };
