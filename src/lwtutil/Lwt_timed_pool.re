/**
  Lwt pool with timeouts.

  Based off {{: https://github.com/ocsigen/lwt/blob/master/src/core/lwt_pool.ml} Lwt_pool}.
 */
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

module type S = {
  type t('a);

  let init:
    (
      ~max: int,
      ~create: create('a),
      ~dispose: dispose('a),
      ~check: check('a),
      ~validate: validate('a)
    ) =>
    t('a);

  let use: (t('a), int, 'a => (Lwt.t('b), 'a)) => Lwt.t(option('b));

  let add: t('a) => Lwt.t(bool);

  let fill: (t('a), int) => Lwt.t(unit);

  let clear: t('a) => Lwt.t(unit);
};

module Make = (Lwt_timed: Lwt_timed.S) => {
  type member('a) = (int, 'a);

  type t('a) = {
    /** Maximum size of the pool. */
    max: int,
    /** Size of the pool. */
    mutable count: int,
    /** Current latest member id. */
    mutable id: int,
    /** Flag indicating if members have been cleared out. */
    cleared: ref(ref(bool)),
    /** Queue of available members. */
    queue: Queue.t(member('a)),
    /** Resolvers waiting for an available member. */
    waiters: Lwt_dllist.t(Lwt.u(member('a))),
    create: create('a),
    dispose: dispose('a),
    check: check('a),
    validate: validate('a),
  };

  let init = (~max, ~create, ~dispose, ~check, ~validate) => {
    let count = 0;
    let id = 0;
    let cleared = ref(ref(false));
    let queue = Queue.create();
    let waiters = Lwt_dllist.create();
    {
      max,
      count,
      id,
      cleared,
      queue,
      waiters,
      create,
      dispose,
      check,
      validate,
    };
  };

  let create_replacement = pool => {
    pool.id = pool.id + 1;
    let+ c = pool.create();
    (pool.id, c);
  };

  /**
    Create a fresh pool member.
   */
  let create = pool =>
    Lwt.catch(
      () => {
        pool.count = pool.count + 1;
        create_replacement(pool);
      },
      exn => {
        pool.count = pool.count - 1;
        Lwt.fail(exn);
      },
    );

  /**
    Dispose of a pool member.
   */
  let dispose = (pool, (_id, c)) => {
    let* () = pool.dispose(c);
    pool.count = pool.count - 1;
    Lwt.return_unit;
  };

  /**
    Release a pool member.
   */
  let release = (pool, (id, c)) => {
    /* If there are waiters, fulfill the oldest; otherwise, restore back to
     * queue. */
    switch (Lwt_dllist.take_opt_l(pool.waiters)) {
    | Some(r) => Lwt.wakeup_later(r, (id, c))
    | None => Queue.push((id, c), pool.queue)
    };
  };

  /**
    Check a member after [use] resulted in a failed computation and release it if
    it is still valid.
   */
  let check_release = (pool, (id, c), cleared) => {
    let* ok = pool.check(c);
    if (cleared || !ok) {
      /* Member is not ok or pool was cleared; dispose. */
      dispose(
        pool,
        (id, c),
      );
    } else {
      /* Member is ok and pool was not cleared; release. */
      release(pool, (id, c));
      Lwt.return_unit;
    };
  };

  let replace_disposed = pool =>
    switch (Lwt_dllist.take_opt_l(pool.waiters)) {
    | None => ()
    | Some(r) =>
      Lwt.on_any(
        Lwt.apply(create_replacement, pool),
        c => Lwt.wakeup_later(r, c),
        exn => Lwt.wakeup_later_exn(r, exn),
      )
    };

  /**
    Validate a member is still valid before using it.
   */
  let validate_return = (pool, (id, c)) =>
    Lwt.try_bind(
      () => pool.validate(c),
      fun /* Validation ok; return. */
      | true => (id, c) |> Lwt.return /* Validation failed; create a new one. */
      | false => dispose(pool, (id, c)) >>= (() => create(pool)) /* Validation failed; create a new one if there is a waiter. */,
      exn => {
        let* () = dispose(pool, (id, c));
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
    | Some((id, c)) => (id, c) |> validate_return(pool)
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
    let* (id, c) = acquire(pool);
    let cleared = pool.cleared^ /* Run [f] with the member and wrap a timeout. */;

    let (q, c) = f(c);
    let q =
      Lwt.catch(
        () => q |> Lwt_timed.wrap(timeout) /* If failure, check for validaty and release. */,
        exn =>
          check_release(pool, (id, c), cleared^) >>= (() => Lwt.fail(exn)),
      );

    let* x = q;
    if (cleared^) {
      /* Pool was cleared while promise was resolving; dispose. */
      let* () = dispose(pool, (id, c));
      Lwt.return_none;
    } else {
      switch (x) {
      /* Succeeded, release the member and return. */
      | Some(x) =>
        release(pool, (id, c));
        x |> Lwt.return_some /* Timed out; dispose of the member. */;
      | None => dispose(pool, (id, c)) >>= (_ => Lwt.return_none)
      };
    };
  };

  let add = pool =>
    /* If capacity available, create a fresh member and release it. */
    if (pool.count < pool.max) {
      create(pool) >|= release(pool) >|= (() => true);
    } else {
      Lwt.return_false;
    };

  let fill = (pool, count) => {
    let rec fill =
      fun
      | 0 => Lwt.return_unit
      | n => {
          let* created = add(pool);
          if (created) {
            fill(n - 1);
          } else {
            Lwt.return_unit;
          };
        };

    fill(count);
  };

  let clear = pool => {
    let members = Queue.fold((ms, m) => [m, ...ms], [], pool.queue);
    Queue.clear(pool.queue) /* Honestly I don't really get this code ¯\_(ツ)_/¯. */;

    let old_cleared = pool.cleared^;
    old_cleared := true;
    pool.cleared := ref(false);

    Lwt_list.iter_s(dispose(pool), members);
  };
};
