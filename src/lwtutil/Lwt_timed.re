open Lwt.Infix;

module type TIMER = {
  let delay: (unit => unit, int) => unit;
};

module type S = {
  let wrap: (int, Lwt.t('a)) => Lwt.t(option('a));
};

exception TimedOut;
module Make = (T: TIMER) => {
  /**
    [timer timeout] is a promise that resolves with [None] after [timeout]
    milliseconds.
   */
  let timer = timeout => {
    let (q', r) = Lwt.task();
    let () = T.delay(() => Lwt.wakeup_exn(r, TimedOut), timeout);
    Lwt.catch(
      () => q',
      fun
      | TimedOut => Lwt.return_none
      | exn => Lwt.fail(exn),
    );
  };

  let wrap = (timeout, q) => Lwt.pick([q >|= Option.some, timer(timeout)]);
};
