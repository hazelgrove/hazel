open Lwt.Infix;

type t('a) = Lwt.t('a);

exception TimedOut;

let timer = timeout => {
  let (q', r) = Lwt.task();
  let _ =
    Js_of_ocaml.Dom_html.setTimeout(
      () => Lwt.wakeup_exn(r, TimedOut),
      float_of_int(timeout),
    );
  Lwt.catch(
    () => q',
    fun
    | TimedOut => Lwt.return_none
    | exn => Lwt.fail(exn),
  );
};

let wrap = (timeout, q) => Lwt.pick([q >|= Option.some, timer(timeout)]);
