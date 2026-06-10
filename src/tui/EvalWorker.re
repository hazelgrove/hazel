open Haz3lcore;

/* Asynchronous evaluation in a forked worker process — the native
   counterpart of the web's evaluation WebWorker. The parent's select
   loop watches the pipe; an edit kills the worker outright (SIGKILL),
   so the step limit only bounds background CPU burn, never UI latency.
   Results (display text + probe samples) come back as a sexp. */

type t = {
  pid: int,
  fd: Unix.file_descr,
};

type payload = (ResultView.t, Language.Dynamics.Map.t);

let sexp_of_payload = ((r, d): payload): Sexplib.Sexp.t =>
  List([ResultView.sexp_of_t(r), Language.Dynamics.Map.sexp_of_t(d)]);

let payload_of_sexp = (s: Sexplib.Sexp.t): payload =>
  switch (s) {
  | List([r, d]) => (
      ResultView.t_of_sexp(r),
      Language.Dynamics.Map.t_of_sexp(d),
    )
  | _ => raise(Invalid_argument("EvalWorker.payload_of_sexp"))
  };

let start = (statics: CachedStatics.t): t => {
  let (read_end, write_end) = Unix.pipe();
  switch (Unix.fork()) {
  | 0 =>
    /* child: evaluate and write the payload to the pipe. The child
       shares the parent's terminal, so silence stdout/stderr first
       (stray core print_endlines would corrupt the frame; the at_exit
       terminal-restore handler inherited from the parent goes to
       /dev/null too). */
    Unix.close(read_end);
    let devnull = Unix.openfile("/dev/null", [Unix.O_WRONLY], 0);
    Unix.dup2(devnull, Unix.stdout);
    Unix.dup2(devnull, Unix.stderr);
    let payload =
      switch (ResultView.run(statics)) {
      | p => p
      | exception exn => (
          ResultView.EvalErr("eval worker: " ++ Printexc.to_string(exn)),
          Language.Dynamics.Map.empty,
        )
      };
    let oc = Unix.out_channel_of_descr(write_end);
    output_string(oc, Sexplib.Sexp.to_string(sexp_of_payload(payload)));
    close_out(oc);
    exit(0);
  | pid =>
    Unix.close(write_end);
    {
      pid,
      fd: read_end,
    };
  };
};

let reap = (pid: int): unit =>
  switch (Unix.waitpid([], pid)) {
  | _ => ()
  | exception _ => ()
  };

/* Stop a worker whose result is no longer wanted (e.g. the program
   was edited): kill outright and clean up. */
let kill = (w: t): unit => {
  switch (Unix.kill(w.pid, Sys.sigkill)) {
  | () => ()
  | exception _ => ()
  };
  reap(w.pid);
  switch (Unix.close(w.fd)) {
  | () => ()
  | exception _ => ()
  };
};

/* Called when the worker's fd selects readable: drain the pipe, reap
   the child, parse. None if the worker died mid-write or wrote junk. */
let collect = (w: t): option(payload) => {
  let buf = Stdlib.Buffer.create(4096);
  let bytes = Bytes.create(65536);
  let rec drain = () =>
    switch (Unix.read(w.fd, bytes, 0, 65536)) {
    | 0 => ()
    | n =>
      Stdlib.Buffer.add_subbytes(buf, bytes, 0, n);
      drain();
    | exception (Unix.Unix_error(EINTR, _, _)) => drain()
    };
  drain();
  switch (Unix.close(w.fd)) {
  | () => ()
  | exception _ => ()
  };
  reap(w.pid);
  switch (Sexplib.Sexp.of_string(Stdlib.Buffer.contents(buf))) {
  | s =>
    switch (payload_of_sexp(s)) {
    | p => Some(p)
    | exception _ => None
    }
  | exception _ => None
  };
};
