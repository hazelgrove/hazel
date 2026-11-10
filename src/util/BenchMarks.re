/* (^report(46), ^ticket(694)) A timeline for a benchmark in the page: where an
   edit's time goes. When the page defines globalThis.hazelBenchMarks (an
   array), each mark is pushed onto it as {what, at, ms} (performance.now, in
   ms), with any extra fields; when it does not, nothing is timed and nothing
   is kept. The Firefox check (scripts/jit_slides.py --edit) sets it just
   before an edit and reads it once the picture has changed: each update and
   calculate, the statics, the view, the work after display, and each message
   to and from the evaluation worker. */
open Js_of_ocaml;

let target = () => {
  let a = Js.Unsafe.get(Js.Unsafe.global, "hazelBenchMarks");
  Js.Optdef.test(a) ? Some(a) : None;
};

let on = (): bool => target() != None;

let now = (): float =>
  Js.float_of_number(
    Js.Unsafe.meth_call(
      Js.Unsafe.get(Js.Unsafe.global, "performance"),
      "now",
      [||],
    ),
  );

/* (^report(46)) the epoch's ms at this page's (or worker's) performance.now 0:
   the worker's marks, made against its own clock, are placed on the page's
   by it */
let time_origin = (): float =>
  Js.float_of_number(
    Js.Unsafe.get(
      Js.Unsafe.get(Js.Unsafe.global, "performance"),
      "timeOrigin",
    ),
  );

let num = (f: float): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.number_of_float(f));
let str = (s: string): Js.Unsafe.any => Js.Unsafe.inject(Js.string(s));

let push = (a, fields: list((string, Js.Unsafe.any))): unit =>
  ignore(
    Js.Unsafe.meth_call(
      a,
      "push",
      [|Js.Unsafe.inject(Js.Unsafe.obj(Array.of_list(fields)))|],
    ),
  );

/* a moment ([at], or now), with no duration unless [extra] gives one */
let mark =
    (~at=?, ~extra: list((string, Js.Unsafe.any))=[], what: string): unit =>
  switch (target()) {
  | None => ()
  | Some(a) =>
    let at =
      switch (at) {
      | Some(t) => t
      | None => now()
      };
    push(a, [("what", str(what)), ("at", num(at)), ...extra]);
  };

/* f's duration, marked at its start; [extra] is asked for only when marking,
   after f, so it can read what f did */
let time =
    /* (unit => list((string, Js.Unsafe.any))) */
    (~extra=?, what: string, f: unit => 'r): 'r =>
  switch (target()) {
  | None => f()
  | Some(a) =>
    let t0 = now();
    let r = f();
    let t1 = now();
    let more =
      switch (extra) {
      | Some(e) => e()
      | None => []
      };
    push(
      a,
      [
        ("what", str(what)),
        ("at", num(t0)),
        ("ms", num(t1 -. t0)),
        ...more,
      ],
    );
    r;
  };
