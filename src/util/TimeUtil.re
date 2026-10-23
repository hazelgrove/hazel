/* Run f and return how long it took (wall-clock via performance.now) alongside
   its result. */
let timed: 'a. (unit => 'a) => (Core.Time_ns.Span.t, 'a) =
  f => {
    let t0 = JsUtil.precise_timestamp();
    let x = f();
    (Core.Time_ns.Span.of_ms(JsUtil.precise_timestamp() -. t0), x);
  };

/* A duration for types that derive their converters. Core gives Time_ns.Span a
   pp and sexp converters but no yojson ones, so naming it here is what lets a
   deriving type reach a full set: a field of type TimeUtil.span resolves to the
   five below. Json is integer nanoseconds — Time_ns's own representation — as a
   bigint literal, since jsoo's int is 32-bit and 1.07s of nanoseconds would
   overflow it. */
type span = Core.Time_ns.Span.t;

let pp_span = Core.Time_ns.Span.pp;
let sexp_of_span = Core.Time_ns.Span.sexp_of_t;
let span_of_sexp = Core.Time_ns.Span.t_of_sexp;

let yojson_of_span = (s: span): Yojson.Safe.t =>
  `Intlit(Core.Int63.to_string(Core.Time_ns.Span.to_int63_ns(s)));

let span_of_yojson = (json: Yojson.Safe.t): span =>
  switch (json) {
  | `Intlit(ns) => Core.Time_ns.Span.of_int63_ns(Core.Int63.of_string(ns))
  | `Int(ns) => Core.Time_ns.Span.of_int63_ns(Core.Int63.of_int(ns))
  | _ => failwith("TimeUtil.span_of_yojson: expected integer nanoseconds")
  };

let format_time_diff = (prior: float): string => {
  open Float;
  let now = JsUtil.timestamp();
  let diff_seconds = (now -. prior) /. 1000.0;
  let diff_mins = round_down(diff_seconds /. 60.0);
  let diff_hours = round_down(diff_mins /. 60.0);
  let diff_days = round_down(diff_hours /. 24.0);

  if (diff_mins < 1.0) {
    sprintf("<1 min ago");
  } else if (diff_mins < 60.0) {
    diff_mins < 2.0
      ? sprintf("%.0f min ago", diff_mins)
      : sprintf("%.0f mins ago", diff_mins);
  } else if (diff_hours < 24.0) {
    diff_hours < 2.0
      ? sprintf("%.0f hour ago", diff_hours)
      : sprintf("%.0f hours ago", diff_hours);
  } else {
    diff_days < 2.0
      ? sprintf("%.0f day ago", diff_days)
      : sprintf("%.0f days ago", diff_days);
  };
};
