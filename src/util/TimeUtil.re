let print_time_prefix =
    (name: string, start_time: float, end_time: float): unit => {
  Printf.printf(
    "%-32s%4.0fms",
    name ++ ":",
    1000.0 *. (end_time -. start_time),
  );
};

let print_time = (name: string, start_time: float, end_time: float): unit => {
  print_time_prefix(name, start_time, end_time);
  Printf.printf("\n%!");
};

let print_times =
    (name: string, start_time: float, times: list((string, float))): unit => {
  assert(!List.is_empty(times));
  let end_time =
    List.fold_left(~f=(_, last) => snd(last), ~init=-1.0, times);
  print_time_prefix(name, start_time, end_time);
  Printf.printf(" [");
  let rec go = (previous_time: float, times: list((string, float))): unit => {
    switch (times) {
    | [] => ()
    | [(name, time), ...times] =>
      Printf.printf(" %s: %.0fms", name, 1000.0 *. (time -. previous_time));
      go(time, times);
    };
  };
  go(start_time, times);
  Printf.printf(" ]\n%!");
};

let measure_time = (name: string, measure: bool, f: unit => 'a): 'a =>
  if (measure) {
    let start_time =
      Time_float.now()
      |> Time_float.to_span_since_epoch
      |> Time_float.Span.to_sec;
    let x = f();
    let end_time =
      Time_float.now()
      |> Time_float.to_span_since_epoch
      |> Time_float.Span.to_sec;
    print_time(name, start_time, end_time);
    x;
  } else {
    f();
  };

let format_time_diff = (prior: float): string => {
  open Float;
  let now = JsUtil.timestamp();
  let diff_seconds = (now -. prior) /. 1000.0;
  let diff_mins = round_down(diff_seconds /. 60.0);
  let diff_hours = round_down(diff_mins /. 60.0);
  let diff_days = round_down(diff_hours /. 24.0);

  if (diff_mins < 1.0) {
    Stdlib.Printf.sprintf("<1 min ago");
  } else if (diff_mins < 60.0) {
    diff_mins < 2.0
      ? Stdlib.Printf.sprintf("%.0f min ago", diff_mins)
      : Stdlib.Printf.sprintf("%.0f mins ago", diff_mins);
  } else if (diff_hours < 24.0) {
    diff_hours < 2.0
      ? Stdlib.Printf.sprintf("%.0f hour ago", diff_hours)
      : Stdlib.Printf.sprintf("%.0f hours ago", diff_hours);
  } else {
    diff_days < 2.0
      ? Stdlib.Printf.sprintf("%.0f day ago", diff_days)
      : Stdlib.Printf.sprintf("%.0f days ago", diff_days);
  };
};
