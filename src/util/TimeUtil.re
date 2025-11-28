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
  assert(times != []);
  let end_time = List.fold_left((_, last) => snd(last), -1.0, times);
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
    let start_time = Sys.time();
    let x = f();
    let end_time = Sys.time();
    print_time(name, start_time, end_time);
    x;
  } else {
    f();
  };

let format_time_diff = (prior: float): string => {
  let now = JsUtil.timestamp();
  let diff_seconds = (now -. prior) /. 1000.0;
  let diff_mins = floor(diff_seconds /. 60.0);
  let diff_hours = floor(diff_mins /. 60.0);
  let diff_days = floor(diff_hours /. 24.0);

  if (diff_mins < 1.0) {
    Printf.sprintf("<1 min ago");
  } else if (diff_mins < 60.0) {
    diff_mins < 2.0
      ? Printf.sprintf("%.0f min ago", diff_mins)
      : Printf.sprintf("%.0f mins ago", diff_mins);
  } else if (diff_hours < 24.0) {
    diff_hours < 2.0
      ? Printf.sprintf("%.0f hour ago", diff_hours)
      : Printf.sprintf("%.0f hours ago", diff_hours);
  } else {
    diff_days < 2.0
      ? Printf.sprintf("%.0f day ago", diff_days)
      : Printf.sprintf("%.0f days ago", diff_days);
  };
};

let format_duration_ms = (ms: float): string => {
  /*
     ms is in milliseconds; convert to human-friendly string
     Examples:
       <1000 -> "<1s"
       1500 -> "1.5s"
       62000 -> "1m 2s"
       3660000 -> "1h 1m 0s"
   */
  let total_seconds = ms /. 1000.0;
  if (total_seconds < 1.0) {
    "<1s";
  } else if (total_seconds < 60.0) {
    if (total_seconds < 10.0) {
      Printf.sprintf("%.1fs", total_seconds);
    } else {
      Printf.sprintf("%.0fs", total_seconds);
    };
  } else if (total_seconds < 3600.0) {
    let mins = floor(total_seconds /. 60.0) |> int_of_float;
    let secs =
      int_of_float(floor(total_seconds -. float_of_int(mins) *. 60.0));
    Printf.sprintf("%dm %ds", mins, secs);
  } else {
    let hours = floor(total_seconds /. 3600.0) |> int_of_float;
    let remainder = total_seconds -. float_of_int(hours) *. 3600.0;
    let mins = floor(remainder /. 60.0) |> int_of_float;
    let secs = int_of_float(floor(remainder -. float_of_int(mins) *. 60.0));
    Printf.sprintf("%dh %dm %ds", hours, mins, secs);
  };
};
