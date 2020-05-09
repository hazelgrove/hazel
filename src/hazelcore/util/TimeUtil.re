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
