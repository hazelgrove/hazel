let print_time_prefix: (string, float, float) => unit;

let print_time: (string, float, float) => unit;

let print_times: (string, float, list((string, float))) => unit;

let measure_time: (string, bool, unit => 'a) => 'a;
