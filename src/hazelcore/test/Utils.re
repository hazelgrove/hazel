let run_n_times = (n: int, name: string, func, args) => {
  let start = Sys.time();
  for (_ in 1 to n) {
    let _ = func(args);
    ();
  };

  let elapsed = Sys.time() -. start;

  print_endline(
    "Elapsed time for test ("
    ++ name
    ++ ") over "
    ++ string_of_int(n)
    ++ " runs: "
    ++ string_of_float(elapsed)
    ++ " sec",
  );
};
