/* Wall-clock time per test, printed when the run ends. The JUnit report
   records time="0" for every test, so this is the only way to see where the
   Full tests job spends its 45-75 minutes. Lines start with TIMING so they
   can be grepped out of a CI log. */

let times: ref(list((string, string, float))) = ref([]);

let wrap =
    (suite: list((string, list(Alcotest.test_case(unit)))))
    : list((string, list(Alcotest.test_case(unit)))) =>
  List.map(
    ((group, cases)) =>
      (
        group,
        List.map(
          ((name, speed, f)) =>
            (
              name,
              speed,
              () => {
                let t0 = Unix.gettimeofday();
                Fun.protect(
                  ~finally=
                    () =>
                      times :=
                        [(group, name, Unix.gettimeofday() -. t0), ...times^],
                  f,
                );
              },
            ),
          cases,
        ),
      ),
    suite,
  );

let report = (~top=40, ()) => {
  let all = times^;
  let total = List.fold_left((acc, (_, _, t)) => acc +. t, 0., all);
  let by_group = Hashtbl.create(128);
  List.iter(
    ((g, _, t)) => {
      let (n, s) =
        Option.value(Hashtbl.find_opt(by_group, g), ~default=(0, 0.));
      Hashtbl.replace(by_group, g, (n + 1, s +. t));
    },
    all,
  );
  let groups =
    Hashtbl.fold((g, (n, s), acc) => [(g, n, s), ...acc], by_group, [])
    |> List.sort(((_, _, a), (_, _, b)) => compare(b, a));
  Printf.eprintf(
    "\nTIMING total %.1fs over %d tests that ran\n",
    total,
    List.length(all),
  );
  List.iter(
    ((g, n, s)) =>
      Printf.eprintf(
        "TIMING group %8.1fs %5.1f%% %5d tests  %s\n",
        s,
        100. *. s /. max(total, 1e-9),
        n,
        g,
      ),
    groups,
  );
  List.sort(((_, _, a), (_, _, b)) => compare(b, a), all)
  |> List.iteri((i, (g, name, t)) =>
       if (i < top) {
         Printf.eprintf("TIMING test %8.1fs  %s :: %s\n", t, g, name);
       }
     );
  flush(stderr);
};
