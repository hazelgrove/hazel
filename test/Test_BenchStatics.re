open Alcotest;
open Haz3lcore;
open Language;

/* Informational statics timing over the bench corpus
   (hazel-programs/bench). Always passes; timings print to the log.
   Run: bash test/run_node.sh test 'BenchStatics' */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

let time_statics = (src: string): option(float) =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | None => None
  | Some(seg) =>
    let term = MakeTerm.go(seg).term;
    let t0 = Sys.time();
    let _ =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      );
    Some((Sys.time() -. t0) *. 1000.);
  };

let tests = (
  "BenchStatics",
  [
    test_case(
      "corpus statics timing (informational)",
      `Quick,
      () =>
        List.iter(
          name => {
            let path = "hazel-programs/bench/" ++ name;
            let path =
              Sys.file_exists(path) ? path : "../hazel-programs/bench/" ++ name;
            switch (read_file(path)) {
            | None => Printf.printf("BENCHSTATICS %s: <unreadable>\n", name)
            | Some(src) =>
              switch (time_statics(src)) {
              | Some(ms) =>
                Printf.printf(
                  "BENCHSTATICS %s (%d lines): %.0fms\n",
                  name,
                  List.length(String.split_on_char('\n', src)),
                  ms,
                )
              | None => Printf.printf("BENCHSTATICS %s: <no parse>\n", name)
              }
            };
          },
          ["bench-1k.hz", "bench-2k5.hz", "bench-5k.hz"],
        ),
    ),
  ],
);
