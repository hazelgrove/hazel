open Tezt;
open Tezt.Base;

open Lwtutil.Lwt_observable;

let register_regression = (title, tags, f) =>
  Regression.register(
    ~__FILE__,
    ~title,
    ~tags=["hazelweb", "lwtobservable"] @ tags,
    ~output_file=title,
    () => {
      let buf = Buffer.create(100);
      buf |> f;
      Regression.capture(Buffer.contents(buf));
      unit;
    },
  );

let observer = next => {next, complete: () => ()};

let () =
  register_regression(
    "single observer",
    [],
    buf => {
      let o = create();
      let _ = subscribe(o, observer(Buffer.add_int16_le(buf)));

      next(o, 0);
      next(o, 1);
      next(o, 2);
    },
  );

let () = Test.run();
