open Js_of_ocaml;

/* base64 (the embedded JS string stays unescaped); decoded by the
   platform's atob, present in browsers, workers and node */
let blob = [%blob "stdlib.blob"];

/* each step timed as a performance measure, for the boot benchmark */
let timed = (name: string, f: unit => 'a): 'a => {
  let perf = Js.Unsafe.global##.performance;
  let start = "stdlib:" ++ name ++ ":start";
  perf##mark(Js.string(start)) |> ignore;
  let r = f();
  perf##measure(Js.string("stdlib:" ++ name), Js.string(start)) |> ignore;
  r;
};

let install = () => {
  let raw =
    timed("atob", () =>
      Js.to_bytestring(Js.Unsafe.global##atob(Js.string(blob)))
    );
  let baked = timed("decode", () => Language.Library.decode(raw));
  Language.Library.install_baked(baked);
};

/* the definitions evaluated now rather than at the first evaluation: for
   the worker, which starts with the app and evaluates first thing */
let evaluate = () =>
  timed("eval", () => ignore(Language.Builtins.env_init()));
