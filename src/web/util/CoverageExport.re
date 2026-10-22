open Js_of_ocaml;

/* (file name, contents) per JS context, named by one shared timestamp. */
let files = (main_snapshot: unit => option(string)): list((string, string)) => {
  let stamp = Printf.sprintf("%.0f", Util.JsUtil.timestamp());
  let name = context =>
    Printf.sprintf("hazel-%s-%s.coverage", stamp, context);
  let main =
    Option.to_list(main_snapshot()) |> List.map(c => (name("main"), c));
  let workers =
    WorkerClient.worker_coverage()
    |> List.mapi((i, c) => (name(Printf.sprintf("worker%d", i)), c));
  main @ workers;
};

let files_js = main_snapshot =>
  files(main_snapshot)
  |> List.map(((name, contents)) =>
       Js.Unsafe.obj([|
         ("name", Js.Unsafe.inject(Js.string(name))),
         ("contents", Js.Unsafe.inject(Js.string(contents))),
       |])
     )
  |> Array.of_list
  |> Js.array;

let save = main_snapshot =>
  List.iter(
    ((filename, contents)) =>
      Util.JsUtil.download_string_file(
        ~filename,
        ~content_type="application/octet-stream",
        ~contents,
      ),
    files(main_snapshot),
  );

let download =
  Option.map(
    (main_snapshot, ()) => save(main_snapshot),
    CoverageHook.snapshot,
  );

let install = () =>
  switch (CoverageHook.snapshot) {
  | Some(main_snapshot) =>
    Js.export(
      "hazelCoverage",
      Js.Unsafe.obj([|
        (
          "download",
          Js.Unsafe.inject(Js.wrap_callback(() => save(main_snapshot))),
        ),
        (
          "files",
          Js.Unsafe.inject(Js.wrap_callback(() => files_js(main_snapshot))),
        ),
      |]),
    )
  | None => ()
  };
