open Js_of_ocaml;

type file = {
  name: string,
  contents: string,
};

/* One per JS context, named by one shared timestamp. */
let files = (main_snapshot: unit => option(string)): list(file) => {
  let stamp = Printf.sprintf("%.0f", Util.JsUtil.timestamp());
  let name = context =>
    Printf.sprintf("hazel-%s-%s.coverage", stamp, context);
  let main =
    Option.to_list(main_snapshot())
    |> List.map(~f=contents =>
         {
           name: name("main"),
           contents,
         }
       );
  let workers =
    WorkerClient.worker_coverage()
    |> List.mapi(~f=(i, contents) =>
         {
           name: name(Printf.sprintf("worker%d", i)),
           contents,
         }
       );
  main @ workers;
};

let files_js = main_snapshot =>
  files(main_snapshot)
  |> List.map(~f=({name, contents}) =>
       Js.Unsafe.obj([|
         ("name", Js.Unsafe.inject(Js.string(name))),
         ("contents", Js.Unsafe.inject(Js.string(contents))),
       |])
     )
  |> Array.of_list
  |> Js.array;

let save = main_snapshot =>
  List.iter(files(main_snapshot), ~f=({name, contents}) =>
    Util.JsUtil.download_string_file(
      ~filename=name,
      ~content_type="application/octet-stream",
      ~contents,
    )
  );

let download =
  Option.map(CoverageHook.snapshot, ~f=(main_snapshot, ()) =>
    save(main_snapshot)
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
