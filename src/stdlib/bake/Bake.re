/* Bakes the Hazel-source standard library (plans/stdlib.md §2) at build
   time; LibraryBake does the work. usage: bake <blob> <text> <file.hz>...
   writes base64 Marshal of Library.baked, and the concatenated source the
   app shows as the library document */
open Haz3lcore;

let read = path => In_channel.with_open_bin(path, In_channel.input_all);

let write_base64 = (path, v) =>
  Out_channel.with_open_bin(path, oc =>
    Out_channel.output_string(
      oc,
      Js_of_ocaml.Js.to_string(
        Js_of_ocaml.Js.Unsafe.global##btoa(
          Js_of_ocaml.Js.bytestring(Marshal.to_string(v, [])),
        ),
      ),
    )
  );

let () =
  switch (Array.to_list(Sys.argv)) {
  | [_, out, text_out, ...files] when files != [] =>
    let text =
      files
      |> List.map(read)
      |> List.map(String.trim)
      |> String.concat(";\n");
    switch (LibraryBake.of_text(text)) {
    | Error(msg) =>
      prerr_endline("bake: " ++ msg);
      exit(1);
    | Ok((baked, segment)) =>
      let kb = v =>
        Printf.sprintf(
          "%dKB",
          String.length(Marshal.to_string(v, [])) / 1024,
        );
      prerr_endline(
        Printf.sprintf(
          "bake: entries %dKB, definitions %dKB; segment %s",
          String.length(baked.entries) / 1024,
          Array.fold_left(
            (n, d: Language.Library.def) => n + String.length(d.elab),
            0,
            baked.defs,
          )
          / 1024,
          kb(segment),
        ),
      );
      write_base64(out, baked);
      Out_channel.with_open_bin(text_out, oc =>
        Out_channel.output_string(oc, text)
      );
    };
  | _ =>
    prerr_endline("usage: bake <blob> <text> <file.hz>...");
    exit(2);
  };
