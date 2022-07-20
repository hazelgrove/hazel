open Cmdliner;

open Grain;
open Rt;

let mds =
  [
    Hazel.Rt.Ast.impl_md,
    Hazel.Rt.AstMk.impl_md,
    Hazel.Rt.AstPrint.impl_md,
    Hazel.Rt.AstPrint.impl_md,
    Hazel.Rt.Sum.impl_md,
  ]
  |> List.filter_map(f => f());

let gen_md = (outdir, path, md) => {
  module Unix = Core.Unix;

  let path =
    switch (path) {
    | ImportRel(path)
    | ImportStd(path) => path
    };
  let path = Filename.concat(outdir, path ++ ".gr");
  let dir = Filename.dirname(path);

  /* Create output directory if it doesn't exist. */
  Unix.mkdir_p(dir);

  let contents = md |> Grain.print;

  /* Write the file. */
  let _ =
    Unix.with_file(
      path,
      ~mode=Unix.[O_WRONLY, O_CREAT],
      ~f=Unix.single_write_substring(~buf=contents),
    );

  /* Format the file. */
  let _ =
    Grain.Cli.Format.(
      Grain.Cli.make(~grain="grain")
      |> make(~source=path)
      |> with_output(path)
      |> to_command
      |> Grain.Cli.execute(~capture_stdout=false)
    );
  ();
};

let gen = outdir => {
  /* Generate files. */
  mds
  |> List.map(md => FileModule.(path(md), to_module(md)))
  |> List.iter(((path, md)) => gen_md(outdir, path, md));
};

let cmd = {
  /* Use install lib directory by default. */
  let lib_directory = Hazelcrt_files.Sites.lib |> List.hd;

  let outdir = {
    let doc = "Output directory.";
    let docv = "OUTDIR";
    Arg.(value & pos(0, string, lib_directory) & info([], ~docv, ~doc));
  };

  let doc = "hazelc runtime file generator.";
  let info = Cmd.info("gen", ~version="%%VERSION%%", ~doc);
  Cmd.v(info, Term.(const(gen) $ outdir));
};

let main = () => cmd |> Cmd.eval |> exit;
let () = main();
