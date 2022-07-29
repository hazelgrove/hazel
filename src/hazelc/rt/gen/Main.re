open Cmdliner;

open Grain;

let fmodls = Modules.list;

let gen_ = (outdir, fmodl) => {
  module Unix = Core.Unix;

  let outdir = outdir |> Path.v;
  let path =
    fmodl
    |> FileModule.Full.path
    |> Path.append(outdir)
    |> Path.add_ext("gr");
  let dir = path |> Path.split_base |> fst |> Path.to_string;
  let path = path |> Path.to_string;

  let modl = fmodl |> FileModule.Full.modl;
  let contents = modl |> Grain.print;

  /* Create output directory if it doesn't exist. */
  Unix.mkdir_p(dir);

  /* Write the file. */
  let _ =
    Unix.with_file(
      path,
      ~mode=Unix.[O_WRONLY, O_CREAT],
      ~f=Unix.single_write_substring(~buf=contents),
    );

  /* Format the file. */
  let grain = Grain.Cli.make(~grain="grain");
  let _ =
    Grain.Cli.Format.(
      grain
      |> make(~source=path)
      |> with_output(path)
      |> to_command
      |> Grain.Cli.execute(~capture_stdout=false)
    );

  ();
};

let gen = outdir => {
  /* Generate files. */
  fmodls |> List.iter(fmodl => gen_(outdir, fmodl));
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
