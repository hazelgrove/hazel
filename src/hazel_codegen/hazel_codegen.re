/* Compile-time codegen tool: converts .hz source files to .ml slide files.
      Used by dune rules to generate PersistentSegment.t records from plain text.

      Usage: node hazel_codegen.bc.js <name> <input.hz> <output.ml>
   */

let () = {
  let args = Sys.argv;
  if (Array.length(args) != 4) {
    prerr_endline("Usage: hazel_codegen <slide-name> <input.hz> <output.ml>");
    exit(1);
  };

  let name = args[1];
  let input_path = args[2];
  let output_path = args[3];

  let program = Core.In_channel.read_all(input_path);

  switch (Haz3lcore.Parser.to_zipper(program)) {
  | None =>
    prerr_endline("Failed to parse: " ++ input_path);
    exit(1);
  | Some(zipper) =>
    let persisted = Haz3lcore.PersistentSegment.persist(zipper);

    let ml_content =
      "let out : string * Haz3lcore.PersistentSegment.t =\n"
      ++ "  ( \""
      ++ String.escaped(name)
      ++ "\",\n"
      ++ "    {\n"
      ++ "      segment =\n"
      ++ "        \""
      ++ String.escaped(persisted.segment)
      ++ "\";\n"
      ++ "      backup_text =\n"
      ++ "        \""
      ++ String.escaped(persisted.backup_text)
      ++ "\";\n"
      ++ "      refractors =\n"
      ++ "        \""
      ++ String.escaped(persisted.refractors)
      ++ "\";\n"
      ++ "    } )\n";

    Core.Out_channel.write_all(output_path, ~data=ml_content);
  };
};
